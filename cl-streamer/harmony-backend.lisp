(defpackage #:cl-streamer/harmony
  (:use #:cl #:alexandria)
  (:local-nicknames (#:harmony #:org.shirakumo.fraf.harmony)
                    (#:mixed #:org.shirakumo.fraf.mixed))
  (:export #:audio-pipeline
           #:make-audio-pipeline
           #:start-pipeline
           #:stop-pipeline
           #:play-file
           #:play-list
           #:pipeline-encoder
           #:pipeline-server
           #:make-streaming-server))

(in-package #:cl-streamer/harmony)

;;; ---- Streaming Drain ----
;;; Custom drain that captures PCM from Harmony's pack buffer
;;; and feeds it to the encoder/stream server, replacing the
;;; dummy drain which just discards audio data.

(defclass streaming-drain (mixed:drain)
  ((encoder :initarg :encoder :accessor drain-encoder)
   (mount-path :initarg :mount-path :accessor drain-mount-path :initform "/stream.mp3")
   (channels :initarg :channels :accessor drain-channels :initform 2)))

(defmethod mixed:free ((drain streaming-drain)))

(defmethod mixed:start ((drain streaming-drain)))

(defmethod mixed:mix ((drain streaming-drain))
  "Read interleaved float PCM from the pack buffer, encode to MP3, write to stream.
   The pack buffer is (unsigned-byte 8) with IEEE 754 single-floats (4 bytes each).
   Layout: L0b0 L0b1 L0b2 L0b3 R0b0 R0b1 R0b2 R0b3 L1b0 ... (interleaved stereo)"
  (mixed:with-buffer-tx (data start size (mixed:pack drain))
    (when (> size 0)
      (let* ((channels (drain-channels drain))
             (bytes-per-sample 4)  ; single-float = 4 bytes
             (total-floats (floor size bytes-per-sample))
             (num-samples (floor total-floats channels))
             (pcm-buffer (make-array (* num-samples channels)
                                     :element-type '(signed-byte 16))))
        ;; Convert raw bytes -> single-float -> signed-16
        (cffi:with-pointer-to-vector-data (ptr data)
          (loop for i below (* num-samples channels)
                for byte-offset = (+ start (* i bytes-per-sample))
                for sample = (cffi:mem-ref ptr :float byte-offset)
                do (setf (aref pcm-buffer i) (float-to-s16 sample))))
        (handler-case
            (let ((mp3-data (cl-streamer:encode-pcm-interleaved
                             (drain-encoder drain) pcm-buffer num-samples)))
              (when (> (length mp3-data) 0)
                (cl-streamer:write-audio-data (drain-mount-path drain) mp3-data)))
          (error (e)
            (log:warn "Encode error in drain: ~A" e)))))
    ;; Sleep for the duration of audio we just processed
    ;; size = bytes, each frame = channels * 4 bytes (single-float)
    (let* ((channels (drain-channels drain))
           (bytes-per-frame (* channels 4))
           (frames (floor size bytes-per-frame))
           (samplerate (mixed:samplerate (mixed:pack drain))))
      (when (> frames 0)
        (sleep (/ frames samplerate))))
    (mixed:finish size)))

(defmethod mixed:end ((drain streaming-drain)))

;;; ---- Audio Pipeline ----

(defclass audio-pipeline ()
  ((harmony-server :initform nil :accessor pipeline-harmony-server)
   (encoder :initarg :encoder :accessor pipeline-encoder)
   (stream-server :initarg :stream-server :accessor pipeline-server)
   (mount-path :initarg :mount-path :accessor pipeline-mount-path :initform "/stream.mp3")
   (sample-rate :initarg :sample-rate :accessor pipeline-sample-rate :initform 44100)
   (channels :initarg :channels :accessor pipeline-channels :initform 2)
   (running :initform nil :accessor pipeline-running-p)))

(defun make-audio-pipeline (&key encoder stream-server (mount-path "/stream.mp3")
                                 (sample-rate 44100) (channels 2))
  "Create an audio pipeline connecting Harmony to the stream server via an encoder."
  (make-instance 'audio-pipeline
                 :encoder encoder
                 :stream-server stream-server
                 :mount-path mount-path
                 :sample-rate sample-rate
                 :channels channels))

(defun start-pipeline (pipeline)
  "Start the audio pipeline - initializes Harmony with our streaming drain."
  (when (pipeline-running-p pipeline)
    (error "Pipeline already running"))
  (mixed:init)
  (let* ((server (harmony:make-simple-server
                  :name "CL-Streamer"
                  :samplerate (pipeline-sample-rate pipeline)
                  :latency 0.05
                  :drain :dummy
                  :output-channels (pipeline-channels pipeline)))
         (output (harmony:segment :output server))
         (old-drain (harmony:segment :drain output))
         (pack (mixed:pack old-drain))
         (drain (make-instance 'streaming-drain
                               :encoder (pipeline-encoder pipeline)
                               :mount-path (pipeline-mount-path pipeline)
                               :channels (pipeline-channels pipeline))))
    ;; Wire our streaming drain to the same pack buffer
    (setf (mixed:pack drain) pack)
    ;; Swap: withdraw old dummy drain, add our streaming drain
    (mixed:withdraw old-drain output)
    (mixed:add drain output)
    (setf (pipeline-harmony-server pipeline) server)
    (mixed:start server))
  (setf (pipeline-running-p pipeline) t)
  (log:info "Audio pipeline started with streaming drain")
  pipeline)

(defun stop-pipeline (pipeline)
  "Stop the audio pipeline."
  (setf (pipeline-running-p pipeline) nil)
  (when (pipeline-harmony-server pipeline)
    (mixed:end (pipeline-harmony-server pipeline))
    (setf (pipeline-harmony-server pipeline) nil))
  (log:info "Audio pipeline stopped")
  pipeline)

(defun play-file (pipeline file-path &key (mixer :music) title (on-end :free))
  "Play an audio file through the pipeline.
   The file will be decoded by Harmony and encoded for streaming.
   If TITLE is given, update ICY metadata with it.
   FILE-PATH can be a string or pathname.
   ON-END is passed to harmony:play (default :free)."
  (let* ((path (pathname file-path))
         (server (pipeline-harmony-server pipeline))
         (harmony:*server* server)
         (display-title (or title (pathname-name path))))
    ;; Update ICY metadata so listeners see the track name
    (cl-streamer:set-now-playing (pipeline-mount-path pipeline) display-title)
    (let ((voice (harmony:play path :mixer mixer :on-end on-end)))
      (log:info "Now playing: ~A" display-title)
      voice)))

(defun play-list (pipeline file-list &key (gap 0.5))
  "Play a list of file paths sequentially through the pipeline.
   Each entry can be a string (path) or a plist (:file path :title title).
   GAP is seconds of silence between tracks."
  (bt:make-thread
   (lambda ()
     (loop for entry in file-list
           while (pipeline-running-p pipeline)
           do (multiple-value-bind (path title)
                  (if (listp entry)
                      (values (getf entry :file) (getf entry :title))
                      (values entry nil))
                (handler-case
                    (let* ((done-lock (bt:make-lock "track-done"))
                           (done-cv (bt:make-condition-variable :name "track-done"))
                           (done-p nil)
                           (server (pipeline-harmony-server pipeline))
                           (harmony:*server* server)
                           (voice (play-file pipeline path
                                             :title title
                                             :on-end (lambda (voice)
                                                       (declare (ignore voice))
                                                       (bt:with-lock-held (done-lock)
                                                         (setf done-p t)
                                                         (bt:condition-notify done-cv))))))
                      (declare (ignore voice))
                      ;; Wait for the track to finish via callback
                      (bt:with-lock-held (done-lock)
                        (loop until (or done-p (not (pipeline-running-p pipeline)))
                              do (bt:condition-wait done-cv done-lock)))
                      (when (> gap 0) (sleep gap)))
                  (error (e)
                    (log:warn "Error playing ~A: ~A" path e)
                    (sleep 1))))))
   :name "cl-streamer-playlist"))

(declaim (inline float-to-s16))
(defun float-to-s16 (sample)
  "Convert a float sample (-1.0 to 1.0) to signed 16-bit integer."
  (let ((clamped (max -1.0 (min 1.0 sample))))
    (the (signed-byte 16) (round (* clamped 32767.0)))))
