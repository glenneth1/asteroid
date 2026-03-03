(defpackage #:cl-streamer/harmony
  (:use #:cl #:alexandria)
  (:local-nicknames (#:harmony #:org.shirakumo.fraf.harmony)
                    (#:mixed #:org.shirakumo.fraf.mixed))
  (:export #:audio-pipeline
           #:make-audio-pipeline
           #:add-pipeline-output
           #:start-pipeline
           #:stop-pipeline
           #:play-file
           #:play-list
           #:pipeline-server
           #:make-streaming-server))

(in-package #:cl-streamer/harmony)

;;; ---- Streaming Drain ----
;;; Custom drain that captures PCM from Harmony's pack buffer
;;; and feeds it to the encoder/stream server, replacing the
;;; dummy drain which just discards audio data.

(defclass streaming-drain (mixed:drain)
  ((outputs :initarg :outputs :accessor drain-outputs :initform nil
            :documentation "List of (encoder . mount-path) pairs")
   (channels :initarg :channels :accessor drain-channels :initform 2)))

(defun drain-add-output (drain encoder mount-path)
  "Add an encoder/mount pair to the drain."
  (push (cons encoder mount-path) (drain-outputs drain)))

(defun drain-remove-output (drain mount-path)
  "Remove an encoder/mount pair by mount path."
  (setf (drain-outputs drain)
        (remove mount-path (drain-outputs drain) :key #'cdr :test #'string=)))

(defmethod mixed:free ((drain streaming-drain)))

(defmethod mixed:start ((drain streaming-drain)))

(defmethod mixed:mix ((drain streaming-drain))
  "Read interleaved float PCM from the pack buffer, encode to all outputs.
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
        ;; Feed PCM to all encoder/mount pairs
        (dolist (output (drain-outputs drain))
          (let ((encoder (car output))
                (mount-path (cdr output)))
            (handler-case
                (let ((encoded (encode-for-output encoder pcm-buffer num-samples)))
                  (when (> (length encoded) 0)
                    (cl-streamer:write-audio-data mount-path encoded)))
              (error (e)
                (log:warn "Encode error for ~A: ~A" mount-path e)))))))
    ;; Sleep for most of the audio duration (leave headroom for encoding)
    (let* ((channels (drain-channels drain))
           (bytes-per-frame (* channels 4))
           (frames (floor size bytes-per-frame))
           (samplerate (mixed:samplerate (mixed:pack drain))))
      (when (> frames 0)
        (sleep (* 0.9 (/ frames samplerate)))))
    (mixed:finish size)))

(defgeneric encode-for-output (encoder pcm-buffer num-samples)
  (:documentation "Encode PCM samples using the given encoder. Returns byte vector."))

(defmethod encode-for-output ((encoder cl-streamer::mp3-encoder) pcm-buffer num-samples)
  (cl-streamer:encode-pcm-interleaved encoder pcm-buffer num-samples))

(defmethod encode-for-output ((encoder cl-streamer::aac-encoder) pcm-buffer num-samples)
  (cl-streamer:encode-aac-pcm encoder pcm-buffer num-samples))

(defmethod mixed:end ((drain streaming-drain)))

;;; ---- Audio Pipeline ----

(defclass audio-pipeline ()
  ((harmony-server :initform nil :accessor pipeline-harmony-server)
   (drain :initform nil :accessor pipeline-drain)
   (stream-server :initarg :stream-server :accessor pipeline-server)
   (mount-path :initarg :mount-path :accessor pipeline-mount-path :initform "/stream.mp3")
   (sample-rate :initarg :sample-rate :accessor pipeline-sample-rate :initform 44100)
   (channels :initarg :channels :accessor pipeline-channels :initform 2)
   (running :initform nil :accessor pipeline-running-p)))

(defun make-audio-pipeline (&key encoder stream-server (mount-path "/stream.mp3")
                                 (sample-rate 44100) (channels 2))
  "Create an audio pipeline connecting Harmony to the stream server via an encoder.
   The initial encoder/mount-path pair is added as the first output.
   Additional outputs can be added with add-pipeline-output."
  (let ((pipeline (make-instance 'audio-pipeline
                                 :stream-server stream-server
                                 :mount-path mount-path
                                 :sample-rate sample-rate
                                 :channels channels)))
    (when encoder
      (setf (slot-value pipeline 'drain)
            (make-instance 'streaming-drain :channels channels))
      (drain-add-output (pipeline-drain pipeline) encoder mount-path))
    pipeline))

(defun add-pipeline-output (pipeline encoder mount-path)
  "Add an additional encoder/mount output to the pipeline.
   Can be called before or after start-pipeline."
  (unless (pipeline-drain pipeline)
    (setf (pipeline-drain pipeline)
          (make-instance 'streaming-drain :channels (pipeline-channels pipeline))))
  (drain-add-output (pipeline-drain pipeline) encoder mount-path))

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
         (drain (pipeline-drain pipeline)))
    ;; Wire our streaming drain to the same pack buffer
    (setf (mixed:pack drain) pack)
    ;; Swap: withdraw old dummy drain, add our streaming drain
    (mixed:withdraw old-drain output)
    (mixed:add drain output)
    (setf (pipeline-harmony-server pipeline) server)
    (mixed:start server))
  (setf (pipeline-running-p pipeline) t)
  (log:info "Audio pipeline started with streaming drain (~A outputs)"
            (length (drain-outputs (pipeline-drain pipeline))))
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

(defun voice-remaining-seconds (voice)
  "Return estimated seconds remaining for a voice, or NIL if unknown."
  (handler-case
      (let ((pos (mixed:frame-position voice))
            (total (mixed:frame-count voice))
            (sr (mixed:samplerate voice)))
        (when (and pos total sr (> total 0) (> sr 0))
          (/ (- total pos) sr)))
    (error () nil)))

(defun volume-ramp (voice target-volume duration &key (steps 20))
  "Smoothly ramp a voice's volume to TARGET-VOLUME over DURATION seconds.
   Runs in the calling thread (blocks for DURATION seconds)."
  (let* ((start-volume (mixed:volume voice))
         (delta (- target-volume start-volume))
         (step-time (/ duration steps)))
    (loop for i from 1 to steps
          for fraction = (/ i steps)
          for vol = (+ start-volume (* delta fraction))
          do (setf (mixed:volume voice) (max 0.0 (min 1.0 (float vol))))
             (sleep step-time))))

(defun play-list (pipeline file-list &key (crossfade-duration 3.0)
                                          (fade-in 2.0)
                                          (fade-out 2.0))
  "Play a list of file paths sequentially through the pipeline.
   Each entry can be a string (path) or a plist (:file path :title title).
   CROSSFADE-DURATION is how early to start the next track (seconds).
   FADE-IN/FADE-OUT control the volume ramp durations.
   Both voices play simultaneously through the mixer during crossfade."
  (bt:make-thread
   (lambda ()
     (let ((prev-voice nil))
       (loop for entry in file-list
             for idx from 0
             while (pipeline-running-p pipeline)
             do (multiple-value-bind (path title)
                    (if (listp entry)
                        (values (getf entry :file) (getf entry :title))
                        (values entry nil))
                  (handler-case
                      (let* ((server (pipeline-harmony-server pipeline))
                             (harmony:*server* server)
                             (voice (play-file pipeline path :title title
                                                :on-end :disconnect)))
                        (when voice
                          ;; If this isn't the first track, fade in from 0
                          (when (and prev-voice (> idx 0))
                            (setf (mixed:volume voice) 0.0)
                            ;; Fade in new voice and fade out old voice in parallel
                            (let ((fade-thread
                                    (bt:make-thread
                                     (lambda ()
                                       (volume-ramp prev-voice 0.0 fade-out)
                                       (harmony:stop prev-voice))
                                     :name "cl-streamer-fadeout")))
                              (volume-ramp voice 1.0 fade-in)
                              (bt:join-thread fade-thread)))
                          ;; Wait for track to approach its end
                          (sleep 0.5) ; let decoder start
                          (loop while (and (pipeline-running-p pipeline)
                                           (not (mixed:done-p voice)))
                                for remaining = (voice-remaining-seconds voice)
                                ;; Start crossfade when we're within crossfade-duration of the end
                                when (and remaining
                                          (<= remaining crossfade-duration)
                                          (not (mixed:done-p voice)))
                                  do (setf prev-voice voice)
                                     (return)  ; break out to start next track
                                do (sleep 0.1))
                          ;; If track ended naturally (no crossfade), clean up
                          (when (mixed:done-p voice)
                            (harmony:stop voice)
                            (setf prev-voice nil))))
                    (error (e)
                      (log:warn "Error playing ~A: ~A" path e)
                      (sleep 1)))))
       ;; Clean up last voice
       (when prev-voice
         (let ((harmony:*server* (pipeline-harmony-server pipeline)))
           (volume-ramp prev-voice 0.0 fade-out)
           (harmony:stop prev-voice)))))
   :name "cl-streamer-playlist"))

(declaim (inline float-to-s16))
(defun float-to-s16 (sample)
  "Convert a float sample (-1.0 to 1.0) to signed 16-bit integer."
  (let ((clamped (max -1.0 (min 1.0 sample))))
    (the (signed-byte 16) (round (* clamped 32767.0)))))
