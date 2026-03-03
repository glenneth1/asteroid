(defpackage #:cl-streamer/harmony
  (:use #:cl #:alexandria)
  (:local-nicknames (#:harmony #:org.shirakumo.fraf.harmony)
                    (#:mixed #:org.shirakumo.fraf.mixed))
  (:export #:audio-pipeline
           #:make-audio-pipeline
           #:start-pipeline
           #:stop-pipeline
           #:play-file
           #:pipeline-encoder
           #:pipeline-server
           #:make-streaming-server))

(in-package #:cl-streamer/harmony)

(defclass audio-pipeline ()
  ((harmony-server :initform nil :accessor pipeline-harmony-server)
   (encoder :initarg :encoder :accessor pipeline-encoder)
   (stream-server :initarg :stream-server :accessor pipeline-server)
   (mount-path :initarg :mount-path :accessor pipeline-mount-path :initform "/stream.mp3")
   (sample-rate :initarg :sample-rate :accessor pipeline-sample-rate :initform 44100)
   (channels :initarg :channels :accessor pipeline-channels :initform 2)
   (running :initform nil :accessor pipeline-running-p)
   (encode-thread :initform nil :accessor pipeline-encode-thread)))

(defun make-streaming-server (&key (name "CL-Streamer") (samplerate 44100) (latency 0.02))
  "Create a Harmony server configured for streaming (no audio output).
   Uses :dummy drain so audio goes to buffer instead of speakers."
  (mixed:init)
  (harmony:make-simple-server :name name
                              :samplerate samplerate
                              :latency latency
                              :drain :dummy
                              :output-channels 2))

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
  "Start the audio pipeline - initializes Harmony and begins encoding."
  (when (pipeline-running-p pipeline)
    (error "Pipeline already running"))
  (let ((server (make-streaming-server :samplerate (pipeline-sample-rate pipeline))))
    (setf (pipeline-harmony-server pipeline) server)
    (mixed:start server))
  (setf (pipeline-running-p pipeline) t)
  (setf (pipeline-encode-thread pipeline)
        (bt:make-thread (lambda () (encode-loop pipeline))
                        :name "cl-streamer-encode"))
  (log:info "Audio pipeline started")
  pipeline)

(defun stop-pipeline (pipeline)
  "Stop the audio pipeline."
  (setf (pipeline-running-p pipeline) nil)
  (when (pipeline-encode-thread pipeline)
    (ignore-errors (bt:join-thread (pipeline-encode-thread pipeline)))
    (setf (pipeline-encode-thread pipeline) nil))
  (when (pipeline-harmony-server pipeline)
    (mixed:end (pipeline-harmony-server pipeline))
    (setf (pipeline-harmony-server pipeline) nil))
  (log:info "Audio pipeline stopped")
  pipeline)

(defun play-file (pipeline file-path &key (mixer :music))
  "Play an audio file through the pipeline.
   The file will be decoded by Harmony and encoded for streaming."
  (let* ((server (pipeline-harmony-server pipeline))
         (harmony:*server* server))
    (let ((voice (harmony:play file-path :mixer mixer)))
      (log:info "Playing: ~A" file-path)
      voice)))

(defun get-pack-buffer (pipeline)
  "Get the packer's pack (bip-buffer) from Harmony's output chain."
  (let* ((server (pipeline-harmony-server pipeline))
         (output (harmony:segment :output server))
         (packer (harmony:segment :packer output)))
    (mixed:pack packer)))

(defun encode-loop (pipeline)
  "Main encoding loop - reads PCM from Harmony's packer, encodes, writes to stream."
  (let ((encoder (pipeline-encoder pipeline))
        (mount-path (pipeline-mount-path pipeline))
        (channels (pipeline-channels pipeline))
        (frame-size 1152))
    (loop while (pipeline-running-p pipeline)
          do (handler-case
                 (let* ((pack (get-pack-buffer pipeline))
                        (available (mixed:available-read pack))
                        (needed (* frame-size channels 2)))
                   (if (>= available needed)
                       (multiple-value-bind (data start size)
                           (mixed:request-read pack needed)
                         (declare (ignore start))
                         (when (and data (>= size needed))
                           (let* ((samples (floor size (* channels 2)))
                                  (pcm-buffer (make-array (* samples channels)
                                                          :element-type '(signed-byte 16))))
                             (loop for i below (* samples channels)
                                   do (setf (aref pcm-buffer i)
                                            (let ((byte-offset (* i 2)))
                                              (logior (aref data byte-offset)
                                                      (ash (let ((hi (aref data (1+ byte-offset))))
                                                             (if (> hi 127) (- hi 256) hi))
                                                           8)))))
                             (mixed:finish-read pack size)
                             (let ((mp3-data (cl-streamer::encode-pcm-interleaved
                                              encoder pcm-buffer samples)))
                               (when (> (length mp3-data) 0)
                                 (cl-streamer::write-audio-data mount-path mp3-data))))))
                       (sleep 0.005)))
               (error (e)
                 (log:warn "Encode error: ~A" e)
                 (sleep 0.1))))))
