;;; End-to-end streaming test
;;; Usage: sbcl --load test-stream.lisp
;;;
;;; Then open http://localhost:8000/stream.mp3 in VLC or browser

(push #p"/home/glenn/SourceCode/harmony/" asdf:*central-registry*)
(push #p"/home/glenn/SourceCode/asteroid/cl-streamer/" asdf:*central-registry*)

(ql:quickload '(:cl-streamer :cl-streamer/encoder :cl-streamer/harmony))

(format t "~%=== CL-Streamer End-to-End Test ===~%")
(format t "LAME version: ~A~%" (cl-streamer::lame-version))

;; 1. Create and start stream server
(format t "~%[1] Starting stream server on port 8000...~%")
(cl-streamer:start :port 8000)

;; 2. Add mount point
(format t "[2] Adding mount point /stream.mp3...~%")
(cl-streamer:add-mount cl-streamer:*server* "/stream.mp3"
                       :content-type "audio/mpeg"
                       :bitrate 128
                       :name "Asteroid Radio (CL-Streamer Test)")

;; 3. Create MP3 encoder
(format t "[3] Creating MP3 encoder (128kbps, 44100Hz, stereo)...~%")
(defvar *encoder* (cl-streamer:make-mp3-encoder :sample-rate 44100
                                                 :channels 2
                                                 :bitrate 128))

;; 4. Create and start audio pipeline
(format t "[4] Starting audio pipeline with Harmony...~%")
(defvar *pipeline* (cl-streamer/harmony:make-audio-pipeline
                    :encoder *encoder*
                    :stream-server cl-streamer:*server*
                    :mount-path "/stream.mp3"
                    :sample-rate 44100
                    :channels 2))

(cl-streamer/harmony:start-pipeline *pipeline*)

;; 5. Play a test file
(format t "[5] Playing test file...~%")
(defvar *test-file*
  #p"/home/glenn/SourceCode/asteroid/music/library/Amon_Tobin - Dark Jovian/01 Dark Jovian.flac")

(cl-streamer/harmony:play-file *pipeline* *test-file*)
(cl-streamer:set-now-playing "/stream.mp3" "Amon Tobin - Dark Jovian")

(format t "~%=== Stream is live! ===~%")
(format t "Listen at: http://localhost:8000/stream.mp3~%")
(format t "Listeners: ~A~%" (cl-streamer:get-listener-count))
(format t "~%Press Enter to stop...~%")

(read-line)

;; Cleanup
(format t "Stopping...~%")
(cl-streamer/harmony:stop-pipeline *pipeline*)
(cl-streamer:close-encoder *encoder*)
(cl-streamer:stop)
(format t "Done.~%")
