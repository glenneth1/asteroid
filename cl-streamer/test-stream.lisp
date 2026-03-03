;;; End-to-end streaming test with playlist
;;; Usage: sbcl --load test-stream.lisp
;;;
;;; Then open http://localhost:8000/stream.mp3 in VLC or browser
;;; ICY metadata will show track names as they change.

(push #p"/home/glenn/SourceCode/harmony/" asdf:*central-registry*)
(push #p"/home/glenn/SourceCode/asteroid/cl-streamer/" asdf:*central-registry*)

(ql:quickload '(:cl-streamer :cl-streamer/encoder :cl-streamer/harmony))

(format t "~%=== CL-Streamer Playlist Test ===~%")
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

;; 5. Build a playlist from the music library
(format t "[5] Building playlist from music library...~%")
(defvar *music-dir* #p"/home/glenn/SourceCode/asteroid/music/library/")

(defvar *playlist*
  (let ((files nil))
    (dolist (dir (directory (merge-pathnames "*/" *music-dir*)))
      (dolist (flac (directory (merge-pathnames "**/*.flac" dir)))
        (push (list :file (namestring flac)
                    :title (format nil "~A - ~A"
                                   (car (last (pathname-directory flac)))
                                   (pathname-name flac)))
              files)))
    ;; Shuffle and take first 10 tracks
    (subseq (alexandria:shuffle (copy-list files))
            0 (min 10 (length files)))))

(format t "Queued ~A tracks:~%" (length *playlist*))
(dolist (entry *playlist*)
  (format t "  ~A~%" (getf entry :title)))

;; 6. Start playlist playback
(format t "~%[6] Starting playlist...~%")
(cl-streamer/harmony:play-list *pipeline* *playlist*)

(format t "~%=== Stream is live! ===~%")
(format t "Listen at: http://localhost:8000/stream.mp3~%")
(format t "~%Press Enter to stop...~%")

(read-line)

;; Cleanup
(format t "Stopping...~%")
(cl-streamer/harmony:stop-pipeline *pipeline*)
(cl-streamer:close-encoder *encoder*)
(cl-streamer:stop)
(format t "Done.~%")
