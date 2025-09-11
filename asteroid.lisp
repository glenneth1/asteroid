;; -*-lisp-*-
;; (defpackage :asteroid
;;   (:use :cl :radiance)
;;   (:use :asteroid.app-utils)

;;   (:export :-main :start-server :stop-server :run-server))

(in-package :asteroid)

;; Define as RADIANCE module
(define-module asteroid
  (:use #:cl #:radiance #:lass #:r-clip)
  (:domain "asteroid"))

;; Configuration
(defparameter *server-port* 8080)
(defparameter *music-library-path* 
  (merge-pathnames "music/library/" 
                   (asdf:system-source-directory :asteroid)))
(defparameter *supported-formats* '("mp3" "flac" "ogg" "wav"))

;; Database initialization - must be in db:connected trigger
(define-trigger db:connected ()
  "Initialize database collections when database connects"
  (unless (db:collection-exists-p "tracks")
    (db:create "tracks" '((title :text)
                          (artist :text)
                          (album :text)
                          (duration :integer)
                          (file-path :text)
                          (format :text)
                          (bitrate :integer)
                          (added-date :integer)
                          (play-count :integer))))
  
  (unless (db:collection-exists-p "playlists")
    (db:create "playlists" '((name :text)
                             (description :text)
                             (created-date :integer)
                             (track-ids :text))))
  
  (format t "Database collections initialized~%"))

;; Music library scanning functions
(defun supported-audio-file-p (pathname)
  "Check if file has a supported audio format extension"
  (let ((extension (string-downcase (pathname-type pathname))))
    (member extension *supported-formats* :test #'string=)))

(defun scan-directory-for-music (directory)
  "Recursively scan directory for supported audio files"
  (when (cl-fad:directory-exists-p directory)
    (remove-if-not #'supported-audio-file-p
                   (cl-fad:list-directory directory :follow-symlinks nil))))

(defun extract-metadata-with-taglib (file-path)
  "Extract metadata using taglib library"
  (handler-case
      (let* ((audio-file (audio-streams:open-audio-file (namestring file-path)))
             (file-info (sb-posix:stat file-path))
             (format (string-downcase (pathname-type file-path))))
        (list :file-path (namestring file-path)
              :format format
              :size (sb-posix:stat-size file-info)
              :modified (sb-posix:stat-mtime file-info)
              :title (or (abstract-tag:title audio-file) (pathname-name file-path))
              :artist (or (abstract-tag:artist audio-file) "Unknown Artist")
              :album (or (abstract-tag:album audio-file) "Unknown Album")
              :duration (or (and (slot-exists-p audio-file 'audio-streams::duration)
                                 (slot-boundp audio-file 'audio-streams::duration)
                                 (round (audio-streams::duration audio-file)))
                            0)
              :bitrate (or (and (slot-exists-p audio-file 'audio-streams::bit-rate)
                                (slot-boundp audio-file 'audio-streams::bit-rate)
                                (round (audio-streams::bit-rate audio-file)))
                           0)))
    (error (e)
      (format t "Warning: Could not extract metadata from ~a: ~a~%" file-path e)
      ;; Fallback to basic file metadata
      (extract-basic-metadata file-path))))

(defun extract-basic-metadata (file-path)
  "Extract basic file metadata (fallback when taglib fails)"
  (when (probe-file file-path)
    (let ((file-info (sb-posix:stat file-path)))
      (list :file-path (namestring file-path)
            :format (string-downcase (pathname-type file-path))
            :size (sb-posix:stat-size file-info)
            :modified (sb-posix:stat-mtime file-info)
            :title (pathname-name file-path)
            :artist "Unknown Artist"
            :album "Unknown Album"
            :duration 0
            :bitrate 0))))

(defun insert-track-to-database (metadata)
  "Insert track metadata into database"
  (db:insert "tracks" 
             (list (list "title" (getf metadata :title))
                   (list "artist" (getf metadata :artist))
                   (list "album" (getf metadata :album))
                   (list "duration" (getf metadata :duration))
                   (list "file-path" (getf metadata :file-path))
                   (list "format" (getf metadata :format))
                   (list "bitrate" (getf metadata :bitrate))
                   (list "added-date" (local-time:timestamp-to-unix (local-time:now)))
                   (list "play-count" 0))))

(defun scan-music-library (&optional (directory *music-library-path*))
  "Scan music library directory and add tracks to database"
  (format t "Scanning music library: ~a~%" directory)
  (let ((audio-files (scan-directory-for-music directory))
        (added-count 0))
    (dolist (file audio-files)
      (let ((metadata (extract-metadata-with-taglib file)))
        (when metadata
          (handler-case
              (progn
                (insert-track-to-database metadata)
                (incf added-count)
                (format t "Added: ~a~%" (getf metadata :file-path)))
            (error (e)
              (format t "Error adding ~a: ~a~%" file e))))))
    (format t "Library scan complete. Added ~a tracks.~%" added-count)
    added-count))

;; Initialize music directory structure
(defun ensure-music-directories ()
  "Create music directory structure if it doesn't exist"
  (let ((base-dir (merge-pathnames "music/" (asdf:system-source-directory :asteroid))))
    (ensure-directories-exist (merge-pathnames "library/" base-dir))
    (ensure-directories-exist (merge-pathnames "incoming/" base-dir))
    (ensure-directories-exist (merge-pathnames "temp/" base-dir))
    (format t "Music directories initialized at ~a~%" base-dir)))

;; Simple file copy endpoint for manual uploads
(define-page copy-files #@"/admin/copy-files" ()
  "Copy files from incoming directory to library"
  (handler-case
      (let ((incoming-dir (merge-pathnames "music/incoming/" 
                                         (asdf:system-source-directory :asteroid)))
            (library-dir (merge-pathnames "music/library/" 
                                        (asdf:system-source-directory :asteroid)))
            (files-copied 0))
        (ensure-directories-exist incoming-dir)
        (ensure-directories-exist library-dir)
        
        ;; Process all files in incoming directory
        (dolist (file (directory (merge-pathnames "*.*" incoming-dir)))
          (when (probe-file file)
            (let* ((filename (file-namestring file))
                   (file-extension (string-downcase (or (pathname-type file) "")))
                   (target-path (merge-pathnames filename library-dir)))
              (when (member file-extension *supported-formats* :test #'string=)
                (alexandria:copy-file file target-path)
                (delete-file file)
                (incf files-copied)
                ;; Extract metadata and add to database
                (let ((metadata (extract-metadata-with-taglib target-path)))
                  (insert-track-to-database metadata))))))
        
        (setf (radiance:header "Content-Type") "application/json")
        (cl-json:encode-json-to-string
         `(("status" . "success")
           ("message" . ,(format nil "Copied ~d files to library" files-copied))
           ("files-copied" . ,files-copied))))
    (error (e)
      (setf (radiance:header "Content-Type") "application/json")
      (cl-json:encode-json-to-string
       `(("status" . "error")
         ("message" . ,(format nil "Copy failed: ~a" e)))))))

;; API Routes
(define-page admin-scan-library #@"/admin/scan-library" ()
  "API endpoint to scan music library"
  (handler-case
      (let ((tracks-added (scan-music-library)))
        (setf (radiance:header "Content-Type") "application/json")
        (cl-json:encode-json-to-string
         `(("status" . "success")
           ("message" . "Library scan completed")
           ("tracks-added" . ,tracks-added))))
    (error (e)
      (setf (radiance:header "Content-Type") "application/json")
      (cl-json:encode-json-to-string
       `(("status" . "error")
         ("message" . ,(format nil "Scan failed: ~a" e)))))))

(define-page admin-tracks #@"/admin/tracks" ()
  "API endpoint to view all tracks in database"
  (handler-case
      (let ((tracks (db:select "tracks" (db:query :all))))
        (setf (radiance:header "Content-Type") "application/json")
        (cl-json:encode-json-to-string
         `(("status" . "success")
           ("tracks" . ,(mapcar (lambda (track)
                                  `(("id" . ,(gethash "_id" track))
                                    ("title" . ,(gethash "title" track))
                                    ("artist" . ,(gethash "artist" track))
                                    ("album" . ,(gethash "album" track))
                                    ("duration" . ,(gethash "duration" track))
                                    ("file-path" . ,(gethash "file-path" track))
                                    ("format" . ,(gethash "format" track))
                                    ("bitrate" . ,(gethash "bitrate" track))
                                    ("added-date" . ,(gethash "added-date" track))
                                    ("play-count" . ,(gethash "play-count" track))))
                                tracks)))))
    (error (e)
      (setf (radiance:header "Content-Type") "application/json")
      (cl-json:encode-json-to-string
       `(("status" . "error")
         ("message" . ,(format nil "Failed to retrieve tracks: ~a" e)))))))

(defun get-track-by-id (track-id)
  "Retrieve track from database by ID"
  (let* ((id (if (stringp track-id) (parse-integer track-id) track-id))
         (tracks (db:select "tracks" (db:query (:= '_id id)))))
    (when tracks (first tracks))))

(defun get-mime-type-for-format (format)
  "Get MIME type for audio format"
  (cond
    ((string= format "mp3") "audio/mpeg")
    ((string= format "flac") "audio/flac")
    ((string= format "ogg") "audio/ogg")
    ((string= format "wav") "audio/wav")
    (t "application/octet-stream")))

(define-page stream-track #@"/tracks/(.*)/stream" (:uri-groups (track-id))
  "Stream audio file by track ID"
  (handler-case
      (let* ((id (parse-integer track-id))
             (track (get-track-by-id id)))
        (if track
            (let* ((file-path (first (gethash "file-path" track)))
                   (format (first (gethash "format" track)))
                   (file (probe-file file-path)))
              (if file
                  (progn
                    ;; Set appropriate headers for audio streaming
                    (setf (radiance:header "Content-Type") (get-mime-type-for-format format))
                    (setf (radiance:header "Accept-Ranges") "bytes")
                    (setf (radiance:header "Cache-Control") "public, max-age=3600")
                    ;; Increment play count
                    (db:update "tracks" (db:query (:= '_id id))
                               `(("play-count" ,(1+ (first (gethash "play-count" track))))))
                    ;; Return file contents
                    (alexandria:read-file-into-byte-vector file))
                  (progn
                    (setf (radiance:header "Content-Type") "application/json")
                    (cl-json:encode-json-to-string
                     `(("status" . "error")
                       ("message" . "Audio file not found on disk"))))))
            (progn
              (setf (radiance:header "Content-Type") "application/json")
              (cl-json:encode-json-to-string
               `(("status" . "error")
                 ("message" . "Track not found"))))))
    (error (e)
      (setf (radiance:header "Content-Type") "application/json")
      (cl-json:encode-json-to-string
       `(("status" . "error")
         ("message" . ,(format nil "Streaming error: ~a" e)))))))

;; Player state management
(defvar *current-track* nil "Currently playing track")
(defvar *player-state* :stopped "Player state: :playing, :paused, :stopped")
(defvar *play-queue* '() "List of track IDs in play queue")
(defvar *current-position* 0 "Current playback position in seconds")

(defun get-player-status ()
  "Get current player status"
  `(("state" . ,(string-downcase (symbol-name *player-state*)))
    ("current-track" . ,*current-track*)
    ("position" . ,*current-position*)
    ("queue-length" . ,(length *play-queue*))))


;; Define CLIP attribute processor for data-text
(clip:define-attribute-processor data-text (node value)
  (plump:clear node)
  (plump:make-text-node node (clip:clipboard value)))

;; LASS CSS generation
(defun generate-css ()
  "Generate CSS from LASS file"
  (lass:compile-and-write 
   (read-from-string 
    (alexandria:read-file-into-string 
     (merge-pathnames "static/asteroid.lass" 
                      (asdf:system-source-directory :asteroid))))))

;; Generate CSS file using LASS
(defun compile-styles ()
  "Generate CSS file using LASS"
  (ensure-directories-exist "static/")
  (let ((css-file (merge-pathnames "static/asteroid.css")))
    (with-open-file (out css-file
                         :direction :output
                         :if-exists :supersede)
      (write-string (generate-css) out))))

;; Player control API endpoints
(define-page api-play #@"/api/play" ()
  "Start playing a track by ID"
  (setf (radiance:header "Content-Type") "application/json")
  (handler-case
      (let* ((track-id (radiance:get-var "track-id"))
             (id (parse-integer track-id))
             (track (get-track-by-id id)))
        (if track
            (progn
              (setf *current-track* id)
              (setf *player-state* :playing)
              (setf *current-position* 0)
              (cl-json:encode-json-to-string
               `(("status" . "success")
                 ("message" . "Playback started")
                 ("track" . (("id" . ,id)
                            ("title" . ,(first (gethash "title" track)))
                            ("artist" . ,(first (gethash "artist" track)))))
                 ("player" . ,(get-player-status)))))
            (cl-json:encode-json-to-string
             `(("status" . "error")
               ("message" . "Track not found")))))
    (error (e)
      (cl-json:encode-json-to-string
       `(("status" . "error")
         ("message" . ,(format nil "Play error: ~a" e)))))))

(define-page api-pause #@"/api/pause" ()
  "Pause current playback"
  (setf *player-state* :paused)
  (setf (radiance:header "Content-Type") "application/json")
  (cl-json:encode-json-to-string
   `(("status" . "success")
     ("message" . "Playback paused")
     ("player" . ,(get-player-status)))))

(define-page api-stop #@"/api/stop" ()
  "Stop current playback"
  (setf *player-state* :stopped)
  (setf *current-track* nil)
  (setf *current-position* 0)
  (setf (radiance:header "Content-Type") "application/json")
  (cl-json:encode-json-to-string
   `(("status" . "success")
     ("message" . "Playback stopped")
     ("player" . ,(get-player-status)))))

(define-page api-resume #@"/api/resume" ()
  "Resume paused playback"
  (setf (radiance:header "Content-Type") "application/json")
  (if (eq *player-state* :paused)
      (progn
        (setf *player-state* :playing)
        (cl-json:encode-json-to-string
         `(("status" . "success")
           ("message" . "Playback resumed")
           ("player" . ,(get-player-status)))))
      (cl-json:encode-json-to-string
       `(("status" . "error")
         ("message" . "Player is not paused")))))

(define-page api-player-status #@"/api/player-status" ()
  "Get current player status"
  (setf (radiance:header "Content-Type") "application/json")
  (cl-json:encode-json-to-string
   `(("status" . "success")
     ("player" . ,(get-player-status)))))

;; Configure static file serving for other files
(define-page static #@"/static/(.*)" (:uri-groups (path))
  (serve-file (merge-pathnames (concatenate 'string "static/" path) 
                               (asdf:system-source-directory :asteroid))))

;; RADIANCE route handlers
(define-page index #@"/" ()
  (let ((template-path (merge-pathnames "template/front-page.chtml" 
                                       (asdf:system-source-directory :asteroid))))
    (clip:process-to-string 
     (plump:parse (alexandria:read-file-into-string template-path))
     :title "🎵 ASTEROID RADIO 🎵"
     :station-name "🎵 ASTEROID RADIO 🎵"
     :status-message "🟢 LIVE - Broadcasting asteroid music for hackers"
     :listeners "0"
     :stream-quality "128kbps MP3"
     :now-playing-artist "The Void"
     :now-playing-track "Silence"
     :now-playing-album "Startup Sounds"
     :now-playing-duration "∞")))

(define-page admin #@"/admin" ()
  (let ((template-path (merge-pathnames "template/admin.chtml" 
                                       (asdf:system-source-directory :asteroid)))
        (track-count (handler-case 
                       (length (db:select "tracks" (db:query :all)))
                       (error () 0))))
    (clip:process-to-string 
     (plump:parse (alexandria:read-file-into-string template-path))
     :title "Asteroid Radio - Admin Dashboard"
     :server-status "🟢 Running"
     :database-status (handler-case 
                        (if (db:connected-p) "🟢 Connected" "🔴 Disconnected")
                        (error () "🔴 No Database Backend"))
     :liquidsoap-status "🔴 Not Running"
     :icecast-status "🔴 Not Running"
     :track-count (format nil "~d" track-count)
     :library-path "/home/glenn/Projects/Code/asteroid/music/library/")))

(define-page player #@"/player/" ()
  (let ((template-path (merge-pathnames "template/player.chtml" 
                                       (asdf:system-source-directory :asteroid))))
    (clip:process-to-string 
     (plump:parse (alexandria:read-file-into-string template-path))
     :title "Asteroid Radio - Web Player"
     :stream-url "http://localhost:8000/asteroid"
     :bitrate "128kbps MP3"
     :now-playing-artist "The Void"
     :now-playing-track "Silence"
     :now-playing-album "Startup Sounds"
     :player-status "Stopped")))

(define-page status-api #@"/status" ()
  (setf (radiance:header "Content-Type") "application/json")
  (cl-json:encode-json-to-string
   `(("status" . "running")
     ("server" . "asteroid-radio")
     ("version" . "0.1.0")
     ("uptime" . ,(get-universal-time))
     ("now-playing" . (("title" . "Silence")
                       ("artist" . "The Void")
                       ("album" . "Startup Sounds")))
     ("listeners" . 0)
     ("stream-url" . "http://localhost:8000/asteroid"))))

;; RADIANCE server management functions
(defun start-server (&key (port *server-port*))
  "Start the Asteroid Radio RADIANCE server"
  (format t "Starting Asteroid Radio RADIANCE server on port ~a~%"  port)
  (compile-styles)  ; Generate CSS file using LASS
  (radiance:startup)
  (format t "Server started! Visit http://localhost:~a/asteroid/~%" port))

(defun stop-server ()
  "Stop the Asteroid Radio RADIANCE server"
  (format t "Stopping Asteroid Radio server...~%")
  (radiance:shutdown)
  (format t "Server stopped.~%"))

(defun run-server (&key (port *server-port*))
  "Start the server and keep it running (blocking)"
  (start-server :port port)
  (format t "Server running. Press Ctrl+C to stop.~%")
  ;; Keep the server running
  (handler-case
      (loop (sleep 1))
    (sb-sys:interactive-interrupt ()
      (format t "~%Received interrupt, stopping server...~%")
      (stop-server))))

(defun -main (&optional args)
  (declare (ignorable args))
  (format t "~%🎵 ASTEROID RADIO - Music for Hackers 🎵~%")
  (format t "Starting RADIANCE web server...~%")
  (run-server))

