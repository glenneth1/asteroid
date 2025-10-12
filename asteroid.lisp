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

;; Configuration -- this will be refactored to a dedicated
;; configuration logic. Probably using 'ubiquity
(defparameter *server-port* 8080)
(defparameter *music-library-path* 
  (merge-pathnames "music/library/" 
                   (asdf:system-source-directory :asteroid)))
(defparameter *supported-formats* '("mp3" "flac" "ogg" "wav"))

;; Configure JSON as the default API format
(define-api-format json (data)
  "JSON API format for Radiance"
  (setf (header "Content-Type") "application/json")
  (cl-json:encode-json-to-string data))

;; Set JSON as the default API format
(setf *default-api-format* "json")

;; API Routes using Radiance's define-api
;; API endpoints are accessed at /api/<name> automatically
;; They use lambda-lists for parameters and api-output for responses

(define-api asteroid/admin/scan-library () ()
  "API endpoint to scan music library"
  (require-role :admin)
  (handler-case
      (let ((tracks-added (scan-music-library)))
        (api-output `(("status" . "success")
                      ("message" . "Library scan completed")
                      ("tracks-added" . ,tracks-added))))
    (error (e)
      (api-output `(("status" . "error")
                    ("message" . ,(format nil "Scan failed: ~a" e)))
                  :status 500))))

(define-api asteroid/admin/tracks () ()
  "API endpoint to view all tracks in database"
  (require-authentication)
  (handler-case
      (let ((tracks (db:select "tracks" (db:query :all))))
        (api-output `(("status" . "success")
                      ("tracks" . ,(mapcar (lambda (track)
                                             `(("id" . ,(gethash "_id" track))
                                               ("title" . ,(first (gethash "title" track)))
                                               ("artist" . ,(first (gethash "artist" track)))
                                               ("album" . ,(first (gethash "album" track)))
                                               ("duration" . ,(first (gethash "duration" track)))
                                               ("format" . ,(first (gethash "format" track)))
                                               ("bitrate" . ,(first (gethash "bitrate" track)))))
                                           tracks)))))
    (error (e)
      (api-output `(("status" . "error")
                    ("message" . ,(format nil "Error retrieving tracks: ~a" e)))
                  :status 500))))

;; Playlist API endpoints
(define-api asteroid/playlists () ()
  "Get all playlists for current user"
  (require-authentication)
  (handler-case
      (let* ((user (get-current-user))
             (user-id-raw (gethash "_id" user))
             (user-id (if (listp user-id-raw) (first user-id-raw) user-id-raw))
             (playlists (get-user-playlists user-id)))
        (format t "Fetching playlists for user-id: ~a~%" user-id)
        (format t "Found ~a playlists~%" (length playlists))
        (api-output `(("status" . "success")
                      ("playlists" . ,(mapcar (lambda (playlist)
                                                (let ((name-val (gethash "name" playlist))
                                                      (desc-val (gethash "description" playlist))
                                                      (track-ids-val (gethash "track-ids" playlist))
                                                      (created-val (gethash "created-date" playlist))
                                                      (id-val (gethash "_id" playlist)))
                                                  (format t "Playlist ID: ~a (type: ~a)~%" id-val (type-of id-val))
                                                  ;; Calculate track count from comma-separated string
                                                  ;; Handle nil, empty string, or list containing empty string
                                                  (let* ((track-ids-str (if (listp track-ids-val) 
                                                                           (first track-ids-val) 
                                                                           track-ids-val))
                                                         (track-count (if (and track-ids-str 
                                                                              (stringp track-ids-str)
                                                                              (not (string= track-ids-str "")))
                                                                         (length (cl-ppcre:split "," track-ids-str))
                                                                         0)))
                                                    `(("id" . ,(if (listp id-val) (first id-val) id-val))
                                                      ("name" . ,(if (listp name-val) (first name-val) name-val))
                                                      ("description" . ,(if (listp desc-val) (first desc-val) desc-val))
                                                      ("track-count" . ,track-count)
                                                      ("created-date" . ,(if (listp created-val) (first created-val) created-val))))))
                                              playlists)))))
    (error (e)
      (api-output `(("status" . "error")
                    ("message" . ,(format nil "Error retrieving playlists: ~a" e)))
                  :status 500))))

(define-api asteroid/playlists/create (name &optional description) ()
  "Create a new playlist"
  (require-authentication)
  (handler-case
      (let* ((user (get-current-user))
             (user-id-raw (gethash "_id" user))
             (user-id (if (listp user-id-raw) (first user-id-raw) user-id-raw)))
        (format t "Creating playlist for user-id: ~a, name: ~a~%" user-id name)
        (create-playlist user-id name description)
        (format t "Playlist created successfully~%")
        (if (string= "true" (post/get "browser"))
            (redirect "/asteroid/")
            (api-output `(("status" . "success")
                          ("message" . "Playlist created successfully")))))
    (error (e)
      (api-output `(("status" . "error")
                    ("message" . ,(format nil "Error creating playlist: ~a" e)))
                  :status 500))))

(define-api asteroid/playlists/add-track (playlist-id track-id) ()
  "Add a track to a playlist"
  (require-authentication)
  (handler-case
      (let ((pl-id (parse-integer playlist-id :junk-allowed t))
            (tr-id (parse-integer track-id :junk-allowed t)))
        (add-track-to-playlist pl-id tr-id)
        (if (string= "true" (post/get "browser"))
            (redirect "/asteroid/")
            (api-output `(("status" . "success")
                          ("message" . "Track added to playlist")))))
    (error (e)
      (api-output `(("status" . "error")
                    ("message" . ,(format nil "Error adding track: ~a" e)))
                  :status 500))))

(define-api asteroid/playlists/get (playlist-id) ()
  "Get playlist details with tracks"
  (require-authentication)
  (handler-case
      (let* ((id (parse-integer playlist-id :junk-allowed t))
             (playlist (get-playlist-by-id id)))
        (format t "Looking for playlist ID: ~a~%" id)
        (format t "Found playlist: ~a~%" (if playlist "YES" "NO"))
        (if playlist
            (let* ((track-ids-raw (gethash "tracks" playlist))
                   (track-ids (if (listp track-ids-raw) track-ids-raw (list track-ids-raw)))
                   (tracks (mapcar (lambda (track-id)
                                     (let ((track-list (db:select "tracks" (db:query (:= "_id" track-id)))))
                                       (when (> (length track-list) 0)
                                         (first track-list))))
                                   track-ids))
                   (valid-tracks (remove nil tracks)))
              (api-output `(("status" . "success")
                            ("playlist" . (("id" . ,id)
                                          ("name" . ,(let ((n (gethash "name" playlist)))
                                                      (if (listp n) (first n) n)))
                                          ("tracks" . ,(mapcar (lambda (track)
                                                                `(("id" . ,(gethash "_id" track))
                                                                  ("title" . ,(gethash "title" track))
                                                                  ("artist" . ,(gethash "artist" track))
                                                                  ("album" . ,(gethash "album" track))))
                                                              valid-tracks)))))))
            (api-output `(("status" . "error")
                          ("message" . "Playlist not found"))
                        :status 404)))
    (error (e)
      (api-output `(("status" . "error")
                    ("message" . ,(format nil "Error retrieving playlist: ~a" e)))
                  :status 500))))

;; API endpoint to get all tracks (for web player)
(define-api asteroid/tracks () ()
  "Get all tracks for web player"
  (require-authentication)
  (handler-case
      (let ((tracks (db:select "tracks" (db:query :all))))
        (api-output `(("status" . "success")
                      ("tracks" . ,(mapcar (lambda (track)
                                             `(("id" . ,(gethash "_id" track))
                                               ("title" . ,(gethash "title" track))
                                               ("artist" . ,(gethash "artist" track))
                                               ("album" . ,(gethash "album" track))
                                               ("duration" . ,(gethash "duration" track))
                                               ("format" . ,(gethash "format" track))))
                                           tracks)))))
    (error (e)
      (api-output `(("status" . "error")
                    ("message" . ,(format nil "Error retrieving tracks: ~a" e)))
                  :status 500))))

;; API endpoint to get track by ID (for streaming)
(define-page api-get-track-by-id #@"/api/tracks/(.*)" (:uri-groups (track-id))
  "Retrieve track from database by ID"
  (let* ((id (if (stringp track-id) (parse-integer track-id) track-id))
         (tracks (db:select "tracks" (db:query (:= '_id id)))))
    (when tracks (first tracks))))
(defun get-track-by-id (track-id)
  "Get a track by its ID - handles type mismatches"
  (format t "get-track-by-id called with: ~a (type: ~a)~%" track-id (type-of track-id))
  ;; Try direct query first
  (let ((tracks (db:select "tracks" (db:query (:= "_id" track-id)))))
    (if (> (length tracks) 0)
        (progn
          (format t "Found via direct query~%")
          (first tracks))
        ;; If not found, search manually (ID might be stored as list)
        (let ((all-tracks (db:select "tracks" (db:query :all))))
          (format t "Searching through ~a tracks manually~%" (length all-tracks))
          (find-if (lambda (track)
                     (let ((stored-id (gethash "_id" track)))
                       (or (equal stored-id track-id)
                           (and (listp stored-id) (equal (first stored-id) track-id)))))
                   all-tracks)))))

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

;; Liquidsoap Telnet Control Functions
(defun liquidsoap-telnet-command (command)
  "Send a command to Liquidsoap telnet server and return response"
  (handler-case
    (let ((socket (usocket:socket-connect "127.0.0.1" 1234)))
      (unwind-protect
        (progn
          (format (usocket:socket-stream socket) "~A~%" command)
          (force-output (usocket:socket-stream socket))
          (let ((response-lines '())
                (line nil))
            ;; Read all lines until "END"
            (loop while (and (setf line (read-line (usocket:socket-stream socket) nil))
                            (not (string= line "END")))
                  do (push line response-lines))
            (format nil "~{~A~%~}" (reverse response-lines))))
        (usocket:socket-close socket)))
    (error (e)
      (format t "Error communicating with Liquidsoap: ~A~%" e)
      nil)))

(defun liquidsoap-skip-track ()
  "Skip to the next track in Liquidsoap"
  (liquidsoap-telnet-command "music.skip"))

(defun liquidsoap-reload-playlist ()
  "Reload the playlist in Liquidsoap"
  (liquidsoap-telnet-command "music.reload"))

(defun liquidsoap-get-metadata ()
  "Get current track metadata from Liquidsoap"
  (let ((response (liquidsoap-telnet-command "output.icecast.metadata")))
    (when response
      (parse-liquidsoap-metadata response))))

(defun liquidsoap-get-queue ()
  "Get the current request queue from Liquidsoap"
  (liquidsoap-telnet-command "request.all"))

(defun liquidsoap-queue-track (file-path)
  "Queue a specific track in Liquidsoap"
  (liquidsoap-telnet-command (format nil "request.push ~A" file-path)))

(defun parse-liquidsoap-metadata (metadata-string)
  "Parse Liquidsoap metadata string into artist/title/album"
  (let ((artist "Unknown Artist")
        (title "Unknown Track")
        (album "Unknown Album"))
    (when metadata-string
      ;; Parse the first track (most recent) from the metadata
      ;; Format: --- 2 ---\nkey="value"\nkey="value"...
      (let ((lines (cl-ppcre:split "\\n" metadata-string)))
        (dolist (line lines)
          (cond
            ((cl-ppcre:scan "^artist=" line)
             (setf artist (cl-ppcre:regex-replace "^artist=\"([^\"]+)\"" line "\\1")))
            ((cl-ppcre:scan "^title=" line)
             (setf title (cl-ppcre:regex-replace "^title=\"([^\"]+)\"" line "\\1")))
            ((cl-ppcre:scan "^album=" line)
             (setf album (cl-ppcre:regex-replace "^album=\"([^\"]+)\"" line "\\1")))))))
    (list :artist artist :title title :album album)))


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
(define-api asteroid/player/play (track-id) ()
  "Start playing a track by ID"
  (handler-case
      (let* ((id (parse-integer track-id))
             (track (get-track-by-id id)))
        (if track
            (progn
              (setf *current-track* id)
              (setf *player-state* :playing)
              (setf *current-position* 0)
              (api-output `(("status" . "success")
                            ("message" . "Playback started")
                            ("track" . (("id" . ,id)
                                       ("title" . ,(first (gethash "title" track)))
                                       ("artist" . ,(first (gethash "artist" track)))))
                            ("player" . ,(get-player-status)))))
            (api-output `(("status" . "error")
                          ("message" . "Track not found"))
                        :status 404)))
    (error (e)
      (api-output `(("status" . "error")
                    ("message" . ,(format nil "Play error: ~a" e)))
                  :status 500))))

(define-api asteroid/player/pause () ()
  "Pause current playback"
  (setf *player-state* :paused)
  (api-output `(("status" . "success")
                ("message" . "Playback paused")
                ("player" . ,(get-player-status)))))

(define-api asteroid/player/stop () ()
  "Stop current playback"
  (setf *player-state* :stopped)
  (setf *current-track* nil)
  (setf *current-position* 0)
  (api-output `(("status" . "success")
                ("message" . "Playback stopped")
                ("player" . ,(get-player-status)))))

(define-api asteroid/player/resume () ()
  "Resume paused playback"
  (if (eq *player-state* :paused)
      (progn
        (setf *player-state* :playing)
        (api-output `(("status" . "success")
                      ("message" . "Playback resumed")
                      ("player" . ,(get-player-status)))))
      (api-output `(("status" . "error")
                    ("message" . "Player is not paused"))
                  :status 400)))

(define-api asteroid/player/status () ()
  "Get current player status"
  (api-output `(("status" . "success")
                ("player" . ,(get-player-status)))))

;; DJ Control API Endpoints
(define-api asteroid/dj/skip () ()
  "Skip to the next track via Liquidsoap telnet"
  (require-role :admin)  ; Only admins can use DJ controls
  (handler-case
      (let ((result (liquidsoap-skip-track)))
        (if result
            (api-output `(("status" . "success")
                          ("message" . "Track skipped successfully")
                          ("liquidsoap-response" . ,result)))
            (api-output `(("status" . "error")
                          ("message" . "Failed to communicate with Liquidsoap"))
                        :status 503)))
    (error (e)
      (api-output `(("status" . "error")
                    ("message" . ,(format nil "Skip error: ~a" e)))
                  :status 500))))

(define-api asteroid/dj/reload-playlist () ()
  "Reload the playlist in Liquidsoap"
  (require-role :admin)
  (handler-case
      (let ((result (liquidsoap-reload-playlist)))
        (if result
            (api-output `(("status" . "success")
                          ("message" . "Playlist reloaded successfully")
                          ("liquidsoap-response" . ,result)))
            (api-output `(("status" . "error")
                          ("message" . "Failed to communicate with Liquidsoap"))
                        :status 503)))
    (error (e)
      (api-output `(("status" . "error")
                    ("message" . ,(format nil "Reload error: ~a" e)))
                  :status 500))))

(define-api asteroid/dj/queue (track-id) ()
  "Queue a specific track by ID"
  (require-role :admin)
  (handler-case
      (let* ((id (parse-integer track-id :junk-allowed t))
             (track (get-track-by-id id)))
        (if track
            (let* ((file-path (first (gethash "file-path" track)))
                   (result (liquidsoap-queue-track file-path)))
              (if result
                  (api-output `(("status" . "success")
                                ("message" . "Track queued successfully")
                                ("track" . (("id" . ,id)
                                           ("title" . ,(first (gethash "title" track)))
                                           ("artist" . ,(first (gethash "artist" track)))))
                                ("liquidsoap-response" . ,result)))
                  (api-output `(("status" . "error")
                                ("message" . "Failed to queue track in Liquidsoap"))
                              :status 503)))
            (api-output `(("status" . "error")
                          ("message" . "Track not found"))
                        :status 404)))
    (error (e)
      (api-output `(("status" . "error")
                    ("message" . ,(format nil "Queue error: ~a" e)))
                  :status 500))))

(define-api asteroid/dj/queue-status () ()
  "Get the current Liquidsoap request queue"
  (require-role :admin)
  (handler-case
      (let ((result (liquidsoap-get-queue)))
        (if result
            (api-output `(("status" . "success")
                          ("queue" . ,result)))
            (api-output `(("status" . "error")
                          ("message" . "Failed to get queue from Liquidsoap"))
                        :status 503)))
    (error (e)
      (api-output `(("status" . "error")
                    ("message" . ,(format nil "Queue status error: ~a" e)))
                  :status 500))))

(define-api asteroid/dj/metadata () ()
  "Get enhanced metadata directly from Liquidsoap"
  (require-authentication)  ; Any authenticated user can see metadata
  (handler-case
      (let ((metadata (liquidsoap-get-metadata)))
        (if metadata
            (api-output `(("status" . "success")
                          ("metadata" . (("artist" . ,(getf metadata :artist))
                                        ("title" . ,(getf metadata :title))
                                        ("album" . ,(getf metadata :album))))))
            (api-output `(("status" . "error")
                          ("message" . "Failed to get metadata from Liquidsoap"))
                        :status 503)))
    (error (e)
      (api-output `(("status" . "error")
                    ("message" . ,(format nil "Metadata error: ~a" e)))
                  :status 500))))

;; Profile API Routes - TEMPORARILY COMMENTED OUT
#|
(define-page api-user-profile #@"/api/user/profile" ()
  "Get current user profile information"
  (require-authentication)
  (setf (radiance:header "Content-Type") "application/json")
  (let ((current-user (auth:current-user)))
    (cl-json:encode-json-to-string
     `(("status" . "success")
       ("user" . (("username" . ,(gethash "username" current-user))
                  ("role" . ,(gethash "role" current-user))
                  ("created_at" . ,(gethash "created_at" current-user))
                  ("last_active" . ,(get-universal-time))))))))

(define-page api-user-listening-stats #@"/api/user/listening-stats" ()
  "Get user listening statistics"
  (require-authentication)
  (setf (radiance:header "Content-Type") "application/json")
  ;; TODO: Implement actual listening statistics from database
  ;; For now, return mock data
  (cl-json:encode-json-to-string
   `(("status" . "success")
     ("stats" . (("total_listen_time" . 0)
                 ("tracks_played" . 0)
                 ("session_count" . 0)
                 ("favorite_genre" . "Unknown"))))))

(define-page api-user-recent-tracks #@"/api/user/recent-tracks" ()
  "Get user's recently played tracks"
  (require-authentication)
  (setf (radiance:header "Content-Type") "application/json")
  ;; TODO: Implement actual recent tracks from database
  ;; For now, return empty array
  (cl-json:encode-json-to-string
   `(("status" . "success")
     ("tracks" . #()))))

(define-page api-user-top-artists #@"/api/user/top-artists" ()
  "Get user's top artists"
  (require-authentication)
  (setf (radiance:header "Content-Type") "application/json")
  ;; TODO: Implement actual top artists from database
  ;; For now, return empty array
  (cl-json:encode-json-to-string
   `(("status" . "success")
     ("artists" . #()))))

(define-page api-user-export-data #@"/api/user/export-data" ()
  "Export user listening data"
  (require-authentication)
  (setf (radiance:header "Content-Type") "application/json")
  (setf (radiance:header "Content-Disposition") "attachment; filename=listening-data.json")
  ;; TODO: Implement actual data export
  (cl-json:encode-json-to-string
   `(("user" . ,(gethash "username" (auth:current-user)))
     ("export_date" . ,(get-universal-time))
     ("listening_history" . #())
     ("statistics" . (("total_listen_time" . 0)
                      ("tracks_played" . 0)
                      ("session_count" . 0))))))

(define-page api-user-clear-history #@"/api/user/clear-history" ()
  "Clear user listening history"
  (require-authentication)
  (setf (radiance:header "Content-Type") "application/json")
  ;; TODO: Implement actual history clearing
  (cl-json:encode-json-to-string
   `(("status" . "success")
     ("message" . "Listening history cleared successfully"))))
|#

;; Front page
(define-page front-page #@"/" ()
  "Main front page"
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

;; Configure static file serving for other files
(define-page static #@"/static/(.*)" (:uri-groups (path))
  (serve-file (merge-pathnames (concatenate 'string "static/" path) 
                               (asdf:system-source-directory :asteroid))))

;; Status check functions
(defun check-icecast-status ()
  "Check if Icecast server is running and accessible"
  (handler-case
      (let ((response (drakma:http-request "http://localhost:8000/status-json.xsl"
                                          :want-stream nil
                                          :connection-timeout 2)))
        (if response "🟢 Running" "🔴 Not Running"))
    (error () "🔴 Not Running")))

(defun check-liquidsoap-status ()
  "Check if Liquidsoap is running via Docker"
  (handler-case
      (let* ((output (with-output-to-string (stream)
                       (uiop:run-program '("docker" "ps" "--filter" "name=liquidsoap" "--format" "{{.Status}}")
                                        :output stream
                                        :error-output nil
                                        :ignore-error-status t)))
             (running-p (search "Up" output)))
        (if running-p "🟢 Running" "🔴 Not Running"))
    (error () "🔴 Not Running")))

;; Admin page (requires authentication)
(define-page admin #@"/admin" ()
  "Admin dashboard"
  (require-authentication)
  (let ((template-path (merge-pathnames "template/admin.chtml" 
                                       (asdf:system-source-directory :asteroid)))
        (track-count (handler-case 
                       (length (db:select "tracks" (db:query :all)))
                       (error () 0))))
    (clip:process-to-string 
     (plump:parse (alexandria:read-file-into-string template-path))
     :title "🎵 ASTEROID RADIO - Admin Dashboard"
     :server-status "🟢 Running"
     :database-status (handler-case 
                        (if (db:connected-p) "🟢 Connected" "🔴 Disconnected")
                        (error () "🔴 No Database Backend"))
     :liquidsoap-status (check-liquidsoap-status)
     :icecast-status (check-icecast-status)
     :track-count (format nil "~d" track-count)
     :library-path "/home/glenn/Projects/Code/asteroid/music/library/")))

;; User Management page (requires authentication)
(define-page users-management #@"/admin/user" ()
  "User Management dashboard"
  (require-authentication)
  (let ((template-path (merge-pathnames "template/users.chtml"
                                       (asdf:system-source-directory :asteroid))))
    (clip:process-to-string 
     (plump:parse (alexandria:read-file-into-string template-path))
     :title "🎵 ASTEROID RADIO - User Management")))

;; User Profile page (requires authentication)
(define-page user-profile #@"/profile" ()
  "User profile page"
  (require-authentication)
  (let ((template-path (merge-pathnames "template/profile.chtml"
                                       (asdf:system-source-directory :asteroid))))
    (clip:process-to-string 
     (plump:parse (alexandria:read-file-into-string template-path))
     :title "🎧 admin - Profile | Asteroid Radio"
     :username "admin"
     :user-role "admin"
     :join-date "Unknown"
     :last-active "Unknown"
     :total-listen-time "0h 0m"
     :tracks-played "0"
     :session-count "0"
     :favorite-genre "Unknown"
     :recent-track-1-title ""
     :recent-track-1-artist ""
     :recent-track-1-duration ""
     :recent-track-1-played-at ""
     :recent-track-2-title ""
     :recent-track-2-artist ""
     :recent-track-2-duration ""
     :recent-track-2-played-at ""
     :recent-track-3-title ""
     :recent-track-3-artist ""
     :recent-track-3-duration ""
     :recent-track-3-played-at ""
     :top-artist-1 ""
     :top-artist-1-plays ""
     :top-artist-2 ""
     :top-artist-2-plays ""
     :top-artist-3 ""
     :top-artist-3-plays ""
     :top-artist-4 ""
     :top-artist-4-plays ""
     :top-artist-5 ""
     :top-artist-5-plays "")))

;; Helper functions for profile page - TEMPORARILY COMMENTED OUT
#|
(defun format-timestamp (stream timestamp &key format)
  "Format a timestamp for display"
  (declare (ignore stream format))
  (if timestamp
      (multiple-value-bind (second minute hour date month year)
          (decode-universal-time timestamp)
        (format nil "~a ~d, ~d" 
                (nth (1- month) '("January" "February" "March" "April" "May" "June"
                                 "July" "August" "September" "October" "November" "December"))
                date year))
      "Unknown"))

(defun format-relative-time (timestamp)
  "Format a timestamp as relative time (e.g., '2 hours ago')"
  (if timestamp
      (let* ((now (get-universal-time))
             (diff (- now timestamp))
             (minutes (floor diff 60))
             (hours (floor minutes 60))
             (days (floor hours 24)))
        (cond
          ((< diff 60) "Just now")
          ((< minutes 60) (format nil "~d minute~p ago" minutes minutes))
          ((< hours 24) (format nil "~d hour~p ago" hours hours))
          (t (format nil "~d day~p ago" days days))))
      "Unknown"))

;; User Profile page (requires authentication)
(define-page user-profile #@"/profile" ()
  "User profile page with listening statistics and track data"
  (require-authentication)
  (let* ((current-user (auth:current-user))
         (username (gethash "username" current-user))
         (template-path (merge-pathnames "template/profile.chtml"
                                        (asdf:system-source-directory :asteroid))))
    (clip:process-to-string 
     (plump:parse (alexandria:read-file-into-string template-path))
     :title (format nil "🎧 ~a - Profile | Asteroid Radio" username)
     :username (or username "Unknown User")
     :user-role "listener"
     :join-date "Unknown"
     :last-active "Unknown"
     :total-listen-time "0h 0m"
     :tracks-played "0"
     :session-count "0"
     :favorite-genre "Unknown"
     :recent-track-1-title ""
     :recent-track-1-artist ""
     :recent-track-1-duration ""
     :recent-track-1-played-at ""
     :recent-track-2-title ""
     :recent-track-2-artist ""
     :recent-track-2-duration ""
     :recent-track-2-played-at ""
     :recent-track-3-title ""
     :recent-track-3-artist ""
     :recent-track-3-duration ""
     :recent-track-3-played-at ""
     :top-artist-1 ""
     :top-artist-1-plays ""
     :top-artist-2 ""
     :top-artist-2-plays ""
     :top-artist-3 ""
     :top-artist-3-plays ""
     :top-artist-4 ""
     :top-artist-4-plays ""
     :top-artist-5 ""
     :top-artist-5-plays "")))
|#

;; Auth status API endpoint
(define-api asteroid/auth-status () ()
  "Check if user is logged in and their role"
  (handler-case
      (let* ((user-id (session:field "user-id"))
             (user (when user-id (find-user-by-id user-id))))
        (api-output `(("loggedIn" . ,(if user t nil))
                      ("isAdmin" . ,(if (and user (user-has-role-p user :admin)) t nil))
                      ("username" . ,(if user 
                                        (let ((username (gethash "username" user)))
                                          (if (listp username) (first username) username))
                                        nil)))))
    (error (e)
      (api-output `(("loggedIn" . nil)
                    ("isAdmin" . nil)
                    ("error" . ,(format nil "~a" e)))
                  :status 500))))

;; Register page (GET)
(define-page register #@"/register" ()
  "User registration page"
  (let ((username (radiance:post-var "username"))
        (email (radiance:post-var "email"))
        (password (radiance:post-var "password"))
        (confirm-password (radiance:post-var "confirm-password")))
    (if (and username password)
        ;; Handle registration form submission
        (cond
          ;; Validate passwords match
          ((not (string= password confirm-password))
           (render-template-with-plist "register"
            :title "Asteroid Radio - Register"
            :display-error "display: block;"
            :display-success "display: none;"
            :error-message "Passwords do not match"
            :success-message ""))
          
          ;; Check if username already exists
          ((find-user-by-username username)
           (render-template-with-plist "register"
            :title "Asteroid Radio - Register"
            :display-error "display: block;"
            :display-success "display: none;"
            :error-message "Username already exists"
            :success-message ""))
          
          ;; Create the user
          (t
           (if (create-user username email password :role :listener :active t)
               (progn
                 ;; Auto-login after successful registration
                 (let ((user (find-user-by-username username)))
                   (when user
                     (let ((user-id (gethash "_id" user)))
                       (setf (session:field "user-id") (if (listp user-id) (first user-id) user-id)))))
                 (radiance:redirect "/asteroid/"))
               (render-template-with-plist "register"
                :title "Asteroid Radio - Register"
                :display-error "display: block;"
                :display-success "display: none;"
                :error-message "Registration failed. Please try again."
                :success-message ""))))
        ;; Show registration form (no POST data)
        (render-template-with-plist "register"
         :title "Asteroid Radio - Register"
         :display-error "display: none;"
         :display-success "display: none;"
         :error-message ""
         :success-message ""))))

(define-page player #@"/player" ()
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

(define-api asteroid/status () ()
  "Get server status"
  (api-output `(("status" . "running")
                ("server" . "asteroid-radio")
                ("version" . "0.1.0")
                ("uptime" . ,(get-universal-time))
                ("now-playing" . (("title" . "Silence")
                                  ("artist" . "The Void")
                                  ("album" . "Startup Sounds")))
                ("listeners" . 0)
                ("stream-url" . "http://localhost:8000/asteroid.mp3")
                ("stream-status" . "live"))))

;; Live stream status from Icecast
(define-api asteroid/icecast-status () ()
  "Get live status from Icecast server"
  (handler-case
    (let* ((icecast-url "http://localhost:8000/admin/stats.xml")
           (response (drakma:http-request icecast-url 
                                         :want-stream nil
                                         :basic-authorization '("admin" "asteroid_admin_2024"))))
      (if response
          (let ((xml-string (if (stringp response) 
                                response 
                                (babel:octets-to-string response :encoding :utf-8))))
            ;; Simple XML parsing to extract source information
            ;; Look for <source mount="/asteroid.mp3"> sections and extract title, listeners, etc.
            (multiple-value-bind (match-start match-end)
                (cl-ppcre:scan "<source mount=\"/asteroid\\.mp3\">" xml-string)
              (if match-start
                  (let* ((source-section (subseq xml-string match-start 
                                                 (or (cl-ppcre:scan "</source>" xml-string :start match-start)
                                                     (length xml-string))))
                         (title (or (cl-ppcre:regex-replace-all ".*<title>(.*?)</title>.*" source-section "\\1") "Unknown"))
                         (listeners (or (cl-ppcre:regex-replace-all ".*<listeners>(.*?)</listeners>.*" source-section "\\1") "0")))
                    ;; Return JSON in format expected by frontend
                    (api-output
                     `(("icestats" . (("source" . (("listenurl" . "http://localhost:8000/asteroid.mp3")
                                                   ("title" . ,title)
                                                   ("listeners" . ,(parse-integer listeners :junk-allowed t)))))))))
                  ;; No source found, return empty
                  (api-output
                   `(("icestats" . (("source" . nil))))))))
          (api-output
           `(("error" . "Could not connect to Icecast server"))
           :status 503)))
    (error (e)
      (api-output
       `(("error" . ,(format nil "Icecast connection failed: ~a" e)))
       :status 500))))


;; RADIANCE server management functions

(defun start-server (&key (port *server-port*))
  "Start the Asteroid Radio RADIANCE server"
  (format t "Starting Asteroid Radio RADIANCE server on port ~a~%"  port)
  (compile-styles)  ; Generate CSS file using LASS
  
  ;; Ensure RADIANCE environment is properly set before startup
  (unless (radiance:environment)
    (setf (radiance:environment) "default"))
  
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

(defun ensure-radiance-environment ()
  "Ensure RADIANCE environment is properly configured for persistence"
  (unless (radiance:environment)
    (setf (radiance:environment) "default"))
  
  ;; Ensure the database directory exists
  (let ((db-dir (merge-pathnames ".config/radiance/default/i-lambdalite/radiance.db/"
                                 (user-homedir-pathname))))
    (ensure-directories-exist db-dir)
    (format t "Database directory: ~a~%" db-dir)))

(defun -main (&optional args (debug t))
  (declare (ignorable args))
  (format t "~&args of asteroid: ~A~%" args)
  (format t "~%🎵 ASTEROID RADIO - Music for Hackers 🎵~%")
  (format t "Starting RADIANCE web server...~%")
  (when debug
    (slynk:create-server :port 4009 :dont-close t))
  
  ;; Ensure proper environment setup before starting
  (ensure-radiance-environment)
  
  ;; Initialize user management before server starts
  (initialize-user-system)
  
  ;; TODO: Add auto-scan on startup once database timing issues are resolved
  ;; For now, use the "Scan Library" button in the admin interface
  
  (run-server))

