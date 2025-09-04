;; -*-lisp-*-
;; (defpackage :asteroid
;;   (:use :cl :radiance)
;;   (:use :asteroid.app-utils)

;;   (:export :-main :start-server :stop-server :run-server))

(in-package :asteroid)

;; Define as RADIANCE module
(define-module asteroid
  (:use #:cl #:radiance #:lass)
  (:domain "asteroid"))

;; Configuration
(defparameter *server-port* 8080)

;; Read and compile LASS from file
(defun generate-css ()
  "Generate CSS by reading LASS from static/asteroid.lass file"
  (let ((lass-file (merge-pathnames "static/asteroid.lass")))
    (lass:compile-and-write
     (with-open-file (in lass-file)
       (read in)))))

;; Generate CSS file using LASS
(defun compile-styles ()
  "Generate CSS file using LASS"
  (ensure-directories-exist "static/")
  (let ((css-file (merge-pathnames "static/asteroid.css")))
    (with-open-file (out css-file
                         :direction :output
                         :if-exists :supersede)
      (write-string (generate-css) out))))

;; Configure static file serving for other files
(define-page static #@"/static/(.*)" (:uri-groups (path))
  (serve-file (merge-pathnames (concatenate 'string "static/" path) 
                               (asdf:system-source-directory :asteroid))))

;; RADIANCE route handlers
(define-page index #@"/" ()
  (spinneret:with-html-string
    (:doctype)
    (:html
     (:head
      (:title "🎵 ASTEROID RADIO 🎵")
      (:meta :charset "utf-8")
      (:meta :name "viewport" :content "width=device-width, initial-scale=1")
      (:link :rel "stylesheet" :type "text/css" :href "/static/asteroid.css"))
     (:body
      (:div.container
       (:h1 "🎵 ASTEROID RADIO 🎵")
       (:div.status
        (:h2 "Station Status")
        (:p "🟢 LIVE - Broadcasting asteroid music for hackers")
        (:p "Current listeners: 0")
        (:p "Stream quality: 128kbps MP3"))
       (:div.nav
        (:a :href "/admin" "Admin Dashboard")
        (:a :href "/player" "Web Player")
        (:a :href "/api/status" "API Status"))
       (:div
        (:h2 "Now Playing")
        (:p "Artist: The Void")
        (:p "Track: Silence")
        (:p "Album: Startup Sounds")
        (:p "Duration: ∞")))))))

(define-page admin #@"/admin" ()
  (spinneret:with-html-string
    (:doctype)
    (:html
     (:head
      (:title "Asteroid Radio - Admin Dashboard")
      (:meta :charset "utf-8")
      (:link :rel "stylesheet" :type "text/css" :href "/static/asteroid.css"))
     (:body
      (:div.container
       (:a.back :href "/" "← Back to Main")
       (:h1 "Admin Dashboard")
       (:div.panel
        (:h2 "Playback Control")
        (:button "Play")
        (:button "Pause")
        (:button "Skip")
        (:button "Stop"))
       (:div.panel
        (:h2 "Library Management")
        (:button "Upload Music")
        (:button "Manage Playlists")
        (:button "Scan Library"))
       (:div.panel
        (:h2 "Live DJ")
        (:button "Go Live")
        (:button "End Session")
        (:button "Mic Check"))
       (:div.panel
        (:h2 "System Status")
        (:p "Server: Running")
        (:p "Database: Not Connected")
        (:p "Liquidsoap: Not Running")
        (:p "Icecast: Not Running")))))))

(define-page player #@"/player" ()
  (spinneret:with-html-string
    (:doctype)
    (:html
     (:head
      (:title "Asteroid Radio - Web Player")
      (:meta :charset "utf-8")
      (:link :rel "stylesheet" :type "text/css" :href "/static/asteroid.css"))
     (:body
      (:a.back :href "/" "← Back to Main")
      (:div.player
       (:h1 "🎵 ASTEROID RADIO PLAYER 🎵")
       (:div.now-playing
        (:div "Now Playing:")
        (:div "Silence - The Sound of Startup"))
       (:div.controls
        (:button "▶ Play Stream")
        (:button "⏸ Pause")
        (:button "🔊 Volume"))
       (:div
        (:p "Stream URL: http://localhost:8000/asteroid")
        (:p "Bitrate: 128kbps MP3")
        (:p "Status: Offline")))))))

(define-page api/status #@"/api/status" ()
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

