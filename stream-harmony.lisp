;;;; stream-harmony.lisp - CL-Streamer / Harmony integration for Asteroid Radio
;;;; In-process audio streaming via Harmony + cl-streamer.
;;;; Provides the same data interface to frontend-partials and admin APIs.

(in-package :asteroid)

;;; ---- Configuration ----

(defvar *harmony-pipeline* nil
  "The active cl-streamer/harmony audio pipeline.")

(defvar *harmony-stream-port* 8000
  "Port for the cl-streamer HTTP stream server.")

;; Encoder instances are now owned by the pipeline (Phase 2).
;; Kept as aliases for backward compatibility with any external references.
(defun harmony-mp3-encoder ()
  "Get the MP3 encoder from the pipeline (if running)."
  (when *harmony-pipeline*
    (car (find "/asteroid.mp3" (cl-streamer/harmony:pipeline-encoders *harmony-pipeline*)
               :key #'cdr :test #'string=))))

(defun harmony-aac-encoder ()
  "Get the AAC encoder from the pipeline (if running)."
  (when *harmony-pipeline*
    (car (find "/asteroid.aac" (cl-streamer/harmony:pipeline-encoders *harmony-pipeline*)
               :key #'cdr :test #'string=))))

(defvar *harmony-state-file*
  (merge-pathnames ".playback-state.lisp" (asdf:system-source-directory :asteroid))
  "File to persist current playback position across restarts.")

;;; ---- Playback State Persistence ----

(defvar *current-playlist-path* nil
  "Path of the currently active playlist file.")

(defvar *resumed-from-saved-state* nil
  "Set to T when startup successfully resumed from saved playback state.
   Prevents the scheduler from overwriting the resumed position.")

(defun save-playback-state (track-file-path)
  "Save the current track file path and playlist to the state file.
   Called on each track change so we can resume after restart."
  (handler-case
      (with-open-file (s *harmony-state-file*
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
        (prin1 (list :track-file track-file-path
                     :playlist (when *current-playlist-path*
                                 (namestring *current-playlist-path*))
                     :timestamp (get-universal-time))
               s))
    (error (e)
      (log:warn "Could not save playback state: ~A" e))))

(defun load-playback-state ()
  "Load the saved playback state. Returns plist or NIL."
  (handler-case
      (when (probe-file *harmony-state-file*)
        (with-open-file (s *harmony-state-file* :direction :input)
          (read s nil nil)))
    (error (e)
      (log:warn "Could not load playback state: ~A" e)
      nil)))

(defun resume-from-saved-state ()
  "Load saved playback state, resolve the correct playlist, and return
   (values file-list playlist-path) starting after the saved track.
   If the currently scheduled playlist differs from the saved one,
   uses the scheduled playlist from the beginning instead.
   Returns NIL if no state or playlist found."
  (let ((state (load-playback-state)))
    (when state
      (let* ((saved-file (getf state :track-file))
             (saved-playlist (getf state :playlist))
             (saved-playlist-name (when saved-playlist
                                    (file-namestring (pathname saved-playlist))))
             ;; Check what should be playing right now
             (scheduled-name (get-current-scheduled-playlist))
             (scheduled-path (when scheduled-name
                               (let ((p (merge-pathnames scheduled-name (get-playlists-directory))))
                                 (probe-file p))))
             ;; If scheduled playlist differs from saved, use scheduled (start fresh)
             (playlist-changed-p (and scheduled-name saved-playlist-name
                                      (not (string= scheduled-name saved-playlist-name))))
             (playlist-path (if playlist-changed-p
                                scheduled-path
                                (or (and saved-playlist
                                         (probe-file (pathname saved-playlist)))
                                    scheduled-path)))
             (file-list (when playlist-path
                          (m3u-to-file-list playlist-path))))
        (when file-list
          (setf *current-playlist-path* playlist-path)
          (setf *resumed-from-saved-state* t)
          (if playlist-changed-p
              ;; Different playlist should be active — start from beginning
              (progn
                (log:info "Scheduled playlist changed: ~A -> ~A, starting from beginning"
                          saved-playlist-name scheduled-name)
                (values file-list playlist-path))
              ;; Same playlist — resume from saved position
              (let ((pos (when saved-file
                           (position saved-file file-list :test #'string=))))
                (if pos
                    (let ((remaining (nthcdr (1+ pos) file-list)))
                      (if remaining
                          (progn
                            (log:info "Resuming after track ~A (~A of ~A) from ~A"
                                      (file-namestring saved-file) (1+ pos) (length file-list)
                                      (file-namestring playlist-path))
                            (values remaining playlist-path))
                          (progn
                            (log:info "Last saved track was final, restarting ~A"
                                      (file-namestring playlist-path))
                            (values file-list playlist-path))))
                    (progn
                      (log:info "Saved track not found, starting ~A from beginning"
                                (file-namestring playlist-path))
                      (values file-list playlist-path))))))))))

;;; ---- M3U Playlist Loading ----

(defun m3u-to-file-list (m3u-path)
  "Parse an M3U playlist file and return a list of host file paths.
   Converts Docker paths (/app/music/...) back to host paths.
   Skips comment lines and blank lines."
  (when (probe-file m3u-path)
    (with-open-file (stream m3u-path :direction :input)
      (loop for line = (read-line stream nil)
            while line
            for trimmed = (string-trim '(#\Space #\Tab #\Return #\Newline) line)
            unless (or (string= trimmed "")
                       (and (> (length trimmed) 0) (char= (char trimmed 0) #\#)))
              collect (convert-from-docker-path trimmed)))))

;;; ---- Track & Playlist Change Callbacks ----

(defun on-harmony-playlist-change (pipeline playlist-path)
  "Called by cl-streamer when a scheduler playlist actually starts playing.
   Updates *current-playlist-path* only now, not at queue time."
  (declare (ignore pipeline))
  (setf *current-playlist-path* playlist-path)
  (log:info "Playlist now active: ~A" (file-namestring playlist-path)))

(defun on-harmony-track-change (pipeline track-info)
  "Called by cl-streamer when a track changes.
   Updates recently-played lists and finds the track in the database."
  (declare (ignore pipeline))
  (let* ((display-title (getf track-info :display-title))
         (artist (getf track-info :artist))
         (title (getf track-info :title))
         (file-path (getf track-info :file))
         (track-id (or (find-track-by-title display-title)
                       (find-track-by-file-path file-path))))
    (when (and display-title
               (not (string= display-title "Unknown")))
      ;; Update recently played (curated stream)
      (add-recently-played (list :title display-title
                                 :artist artist
                                 :song title
                                 :timestamp (get-universal-time)
                                 :track-id track-id)
                           :curated)
      (setf *last-known-track-curated* display-title))
    ;; Persist current track for resume-on-restart
    (when file-path
      (save-playback-state file-path))
    (log:info "Track change: ~A (track-id: ~A)" display-title track-id)))

(defun find-track-by-file-path (file-path)
  "Find a track in the database by file path. Returns track ID or nil."
  (when file-path
    (handler-case
        (with-db
          (postmodern:query
           (:limit
            (:select '_id :from 'tracks
             :where (:= 'file-path file-path))
            1)
           :single))
      (error () nil))))

;;; ---- Now-Playing Data Source ----
;;; These functions provide now-playing data from cl-streamer's pipeline state.

(defun harmony-now-playing (&optional (mount "asteroid.mp3"))
  "Get now-playing information from cl-streamer pipeline.
   Returns an alist with now-playing data, or NIL if the pipeline is not running."
  (when (and *harmony-pipeline*
             (cl-streamer/harmony:pipeline-current-track *harmony-pipeline*))
    (let* ((track-info (cl-streamer/harmony:pipeline-current-track *harmony-pipeline*))
           (display-title (or (getf track-info :display-title) "Unknown"))
           (listeners (cl-streamer:get-listener-count))
           (track-id (or (find-track-by-title display-title)
                         (find-track-by-file-path (getf track-info :file)))))
      `((:listenurl . ,(format nil "~A/~A" *stream-base-url* mount))
        (:title . ,display-title)
        (:listeners . ,(or listeners 0))
        (:track-id . ,track-id)
        (:favorite-count . ,(or (get-track-favorite-count display-title) 0))))))

;;; ---- Pipeline Lifecycle ----

(defun start-harmony-streaming (&key (port *harmony-stream-port*)
                                     (mp3-bitrate 128)
                                     (aac-bitrate 128))
  "Start the cl-streamer pipeline with MP3 and AAC outputs.
   Should be called once during application startup.
   MP3-BITRATE and AAC-BITRATE are in kbps (e.g. 128)."
  (when *harmony-pipeline*
    (log:warn "Harmony streaming already running")
    (return-from start-harmony-streaming *harmony-pipeline*))

  ;; Create pipeline from declarative spec — server, mounts, encoders all handled
  (setf *harmony-pipeline*
        (cl-streamer/harmony:make-pipeline
         :port port
         :outputs (list (list :format :mp3
                              :mount "/asteroid.mp3"
                              :bitrate mp3-bitrate
                              :name "Asteroid Radio MP3")
                        (list :format :aac
                              :mount "/asteroid.aac"
                              :bitrate aac-bitrate
                              :name "Asteroid Radio AAC"))))

  ;; Register hooks
  (cl-streamer/harmony:pipeline-add-hook *harmony-pipeline*
                                         :track-change #'on-harmony-track-change)
  (cl-streamer/harmony:pipeline-add-hook *harmony-pipeline*
                                         :playlist-change #'on-harmony-playlist-change)

  ;; Start the audio pipeline
  (cl-streamer/harmony:pipeline-start *harmony-pipeline*)

  (log:info "Harmony streaming started on port ~A (MP3 + AAC)" port)
  *harmony-pipeline*)

(defun stop-harmony-streaming ()
  "Stop the cl-streamer pipeline and stream server.
   Pipeline owns encoders and server — cleanup is automatic."
  (when *harmony-pipeline*
    (cl-streamer/harmony:pipeline-stop *harmony-pipeline*)
    (setf *harmony-pipeline* nil))
  (log:info "Harmony streaming stopped"))

;;; ---- Playlist Control ----

(defun harmony-load-playlist (m3u-path &key (skip nil))
  "Load and start playing an M3U playlist through the Harmony pipeline.
   Converts Docker paths to host paths and feeds them to play-list.
   When SKIP is T, immediately crossfade to the new playlist.
   When SKIP is NIL (default), queue tracks to play after the current track."
  (when *harmony-pipeline*
    (let ((file-list (m3u-to-file-list m3u-path)))
      (when file-list
        ;; Store pending playlist path on pipeline — it will be applied
        ;; when drain-queue-into-remaining fires and the new tracks
        ;; actually start playing, not now at queue time.
        (setf (cl-streamer/harmony:pipeline-pending-playlist-path *harmony-pipeline*)
              (pathname m3u-path))
        ;; Clear any existing queue and load new files
        (cl-streamer/harmony:pipeline-clear-queue *harmony-pipeline*)
        (cl-streamer/harmony:pipeline-queue-files *harmony-pipeline*
                                                  (mapcar (lambda (path)
                                                            (list :file path))
                                                          file-list))
        ;; Only skip if explicitly requested
        (when skip
          (cl-streamer/harmony:pipeline-skip *harmony-pipeline*))
        (log:info "Loaded playlist ~A (~A tracks)" (file-namestring m3u-path) (length file-list))
        (length file-list)))))

(defun harmony-skip-track ()
  "Skip the current track (crossfades to next)."
  (when *harmony-pipeline*
    (cl-streamer/harmony:pipeline-skip *harmony-pipeline*)
    t))

(defun harmony-get-status ()
  "Get current pipeline status."
  (if *harmony-pipeline*
      (let ((track (cl-streamer/harmony:pipeline-current-track *harmony-pipeline*))
            (listeners (cl-streamer:get-listener-count)))
        (list :running t
              :current-track (getf track :display-title)
              :artist (getf track :artist)
              :title (getf track :title)
              :album (getf track :album)
              :listeners listeners
              :queue-length (length (cl-streamer/harmony:pipeline-get-queue
                                    *harmony-pipeline*))))
      (list :running nil)))
