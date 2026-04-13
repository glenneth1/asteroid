;;;; stream-harmony.lisp - CL-Streamer / Harmony integration for Asteroid Radio
;;;; In-process audio streaming via Harmony + cl-streamer.
;;;; Provides the same data interface to frontend-partials and admin APIs.

(in-package :asteroid)

;;; ---- Configuration ----

(defvar *harmony-pipeline* nil
  "The active cl-streamer/harmony audio pipeline.")

(defvar *harmony-stream-port* 8000
  "Port for the cl-streamer HTTP stream server.")

(defvar *shuffle-pipeline* nil
  "The shuffle stream pipeline  - plays random tracks from the music library.")

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
              ;; Different playlist should be active  - start from beginning
              (progn
                (log:info "Scheduled playlist changed: ~A -> ~A, starting from beginning"
                          saved-playlist-name scheduled-name)
                (values file-list playlist-path))
              ;; Same playlist  - resume from saved position
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

(defvar *pending-save-file* nil
  "The file path that will be saved on the NEXT track change.
   This one-track delay ensures we persist the track that was actually playing,
   not the one being loaded during crossfade.")

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
    ;; Save the PREVIOUS track (which was actually playing) and queue this one
    (when *pending-save-file*
      (save-playback-state *pending-save-file*))
    (setf *pending-save-file* file-path)
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
   Returns the current pipeline title, remaining seconds, and a server
   timestamp (epoch ms) of when the metadata last changed. The client
   uses this timestamp plus its known buffer lag to schedule UI updates."
  (when (and *harmony-pipeline*
             (cl-streamer/harmony:pipeline-current-track *harmony-pipeline*))
    (let* ((track-info (cl-streamer/harmony:pipeline-current-track *harmony-pipeline*))
           (display-title (or (getf track-info :display-title) "Unknown"))
           (listeners (cl-streamer:pipeline-listener-count *harmony-pipeline*))
           (track-id (or (find-track-by-title display-title)
                         (find-track-by-file-path (getf track-info :file))))
           (raw-remaining (cl-streamer/harmony:pipeline-track-remaining *harmony-pipeline*))
           (remaining (when raw-remaining (max 0 (floor raw-remaining))))
           ;; Server epoch ms when metadata last changed
           (server (cl-streamer/harmony:pipeline-server *harmony-pipeline*))
           (changed-at (when server
                         (cl-streamer:get-metadata-changed-at
                          server (format nil "/~A" mount)))))
      `((:listenurl . ,(format nil "~A/~A" *stream-base-url* mount))
        (:title . ,display-title)
        (:listeners . ,(or listeners 0))
        (:track-id . ,track-id)
        (:favorite-count . ,(or (get-track-favorite-count display-title) 0))
        ,@(when remaining `((:remaining . ,remaining)))
        ,@(when changed-at `((:changed-at . ,changed-at)))))))

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

  ;; Create pipeline from declarative spec  - server, mounts, encoders all handled
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
   Pipeline owns encoders and server  - cleanup is automatic."
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
        ;; Store pending playlist path on pipeline  - it will be applied
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
            (listeners (cl-streamer:pipeline-listener-count *harmony-pipeline*)))
        (list :running t
              :current-track (getf track :display-title)
              :artist (getf track :artist)
              :title (getf track :title)
              :album (getf track :album)
              :listeners listeners
              :queue-length (length (cl-streamer/harmony:pipeline-get-queue
                                    *harmony-pipeline*))))
      (list :running nil)))

;;; ============================================================
;;; Shuffle Stream  - random tracks from the music library
;;; ============================================================

(defvar *shuffle-batch-size* 20
  "Number of tracks to queue at a time on the shuffle pipeline.")

(defun scan-music-library-files (&optional (directory *music-library-path*))
  "Recursively scan DIRECTORY for supported audio files.
   Returns a list of native namestrings (real OS paths without SBCL's
   bracket escaping, safe to pass through parse-native-namestring later)."
  (let ((files nil)
        (extensions *supported-formats*))
    (labels ((scan (dir)
               (handler-case
                   (dolist (entry (uiop:directory-files dir))
                     (let ((ext (pathname-type entry)))
                       (when (and ext (member ext extensions :test #'string-equal))
                         (push (sb-ext:native-namestring entry) files))))
                 (error (e)
                   (log:debug "Error scanning ~A: ~A" dir e)))
               (handler-case
                   (dolist (sub (uiop:subdirectories dir))
                     (scan sub))
                 (error (e)
                   (log:debug "Error listing subdirs of ~A: ~A" dir e)))))
      ;; Use parse-native-namestring so directories with brackets (e.g.
      ;; "[FLAC]") are not treated as wildcard patterns by SBCL.
      (scan (sb-ext:parse-native-namestring
             (etypecase directory
               (string directory)
               (pathname (sb-ext:native-namestring directory)))
             nil *default-pathname-defaults*
             :as-directory t)))
    (nreverse files)))

(defvar *shuffle-library-cache* nil
  "Cached list of audio files from the music library for shuffle.")

(defvar *shuffle-library-cache-time* 0
  "Universal time when *shuffle-library-cache* was last refreshed.")

(defvar *shuffle-cache-ttl* 3600
  "Seconds before the shuffle library cache expires (default 1 hour).")

(defun get-shuffle-library ()
  "Return the cached list of music library files, refreshing if stale."
  (when (or (null *shuffle-library-cache*)
            (> (- (get-universal-time) *shuffle-library-cache-time*)
               *shuffle-cache-ttl*))
    (log:info "Scanning music library for shuffle pool...")
    (let ((files (scan-music-library-files)))
      (setf *shuffle-library-cache* files
            *shuffle-library-cache-time* (get-universal-time))
      (log:info "Shuffle pool: ~A tracks" (length files))))
  *shuffle-library-cache*)

(defun shuffle-random-batch (&optional (n *shuffle-batch-size*))
  "Pick N random tracks from the music library (with replacement for small libs)."
  (let ((library (get-shuffle-library)))
    (when library
      (let ((len (length library)))
        (loop repeat (min n len)
              collect (list :file (nth (random len) library)))))))

(defun refill-shuffle-queue ()
  "Queue another batch of random tracks on the shuffle pipeline.
   Called by the track-change hook when the queue is running low."
  (when *shuffle-pipeline*
    (let ((queue-len (length (cl-streamer/harmony:pipeline-get-queue *shuffle-pipeline*))))
      (when (< queue-len (floor *shuffle-batch-size* 2))
        (let ((batch (shuffle-random-batch)))
          (when batch
            (cl-streamer/harmony:pipeline-queue-files *shuffle-pipeline* batch)
            (log:debug "Shuffle: queued ~A tracks (~A in queue)"
                       (length batch) (+ queue-len (length batch)))))))))

(defun on-shuffle-track-change (pipeline track-info)
  "Called by cl-streamer when the shuffle stream changes tracks.
   Updates the shuffle recently-played list and refills the queue."
  (declare (ignore pipeline))
  (let* ((display-title (getf track-info :display-title))
         (artist (getf track-info :artist))
         (title (getf track-info :title))
         (file-path (getf track-info :file))
         (track-id (or (find-track-by-title display-title)
                       (find-track-by-file-path file-path))))
    (when (and display-title (not (string= display-title "Unknown")))
      (add-recently-played (list :title display-title
                                 :artist artist
                                 :song title
                                 :timestamp (get-universal-time)
                                 :track-id track-id)
                           :shuffle)
      (setf *last-known-track-shuffle* display-title))
    (log:info "Shuffle track change: ~A" display-title))
  (refill-shuffle-queue))

(defun shuffle-now-playing (&optional (mount "shuffle.mp3"))
  "Get now-playing information from the shuffle pipeline."
  (when (and *shuffle-pipeline*
             (cl-streamer/harmony:pipeline-current-track *shuffle-pipeline*))
    (let* ((track-info (cl-streamer/harmony:pipeline-current-track *shuffle-pipeline*))
           (display-title (or (getf track-info :display-title) "Unknown"))
           (listeners (cl-streamer:pipeline-listener-count *shuffle-pipeline*)))
      `((:listenurl . ,(format nil "~A/~A" *stream-base-url* mount))
        (:title . ,display-title)
        (:listeners . ,(or listeners 0))
        (:track-id . nil)
        (:favorite-count . 0)))))

;;; ---- Shuffle Pipeline Lifecycle ----

(defun start-shuffle-streaming (&key (mp3-bitrate 128) (aac-bitrate 128))
  "Start the shuffle pipeline, sharing the curated pipeline's stream server.
   Must be called after start-harmony-streaming."
  (when *shuffle-pipeline*
    (log:warn "Shuffle streaming already running")
    (return-from start-shuffle-streaming *shuffle-pipeline*))
  (unless *harmony-pipeline*
    (error "Cannot start shuffle pipeline: curated pipeline not running"))
  (let ((shared-server (cl-streamer/harmony:pipeline-server *harmony-pipeline*)))
    (setf *shuffle-pipeline*
          (cl-streamer/harmony:make-pipeline
           :server shared-server
           :outputs (list (list :format :mp3
                                :mount "/shuffle.mp3"
                                :bitrate mp3-bitrate
                                :name "Asteroid Radio Shuffle MP3")
                          (list :format :aac
                                :mount "/shuffle.aac"
                                :bitrate aac-bitrate
                                :name "Asteroid Radio Shuffle AAC"))))
    ;; Register hooks
    (cl-streamer/harmony:pipeline-add-hook *shuffle-pipeline*
                                           :track-change #'on-shuffle-track-change)
    ;; Seed the queue before starting
    (let ((batch (shuffle-random-batch)))
      (when batch
        (cl-streamer/harmony:pipeline-queue-files *shuffle-pipeline* batch)))
    ;; Start the pipeline and begin playback
    (cl-streamer/harmony:pipeline-start *shuffle-pipeline*)
    ;; Start the play-list loop (plays queued tracks, refill hook keeps it going)
    (let ((initial-files (mapcar (lambda (entry) (getf entry :file))
                                 (cl-streamer/harmony:pipeline-get-queue *shuffle-pipeline*))))
      (when initial-files
        (cl-streamer/harmony:play-list *shuffle-pipeline* initial-files
                                       :crossfade-duration 3.0
                                       :loop-queue t)))
    (log:info "Shuffle streaming started (MP3 + AAC, ~A tracks in pool)"
              (length (get-shuffle-library)))
    *shuffle-pipeline*))

(defun stop-shuffle-streaming ()
  "Stop the shuffle pipeline. Does not stop the shared server."
  (when *shuffle-pipeline*
    (cl-streamer/harmony:pipeline-stop *shuffle-pipeline*)
    (setf *shuffle-pipeline* nil))
  (log:info "Shuffle streaming stopped"))
