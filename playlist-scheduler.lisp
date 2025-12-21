;;;; playlist-scheduler.lisp - Automatic Playlist Scheduling for Asteroid Radio
;;;; Uses cl-cron to load time-based playlists at scheduled times

(in-package :asteroid)

;;; Scheduler Configuration

(defvar *playlist-schedule* 
  '((0  . "midnight-ambient.m3u")   ; 00:00 UTC
    (6  . "morning-drift.m3u")      ; 06:00 UTC
    (12 . "afternoon-orbit.m3u")    ; 12:00 UTC
    (18 . "evening-descent.m3u"))   ; 18:00 UTC
  "Association list mapping hours (UTC) to playlist filenames.
   Each entry is (hour . playlist-filename).")

(defvar *scheduler-enabled* t
  "When true, the playlist scheduler is active.")

(defvar *scheduler-running* nil
  "Internal flag tracking if scheduler cron jobs are registered.")

;;; Scheduler Functions

(defun get-scheduled-playlist-for-hour (hour)
  "Get the playlist filename scheduled for a given hour.
   Returns the playlist for the most recent scheduled time slot."
  (let ((sorted-schedule (sort (copy-list *playlist-schedule*) #'> :key #'car)))
    (or (cdr (find-if (lambda (entry) (<= (car entry) hour)) sorted-schedule))
        (cdar (last sorted-schedule)))))

(defun get-current-scheduled-playlist ()
  "Get the playlist that should be playing right now based on UTC time."
  (let ((current-hour (local-time:timestamp-hour (local-time:now) :timezone local-time:+utc-zone+)))
    (get-scheduled-playlist-for-hour current-hour)))

(defun load-scheduled-playlist (playlist-name)
  "Load a playlist by name, copying it to stream-queue.m3u and triggering playback."
  (let ((playlist-path (merge-pathnames playlist-name (get-playlists-directory))))
    (if (probe-file playlist-path)
        (progn
          (format t "~&[SCHEDULER] Loading playlist: ~a~%" playlist-name)
          (copy-playlist-to-stream-queue playlist-path)
          (load-queue-from-m3u-file)
          (handler-case
              (progn
                (liquidsoap-command "stream-queue_m3u.skip")
                (format t "~&[SCHEDULER] Playlist ~a loaded and crossfade triggered~%" playlist-name))
            (error (e)
              (format t "~&[SCHEDULER] Warning: Could not skip track: ~a~%" e)))
          t)
        (progn
          (format t "~&[SCHEDULER] Error: Playlist not found: ~a~%" playlist-name)
          nil))))

(defun scheduled-playlist-loader (hour playlist-name)
  "Create a function that loads a specific playlist. Used by cl-cron jobs."
  (lambda ()
    (when *scheduler-enabled*
      (format t "~&[SCHEDULER] Triggered at hour ~a UTC - loading ~a~%" hour playlist-name)
      (load-scheduled-playlist playlist-name))))

;;; Cron Job Management

(defun setup-playlist-cron-jobs ()
  "Set up cl-cron jobs for all scheduled playlists."
  (unless *scheduler-running*
    (format t "~&[SCHEDULER] Setting up playlist schedule:~%")
    (dolist (entry *playlist-schedule*)
      (let ((hour (car entry))
            (playlist (cdr entry)))
        (format t "~&[SCHEDULER]   ~2,'0d:00 UTC -> ~a~%" hour playlist)
        (cl-cron:make-cron-job 
         (scheduled-playlist-loader hour playlist)
         :minute 0 
         :hour hour)))
    (setf *scheduler-running* t)
    (format t "~&[SCHEDULER] Playlist schedule configured~%")))

(defun start-playlist-scheduler ()
  "Start the playlist scheduler. Sets up cron jobs and starts cl-cron."
  (setup-playlist-cron-jobs)
  (cl-cron:start-cron)
  (format t "~&[SCHEDULER] Playlist scheduler started~%")
  t)

(defun stop-playlist-scheduler ()
  "Stop the playlist scheduler."
  (cl-cron:stop-cron)
  (setf *scheduler-running* nil)
  (format t "~&[SCHEDULER] Playlist scheduler stopped~%")
  t)

(defun restart-playlist-scheduler ()
  "Restart the playlist scheduler with current configuration."
  (stop-playlist-scheduler)
  (start-playlist-scheduler))

;;; Schedule Management (Database-backed)

(defun load-schedule-from-db ()
  "Load the playlist schedule from the database into *playlist-schedule*."
  (handler-case
      (with-db
        (let ((rows (postmodern:query "SELECT hour, playlist FROM playlist_schedule ORDER BY hour")))
          (when rows
            (setf *playlist-schedule*
                  (mapcar (lambda (row)
                            (cons (first row) (second row)))
                          rows))
            (format t "~&[SCHEDULER] Loaded ~a schedule entries from database~%" (length rows)))))
    (error (e)
      (format t "~&[SCHEDULER] Warning: Could not load schedule from DB: ~a~%" e)
      (format t "~&[SCHEDULER] Using default schedule~%"))))

(defun save-schedule-entry-to-db (hour playlist-name)
  "Save or update a schedule entry in the database."
  (handler-case
      (with-db
        (postmodern:query 
         (:insert-into 'playlist_schedule
          :set 'hour hour 'playlist playlist-name 'updated_at (:now))
         :on-conflict-update 'hour
         :update-set 'playlist playlist-name 'updated_at (:now)))
    (error (e)
      ;; Try simpler upsert approach
      (handler-case
          (with-db
            (postmodern:query
             (format nil "INSERT INTO playlist_schedule (hour, playlist, updated_at) VALUES (~a, '~a', NOW()) ON CONFLICT (hour) DO UPDATE SET playlist = '~a', updated_at = NOW()"
                     hour playlist-name playlist-name)))
        (error (e2)
          (format t "~&[SCHEDULER] Warning: Could not save schedule entry: ~a~%" e2))))))

(defun delete-schedule-entry-from-db (hour)
  "Delete a schedule entry from the database."
  (handler-case
      (with-db
        (postmodern:query (:delete-from 'playlist_schedule :where (:= 'hour hour))))
    (error (e)
      (format t "~&[SCHEDULER] Warning: Could not delete schedule entry: ~a~%" e))))

(defun add-scheduled-playlist (hour playlist-name)
  "Add or update a playlist in the schedule (persists to database)."
  (save-schedule-entry-to-db hour playlist-name)
  (setf *playlist-schedule* 
        (cons (cons hour playlist-name)
              (remove hour *playlist-schedule* :key #'car)))
  (when *scheduler-running*
    (restart-playlist-scheduler))
  *playlist-schedule*)

(defun remove-scheduled-playlist (hour)
  "Remove a playlist from the schedule (persists to database)."
  (delete-schedule-entry-from-db hour)
  (setf *playlist-schedule* 
        (remove hour *playlist-schedule* :key #'car))
  (when *scheduler-running*
    (restart-playlist-scheduler))
  *playlist-schedule*)

(defun get-schedule ()
  "Get the current playlist schedule as a sorted list."
  (sort (copy-list *playlist-schedule*) #'< :key #'car))

(defun get-available-playlists ()
  "Get list of available playlist files from the playlists directory and user-submissions."
  (let ((playlists-dir (get-playlists-directory))
        (submissions-dir (merge-pathnames "user-submissions/" (get-playlists-directory))))
    (append
     ;; Main playlists directory
     (when (probe-file playlists-dir)
       (mapcar #'file-namestring
               (directory (merge-pathnames "*.m3u" playlists-dir))))
     ;; User submissions directory (prefixed with user-submissions/)
     (when (probe-file submissions-dir)
       (mapcar (lambda (path) 
                 (format nil "user-submissions/~a" (file-namestring path)))
               (directory (merge-pathnames "*.m3u" submissions-dir)))))))

(defun get-server-time-info ()
  "Get current server time information in both UTC and local timezone."
  (let* ((now (local-time:now))
         (utc-hour (local-time:timestamp-hour now :timezone local-time:+utc-zone+))
         (utc-minute (local-time:timestamp-minute now :timezone local-time:+utc-zone+)))
    (list :utc-time (local-time:format-timestring nil now 
                      :format '(:year "-" (:month 2) "-" (:day 2) " " (:hour 2) ":" (:min 2) ":" (:sec 2) " UTC")
                      :timezone local-time:+utc-zone+)
          :utc-hour utc-hour
          :utc-minute utc-minute
          :local-time (local-time:format-timestring nil now
                        :format '(:year "-" (:month 2) "-" (:day 2) " " (:hour 2) ":" (:min 2) ":" (:sec 2))))))

(defun get-scheduler-status ()
  "Get the current status of the scheduler."
  (let ((time-info (get-server-time-info)))
    (list :enabled *scheduler-enabled*
          :running *scheduler-running*
          :current-playlist (get-current-scheduled-playlist)
          :schedule (get-schedule)
          :server-time time-info)))

;;; API Endpoints for Admin Interface

(define-api asteroid/scheduler/status () ()
  "Get the current scheduler status"
  (require-role :admin)
  (with-error-handling
    (let* ((status (get-scheduler-status))
           (time-info (getf status :server-time))
           (available-playlists (get-available-playlists)))
      (api-output `(("status" . "success")
                    ("enabled" . ,(if (getf status :enabled) t :json-false))
                    ("running" . ,(if (getf status :running) t :json-false))
                    ("current_playlist" . ,(getf status :current-playlist))
                    ("server_time" . (("utc" . ,(getf time-info :utc-time))
                                      ("utc_hour" . ,(getf time-info :utc-hour))
                                      ("local" . ,(getf time-info :local-time))))
                    ("schedule" . ,(mapcar (lambda (entry)
                                             `(("hour" . ,(car entry))
                                               ("playlist" . ,(cdr entry))))
                                           (getf status :schedule)))
                    ("available_playlists" . ,(coerce available-playlists 'vector)))))))

(define-api asteroid/scheduler/enable () ()
  "Enable the playlist scheduler"
  (require-role :admin)
  (with-error-handling
    (setf *scheduler-enabled* t)
    (unless *scheduler-running*
      (start-playlist-scheduler))
    (api-output `(("status" . "success")
                  ("message" . "Scheduler enabled")))))

(define-api asteroid/scheduler/disable () ()
  "Disable the playlist scheduler (stops automatic playlist changes)"
  (require-role :admin)
  (with-error-handling
    (setf *scheduler-enabled* nil)
    (api-output `(("status" . "success")
                  ("message" . "Scheduler disabled - playlists will not auto-change")))))

(define-api asteroid/scheduler/load-current () ()
  "Manually load the playlist that should be playing now based on schedule"
  (require-role :admin)
  (with-error-handling
    (let ((playlist (get-current-scheduled-playlist)))
      (if (load-scheduled-playlist playlist)
          (api-output `(("status" . "success")
                        ("message" . ,(format nil "Loaded scheduled playlist: ~a" playlist))
                        ("playlist" . ,playlist)))
          (api-output `(("status" . "error")
                        ("message" . ,(format nil "Failed to load playlist: ~a" playlist)))
                      :status 500)))))

(define-api asteroid/scheduler/schedule () ()
  "Get the current playlist schedule"
  (require-role :admin)
  (with-error-handling
    (api-output `(("status" . "success")
                  ("schedule" . ,(mapcar (lambda (entry)
                                           `(("hour" . ,(car entry))
                                             ("playlist" . ,(cdr entry))
                                             ("time_label" . ,(format nil "~2,'0d:00 UTC" (car entry)))))
                                         (get-schedule)))))))

(define-api asteroid/scheduler/update (hour playlist) ()
  "Add or update a scheduled playlist (hour is 0-23 UTC)"
  (require-role :admin)
  (with-error-handling
    (let ((hour-int (parse-integer hour :junk-allowed t)))
      (if (and hour-int (>= hour-int 0) (<= hour-int 23))
          (let ((playlist-path (merge-pathnames playlist (get-playlists-directory))))
            (if (probe-file playlist-path)
                (progn
                  (add-scheduled-playlist hour-int playlist)
                  (api-output `(("status" . "success")
                                ("message" . ,(format nil "Schedule updated: ~2,'0d:00 UTC -> ~a" hour-int playlist))
                                ("schedule" . ,(mapcar (lambda (entry)
                                                         `(("hour" . ,(car entry))
                                                           ("playlist" . ,(cdr entry))))
                                                       (get-schedule))))))
                (api-output `(("status" . "error")
                              ("message" . ,(format nil "Playlist not found: ~a" playlist)))
                            :status 404)))
          (api-output `(("status" . "error")
                        ("message" . "Invalid hour - must be 0-23"))
                      :status 400)))))

(define-api asteroid/scheduler/remove (hour) ()
  "Remove a scheduled playlist"
  (require-role :admin)
  (with-error-handling
    (let ((hour-int (parse-integer hour :junk-allowed t)))
      (if (and hour-int (>= hour-int 0) (<= hour-int 23))
          (progn
            (remove-scheduled-playlist hour-int)
            (api-output `(("status" . "success")
                          ("message" . ,(format nil "Removed schedule for ~2,'0d:00 UTC" hour-int))
                          ("schedule" . ,(mapcar (lambda (entry)
                                                   `(("hour" . ,(car entry))
                                                     ("playlist" . ,(cdr entry))))
                                                 (get-schedule))))))
          (api-output `(("status" . "error")
                        ("message" . "Invalid hour - must be 0-23"))
                      :status 400)))))

;;; Auto-start scheduler when database is connected
;;; This ensures the scheduler starts after the server is fully initialized

(define-trigger db:connected ()
  "Start the playlist scheduler after database connection is established"
  (format t "~&[SCHEDULER] Database connected, starting playlist scheduler...~%")
  (handler-case
      (progn
        ;; Load schedule from database first
        (load-schedule-from-db)
        (start-playlist-scheduler)
        ;; Load the current scheduled playlist on startup
        (let ((current-playlist (get-current-scheduled-playlist)))
          (when current-playlist
            (format t "~&[SCHEDULER] Loading current scheduled playlist: ~a~%" current-playlist)
            (load-scheduled-playlist current-playlist)))
        (format t "~&[SCHEDULER] Scheduler auto-started successfully~%"))
    (error (e)
      (format t "~&[SCHEDULER] Warning: Could not auto-start scheduler: ~a~%" e))))
