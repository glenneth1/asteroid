;;;; dj-session.lisp - Live DJ mixing console for Asteroid Radio
;;;; Provides dual-deck library mixing with crossfader, per-deck volume,
;;;; and external audio input, all feeding into the existing Harmony mixer.
;;;; See docs/DJ-CONSOLE.org for the full design document.

(in-package :asteroid)

;;; ---- DJ Deck ----

(defclass dj-deck ()
  ((name :initarg :name :accessor deck-name
         :documentation "Deck identifier: :a or :b")
   (file-path :initform nil :accessor deck-file-path
              :documentation "Path of loaded track, or NIL if empty")
   (voice :initform nil :accessor deck-voice
          :documentation "Harmony voice object, or NIL")
   (volume :initform 1.0 :accessor deck-volume
           :documentation "Per-deck volume 0.0-1.0 (before crossfader)")
   (state :initform :empty :accessor deck-state
          :documentation "One of :empty :loaded :playing :paused")
   (track-info :initform nil :accessor deck-track-info
               :documentation "Plist: (:artist :title :album :file :display-title)")))

(defmethod print-object ((deck dj-deck) stream)
  (print-unreadable-object (deck stream :type t)
    (format stream "~A ~A" (deck-name deck) (deck-state deck))))

;;; ---- DJ Session ----

(defclass dj-session ()
  ((owner :initarg :owner :accessor session-owner
          :documentation "Username of the DJ who owns this session")
   (started-at :initarg :started-at :accessor session-started-at
               :initform (get-universal-time))
   (deck-a :initform (make-instance 'dj-deck :name :a) :accessor session-deck-a)
   (deck-b :initform (make-instance 'dj-deck :name :b) :accessor session-deck-b)
   (crossfader :initform 0.5 :accessor session-crossfader
               :documentation "0.0 = all A, 1.0 = all B")
   (external-input :initform nil :accessor session-external-input
                   :documentation "External audio voice, or NIL")
   (external-volume :initform 1.0 :accessor session-external-volume)
   (metadata-override :initform nil :accessor session-metadata-override
                      :documentation "Custom ICY metadata text, or NIL for auto-detect")
   (last-poll :initform (get-universal-time) :accessor session-last-poll
              :documentation "Timestamp of last UI poll, for watchdog")
   (saved-playlist-state :initform nil :accessor session-saved-playlist-state
                         :documentation "Saved auto-playlist state for resume on session end")))

(defvar *dj-session* nil
  "The currently active DJ session, or NIL if no DJ is live.")

(defvar *dj-session-lock* (bt:make-lock "dj-session-lock")
  "Lock for DJ session state changes.")

(defvar *dj-watchdog-interval* 60
  "Seconds of no UI polling before a DJ session is auto-ended.")

;;; ---- Crossfader Math ----

(defun crossfader-volumes (position)
  "Return (values vol-a vol-b) for crossfader position 0.0-1.0.
   Uses constant-power (equal-power) curve so perceived volume
   stays consistent across the sweep."
  (let* ((pos (max 0.0 (min 1.0 (float position))))
         (angle (* pos (/ pi 2.0))))
    (values (cos angle)    ;; Deck A: 1.0 at pos=0.0, 0.0 at pos=1.0
            (sin angle)))) ;; Deck B: 0.0 at pos=0.0, 1.0 at pos=1.0

(defun effective-deck-volume (deck crossfader-component)
  "Calculate the effective volume for a deck: deck-volume * crossfader-component."
  (* (deck-volume deck) crossfader-component))

(defun apply-crossfader (session)
  "Apply the current crossfader position to both deck voices."
  (multiple-value-bind (vol-a vol-b)
      (crossfader-volumes (session-crossfader session))
    (let ((deck-a (session-deck-a session))
          (deck-b (session-deck-b session)))
      (when (and (deck-voice deck-a) (eq (deck-state deck-a) :playing))
        (setf (org.shirakumo.fraf.mixed:volume (deck-voice deck-a))
              (float (effective-deck-volume deck-a vol-a))))
      (when (and (deck-voice deck-b) (eq (deck-state deck-b) :playing))
        (setf (org.shirakumo.fraf.mixed:volume (deck-voice deck-b))
              (float (effective-deck-volume deck-b vol-b)))))))

;;; ---- Auto-Playlist Pause / Resume ----

(defun pause-auto-playlist ()
  "Pause the auto-playlist by immediately stopping all voices and clearing the queue.
   The play-list thread will exit when it sees the empty queue and skip flag.
   Returns saved state for restoration."
  (when *harmony-pipeline*
    (let ((state (list :playlist-path (when *current-playlist-path*
                                        (namestring *current-playlist-path*))
                       :current-track (cl-streamer/harmony:pipeline-current-track
                                       *harmony-pipeline*))))
      ;; 1. Clear the queue so play-list has nothing to advance to
      (cl-streamer/harmony:pipeline-clear-queue *harmony-pipeline*)
      ;; 2. Set skip flag so the play-list loop exits its wait
      (cl-streamer/harmony:pipeline-skip *harmony-pipeline*)
      ;; 3. Immediately silence and stop all voices on the mixer
      (cl-streamer/harmony:pipeline-stop-all-voices *harmony-pipeline*)
      (log:info "Auto-playlist paused for DJ session")
      state)))

(defun resume-auto-playlist (saved-state)
  "Resume the auto-playlist from saved state after a DJ session ends."
  (when saved-state
    (let ((playlist-path (getf saved-state :playlist-path)))
      (if playlist-path
          (progn
            (log:info "Resuming auto-playlist: ~A" (file-namestring playlist-path))
            (harmony-load-playlist playlist-path))
          ;; No saved playlist - try loading the current scheduled one
          (let ((scheduled (get-current-scheduled-playlist)))
            (when scheduled
              (let ((path (merge-pathnames scheduled (get-playlists-directory))))
                (when (probe-file path)
                  (log:info "Resuming with scheduled playlist: ~A" scheduled)
                  (harmony-load-playlist path)))))))))

;;; ---- Session Lifecycle ----

(defun start-dj-session (username)
  "Start a new DJ session. Pauses the auto-playlist and creates the session.
   Returns the session, or signals an error if one is already active."
  (bt:with-lock-held (*dj-session-lock*)
    (when *dj-session*
      (error "DJ session already active (owned by ~A)" (session-owner *dj-session*)))
    (let ((saved-state (pause-auto-playlist)))
      (setf *dj-session*
            (make-instance 'dj-session
                           :owner username
                           :started-at (get-universal-time)))
      (setf (session-saved-playlist-state *dj-session*) saved-state)
      ;; Update ICY metadata to show DJ is live
      (update-dj-metadata)
      (log:info "DJ session started by ~A" username)
      *dj-session*)))

(defun end-dj-session (&key (fade-duration 3.0))
  "End the current DJ session. Fades out active decks and resumes auto-playlist."
  (bt:with-lock-held (*dj-session-lock*)
    (unless *dj-session*
      (return-from end-dj-session nil))
    (let ((session *dj-session*)
          (owner (session-owner *dj-session*)))
      ;; Fade out and stop both decks
      (fade-and-stop-deck (session-deck-a session) fade-duration)
      (fade-and-stop-deck (session-deck-b session) fade-duration)
      ;; Disconnect external input if connected
      (when (session-external-input session)
        (disconnect-external-input-internal session))
      ;; Resume auto-playlist
      (resume-auto-playlist (session-saved-playlist-state session))
      ;; Clear session
      (setf *dj-session* nil)
      (log:info "DJ session ended (was owned by ~A)" owner)
      t)))

(defun dj-session-active-p ()
  "Return T if a DJ session is currently active."
  (not (null *dj-session*)))

;;; ---- Deck Control ----

(defun get-deck (session deck-id)
  "Return the deck object for DECK-ID (:a or :b)."
  (ecase deck-id
    (:a (session-deck-a session))
    (:b (session-deck-b session))))

(defun parse-deck-id (deck-string)
  "Parse a deck identifier string (\"a\" or \"b\") to a keyword."
  (cond
    ((string-equal deck-string "a") :a)
    ((string-equal deck-string "b") :b)
    (t (error "Invalid deck identifier: ~A (expected 'a' or 'b')" deck-string))))

(defun load-deck (deck-id file-path)
  "Load a track onto a deck. Stops any currently playing track on that deck.
   Reads metadata from the file and prepares the deck for playback."
  (unless *dj-session*
    (error "No active DJ session"))
  (let* ((deck (get-deck *dj-session* deck-id))
         (pipeline *harmony-pipeline*)
         (server (cl-streamer/harmony:pipeline-harmony-server pipeline))
         (org.shirakumo.fraf.harmony:*server* server))
    ;; Stop current track if playing
    (when (member (deck-state deck) '(:playing :paused))
      (stop-deck-internal deck))
    ;; Read metadata
    (let* ((tags (cl-streamer/harmony:read-audio-metadata file-path))
           (display-title (cl-streamer/harmony:format-display-title file-path))
           (track-info (list :file file-path
                             :display-title display-title
                             :artist (getf tags :artist)
                             :title (getf tags :title)
                             :album (getf tags :album))))
      (setf (deck-file-path deck) file-path
            (deck-track-info deck) track-info
            (deck-state deck) :loaded)
      (log:info "Deck ~A loaded: ~A" deck-id display-title)
      track-info)))

(defun play-deck (deck-id)
  "Start or resume playback on a deck."
  (unless *dj-session*
    (error "No active DJ session"))
  (let* ((session *dj-session*)
         (deck (get-deck session deck-id))
         (pipeline *harmony-pipeline*)
         (server (cl-streamer/harmony:pipeline-harmony-server pipeline))
         (org.shirakumo.fraf.harmony:*server* server))
    (ecase (deck-state deck)
      (:empty
       (error "Deck ~A has no track loaded" deck-id))
      (:loaded
       ;; Create a new voice and start playing
       (let ((voice (org.shirakumo.fraf.harmony:play
                     (sb-ext:parse-native-namestring (deck-file-path deck))
                     :mixer :music
                     :on-end :disconnect)))
         (setf (deck-voice deck) voice
               (deck-state deck) :playing)
         ;; Apply current crossfader volumes
         (apply-crossfader session)
         ;; Update ICY metadata
         (update-dj-metadata)
         (log:info "Deck ~A playing: ~A" deck-id
                   (getf (deck-track-info deck) :display-title))))
      (:paused
       ;; Resume - re-apply crossfader to restore volume
       (when (deck-voice deck)
         (setf (deck-state deck) :playing)
         (apply-crossfader session)
         (update-dj-metadata)
         (log:info "Deck ~A resumed" deck-id)))
      (:playing
       ;; Already playing, no-op
       nil))))

(defun pause-deck (deck-id)
  "Pause playback on a deck. Mutes the voice but keeps the position."
  (unless *dj-session*
    (error "No active DJ session"))
  (let ((deck (get-deck *dj-session* deck-id)))
    (when (and (eq (deck-state deck) :playing)
               (deck-voice deck))
      (setf (org.shirakumo.fraf.mixed:volume (deck-voice deck)) 0.0)
      (setf (deck-state deck) :paused)
      (update-dj-metadata)
      (log:info "Deck ~A paused" deck-id))))

(defun stop-deck (deck-id)
  "Stop playback and unload the track from a deck."
  (unless *dj-session*
    (error "No active DJ session"))
  (let ((deck (get-deck *dj-session* deck-id)))
    (stop-deck-internal deck)
    (update-dj-metadata)
    (log:info "Deck ~A stopped" deck-id)))

(defun stop-deck-internal (deck)
  "Internal: stop a deck's voice and reset state."
  (when (deck-voice deck)
    (let* ((pipeline *harmony-pipeline*)
           (server (cl-streamer/harmony:pipeline-harmony-server pipeline))
           (org.shirakumo.fraf.harmony:*server* server))
      (handler-case
          (org.shirakumo.fraf.harmony:stop (deck-voice deck))
        (error (e)
          (log:debug "Error stopping deck voice: ~A" e)))))
  (setf (deck-voice deck) nil
        (deck-file-path deck) nil
        (deck-track-info deck) nil
        (deck-state deck) :empty))

(defun fade-and-stop-deck (deck duration)
  "Fade a deck to silence and stop it. Runs in current thread."
  (when (and (deck-voice deck)
             (member (deck-state deck) '(:playing :paused)))
    (handler-case
        (progn
          (cl-streamer/harmony:volume-ramp (deck-voice deck) 0.0 duration)
          (stop-deck-internal deck))
      (error (e)
        (log:debug "Error fading deck: ~A" e)
        (stop-deck-internal deck)))))

(defun seek-deck (deck-id position-seconds)
  "Seek to a position (in seconds) on a deck."
  (unless *dj-session*
    (error "No active DJ session"))
  (let ((deck (get-deck *dj-session* deck-id)))
    (when (and (deck-voice deck)
               (member (deck-state deck) '(:playing :paused)))
      (let ((frame (round (* position-seconds
                             (org.shirakumo.fraf.mixed:samplerate (deck-voice deck))))))
        (org.shirakumo.fraf.mixed:seek (deck-voice deck) frame)
        (log:info "Deck ~A seeked to ~,1Fs" deck-id position-seconds)))))

;;; ---- Crossfader Control ----

(defun set-crossfader (position)
  "Set the crossfader position (0.0-1.0) and update deck volumes."
  (unless *dj-session*
    (error "No active DJ session"))
  (setf (session-crossfader *dj-session*)
        (max 0.0 (min 1.0 (float position))))
  (apply-crossfader *dj-session*)
  (update-dj-metadata))

(defun set-deck-volume (deck-id volume)
  "Set the per-deck volume (0.0-1.0) and reapply crossfader."
  (unless *dj-session*
    (error "No active DJ session"))
  (let ((deck (get-deck *dj-session* deck-id)))
    (setf (deck-volume deck) (max 0.0 (min 1.0 (float volume))))
    (apply-crossfader *dj-session*)))

;;; ---- External Audio Input ----

(defun connect-external-input (source-type &key device)
  "Connect an external audio source to the mixer.
   SOURCE-TYPE is :pulse, :alsa, or :jack.
   DEVICE is the device name/path (source-specific).
   Note: This is a Phase 2 feature."
  (declare (ignore source-type device))
  (unless *dj-session*
    (error "No active DJ session"))
  ;; TODO: Phase 2 - implement local audio capture via cl-mixed
  (log:warn "External audio input not yet implemented")
  (error "External audio input is not yet available (Phase 2)"))

(defun disconnect-external-input ()
  "Disconnect the external audio input."
  (unless *dj-session*
    (error "No active DJ session"))
  (disconnect-external-input-internal *dj-session*))

(defun disconnect-external-input-internal (session)
  "Internal: disconnect external input voice."
  (when (session-external-input session)
    (handler-case
        (let* ((pipeline *harmony-pipeline*)
               (server (cl-streamer/harmony:pipeline-harmony-server pipeline))
               (org.shirakumo.fraf.harmony:*server* server))
          (org.shirakumo.fraf.harmony:stop (session-external-input session)))
      (error (e)
        (log:debug "Error stopping external input: ~A" e)))
    (setf (session-external-input session) nil)
    (log:info "External audio input disconnected")))

(defun set-external-volume (volume)
  "Set the external input volume (0.0-1.0)."
  (unless *dj-session*
    (error "No active DJ session"))
  (setf (session-external-volume *dj-session*)
        (max 0.0 (min 1.0 (float volume))))
  (when (session-external-input *dj-session*)
    (setf (org.shirakumo.fraf.mixed:volume (session-external-input *dj-session*))
          (float (session-external-volume *dj-session*)))))

;;; ---- ICY Metadata ----

(defun set-dj-metadata (text)
  "Set custom ICY metadata for the DJ session.
   Pass NIL to return to auto-detect mode."
  (unless *dj-session*
    (error "No active DJ session"))
  (setf (session-metadata-override *dj-session*) text)
  (update-dj-metadata))

(defun update-dj-metadata ()
  "Update ICY metadata based on DJ session state.
   Uses custom override if set, otherwise auto-detects from louder deck."
  (when (and *dj-session* *harmony-pipeline*)
    (let ((title (or (session-metadata-override *dj-session*)
                     (auto-detect-dj-metadata *dj-session*))))
      (when title
        (cl-streamer/harmony:update-all-mounts-metadata *harmony-pipeline* title)))))

(defun auto-detect-dj-metadata (session)
  "Determine ICY metadata from the louder deck.
   Returns a display string like 'DJ fade - Artist - Title'."
  (let* ((crossfader (session-crossfader session))
         (deck-a (session-deck-a session))
         (deck-b (session-deck-b session))
         (owner (session-owner session))
         ;; Pick the deck that's louder based on crossfader position
         (active-deck (cond
                        ((and (eq (deck-state deck-a) :playing)
                              (eq (deck-state deck-b) :playing))
                         (if (<= crossfader 0.5) deck-a deck-b))
                        ((eq (deck-state deck-a) :playing) deck-a)
                        ((eq (deck-state deck-b) :playing) deck-b)
                        (t nil)))
         (track-title (when (and active-deck (deck-track-info active-deck))
                        (getf (deck-track-info active-deck) :display-title))))
    (if track-title
        (format nil "DJ ~A - ~A" owner track-title)
        (format nil "DJ ~A - Live" owner))))

;;; ---- Deck Position / Status ----

(defun deck-position-info (deck)
  "Return position info for a deck as a plist.
   Includes :position (seconds), :duration (seconds), :remaining (seconds)."
  (if (and (deck-voice deck)
           (member (deck-state deck) '(:playing :paused)))
      (handler-case
          (let* ((pos (org.shirakumo.fraf.mixed:frame-position (deck-voice deck)))
                 (total (org.shirakumo.fraf.mixed:frame-count (deck-voice deck)))
                 (sr (org.shirakumo.fraf.mixed:samplerate (deck-voice deck)))
                 (position-secs (when (and pos sr (> sr 0))
                                  (float (/ pos sr))))
                 (duration-secs (when (and total sr (> sr 0))
                                  (float (/ total sr))))
                 (remaining (when (and position-secs duration-secs)
                              (- duration-secs position-secs))))
            (list :position (or position-secs 0.0)
                  :duration (or duration-secs 0.0)
                  :remaining (or remaining 0.0)))
        (error ()
          (list :position 0.0 :duration 0.0 :remaining 0.0)))
      (list :position 0.0 :duration 0.0 :remaining 0.0)))

(defun deck-status-alist (deck)
  "Return an alist representing a deck's full state for JSON serialization."
  (let ((pos-info (deck-position-info deck)))
    `(("name" . ,(string-downcase (symbol-name (deck-name deck))))
      ("state" . ,(string-downcase (symbol-name (deck-state deck))))
      ("volume" . ,(deck-volume deck))
      ("position" . ,(getf pos-info :position))
      ("duration" . ,(getf pos-info :duration))
      ("remaining" . ,(getf pos-info :remaining))
      ("trackInfo" . ,(when (deck-track-info deck)
                        `(("artist" . ,(or (getf (deck-track-info deck) :artist) ""))
                          ("title" . ,(or (getf (deck-track-info deck) :title) ""))
                          ("album" . ,(or (getf (deck-track-info deck) :album) ""))
                          ("displayTitle" . ,(or (getf (deck-track-info deck) :display-title) ""))))))))

;;; ---- Session Status (for UI polling) ----

(defun dj-session-status ()
  "Return the full DJ session status as an alist for JSON serialization.
   Returns NIL if no session is active."
  (when *dj-session*
    ;; Update last-poll timestamp for watchdog
    (setf (session-last-poll *dj-session*) (get-universal-time))
    `(("active" . t)
      ("owner" . ,(session-owner *dj-session*))
      ("startedAt" . ,(session-started-at *dj-session*))
      ("duration" . ,(- (get-universal-time) (session-started-at *dj-session*)))
      ("deckA" . ,(deck-status-alist (session-deck-a *dj-session*)))
      ("deckB" . ,(deck-status-alist (session-deck-b *dj-session*)))
      ("crossfader" . ,(session-crossfader *dj-session*))
      ("metadataOverride" . ,(session-metadata-override *dj-session*))
      ("externalInput" . ,(not (null (session-external-input *dj-session*))))
      ("externalVolume" . ,(session-external-volume *dj-session*)))))

;;; ---- Library Search ----

(defun search-library-tracks (query &key (limit 50) (offset 0))
  "Search the track database for tracks matching QUERY.
   Returns a list of alists with track info."
  (handler-case
      (with-db
        (let* ((pattern (format nil "%~A%" query))
               (results
                 (postmodern:query
                  (:raw (format nil
                         "SELECT _id, title, artist, album, \"file-path\" FROM tracks WHERE (title ILIKE $1 OR artist ILIKE $1 OR album ILIKE $1) ORDER BY artist, title LIMIT ~A OFFSET ~A"
                         limit offset))
                  pattern
                  :rows)))
          (mapcar (lambda (row)
                    `(("id" . ,(first row))
                      ("title" . ,(or (second row) ""))
                      ("artist" . ,(or (third row) ""))
                      ("album" . ,(or (fourth row) ""))
                      ("filePath" . ,(or (fifth row) ""))))
                  results)))
    (error (e)
      (log:warn "Library search error: ~A" e)
      nil)))

;;; ---- Watchdog ----

(defun dj-watchdog-check ()
  "Check if the DJ session has gone stale (no UI polling).
   Called periodically by cl-cron. Auto-ends stale sessions."
  (when *dj-session*
    (let ((elapsed (- (get-universal-time) (session-last-poll *dj-session*))))
      (when (> elapsed *dj-watchdog-interval*)
        (log:warn "DJ session stale (~As since last poll), auto-ending"
                  elapsed)
        (end-dj-session :fade-duration 5.0)))))
