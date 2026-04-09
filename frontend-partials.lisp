(in-package :asteroid)

(defun find-track-by-title (title)
  "Find a track in the database by its title. Returns track ID or nil.
   Handles 'Artist - Title' format from stream metadata."
  (when (and title (not (string= title "Unknown")))
    (handler-case
        (with-db
          ;; Parse 'Artist - Title' format if present
          (let* ((parts (cl-ppcre:split " - " title :limit 2))
                 (has-artist (> (length parts) 1))
                 (artist-part (when has-artist (first parts)))
                 (title-part (if has-artist (second parts) title))
                 (result
                   (if has-artist
                       ;; Search by both artist and title
                       (postmodern:query
                        (:limit
                         (:select '_id
                          :from 'tracks
                          :where (:and (:ilike 'artist (format nil "%~a%" artist-part))
                                       (:ilike 'title (format nil "%~a%" title-part))))
                         1)
                        :single)
                       ;; Fallback: search by title only
                       (postmodern:query
                        (:limit
                         (:select '_id
                          :from 'tracks
                          :where (:ilike 'title (format nil "%~a%" title-part)))
                         1)
                        :single))))
            result))
      (error (e)
        (declare (ignore e))
        nil))))

(defun get-now-playing-stats (&optional (mount "asteroid.mp3"))
  "Get now-playing stats from the Harmony pipeline.
   When a DJ session is active, returns the DJ deck's current track info instead.
   Returns an alist with :listenurl, :title, :listeners, :track-id, :favorite-count."
  (if (dj-session-active-p)
      ;; DJ session is live — show the active deck's track info
      (let* ((status (dj-session-status))
             (deck-a (cdr (assoc "deckA" status :test #'string=)))
             (deck-b (cdr (assoc "deckB" status :test #'string=)))
             (crossfader (or (cdr (assoc "crossfader" status :test #'string=)) 0.5))
             ;; Pick the dominant deck based on crossfader position
             (active-deck (if (<= crossfader 0.5) deck-a deck-b))
             (track-info (when active-deck
                           (cdr (assoc "trackInfo" active-deck :test #'string=))))
             (display-title (if track-info
                                (or (cdr (assoc "displayTitle" track-info :test #'string=))
                                    "DJ Live")
                                "DJ Live"))
             (owner (or (cdr (assoc "owner" status :test #'string=)) "DJ"))
             (title (format nil "~A [DJ: ~A]" display-title owner))
             (listeners (if *harmony-pipeline*
                           (or (cl-streamer:pipeline-listener-count *harmony-pipeline*) 0)
                           0)))
        `((:listenurl . ,(format nil "~A/~A" *stream-base-url* mount))
          (:title . ,title)
          (:listeners . ,listeners)
          (:track-id . nil)
          (:favorite-count . 0)))
      ;; Normal mode — route to curated or shuffle based on mount name
      (if (search "shuffle" mount :test #'char-equal)
          (shuffle-now-playing mount)
          (harmony-now-playing mount))))

(define-api-with-limit asteroid/partial/now-playing (&optional mount) (:limit 120 :timeout 60)
  "Get Partial HTML with live now-playing status.
   Optional MOUNT parameter specifies which stream to get metadata from.
   Returns partial HTML with current track info."
  (with-error-handling
    (let* ((mount-name (or mount "asteroid.mp3"))
           (now-playing-stats (get-now-playing-stats mount-name)))
      (if now-playing-stats
          (let* ((title (cdr (assoc :title now-playing-stats)))
                 (favorite-count (or (get-track-favorite-count title) 0)))
            (setf (header "Content-Type") "text/html")
            (clip:process-to-string
             (load-template "partial/now-playing")
             :stats now-playing-stats
             :track-id (cdr (assoc :track-id now-playing-stats))
             :favorite-count favorite-count))
          (progn
            (setf (header "Content-Type") "text/html")
            (clip:process-to-string
             (load-template "partial/now-playing")
             :connection-error t
             :stats nil))))))

(define-api-with-limit asteroid/partial/now-playing-inline (&optional mount) (:limit 120 :timeout 60)
  "Get inline text with now playing info (for admin dashboard and widgets).
   Optional MOUNT parameter specifies which stream to get metadata from."
  (with-error-handling
    (let* ((mount-name (or mount "asteroid.mp3"))
           (now-playing-stats (get-now-playing-stats mount-name)))
      (if now-playing-stats
          (progn
            (setf (header "Content-Type") "text/plain")
            (cdr (assoc :title now-playing-stats)))
          (progn
            (setf (header "Content-Type") "text/plain")
            "Stream Offline")))))

(define-api-with-limit asteroid/partial/now-playing-json (&optional mount) (:limit 120 :timeout 60)
  "Get JSON with now playing info including track ID for favorites.
   Optional MOUNT parameter specifies which stream to get metadata from."
  ;; Register web listener for geo stats (keeps listener active during playback)
  (register-web-listener)
  (with-error-handling
    (let* ((mount-name (or mount "asteroid.mp3"))
           (now-playing-stats (get-now-playing-stats mount-name)))
      (if now-playing-stats
          (let* ((title (cdr (assoc :title now-playing-stats)))
                 (favorite-count (or (get-track-favorite-count title) 0))
                 (parsed (parse-track-title title))
                 (artist (getf parsed :artist))
                 (song (getf parsed :song))
                 (search-url (generate-music-search-url artist song)))
            (api-output `(("status" . "success")
                          ("title" . ,title)
                          ("listeners" . ,(cdr (assoc :listeners now-playing-stats)))
                          ("track_id" . ,(cdr (assoc :track-id now-playing-stats)))
                          ("favorite_count" . ,favorite-count)
                          ("search_url" . ,search-url))))
          (api-output `(("status" . "offline")
                        ("title" . "Stream Offline")
                        ("track_id" . nil)))))))

(define-api-with-limit asteroid/channel-name () (:limit 180 :timeout 60)
  "Get the current curated channel name for live updates.
   Returns JSON with the channel name from the current playlist's PHASE header."
  (with-error-handling
    (api-output `(("channel_name" . ,(get-curated-channel-name))))))
