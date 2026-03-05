;;;; dj-console.lisp - ParenScript for DJ Console interface
;;;; Handles session management, deck control, crossfader, library search,
;;;; and status polling.

(in-package #:asteroid)

(defparameter *dj-console-js*
  (ps:ps*
   '(progn

    ;; ---- State ----
    (defvar *poll-timer* nil)
    (defvar *session-active* false)
    (defvar *search-debounce* nil)

    ;; ---- Utility ----
    (defun format-time (seconds)
      "Format seconds as M:SS"
      (let* ((secs (ps:chain -math (floor seconds)))
             (m (ps:chain -math (floor (/ secs 60))))
             (s (mod secs 60)))
        (+ m ":" (if (< s 10) (+ "0" s) s))))

    (defun api-post (url params callback)
      "POST to an API endpoint with form params"
      (let ((xhr (ps:new (-x-m-l-http-request))))
        (ps:chain xhr (open "POST" url true))
        (ps:chain xhr (set-request-header "Content-Type" "application/x-www-form-urlencoded"))
        (setf (ps:@ xhr onload)
              (lambda ()
                (let ((data (ps:chain -j-s-o-n (parse (ps:@ xhr response-text)))))
                  (when callback (funcall callback data)))))
        (setf (ps:@ xhr onerror)
              (lambda () (show-message "Network error" "error")))
        (ps:chain xhr (send params))))

    (defun api-get (url callback)
      "GET from an API endpoint"
      (let ((xhr (ps:new (-x-m-l-http-request))))
        (ps:chain xhr (open "GET" url true))
        (setf (ps:@ xhr onload)
              (lambda ()
                (let ((data (ps:chain -j-s-o-n (parse (ps:@ xhr response-text)))))
                  (when callback (funcall callback data)))))
        (setf (ps:@ xhr onerror)
              (lambda () (show-message "Network error" "error")))
        (ps:chain xhr (send))))

    (defun show-message (text msg-type)
      "Show a status message"
      (let ((el (ps:chain document (get-element-by-id "dj-message"))))
        (setf (ps:@ el inner-text) text)
        (setf (ps:@ el class-name) (+ "dj-message " msg-type))
        (setf (ps:@ el style display) "block")
        (set-timeout (lambda ()
                       (setf (ps:@ el style display) "none"))
                     4000)))

    (defun encode-params (obj)
      "Encode an object as URL form params"
      (let ((parts (array)))
        (ps:for-in (key obj)
          (ps:chain parts (push (+ (encode-u-r-i-component key)
                                   "="
                                   (encode-u-r-i-component (ps:getprop obj key))))))
        (ps:chain parts (join "&"))))

    ;; ---- Session Control ----
    (defun start-session ()
      (api-post "/api/asteroid/dj/session/start" ""
                (lambda (data)
                  (if (= (ps:@ data status) "success")
                      (progn
                        (show-message "Session started - you are LIVE!" "success")
                        (set-session-active true))
                      (show-message (or (ps:@ data message) "Failed to start session") "error")))))

    (defun end-session ()
      (when (ps:chain window (confirm "End your DJ session? Auto-playlist will resume."))
        (api-post "/api/asteroid/dj/session/end" ""
                  (lambda (data)
                    (if (= (ps:@ data status) "success")
                        (progn
                          (show-message "Session ended - auto-playlist resuming" "success")
                          (set-session-active false))
                        (show-message (or (ps:@ data message) "Failed to end session") "error"))))))

    (defun set-session-active (active)
      (setf *session-active* active)
      (let ((controls (ps:chain document (get-element-by-id "dj-controls")))
            (btn-live (ps:chain document (get-element-by-id "btn-go-live")))
            (btn-end (ps:chain document (get-element-by-id "btn-end-session")))
            (info (ps:chain document (get-element-by-id "session-info"))))
        (if active
            (progn
              (setf (ps:@ controls class-name) "")
              (setf (ps:@ btn-live style display) "none")
              (setf (ps:@ btn-end style display) "inline-block")
              (setf (ps:@ info inner-h-t-m-l)
                    (+ "<span class='live-indicator'></span>LIVE"))
              ;; Start polling
              (when *poll-timer* (clear-interval *poll-timer*))
              (setf *poll-timer* (set-interval poll-status 500)))
            (progn
              (setf (ps:@ controls class-name) "no-session-overlay")
              (setf (ps:@ btn-live style display) "inline-block")
              (setf (ps:@ btn-end style display) "none")
              (setf (ps:@ info inner-text) "")
              ;; Stop polling
              (when *poll-timer*
                (clear-interval *poll-timer*)
                (setf *poll-timer* nil))
              ;; Reset UI
              (reset-deck-ui "a")
              (reset-deck-ui "b")))))

    ;; ---- Status Polling ----
    (defun poll-status ()
      (api-get "/api/asteroid/dj/session/status"
               (lambda (data)
                 (when (ps:@ data active)
                   (update-deck-ui "a" (ps:@ data deck-a))
                   (update-deck-ui "b" (ps:@ data deck-b))
                   ;; Update crossfader if not being dragged
                   (let ((slider (ps:chain document (get-element-by-id "crossfader"))))
                     (unless (= (ps:@ document active-element) slider)
                       (setf (ps:@ slider value)
                             (ps:chain -math (round (* (ps:@ data crossfader) 100))))))))))

    (defun update-deck-ui (deck-name deck-data)
      "Update a deck's UI from status data"
      (let ((state (ps:@ deck-data state))
            (track (ps:@ deck-data track-info))
            (position (ps:@ deck-data position))
            (duration (ps:@ deck-data duration)))
        ;; State label
        (setf (ps:@ (ps:chain document (get-element-by-id (+ "deck-" deck-name "-state")))
                     inner-text)
              (ps:chain state (to-upper-case)))
        ;; Track info
        (let ((info-el (ps:chain document (get-element-by-id (+ "deck-" deck-name "-info")))))
          (if track
              (setf (ps:@ info-el inner-h-t-m-l)
                    (+ "<div class='deck-artist'>"
                       (or (ps:@ track artist) "") "</div>"
                       "<div class='deck-title'>"
                       (or (ps:@ track title) "") "</div>"
                       "<div class='deck-album'>"
                       (or (ps:@ track album) "") "</div>"))
              (setf (ps:@ info-el inner-h-t-m-l)
                    "<div class='deck-empty'>No track loaded</div>")))
        ;; Progress bar
        (let ((pct (if (and duration (> duration 0))
                       (* (/ position duration) 100)
                       0)))
          (setf (ps:@ (ps:chain document (get-element-by-id (+ "deck-" deck-name "-progress")))
                       style width)
                (+ pct "%")))
        ;; Time display
        (setf (ps:@ (ps:chain document (get-element-by-id (+ "deck-" deck-name "-position")))
                     inner-text)
              (format-time (or position 0)))
        (setf (ps:@ (ps:chain document (get-element-by-id (+ "deck-" deck-name "-duration")))
                     inner-text)
              (format-time (or duration 0)))
        ;; Button states
        (let ((can-play (or (= state "loaded") (= state "paused")))
              (can-pause (= state "playing"))
              (can-stop (or (= state "playing") (= state "paused") (= state "loaded"))))
          (setf (ps:@ (ps:chain document (get-element-by-id (+ "deck-" deck-name "-play")))
                       disabled)
                (not can-play))
          (setf (ps:@ (ps:chain document (get-element-by-id (+ "deck-" deck-name "-pause")))
                       disabled)
                (not can-pause))
          (setf (ps:@ (ps:chain document (get-element-by-id (+ "deck-" deck-name "-stop")))
                       disabled)
                (not can-stop)))
        ;; Active indicator
        (let ((container (ps:chain document (get-element-by-id (+ "deck-" deck-name "-container")))))
          (if (= state "playing")
              (ps:chain (ps:@ container class-list) (add "active"))
              (ps:chain (ps:@ container class-list) (remove "active"))))))

    (defun reset-deck-ui (deck-name)
      "Reset a deck's UI to empty state"
      (setf (ps:@ (ps:chain document (get-element-by-id (+ "deck-" deck-name "-state")))
                   inner-text) "EMPTY")
      (setf (ps:@ (ps:chain document (get-element-by-id (+ "deck-" deck-name "-info")))
                   inner-h-t-m-l) "<div class='deck-empty'>No track loaded</div>")
      (setf (ps:@ (ps:chain document (get-element-by-id (+ "deck-" deck-name "-progress")))
                   style width) "0%")
      (setf (ps:@ (ps:chain document (get-element-by-id (+ "deck-" deck-name "-position")))
                   inner-text) "0:00")
      (setf (ps:@ (ps:chain document (get-element-by-id (+ "deck-" deck-name "-duration")))
                   inner-text) "0:00")
      (setf (ps:@ (ps:chain document (get-element-by-id (+ "deck-" deck-name "-play")))
                   disabled) true)
      (setf (ps:@ (ps:chain document (get-element-by-id (+ "deck-" deck-name "-pause")))
                   disabled) true)
      (setf (ps:@ (ps:chain document (get-element-by-id (+ "deck-" deck-name "-stop")))
                   disabled) true))

    ;; ---- Deck Control ----
    (defun play-deck (deck)
      (api-post "/api/asteroid/dj/deck/play"
                (encode-params (ps:create :deck deck))
                (lambda (data)
                  (unless (= (ps:@ data status) "success")
                    (show-message (or (ps:@ data message) "Play failed") "error")))))

    (defun pause-deck (deck)
      (api-post "/api/asteroid/dj/deck/pause"
                (encode-params (ps:create :deck deck))
                (lambda (data)
                  (unless (= (ps:@ data status) "success")
                    (show-message (or (ps:@ data message) "Pause failed") "error")))))

    (defun stop-deck (deck)
      (api-post "/api/asteroid/dj/deck/stop"
                (encode-params (ps:create :deck deck))
                (lambda (data)
                  (unless (= (ps:@ data status) "success")
                    (show-message (or (ps:@ data message) "Stop failed") "error")))))

    (defun seek-deck (deck event)
      "Seek on a deck by clicking the progress bar"
      (let* ((bar (ps:@ event current-target))
             (rect (ps:chain bar (get-bounding-client-rect)))
             (x (- (ps:@ event client-x) (ps:@ rect left)))
             (pct (/ x (ps:@ rect width)))
             ;; Get duration from the UI display (rough but works)
             (duration-el (ps:chain document (get-element-by-id (+ "deck-" deck "-duration"))))
             (parts (ps:chain (ps:@ duration-el inner-text) (split ":")))
             (dur (+ (* (parse-int (aref parts 0) 10) 60) (parse-int (aref parts 1) 10)))
             (seek-pos (* pct dur)))
        (api-post "/api/asteroid/dj/deck/seek"
                  (encode-params (ps:create :deck deck :position seek-pos))
                  nil)))

    ;; ---- Crossfader ----
    (defun set-crossfader (position)
      (api-post "/api/asteroid/dj/crossfader"
                (encode-params (ps:create :position position))
                nil))

    ;; ---- Volume ----
    (defun set-deck-volume (deck volume)
      (api-post "/api/asteroid/dj/deck/volume"
                (encode-params (ps:create :deck deck :volume volume))
                nil))

    ;; ---- Metadata ----
    (defun set-metadata ()
      (let ((text (ps:@ (ps:chain document (get-element-by-id "metadata-input")) value)))
        (api-post "/api/asteroid/dj/session/metadata"
                  (encode-params (ps:create :text text))
                  (lambda (data)
                    (when (= (ps:@ data status) "success")
                      (show-message "Metadata updated" "success"))))))

    (defun clear-metadata ()
      (setf (ps:@ (ps:chain document (get-element-by-id "metadata-input")) value) "")
      (api-post "/api/asteroid/dj/session/metadata"
                (encode-params (ps:create :text ""))
                (lambda (data)
                  (when (= (ps:@ data status) "success")
                    (show-message "Metadata set to auto-detect" "success")))))

    ;; ---- Library Search ----
    (defun search-library ()
      (let ((query (ps:@ (ps:chain document (get-element-by-id "library-query")) value)))
        (when (> (ps:@ query length) 0)
          (api-get (+ "/api/asteroid/dj/library/search?q=" (encode-u-r-i-component query))
                   render-library-results))))

    (defun render-library-results (data)
      (let ((container (ps:chain document (get-element-by-id "library-results")))
            (results (ps:@ data results)))
        (if (and results (> (ps:@ results length) 0))
            (let ((html "<table><tr><th>Artist</th><th>Title</th><th>Album</th><th>Load</th></tr>"))
              (ps:chain results
                (for-each
                 (lambda (track)
                   (setf html (+ html
                                  "<tr>"
                                  "<td>" (or (ps:@ track artist) "") "</td>"
                                  "<td>" (or (ps:@ track title) "") "</td>"
                                  "<td>" (or (ps:@ track album) "") "</td>"
                                  "<td>"
                                  "<button class='load-btn' onclick='loadTrack(\"a\", "
                                  (ps:@ track id) ")'>A</button>"
                                  "<button class='load-btn' onclick='loadTrack(\"b\", "
                                  (ps:@ track id) ")'>B</button>"
                                  "</td>"
                                  "</tr>")))))
              (setf html (+ html "</table>"))
              (setf (ps:@ container inner-h-t-m-l) html))
            (setf (ps:@ container inner-h-t-m-l)
                  "<p style='color: #666; text-align: center;'>No results found</p>"))))

    (defun load-track (deck track-id)
      (api-post "/api/asteroid/dj/deck/load"
                (encode-params (ps:create :deck deck :track-id track-id))
                (lambda (data)
                  (if (= (ps:@ data status) "success")
                      (show-message (+ "Loaded onto Deck "
                                       (ps:chain deck (to-upper-case)) ": "
                                       (ps:@ data track-info display-title))
                                    "success")
                      (show-message (or (ps:@ data message) "Load failed") "error")))))

    ;; ---- Expose functions to window ----
    (setf (ps:@ window start-session) start-session)
    (setf (ps:@ window end-session) end-session)
    (setf (ps:@ window play-deck) play-deck)
    (setf (ps:@ window pause-deck) pause-deck)
    (setf (ps:@ window stop-deck) stop-deck)
    (setf (ps:@ window seek-deck) seek-deck)
    (setf (ps:@ window set-crossfader) set-crossfader)
    (setf (ps:@ window set-deck-volume) set-deck-volume)
    (setf (ps:@ window set-metadata) set-metadata)
    (setf (ps:@ window clear-metadata) clear-metadata)
    (setf (ps:@ window search-library) search-library)
    (setf (ps:@ window load-track) load-track)

    ;; ---- Init ----
    (defun init-dj-console ()
      "Initialize the DJ console - check if a session is already active"
      ;; Check server-rendered state first, then poll for live status
      (let ((active-val (ps:@ (ps:chain document (get-element-by-id "dj-active")) value)))
        (if (= active-val "true")
            (set-session-active true)
            ;; Also poll in case state changed between page render and load
            (api-get "/api/asteroid/dj/session/status"
                     (lambda (data)
                       (when (ps:@ data active)
                         (set-session-active true)))))))

    ;; Run on page load
    (ps:chain window (add-event-listener "load" init-dj-console))

    ))
  "Compiled JavaScript for DJ console - generated at load time")

(defun generate-dj-console-js ()
  "Return the pre-compiled JavaScript for DJ console"
  *dj-console-js*)
