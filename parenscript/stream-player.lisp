;;;; stream-player.lisp - ParenScript for persistent stream player
;;;; Handles audio-player-frame and popout-player stream reconnect logic

(in-package #:asteroid)

(defparameter *stream-player-js*
  (ps:ps*
   '(progn
     
     ;; ========================================
     ;; Stream Configuration
     ;; ========================================
     
     ;; Get stream configuration for a given channel and quality
     ;; Curated channel has multiple quality options, shuffle has only one
     (defun get-stream-config (stream-base-url channel quality)
       (let ((curated-config (ps:create
                              :aac (ps:create :url (+ stream-base-url "/asteroid.aac")
                                              :type "audio/aac"
                                              :format "AAC 96kbps Stereo"
                                              :mount "asteroid.aac")
                              :mp3 (ps:create :url (+ stream-base-url "/asteroid.mp3")
                                              :type "audio/mpeg"
                                              :format "MP3 128kbps Stereo"
                                              :mount "asteroid.mp3")
                              :low (ps:create :url (+ stream-base-url "/asteroid-low.mp3")
                                              :type "audio/mpeg"
                                              :format "MP3 64kbps Stereo"
                                              :mount "asteroid-low.mp3")))
             (shuffle-config (ps:create :url (+ stream-base-url "/asteroid-shuffle.mp3")
                                        :type "audio/mpeg"
                                        :format "Shuffle MP3 96kbps"
                                        :mount "asteroid-shuffle.mp3")))
         (if (= channel "shuffle")
             shuffle-config
             (ps:getprop curated-config quality))))
     
     ;; Get current channel from selector or localStorage
     (defun get-current-channel ()
       (let ((selector (or (ps:chain document (get-element-by-id "stream-channel"))
                           (ps:chain document (get-element-by-id "popout-stream-channel")))))
         (if selector
             (ps:@ selector value)
             (or (ps:chain local-storage (get-item "stream-channel")) "curated"))))
     
     ;; Get current quality from selector or localStorage
     (defun get-current-quality ()
       (let ((selector (or (ps:chain document (get-element-by-id "stream-quality"))
                           (ps:chain document (get-element-by-id "popout-stream-quality")))))
         (if selector
             (ps:@ selector value)
             (or (ps:chain local-storage (get-item "stream-quality")) "aac"))))
     
     ;; Update quality selector state based on channel
     (defun update-quality-selector-state ()
       (let* ((channel (get-current-channel))
              (quality-selector (or (ps:chain document (get-element-by-id "stream-quality"))
                                    (ps:chain document (get-element-by-id "popout-stream-quality")))))
         (when quality-selector
           (if (= channel "shuffle")
               (progn
                 (setf (ps:@ quality-selector disabled) t)
                 (setf (ps:@ quality-selector title) "Shuffle channel has fixed quality"))
               (progn
                 (setf (ps:@ quality-selector disabled) nil)
                 (setf (ps:@ quality-selector title) ""))))))
     
     ;; Change channel (curated vs shuffle)
     ;; Called from content frame or popout - updates localStorage and notifies frame player
     (defun change-channel ()
       (let* ((channel-selector (or (ps:chain document (get-element-by-id "stream-channel"))
                                    (ps:chain document (get-element-by-id "popout-stream-channel"))))
              (channel (ps:@ channel-selector value))
              (stream-base-url-el (ps:chain document (get-element-by-id "stream-base-url")))
              (stream-base-url (when stream-base-url-el (ps:@ stream-base-url-el value)))
              (quality (get-current-quality))
              (audio-element (or (ps:chain document (get-element-by-id "persistent-audio"))
                                 (ps:chain document (get-element-by-id "live-audio"))))
              (source-element (ps:chain document (get-element-by-id "audio-source")))
              (was-playing (and audio-element (not (ps:@ audio-element paused)))))
         
         ;; Save preference
         (ps:chain local-storage (set-item "stream-channel" channel))
         
         ;; Update quality selector state
         (update-quality-selector-state)
         
         ;; If we have audio element (popout player), update it
         (when (and stream-base-url audio-element source-element)
           (let ((config (get-stream-config stream-base-url channel quality)))
             ;; Swap source and reload
             (setf (ps:@ source-element src) (ps:@ config url))
             (setf (ps:@ source-element type) (ps:@ config type))
             (ps:chain audio-element (load))
             
             ;; Resume playback if it was playing
             (when was-playing
               (ps:chain audio-element (play)
                         (catch (lambda (e)
                                  (ps:chain console (log "Autoplay prevented:" e))))))))
         
         ;; If in frameset mode, notify the player frame to update
         (when (not (= (ps:@ window parent) window))
           (ps:try
            (let ((player-frame (ps:@ (ps:@ window parent) frames "player-frame")))
              (when (and player-frame (ps:@ player-frame sync-channel-from-storage))
                (ps:chain player-frame (sync-channel-from-storage))))
            (:catch (e) nil)))
         
         ;; Refresh now-playing immediately
         (when (ps:chain document (get-element-by-id "mini-now-playing"))
           (ps:chain window (set-timeout update-mini-now-playing 50)))
         (when (or (ps:chain document (get-element-by-id "popout-track-title"))
                   (ps:chain document (get-element-by-id "popout-track-artist")))
           (ps:chain window (set-timeout update-popout-now-playing 50)))))
     
     ;; Sync channel from localStorage (called by content frame when channel changes)
     (defun sync-channel-from-storage ()
       (let* ((channel (or (ps:chain local-storage (get-item "stream-channel")) "curated"))
              (quality (get-current-quality))
              (stream-base-url-el (ps:chain document (get-element-by-id "stream-base-url")))
              (stream-base-url (when stream-base-url-el (ps:@ stream-base-url-el value)))
              (channel-selector (ps:chain document (get-element-by-id "stream-channel")))
              (audio-element (ps:chain document (get-element-by-id "persistent-audio")))
              (source-element (ps:chain document (get-element-by-id "audio-source")))
              (was-playing (and audio-element (not (ps:@ audio-element paused)))))
         
         ;; Update channel selector dropdown to match localStorage
         (when (and channel-selector (not (= (ps:@ channel-selector value) channel)))
           (setf (ps:@ channel-selector value) channel))
         
         (when (and stream-base-url audio-element source-element)
           (let ((config (get-stream-config stream-base-url channel quality)))
             ;; Update quality selector state
             (update-quality-selector-state)
             
             ;; Swap source and reload
             (setf (ps:@ source-element src) (ps:@ config url))
             (setf (ps:@ source-element type) (ps:@ config type))
             (ps:chain audio-element (load))
             
             ;; Resume playback if it was playing
             (when was-playing
               (ps:chain audio-element (play)
                         (catch (lambda (e)
                                  (ps:chain console (log "Autoplay prevented:" e))))))
             
             ;; Refresh now-playing
             (ps:chain window (set-timeout update-mini-now-playing 50))))))
     
     ;; ========================================
     ;; Stream Quality Selection
     ;; ========================================
     
     ;; Change stream quality (bitrate)
     (defun change-stream-quality ()
       (let* ((selector (or (ps:chain document (get-element-by-id "stream-quality"))
                            (ps:chain document (get-element-by-id "popout-stream-quality"))))
              (stream-base-url (ps:@ (ps:chain document (get-element-by-id "stream-base-url")) value))
              (channel (get-current-channel))
              (selected-quality (ps:@ selector value))
              (config (get-stream-config stream-base-url channel selected-quality))
              (audio-element (or (ps:chain document (get-element-by-id "persistent-audio"))
                                 (ps:chain document (get-element-by-id "live-audio"))))
              (source-element (ps:chain document (get-element-by-id "audio-source")))
              (was-playing (and audio-element (not (ps:@ audio-element paused)))))
         
         ;; Save preference
         (ps:chain local-storage (set-item "stream-quality" selected-quality))
         
         ;; Swap source and reload
         (setf (ps:@ source-element src) (ps:@ config url))
         (setf (ps:@ source-element type) (ps:@ config type))
         (ps:chain audio-element (load))
         
         ;; Resume playback if it was playing
         (when was-playing
           (ps:chain audio-element (play)
                     (catch (lambda (e)
                              (ps:chain console (log "Autoplay prevented:" e))))))
         
         ;; Refresh now-playing immediately when user switches streams
         (when (ps:chain document (get-element-by-id "mini-now-playing"))
           (ps:chain window (set-timeout update-mini-now-playing 50)))
         (when (or (ps:chain document (get-element-by-id "popout-track-title"))
                   (ps:chain document (get-element-by-id "popout-track-artist")))
           (ps:chain window (set-timeout update-popout-now-playing 50)))))
     
     ;; ========================================
     ;; Now Playing Updates
     ;; ========================================
     
     ;; Get current mount from channel and quality selection
     (defun get-current-mount ()
       (let* ((channel (get-current-channel))
              (quality (get-current-quality))
              (stream-base-url (ps:@ (ps:chain document (get-element-by-id "stream-base-url")) value))
              (config (get-stream-config stream-base-url channel quality)))
         (if config (ps:@ config mount) "asteroid.mp3")))
     
     ;; Track the last recorded title to avoid duplicate history entries
     (defvar *last-recorded-title* nil)
     
     ;; Record track to listening history (only if logged in)
     (defun record-track-listen (title)
       (when (and title (not (= title "")) (not (= title "Loading...")) (not (= title *last-recorded-title*)))
         (setf *last-recorded-title* title)
         (ps:chain
          (fetch (+ "/api/asteroid/user/history/record?title=" (encode-u-r-i-component title))
                 (ps:create :method "POST"))
          (then (lambda (response)
                  (when (ps:@ response ok)
                    (ps:chain console (log "Recorded listen:" title)))))
          (catch (lambda (error)
                   ;; Silently fail - user might not be logged in
                   nil)))))
     
     ;; Update mini now playing display (for persistent player frame)
     (defun update-mini-now-playing ()
       (let ((mount (get-current-mount)))
         (ps:chain
          (fetch (+ "/api/asteroid/partial/now-playing-json?mount=" mount))
          (then (lambda (response)
                  (if (ps:@ response ok)
                      (ps:chain response (json))
                      nil)))
          (then (lambda (data)
                  (when data
                    (let ((el (ps:chain document (get-element-by-id "mini-now-playing")))
                          (track-id-el (ps:chain document (get-element-by-id "current-track-id-mini")))
                          (title (or (ps:@ data data title) (ps:@ data title) "Loading...")))
                      (when el
                        ;; Check if track changed and record to history
                        (when (not (= (ps:@ el text-content) title))
                          (record-track-listen title))
                        (setf (ps:@ el text-content) title))
                      (when track-id-el
                        (let ((track-id (or (ps:@ data data track_id) (ps:@ data track_id))))
                          (setf (ps:@ track-id-el value) (or track-id ""))))))))
          (catch (lambda (error)
                   (ps:chain console (log "Could not fetch now playing:" error)))))))
     
     ;; Toggle favorite for mini player
     (defun toggle-favorite-mini ()
       (let ((track-id-el (ps:chain document (get-element-by-id "current-track-id-mini")))
             (title-el (ps:chain document (get-element-by-id "mini-now-playing")))
             (btn (ps:chain document (get-element-by-id "favorite-btn-mini"))))
         (let ((track-id (when track-id-el (ps:@ track-id-el value)))
               (title (when title-el (ps:@ title-el text-content)))
               (is-favorited (ps:chain btn class-list (contains "favorited"))))
           ;; Need either track-id or title
           (when (or (and track-id (not (= track-id ""))) title)
             (let ((params (+ "title=" (encode-u-r-i-component (or title ""))
                             (if (and track-id (not (= track-id "")))
                                 (+ "&track-id=" track-id)
                                 ""))))
               (if is-favorited
                   ;; Remove favorite
                   (ps:chain
                    (fetch (+ "/api/asteroid/user/favorites/remove?" params)
                           (ps:create :method "POST"))
                    (then (lambda (response)
                            (cond
                              ((not (ps:@ response ok))
                               (alert "Please log in to manage favorites")
                               nil)
                              (t (ps:chain response (json))))))
                    (then (lambda (data)
                            (when (and data (or (= (ps:@ data status) "success")
                                               (= (ps:@ data data status) "success")))
                              (ps:chain btn class-list (remove "favorited"))
                              (setf (ps:@ (ps:chain btn (query-selector ".star-icon")) text-content) "☆"))))
                    (catch (lambda (error)
                             (ps:chain console (error "Error removing favorite:" error)))))
                   ;; Add favorite
                   (ps:chain
                    (fetch (+ "/api/asteroid/user/favorites/add?" params)
                           (ps:create :method "POST"))
                    (then (lambda (response)
                            (cond
                              ((not (ps:@ response ok))
                               (alert "Please log in to save favorites")
                               nil)
                              (t (ps:chain response (json))))))
                    (then (lambda (data)
                            (when (and data (or (= (ps:@ data status) "success")
                                               (= (ps:@ data data status) "success")))
                              (ps:chain btn class-list (add "favorited"))
                              (setf (ps:@ (ps:chain btn (query-selector ".star-icon")) text-content) "★"))))
                    (catch (lambda (error)
                             (ps:chain console (error "Error adding favorite:" error)))))))))))
     
     ;; Update popout now playing display (parses artist - title)
     (defun update-popout-now-playing ()
       (let ((mount (get-current-mount)))
         (ps:chain
          (fetch (+ "/api/asteroid/partial/now-playing-inline?mount=" mount))
          (then (lambda (response)
                  (if (ps:@ response ok)
                      (ps:chain response (text))
                      "")))
          (then (lambda (html)
                (let* ((parser (ps:new (-d-o-m-parser)))
                       (doc (ps:chain parser (parse-from-string html "text/html")))
                       (track-text (or (ps:@ doc body text-content)
                                       (ps:@ doc body inner-text)
                                       ""))
                       (parts (ps:chain track-text (split " - "))))
                  (if (>= (ps:@ parts length) 2)
                      (progn
                        (let ((artist-el (ps:chain document (get-element-by-id "popout-track-artist")))
                              (title-el (ps:chain document (get-element-by-id "popout-track-title"))))
                          (when artist-el
                            (setf (ps:@ artist-el text-content) (ps:chain (aref parts 0) (trim))))
                          (when title-el
                            (setf (ps:@ title-el text-content)
                                  (ps:chain (ps:chain parts (slice 1) (join " - ")) (trim))))))
                      (progn
                        (let ((title-el (ps:chain document (get-element-by-id "popout-track-title")))
                              (artist-el (ps:chain document (get-element-by-id "popout-track-artist"))))
                          (when title-el
                            (setf (ps:@ title-el text-content) (ps:chain track-text (trim))))
                          (when artist-el
                            (setf (ps:@ artist-el text-content) "Asteroid Radio"))))))))
          (catch (lambda (error)
                   (ps:chain console (error "Error updating now playing:" error)))))))
     
     ;; ========================================
     ;; Status Display
     ;; ========================================
     
     ;; Show status message
     (defun show-status (message is-error)
       (let ((status (ps:chain document (get-element-by-id "stream-status"))))
         (when status
           (setf (ps:@ status text-content) message)
           (setf (ps:@ status style display) "block")
           (setf (ps:@ status style background) (if is-error "#550000" "#005500"))
           (setf (ps:@ status style color) (if is-error "#ff6666" "#66ff66"))
           (unless is-error
             (set-timeout (lambda ()
                            (setf (ps:@ status style display) "none"))
                          3000)))))
     
     ;; Hide status message
     (defun hide-status ()
       (let ((status (ps:chain document (get-element-by-id "stream-status"))))
         (when status
           (setf (ps:@ status style display) "none"))))
     
     ;; ========================================
     ;; Stream Reconnect Logic
     ;; ========================================
     
     ;; Error retry counter and reconnect state
     (defvar *stream-error-count* 0)
     (defvar *reconnect-timeout* nil)
     (defvar *is-reconnecting* false)
     
     ;; Reconnect stream - recreates audio element to fix wedged state
     (defun reconnect-stream ()
       (ps:chain console (log "Reconnecting stream..."))
       (show-status "🔄 Reconnecting..." false)
       
       (let* ((container (ps:chain document (query-selector ".persistent-player")))
              (old-audio (ps:chain document (get-element-by-id "persistent-audio")))
              (stream-base-url (ps:@ (ps:chain document (get-element-by-id "stream-base-url")) value))
              (stream-channel (get-current-channel))
              (stream-quality (get-current-quality))
              (config (get-stream-config stream-base-url stream-channel stream-quality)))
         
         (unless (and container old-audio)
           (show-status "❌ Could not reconnect - reload page" true)
           (return-from reconnect-stream nil))
         
         ;; Save current volume and muted state
         (let ((saved-volume (ps:@ old-audio volume))
               (saved-muted (ps:@ old-audio muted)))
           (ps:chain console (log "Saving volume:" saved-volume "muted:" saved-muted))
           
           ;; Reset spectrum analyzer if it exists
           (when (ps:@ window reset-spectrum-analyzer)
             (ps:chain window (reset-spectrum-analyzer)))
           
           ;; Stop and remove old audio
           (ps:chain old-audio (pause))
           (setf (ps:@ old-audio src) "")
           (ps:chain old-audio (load))
           
           ;; Create new audio element
           (let ((new-audio (ps:chain document (create-element "audio"))))
             (setf (ps:@ new-audio id) "persistent-audio")
             (setf (ps:@ new-audio controls) true)
             (setf (ps:@ new-audio preload) "metadata")
             (setf (ps:@ new-audio cross-origin) "anonymous")
             
             ;; Restore volume and muted state
             (setf (ps:@ new-audio volume) saved-volume)
             (setf (ps:@ new-audio muted) saved-muted)
             
             ;; Create source
             (let ((source (ps:chain document (create-element "source"))))
               (setf (ps:@ source id) "audio-source")
               (setf (ps:@ source src) (ps:@ config url))
               (setf (ps:@ source type) (ps:@ config type))
               (ps:chain new-audio (append-child source)))
             
             ;; Replace old audio with new
             (ps:chain old-audio (replace-with new-audio))
             
             ;; Re-attach event listeners
             (attach-audio-listeners new-audio)
             
             ;; Try to play
             (set-timeout
              (lambda ()
                (ps:chain new-audio (play)
                          (then (lambda ()
                                  (ps:chain console (log "Reconnected successfully"))
                                  (show-status "✓ Reconnected!" false)
                                  ;; Reinitialize spectrum analyzer
                                  (when (ps:@ window init-spectrum-analyzer)
                                    (set-timeout (lambda ()
                                                   (ps:chain window (init-spectrum-analyzer)))
                                                 500))
                                  ;; Also try in content frame
                                  (set-timeout
                                   (lambda ()
                                     (ps:try
                                      (let ((content-frame (ps:@ (ps:@ window parent) frames "content-frame")))
                                        (when (and content-frame (ps:@ content-frame init-spectrum-analyzer))
                                          (when (ps:@ content-frame reset-spectrum-analyzer)
                                            (ps:chain content-frame (reset-spectrum-analyzer)))
                                          (ps:chain content-frame (init-spectrum-analyzer))
                                          (ps:chain console (log "Spectrum analyzer reinitialized in content frame"))))
                                      (:catch (e)
                                        (ps:chain console (log "Could not reinit spectrum in content frame:" e)))))
                                   600)))
                          (catch (lambda (err)
                                   (ps:chain console (log "Reconnect play failed:" err))
                                   (show-status "Click play to start stream" false)))))
              300)))))
     
     ;; Simple reconnect for popout player (just reload and play)
     (defun simple-reconnect (audio-element)
       (ps:chain audio-element (load))
       (ps:chain audio-element (play)
                 (catch (lambda (err)
                          (ps:chain console (log "Reconnect failed:" err))))))
     
     ;; Attach event listeners to audio element
     (defun attach-audio-listeners (audio-element)
       (ps:chain audio-element
                 (add-event-listener "waiting"
                                     (lambda ()
                                       (ps:chain console (log "Audio buffering...")))))
       
       (ps:chain audio-element
                 (add-event-listener "playing"
                                     (lambda ()
                                       (ps:chain console (log "Audio playing"))
                                       (hide-status)
                                       (setf *stream-error-count* 0)
                                       (setf *is-reconnecting* false)
                                       (when *reconnect-timeout*
                                         (clear-timeout *reconnect-timeout*)
                                         (setf *reconnect-timeout* nil)))))
       
       (ps:chain audio-element
                 (add-event-listener "error"
                                     (lambda (e)
                                       (ps:chain console (error "Audio error:" e))
                                       (unless *is-reconnecting*
                                         (setf *stream-error-count* (+ *stream-error-count* 1))
                                         ;; Calculate delay with exponential backoff (3s, 6s, 12s, max 30s)
                                         (let ((delay (ps:chain -math (min (* 3000 (ps:chain -math (pow 2 (- *stream-error-count* 1)))) 30000))))
                                           (show-status (+ "⚠️ Stream error. Reconnecting in " (/ delay 1000) "s... (attempt " *stream-error-count* ")") true)
                                           (setf *is-reconnecting* true)
                                           (setf *reconnect-timeout*
                                                 (set-timeout (lambda () (reconnect-stream)) delay)))))))
       
       (ps:chain audio-element
                 (add-event-listener "stalled"
                                     (lambda ()
                                       (unless *is-reconnecting*
                                         (ps:chain console (log "Audio stalled, will auto-reconnect in 5 seconds..."))
                                         (show-status "⚠️ Stream stalled - reconnecting..." true)
                                         (setf *is-reconnecting* true)
                                         (set-timeout
                                          (lambda ()
                                            (if (< (ps:@ audio-element ready-state) 3)
                                                (reconnect-stream)
                                                (setf *is-reconnecting* false)))
                                          5000)))))
       
       ;; Handle ended event - stream shouldn't end, so reconnect
       (ps:chain audio-element
                 (add-event-listener "ended"
                                     (lambda ()
                                       (unless *is-reconnecting*
                                         (ps:chain console (log "Stream ended unexpectedly, reconnecting..."))
                                         (show-status "⚠️ Stream ended - reconnecting..." true)
                                         (setf *is-reconnecting* true)
                                         (set-timeout (lambda () (reconnect-stream)) 2000)))))
       
       ;; Handle pause event - detect browser throttling muted streams
       (ps:chain audio-element
                 (add-event-listener "pause"
                                     (lambda ()
                                       ;; If paused while muted and we didn't initiate it, browser may have throttled
                                       (when (and (ps:@ audio-element muted) (not *is-reconnecting*))
                                         (ps:chain console (log "Stream paused while muted (possible browser throttling), will reconnect in 3 seconds..."))
                                         (show-status "⚠️ Stream paused - reconnecting..." true)
                                         (setf *is-reconnecting* true)
                                         (set-timeout (lambda () (reconnect-stream)) 3000))))))
     
     ;; Attach simple listeners for popout player
     (defun attach-popout-listeners (audio-element)
       (defvar *popout-error-count* 0)
       (defvar *popout-reconnect-timeout* nil)
       (defvar *popout-is-reconnecting* false)
       
       (ps:chain audio-element
                 (add-event-listener "playing"
                                     (lambda ()
                                       (ps:chain console (log "Audio playing"))
                                       (setf *popout-error-count* 0)
                                       (setf *popout-is-reconnecting* false)
                                       (when *popout-reconnect-timeout*
                                         (clear-timeout *popout-reconnect-timeout*)
                                         (setf *popout-reconnect-timeout* nil)))))
       
       (ps:chain audio-element
                 (add-event-listener "error"
                                     (lambda (e)
                                       (ps:chain console (error "Audio error:" e))
                                       (unless *popout-is-reconnecting*
                                         (setf *popout-error-count* (+ *popout-error-count* 1))
                                         (let ((delay (ps:chain -math (min (* 3000 (ps:chain -math (pow 2 (- *popout-error-count* 1)))) 30000))))
                                           (ps:chain console (log (+ "Stream error. Reconnecting in " (/ delay 1000) "s... (attempt " *popout-error-count* ")")))
                                           (setf *popout-is-reconnecting* true)
                                           (setf *popout-reconnect-timeout*
                                                 (set-timeout (lambda () (simple-reconnect audio-element)) delay)))))))
       
       (ps:chain audio-element
                 (add-event-listener "stalled"
                                     (lambda ()
                                       (unless *popout-is-reconnecting*
                                         (ps:chain console (log "Stream stalled, will auto-reconnect in 5 seconds..."))
                                         (setf *popout-is-reconnecting* true)
                                         (set-timeout
                                          (lambda ()
                                            (if (< (ps:@ audio-element ready-state) 3)
                                                (simple-reconnect audio-element)
                                                (setf *popout-is-reconnecting* false)))
                                          5000)))))
       
       (ps:chain audio-element
                 (add-event-listener "ended"
                                     (lambda ()
                                       (unless *popout-is-reconnecting*
                                         (ps:chain console (log "Stream ended unexpectedly, reconnecting..."))
                                         (setf *popout-is-reconnecting* true)
                                         (set-timeout (lambda () (simple-reconnect audio-element)) 2000)))))
       
       (ps:chain audio-element
                 (add-event-listener "pause"
                                     (lambda ()
                                       (when (and (ps:@ audio-element muted) (not *popout-is-reconnecting*))
                                         (ps:chain console (log "Stream paused while muted (possible browser throttling), reconnecting..."))
                                         (setf *popout-is-reconnecting* true)
                                         (set-timeout (lambda () (simple-reconnect audio-element)) 3000))))))
     
     ;; ========================================
     ;; Frameset Mode
     ;; ========================================
     
     ;; Disable frameset mode function
     (defun disable-frameset-mode ()
       ;; Clear preference
       (ps:chain local-storage (remove-item "useFrameset"))
       ;; Redirect parent window to regular view
       (setf (ps:@ (ps:@ window parent) location href) "/asteroid/"))
     
     ;; ========================================
     ;; Popout Window Communication
     ;; ========================================
     
     ;; Notify parent window that popout is open
     (defun notify-popout-opened ()
       (when (and (ps:@ window opener) (not (ps:@ (ps:@ window opener) closed)))
         (ps:chain (ps:@ window opener) (post-message (ps:create :type "popout-opened") "*"))))
     
     ;; Notify parent when closing
     (defun notify-popout-closing ()
       (when (and (ps:@ window opener) (not (ps:@ (ps:@ window opener) closed)))
         (ps:chain (ps:@ window opener) (post-message (ps:create :type "popout-closed") "*"))))
     
     ;; ========================================
     ;; Initialization
     ;; ========================================
     
     ;; Initialize persistent player (audio-player-frame)
     (defun init-persistent-player ()
       (let ((audio-element (ps:chain document (get-element-by-id "persistent-audio"))))
         (when audio-element
           ;; Try to enable low-latency mode if supported
           (when (ps:@ navigator media-session)
             (setf (ps:@ navigator media-session metadata)
                   (ps:new (-media-metadata
                           (ps:create :title "Asteroid Radio Live Stream"
                                      :artist "Asteroid Radio"
                                      :album "Live Broadcast")))))
           
           ;; Attach event listeners
           (attach-audio-listeners audio-element)
           
           ;; Restore user channel preference
           (let ((channel-selector (ps:chain document (get-element-by-id "stream-channel")))
                 (stream-channel (or (ps:chain local-storage (get-item "stream-channel")) "curated")))
             (when (and channel-selector (not (= (ps:@ channel-selector value) stream-channel)))
               (setf (ps:@ channel-selector value) stream-channel)
               ;; Sync the stream to the saved channel
               (change-channel)))
           
           ;; Restore user quality preference
           (let ((quality-selector (ps:chain document (get-element-by-id "stream-quality")))
                 (stream-quality (or (ps:chain local-storage (get-item "stream-quality")) "aac")))
             (when (and quality-selector (not (= (ps:@ quality-selector value) stream-quality)))
               (setf (ps:@ quality-selector value) stream-quality)))
           
           ;; Update quality selector state based on channel
           (update-quality-selector-state)
           
           ;; Poll server for channel name changes (works across all listeners)
           (let ((last-channel-name nil))
             (set-interval
               (lambda ()
                 (ps:chain
                  (fetch "/api/asteroid/channel-name")
                  (then (lambda (response)
                          (if (ps:@ response ok)
                              (ps:chain response (json))
                              nil)))
                  (then (lambda (data)
                          (when data
                            (let ((current-channel-name (or (ps:@ data data channel_name)
                                                            (ps:@ data channel_name))))
                              (when (and current-channel-name
                                         (not (= current-channel-name last-channel-name)))
                                (setf last-channel-name current-channel-name)
                                ;; Update localStorage for popout player sync
                                (ps:chain local-storage (set-item "curated-channel-name" current-channel-name))
                                (let ((channel-selector (ps:chain document (get-element-by-id "stream-channel"))))
                                  (when channel-selector
                                    (let ((curated-option (ps:chain channel-selector (query-selector "option[value='curated']"))))
                                      (when curated-option
                                        (setf (ps:@ curated-option text-content) (+ "🎧 " current-channel-name)))))))))))
                  (catch (lambda (error)
                           (ps:chain console (log "Could not fetch channel name:" error))))))
               10000))  ;; Poll every 10 seconds
           
           ;; Start now playing updates
           (set-timeout update-mini-now-playing 1000)
           (set-interval update-mini-now-playing 5000))))
     
     ;; Initialize popout player
     (defun init-popout-player ()
       (let ((audio-element (ps:chain document (get-element-by-id "live-audio"))))
         (when audio-element
           ;; Attach event listeners
           (attach-popout-listeners audio-element)
           
           ;; Restore user channel preference
           (let ((channel-selector (ps:chain document (get-element-by-id "popout-stream-channel")))
                 (stream-channel (or (ps:chain local-storage (get-item "stream-channel")) "curated")))
             (when (and channel-selector (not (= (ps:@ channel-selector value) stream-channel)))
               (setf (ps:@ channel-selector value) stream-channel)))
           
           ;; Restore user quality preference
           (let ((quality-selector (ps:chain document (get-element-by-id "popout-stream-quality")))
                 (stream-quality (or (ps:chain local-storage (get-item "stream-quality")) "aac")))
             (when (and quality-selector (not (= (ps:@ quality-selector value) stream-quality)))
               (setf (ps:@ quality-selector value) stream-quality)))
           
           ;; Update quality selector state based on channel
           (update-quality-selector-state)
           
           ;; Listen for channel name changes from localStorage
           (ps:chain window (add-event-listener "storage"
             (lambda (e)
               (when (= (ps:@ e key) "curated-channel-name")
                 (let ((channel-selector (ps:chain document (get-element-by-id "popout-stream-channel"))))
                   (when channel-selector
                     (let ((curated-option (ps:chain channel-selector (query-selector "option[value='curated']"))))
                       (when curated-option
                         (setf (ps:@ curated-option text-content) (+ "🎧 " (ps:@ e new-value)))))))))))
           
           ;; Start now playing updates
           (update-popout-now-playing)
           (set-interval update-popout-now-playing 5000)
           
           ;; Notify parent window
           (notify-popout-opened)
           
           ;; Setup close notification
           (ps:chain window (add-event-listener "beforeunload" notify-popout-closing)))))
     
     ;; Make functions globally accessible
     (setf (ps:@ window get-stream-config) get-stream-config)
     (setf (ps:@ window change-channel) change-channel)
     (setf (ps:@ window sync-channel-from-storage) sync-channel-from-storage)
     (setf (ps:@ window change-stream-quality) change-stream-quality)
     (setf (ps:@ window reconnect-stream) reconnect-stream)
     (setf (ps:@ window disable-frameset-mode) disable-frameset-mode)
     (setf (ps:@ window init-persistent-player) init-persistent-player)
     (setf (ps:@ window init-popout-player) init-popout-player)
     (setf (ps:@ window update-mini-now-playing) update-mini-now-playing)
     (setf (ps:@ window update-popout-now-playing) update-popout-now-playing)
     (setf (ps:@ window toggle-favorite-mini) toggle-favorite-mini)
     
     ;; Auto-initialize on DOMContentLoaded based on which elements exist
     (ps:chain document
               (add-event-listener
                "DOMContentLoaded"
                (lambda ()
                  ;; Check for persistent player (audio-player-frame)
                  (when (ps:chain document (get-element-by-id "persistent-audio"))
                    (init-persistent-player))
                  ;; Check for popout player
                  (when (ps:chain document (get-element-by-id "live-audio"))
                    (init-popout-player))))))
   )
  "Compiled JavaScript for stream player - generated at load time")

(defun generate-stream-player-js ()
  "Generate JavaScript code for the stream player"
  *stream-player-js*)
