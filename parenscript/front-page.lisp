;;;; front-page.lisp - ParenScript version of front-page.js
;;;; Stream quality, now playing, pop-out player, frameset mode

(in-package #:asteroid)

(defparameter *front-page-js*
  (ps:ps*
   '(progn
     
     ;; Stream connection state
     (defvar *stream-error-count* 0)
     (defvar *last-play-attempt* 0)
     (defvar *is-reconnecting* false)
     (defvar *reconnect-timeout* nil)
     
     ;; Stream configuration by channel and quality
     ;; Curated channel has multiple quality options, shuffle has only one
     (defun get-stream-config (stream-base-url channel quality)
       (let ((curated-config (ps:create
                              :aac (ps:create
                                    :url (+ stream-base-url "/asteroid.aac")
                                    :format "AAC 96kbps Stereo"
                                    :type "audio/aac"
                                    :mount "asteroid.aac")
                              :mp3 (ps:create
                                    :url (+ stream-base-url "/asteroid.mp3")
                                    :format "MP3 128kbps Stereo"
                                    :type "audio/mpeg"
                                    :mount "asteroid.mp3")
                              :low (ps:create
                                    :url (+ stream-base-url "/asteroid-low.mp3")
                                    :format "MP3 64kbps Stereo"
                                    :type "audio/mpeg"
                                    :mount "asteroid-low.mp3")))
             (shuffle-config (ps:create
                              :url (+ stream-base-url "/asteroid-shuffle.mp3")
                              :format "Shuffle MP3 96kbps"
                              :type "audio/mpeg"
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
     (defun change-channel ()
       (let* ((channel-selector (or (ps:chain document (get-element-by-id "stream-channel"))
                                    (ps:chain document (get-element-by-id "popout-stream-channel"))))
              (channel (ps:@ channel-selector value))
              (stream-base-url (ps:chain document (get-element-by-id "stream-base-url")))
              (quality (get-current-quality))
              (config (get-stream-config (ps:@ stream-base-url value) channel quality))
              (audio-element (or (ps:chain document (get-element-by-id "live-audio"))
                                 (ps:chain document (get-element-by-id "persistent-audio"))))
              (source-element (ps:chain document (get-element-by-id "audio-source")))
              (was-playing (and audio-element (not (ps:@ audio-element paused)))))
         
         ;; Save preference
         (ps:chain local-storage (set-item "stream-channel" channel))
         
         ;; Update quality selector state
         (update-quality-selector-state)
         
         ;; Update stream information display
         (update-stream-information)
         
         ;; Update audio player
         (when source-element
           (setf (ps:@ source-element src) (ps:@ config url))
           (setf (ps:@ source-element type) (ps:@ config type)))
         (when audio-element
           (ps:chain audio-element (load))
           ;; Resume playback if it was playing
           (when was-playing
             (ps:chain (ps:chain audio-element (play))
                       (catch (lambda (e)
                                (ps:chain console (log "Autoplay prevented:" e)))))))
         
         ;; If in frameset mode, notify the player frame to update
         (when (not (= (ps:@ window parent) window))
           (ps:try
            (let ((player-frame (ps:@ (ps:@ window parent) frames "player-frame")))
              (when (and player-frame (ps:@ player-frame sync-channel-from-storage))
                (ps:chain player-frame (sync-channel-from-storage))))
            (:catch (e) nil)))
         
         ;; Immediately refresh now playing and recently played
         (update-now-playing)
         (when (ps:@ window update-recently-played)
           (ps:chain window (update-recently-played)))))
     
     ;; Change stream quality (bitrate)
     (defun change-stream-quality ()
       (let* ((selector (or (ps:chain document (get-element-by-id "stream-quality"))
                            (ps:chain document (get-element-by-id "popout-stream-quality"))))
              (stream-base-url (ps:chain document (get-element-by-id "stream-base-url")))
              (channel (get-current-channel))
              (quality (ps:@ selector value))
              (config (get-stream-config (ps:@ stream-base-url value) channel quality))
              (audio-element (or (ps:chain document (get-element-by-id "live-audio"))
                                 (ps:chain document (get-element-by-id "persistent-audio"))))
              (source-element (ps:chain document (get-element-by-id "audio-source")))
              (was-playing (and audio-element (not (ps:@ audio-element paused)))))
         
         ;; Save preference
         (ps:chain local-storage (set-item "stream-quality" quality))
         
         ;; Update stream information
         (update-stream-information)
         
         ;; Update audio player
         (when source-element
           (setf (ps:@ source-element src) (ps:@ config url))
           (setf (ps:@ source-element type) (ps:@ config type)))
         (when audio-element
           (ps:chain audio-element (load))
           ;; Resume playback if it was playing
           (when was-playing
             (ps:chain (ps:chain audio-element (play))
                       (catch (lambda (e)
                                (ps:chain console (log "Autoplay prevented:" e)))))))))
     
     ;; Get current mount from channel and quality selection
     ;; Checks local selectors first, then sibling player-frame (for frameset mode)
     (defun get-current-mount ()
       (let* ((channel (get-current-channel))
              (quality (get-current-quality))
              (stream-base-url (or (ps:chain document (get-element-by-id "stream-base-url"))
                                   (when (not (= (ps:@ window parent) window))
                                     (ps:try
                                      (let ((player-frame (ps:@ (ps:@ window parent) frames "player-frame")))
                                        (when player-frame
                                          (ps:chain player-frame document (get-element-by-id "stream-base-url"))))
                                      (:catch (e) nil)))))
              (config (when stream-base-url
                        (get-stream-config (ps:@ stream-base-url value) channel quality))))
         (if config (ps:@ config mount) "asteroid.mp3")))
     
     ;; Update now playing info from API
     (defun update-now-playing ()
       (let ((mount (get-current-mount)))
         (ps:chain
          (fetch (+ "/api/asteroid/partial/now-playing?mount=" mount))
          (then (lambda (response)
                  (let ((content-type (ps:chain response headers (get "content-type"))))
                    (if (ps:chain content-type (includes "text/html"))
                        (ps:chain response (text))
                        (throw (ps:new (-error "Error connecting to stream")))))))
          (then (lambda (data)
                  (setf (ps:@ (ps:chain document (get-element-by-id "now-playing")) inner-h-t-m-l)
                        data)))
          (catch (lambda (error)
                   (ps:chain console (log "Could not fetch stream status:" error)))))))
     
     ;; Update stream information
     (defun update-stream-information ()
       (let* ((channel-selector (or (ps:chain document (get-element-by-id "stream-channel"))
                                    (ps:chain document (get-element-by-id "popout-stream-channel"))))
              (quality-selector (or (ps:chain document (get-element-by-id "stream-quality"))
                                    (ps:chain document (get-element-by-id "popout-stream-quality"))))
              (stream-base-url (ps:chain document (get-element-by-id "stream-base-url")))
              (stream-channel (or (ps:chain local-storage (get-item "stream-channel")) "curated"))
              (stream-quality (or (ps:chain local-storage (get-item "stream-quality")) "aac")))
         
         ;; Update channel selector if needed
         (when (and channel-selector (not (= (ps:@ channel-selector value) stream-channel)))
           (setf (ps:@ channel-selector value) stream-channel))
         
         ;; Update quality selector if needed
         (when (and quality-selector (not (= (ps:@ quality-selector value) stream-quality)))
           (setf (ps:@ quality-selector value) stream-quality))
         
         ;; Update quality selector state (disabled for shuffle)
         (update-quality-selector-state)
         
         ;; Update stream info display
         (when stream-base-url
           (let ((config (get-stream-config (ps:@ stream-base-url value) stream-channel stream-quality)))
             (let ((url-el (ps:chain document (get-element-by-id "stream-url"))))
               (when url-el
                 (setf (ps:@ url-el text-content) (ps:@ config url))))
             (let ((format-el (ps:chain document (get-element-by-id "stream-format"))))
               (when format-el
                 (setf (ps:@ format-el text-content) (ps:@ config format))))
             (let ((status-quality (ps:chain document (query-selector "[data-text=\"stream-quality\"]"))))
               (when status-quality
                 (setf (ps:@ status-quality text-content) (ps:@ config format))))))))
     
     ;; Pop-out player functionality
     (defvar *popout-window* nil)
     
     (defun open-popout-player ()
       ;; Check if popout is already open
       (when (and *popout-window* (not (ps:@ *popout-window* closed)))
         (ps:chain *popout-window* (focus))
         (return))
       
       ;; Calculate centered position
       (let* ((width 420)
              (height 300)
              (left (/ (- (ps:@ screen width) width) 2))
              (top (/ (- (ps:@ screen height) height) 2))
              (features (+ "width=" width ",height=" height ",left=" left ",top=" top
                           ",resizable=yes,scrollbars=no,status=no,menubar=no,toolbar=no,location=no")))
         
         ;; Open popout window
         (setf *popout-window*
               (ps:chain window (open "/asteroid/popout-player" "AsteroidPlayer" features)))
         
         ;; Update button state
         (update-popout-button t)))
     
     (defun update-popout-button (is-open)
       (let ((btn (ps:chain document (get-element-by-id "popout-btn"))))
         (when btn
           (if is-open
               (progn
                 (setf (ps:@ btn text-content) "✓ Player Open")
                 (ps:chain btn class-list (remove "btn-info"))
                 (ps:chain btn class-list (add "btn-success")))
               (progn
                 (setf (ps:@ btn text-content) "🗗 Pop Out Player")
                 (ps:chain btn class-list (remove "btn-success"))
                 (ps:chain btn class-list (add "btn-info")))))))
     
     ;; Frameset mode functionality
     (defun enable-frameset-mode ()
       (ps:chain local-storage (set-item "useFrameset" "true"))
       (setf (ps:@ window location href) "/asteroid/frameset"))
     
     (defun disable-frameset-mode ()
       (ps:chain local-storage (remove-item "useFrameset"))
       (setf (ps:@ window location href) "/asteroid/"))
     
     ;; Stream status UI functions
     (defun show-stream-status (message status-type)
       "Show a status message to the user. status-type: 'error', 'warning', 'success', 'info'"
       (let ((indicator (ps:chain document (get-element-by-id "stream-status-indicator"))))
         (when indicator
           (setf (ps:@ indicator inner-text) message)
           (setf (ps:@ indicator style display) "block")
           (setf (ps:@ indicator style background)
                 (cond
                   ((= status-type "error") "#550000")
                   ((= status-type "warning") "#554400")
                   ((= status-type "success") "#005500")
                   (t "#003355")))
           (setf (ps:@ indicator style border)
                 (cond
                   ((= status-type "error") "1px solid #ff0000")
                   ((= status-type "warning") "1px solid #ffaa00")
                   ((= status-type "success") "1px solid #00ff00")
                   (t "1px solid #00aaff"))))))
     
     (defun hide-stream-status ()
       "Hide the status indicator"
       (let ((indicator (ps:chain document (get-element-by-id "stream-status-indicator"))))
         (when indicator
           (setf (ps:@ indicator style display) "none"))))
     
     (defun show-reconnect-button ()
       "Show the reconnect button"
       (let ((btn (ps:chain document (get-element-by-id "reconnect-btn"))))
         (when btn
           (setf (ps:@ btn style display) "inline-block"))))
     
     (defun hide-reconnect-button ()
       "Hide the reconnect button"
       (let ((btn (ps:chain document (get-element-by-id "reconnect-btn"))))
         (when btn
           (setf (ps:@ btn style display) "none"))))
     
     ;; Recreate audio element to fix wedged state
     (defun recreate-audio-element ()
       "Recreate the audio element entirely to fix wedged MediaElementSource"
       (let* ((container (ps:chain document (get-element-by-id "audio-container")))
              (old-audio (ps:chain document (get-element-by-id "live-audio")))
              (stream-base-url (ps:chain document (get-element-by-id "stream-base-url")))
              (stream-channel (get-current-channel))
              (stream-quality (get-current-quality))
              (config (get-stream-config (ps:@ stream-base-url value) stream-channel stream-quality)))
         
         (when (and container old-audio)
           ;; Reset spectrum analyzer before removing audio
           (when (ps:@ window |resetSpectrumAnalyzer|)
             (ps:chain window (reset-spectrum-analyzer)))
           
           ;; Remove old audio element
           (ps:chain old-audio (pause))
           (setf (ps:@ old-audio src) "")
           (ps:chain old-audio (remove))
           
           ;; Create new audio element
           (let ((new-audio (ps:chain document (create-element "audio"))))
             (setf (ps:@ new-audio id) "live-audio")
             (setf (ps:@ new-audio controls) t)
             (setf (ps:@ new-audio crossorigin) "anonymous")
             (setf (ps:@ new-audio style width) "100%")
             (setf (ps:@ new-audio style margin) "10px 0")
             
             ;; Create source element
             (let ((source (ps:chain document (create-element "source"))))
               (setf (ps:@ source id) "audio-source")
               (setf (ps:@ source src) (ps:@ config url))
               (setf (ps:@ source type) (ps:@ config type))
               (ps:chain new-audio (append-child source)))
             
             ;; Add to container
             (ps:chain container (append-child new-audio))
             
             ;; Re-attach event listeners
             (attach-audio-event-listeners new-audio)
             
             (ps:chain console (log "Audio element recreated"))
             new-audio))))
     
     ;; Main reconnect function
     (defun reconnect-stream ()
       "Reconnect the stream - called by user or automatically"
       (when *is-reconnecting*
         (return))
       
       (setf *is-reconnecting* t)
       (show-stream-status "🔄 Reconnecting to stream..." "info")
       (hide-reconnect-button)
       
       ;; Clear any pending reconnect timeout
       (when *reconnect-timeout*
         (clear-timeout *reconnect-timeout*)
         (setf *reconnect-timeout* nil))
       
       (let ((audio-element (ps:chain document (get-element-by-id "live-audio"))))
         (if audio-element
             ;; Try simple reload first
             (progn
               (ps:chain audio-element (pause))
               (ps:chain audio-element (load))
               
               ;; Resume AudioContext if suspended
               (when (ps:@ window |resetSpectrumAnalyzer|)
                 (ps:chain window (reset-spectrum-analyzer)))
               
               ;; Try to play after a short delay
               (set-timeout
                (lambda ()
                  (ps:chain audio-element (play)
                            (then (lambda ()
                                    (setf *stream-error-count* 0)
                                    (setf *is-reconnecting* false)
                                    (show-stream-status "✓ Stream reconnected!" "success")
                                    (set-timeout hide-stream-status 3000)
                                    
                                    ;; Reinitialize spectrum analyzer
                                    (when (ps:@ window |initSpectrumAnalyzer|)
                                      (set-timeout
                                       (lambda ()
                                         (ps:chain window (init-spectrum-analyzer)))
                                       500))))
                            (catch (lambda (err)
                                     (ps:chain console (log "Simple reconnect failed, recreating audio element:" err))
                                     ;; Simple reload failed, recreate the audio element
                                     (let ((new-audio (recreate-audio-element)))
                                       (when new-audio
                                         (set-timeout
                                          (lambda ()
                                            (ps:chain new-audio (play)
                                                      (then (lambda ()
                                                              (setf *stream-error-count* 0)
                                                              (setf *is-reconnecting* false)
                                                              (show-stream-status "✓ Stream reconnected!" "success")
                                                              (set-timeout hide-stream-status 3000)))
                                                      (catch (lambda (err2)
                                                               (setf *is-reconnecting* false)
                                                               (incf *stream-error-count*)
                                                               (show-stream-status "❌ Could not reconnect. Click play to try again." "error")
                                                               (show-reconnect-button)
                                                               (ps:chain console (log "Reconnect failed:" err2))))))
                                          500)))))))
                500))
             
             ;; No audio element found, try to recreate
             (let ((new-audio (recreate-audio-element)))
               (if new-audio
                   (set-timeout
                    (lambda ()
                      (ps:chain new-audio (play)
                                (then (lambda ()
                                        (setf *is-reconnecting* false)
                                        (show-stream-status "✓ Stream connected!" "success")
                                        (set-timeout hide-stream-status 3000)))
                                (catch (lambda (err)
                                         (setf *is-reconnecting* false)
                                         (show-stream-status "❌ Could not connect. Click play to try again." "error")
                                         (show-reconnect-button)))))
                    500)
                   (progn
                     (setf *is-reconnecting* false)
                     (show-stream-status "❌ Could not create audio player. Please reload the page." "error")))))))
     
     ;; Attach event listeners to audio element
     (defun attach-audio-event-listeners (audio-element)
       "Attach all necessary event listeners to an audio element"
       
       ;; Error handler - retry indefinitely with exponential backoff
       (ps:chain audio-element
                 (add-event-listener "error"
                                     (lambda (err)
                                       (ps:chain console (log "Stream error:" err))
                                       (when *is-reconnecting*
                                         (return))
                                       (incf *stream-error-count*)
                                       ;; Calculate delay with exponential backoff (3s, 6s, 12s, max 30s)
                                       (let ((delay (ps:chain |Math| (min (* 3000 (ps:chain |Math| (pow 2 (- *stream-error-count* 1)))) 30000))))
                                         (show-stream-status (+ "⚠️ Stream error. Reconnecting in " (/ delay 1000) "s... (attempt " *stream-error-count* ")") "warning")
                                         (setf *is-reconnecting* t)
                                         (setf *reconnect-timeout*
                                               (set-timeout reconnect-stream delay))))))
       
       ;; Stalled handler
       (ps:chain audio-element
                 (add-event-listener "stalled"
                                     (lambda ()
                                       (when *is-reconnecting*
                                         (return))
                                       (ps:chain console (log "Stream stalled, will auto-reconnect in 5 seconds..."))
                                       (show-stream-status "⚠️ Stream stalled - reconnecting..." "warning")
                                       (setf *is-reconnecting* t)
                                       (setf *reconnect-timeout*
                                             (set-timeout
                                              (lambda ()
                                                ;; Only reconnect if still stalled
                                                (if (< (ps:@ audio-element ready-state) 3)
                                                    (reconnect-stream)
                                                    (setf *is-reconnecting* false)))
                                              5000)))))
       
       ;; Ended handler - stream shouldn't end, so reconnect
       (ps:chain audio-element
                 (add-event-listener "ended"
                                     (lambda ()
                                       (when *is-reconnecting*
                                         (return))
                                       (ps:chain console (log "Stream ended unexpectedly, reconnecting..."))
                                       (show-stream-status "⚠️ Stream ended - reconnecting..." "warning")
                                       (setf *is-reconnecting* t)
                                       (set-timeout reconnect-stream 2000))))
       
       ;; Pause handler - detect browser throttling muted streams
       (ps:chain audio-element
                 (add-event-listener "pause"
                                     (lambda ()
                                       (when (and (ps:@ audio-element muted)
                                                  (not *is-reconnecting*))
                                         (ps:chain console (log "Stream paused while muted (possible browser throttling), reconnecting..."))
                                         (show-stream-status "⚠️ Stream paused - reconnecting..." "warning")
                                         (setf *is-reconnecting* t)
                                         (set-timeout reconnect-stream 3000)))))
       
       ;; Waiting handler (buffering)
       (ps:chain audio-element
                 (add-event-listener "waiting"
                                     (lambda ()
                                       (ps:chain console (log "Stream buffering..."))
                                       (show-stream-status "⏳ Buffering..." "info"))))
       
       ;; Playing handler - clear any error states
       (ps:chain audio-element
                 (add-event-listener "playing"
                                     (lambda ()
                                       (setf *stream-error-count* 0)
                                       (setf *is-reconnecting* false)
                                       (hide-stream-status)
                                       (hide-reconnect-button)
                                       (when *reconnect-timeout*
                                         (clear-timeout *reconnect-timeout*)
                                         (setf *reconnect-timeout* nil)))))
       
       ;; Pause handler - track when paused for long pause detection
       (ps:chain audio-element
                 (add-event-listener "pause"
                                     (lambda ()
                                       (setf *last-play-attempt* (ps:chain |Date| (now))))))
       
       ;; Play handler - detect long pauses that need reconnection
       (ps:chain audio-element
                 (add-event-listener "play"
                                     (lambda ()
                                       (let ((pause-duration (- (ps:chain |Date| (now)) *last-play-attempt*)))
                                         ;; If paused for more than 30 seconds, reconnect to get fresh stream
                                         (when (> pause-duration 30000)
                                           (ps:chain console (log "Long pause detected, reconnecting for fresh stream..."))
                                           (reconnect-stream))))))
       
       ;; Spectrum analyzer hooks
       (when (ps:@ window |initSpectrumAnalyzer|)
         (ps:chain audio-element (add-event-listener "play"
                                                     (lambda () (ps:chain window (init-spectrum-analyzer))))))
       
       (when (ps:@ window |stopSpectrumAnalyzer|)
         (ps:chain audio-element (add-event-listener "pause"
                                                     (lambda () (ps:chain window (stop-spectrum-analyzer)))))))
     
     (defun redirect-when-frame ()
       (let* ((path (ps:@ window location pathname))
              (is-frameset-page (not (= (ps:@ window parent) (ps:@ window self))))
              (is-content-frame (ps:chain path (includes "asteroid/content"))))
         
         (when (and is-frameset-page (not is-content-frame))
           (setf (ps:@ window location href) "/asteroid/content"))
         
         (when (and (not is-frameset-page) is-content-frame)
           (setf (ps:@ window location href) "/asteroid"))))
     
     ;; Initialize on page load
     (ps:chain document
      (add-event-listener
       "DOMContentLoaded"
       (lambda ()
         ;; Update stream information
         (update-stream-information)

         ;; Periodically update stream info if in frameset
         (let ((is-frameset-page (not (= (ps:@ window parent) (ps:@ window self)))))
           (when is-frameset-page
             (set-interval update-stream-information 10000)))

         ;; Update now playing
         (update-now-playing)

         ;; Refresh now playing immediately when user switches channel or quality
         (let ((channel-selector (or (ps:chain document (get-element-by-id "stream-channel"))
                                     (ps:chain document (get-element-by-id "popout-stream-channel"))))
               (quality-selector (or (ps:chain document (get-element-by-id "stream-quality"))
                                     (ps:chain document (get-element-by-id "popout-stream-quality")))))
           (when channel-selector
             (ps:chain channel-selector
                       (add-event-listener
                        "change"
                        (lambda (_ev)
                          ;; Small delay so localStorage / UI state settles
                          (ps:chain window (set-timeout update-now-playing 50))
                          (when (ps:@ window update-recently-played)
                            (ps:chain window (set-timeout (ps:@ window update-recently-played) 50)))))))
           (when quality-selector
             (ps:chain quality-selector
                       (add-event-listener
                        "change"
                        (lambda (_ev)
                          (ps:chain window (set-timeout update-now-playing 50)))))))

         ;; Attach event listeners to audio element
         (let ((audio-element (ps:chain document (get-element-by-id "live-audio"))))
           (when audio-element
             (attach-audio-event-listeners audio-element)))

         ;; Check frameset preference
         (let ((path (ps:@ window location pathname))
               (is-frameset-page (not (= (ps:@ window parent) (ps:@ window self)))))
           (when (and (= (ps:chain local-storage (get-item "useFrameset")) "true")
                      (not is-frameset-page)
                      (ps:chain path (includes "/asteroid")))
             (setf (ps:@ window location href) "/asteroid/frameset"))
           
           (redirect-when-frame)))))
     
     ;; Update now playing every 5 seconds
     (set-interval update-now-playing 5000)
     
     ;; Listen for messages from popout window
     (ps:chain window
      (add-event-listener
       "message"
       (lambda (event)
         (cond
           ((= (ps:@ event data type) "popout-opened")
            (update-popout-button t))
           ((= (ps:@ event data type) "popout-closed")
            (update-popout-button nil)
            (setf *popout-window* nil))))))
     
     ;; Check if popout is still open periodically
     (set-interval
      (lambda ()
        (when (and *popout-window* (ps:@ *popout-window* closed))
          (update-popout-button nil)
          (setf *popout-window* nil)))
      1000)))
  "Compiled JavaScript for front-page - generated at load time")

(defun generate-front-page-js ()
  "Return the pre-compiled JavaScript for front page"
  *front-page-js*)
