;;;; stream-player.lisp - ParenScript for persistent stream player
;;;; Handles audio-player-frame and popout-player stream reconnect logic

(in-package #:asteroid)

(defparameter *stream-player-js*
  (ps:ps*
   '(progn
     
     ;; ========================================
     ;; Mini Spectrum Analyzer (for player frame)
     ;; ========================================
     
     (defvar *mini-audio-context* nil)
     (defvar *mini-analyser* nil)
     (defvar *mini-media-source* nil)
     (defvar *mini-canvas* nil)
     (defvar *mini-canvas-ctx* nil)
     (defvar *mini-animation-id* nil)
     
     ;; Color themes (same as main spectrum analyzer)
     (defvar *mini-themes* 
       (ps:create
        "monotone" (ps:create "top" "#0047ab" "mid" "#002966" "bottom" "#000d1a")
        "green" (ps:create "top" "#00ff00" "mid" "#00aa00" "bottom" "#005500")
        "blue" (ps:create "top" "#00ffff" "mid" "#0088ff" "bottom" "#0044aa")
        "purple" (ps:create "top" "#ff00ff" "mid" "#aa00aa" "bottom" "#550055")
        "red" (ps:create "top" "#ff0000" "mid" "#aa0000" "bottom" "#550000")
        "amber" (ps:create "top" "#ffaa00" "mid" "#ff6600" "bottom" "#aa3300")
        "rainbow" (ps:create "top" "#ff00ff" "mid" "#00ffff" "bottom" "#00ff00")))
     
     (defun get-mini-theme ()
       "Get current theme from localStorage (shared with main analyzer)"
       (let ((saved-theme (ps:chain local-storage (get-item "spectrum-theme"))))
         (if (and saved-theme (ps:getprop *mini-themes* saved-theme))
             saved-theme
             "green")))
     
     (defun get-mini-style ()
       "Get current style from localStorage (shared with main analyzer)"
       (let ((saved-style (ps:chain local-storage (get-item "spectrum-style"))))
         (if (and saved-style (or (= saved-style "bars") (= saved-style "wave") (= saved-style "dots")))
             saved-style
             "bars")))
     
     (defun init-mini-spectrum (audio-element)
       "Initialize mini spectrum analyzer in player frame"
       (let ((canvas (ps:chain document (get-element-by-id "mini-spectrum-canvas"))))
         (when (and audio-element canvas (not *mini-audio-context*))
           (ps:try
            (progn
              (setf *mini-audio-context* (ps:new (or (ps:@ window |AudioContext|)
                                                      (ps:@ window |webkitAudioContext|))))
              (setf *mini-analyser* (ps:chain *mini-audio-context* (create-analyser)))
              (setf (ps:@ *mini-analyser* |fftSize|) 64)
              (setf (ps:@ *mini-analyser* |smoothingTimeConstant|) 0.8)
              (setf *mini-media-source* (ps:chain *mini-audio-context* (create-media-element-source audio-element)))
              (ps:chain *mini-media-source* (connect *mini-analyser*))
              (ps:chain *mini-analyser* (connect (ps:@ *mini-audio-context* destination)))
              (setf *mini-canvas* canvas)
              (setf *mini-canvas-ctx* (ps:chain canvas (get-context "2d")))
              (ps:chain console (log "Mini spectrum analyzer initialized")))
            (:catch (e)
              (ps:chain console (log "Error initializing mini spectrum:" e)))))))
     
     (defun draw-mini-spectrum ()
       "Draw mini spectrum visualization using theme and style from localStorage"
       (setf *mini-animation-id* (request-animation-frame draw-mini-spectrum))
       (when (and *mini-analyser* *mini-canvas* *mini-canvas-ctx*)
         (let* ((buffer-length (ps:@ *mini-analyser* |frequencyBinCount|))
                (data-array (ps:new (|Uint8Array| buffer-length)))
                (width (ps:@ *mini-canvas* width))
                (height (ps:@ *mini-canvas* height))
                (bar-width (/ width buffer-length))
                (theme-name (get-mini-theme))
                (theme (ps:getprop *mini-themes* theme-name))
                (style (get-mini-style)))
           (ps:chain *mini-analyser* (get-byte-frequency-data data-array))
           ;; Clear with fade effect
           (setf (ps:@ *mini-canvas-ctx* fill-style) "rgba(0, 0, 0, 0.2)")
           (ps:chain *mini-canvas-ctx* (fill-rect 0 0 width height))
           
           (cond
            ;; Bar graph style
            ((= style "bars")
             (dotimes (i buffer-length)
               (let* ((value (ps:getprop data-array i))
                      (bar-height (* (/ value 255) height))
                      (x (* i bar-width)))
                 (when (> bar-height 0)
                   (let ((gradient (ps:chain *mini-canvas-ctx* 
                                             (create-linear-gradient 0 (- height bar-height) 0 height))))
                     (ps:chain gradient (add-color-stop 0 (ps:@ theme top)))
                     (ps:chain gradient (add-color-stop 0.5 (ps:@ theme mid)))
                     (ps:chain gradient (add-color-stop 1 (ps:@ theme bottom)))
                     (setf (ps:@ *mini-canvas-ctx* fill-style) gradient)
                     (ps:chain *mini-canvas-ctx* (fill-rect x (- height bar-height) bar-width bar-height)))))))
            
            ;; Wave/line style
            ((= style "wave")
             (ps:chain *mini-canvas-ctx* (begin-path))
             (setf (ps:@ *mini-canvas-ctx* |lineWidth|) 2)
             (setf (ps:@ *mini-canvas-ctx* |strokeStyle|) (ps:@ theme top))
             (let ((x 0))
               (dotimes (i buffer-length)
                 (let* ((value (ps:getprop data-array i))
                        (bar-height (* (/ value 255) height))
                        (y (- height bar-height)))
                   (if (= i 0)
                       (ps:chain *mini-canvas-ctx* (move-to x y))
                       (ps:chain *mini-canvas-ctx* (line-to x y)))
                   (incf x bar-width))))
             (ps:chain *mini-canvas-ctx* (stroke)))
            
            ;; Dots/particles style
            ((= style "dots")
             (setf (ps:@ *mini-canvas-ctx* |fillStyle|) (ps:@ theme top))
             (let ((x 0))
               (dotimes (i buffer-length)
                 (let* ((value (ps:getprop data-array i))
                        (bar-height (* (/ value 255) height))
                        (y (- height bar-height))
                        (dot-radius (ps:max 1 (/ bar-height 10))))
                   (when (> value 0)
                     (ps:chain *mini-canvas-ctx* (begin-path))
                     (ps:chain *mini-canvas-ctx* (arc x y dot-radius 0 6.283185307179586))
                     (ps:chain *mini-canvas-ctx* (fill)))
                   (incf x bar-width)))))))))
     
     (defun start-mini-spectrum ()
       "Start mini spectrum animation"
       (when (and *mini-analyser* (not *mini-animation-id*))
         (draw-mini-spectrum)))
     
     (defun stop-mini-spectrum ()
       "Stop mini spectrum animation"
       (when *mini-animation-id*
         (cancel-animation-frame *mini-animation-id*)
         (setf *mini-animation-id* nil))
       ;; Clear canvas
       (when (and *mini-canvas* *mini-canvas-ctx*)
         (let ((width (ps:@ *mini-canvas* width))
               (height (ps:@ *mini-canvas* height)))
           (setf (ps:@ *mini-canvas-ctx* fill-style) "#000000")
           (ps:chain *mini-canvas-ctx* (fill-rect 0 0 width height)))))
     
     ;; ========================================
     ;; Stream Configuration
     ;; ========================================
     
     ;; Get stream configuration for a given channel and quality
     ;; Curated channel uses /asteroid.* mounts, shuffle uses /shuffle.* mounts
     (defun get-stream-config (stream-base-url channel quality)
       (let ((prefix (if (= channel "shuffle") "/shuffle" "/asteroid")))
         (let ((config (ps:create
                        :aac (ps:create :url (+ stream-base-url prefix ".aac")
                                        :type "audio/aac"
                                        :format "AAC 96kbps Stereo"
                                        :mount (+ (ps:chain prefix (substring 1)) ".aac"))
                        :mp3 (ps:create :url (+ stream-base-url prefix ".mp3")
                                        :type "audio/mpeg"
                                        :format "MP3 128kbps Stereo"
                                        :mount (+ (ps:chain prefix (substring 1)) ".mp3"))
                        :low (ps:create :url (+ stream-base-url prefix ".mp3")
                                        :type "audio/mpeg"
                                        :format "MP3 128kbps Stereo"
                                        :mount (+ (ps:chain prefix (substring 1)) ".mp3")))))
           (ps:getprop config quality))))
     
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
    
    ;; Track last notified title to avoid duplicate notifications
    (defvar *last-notified-title* nil)
    
    ;; Countdown timer state
    (defvar *track-remaining-seconds* nil)
    (defvar *countdown-interval* nil)
    
    (defun format-countdown (seconds)
      (let ((m (ps:chain -math (floor (/ seconds 60))))
            (s (ps:chain -math (floor (mod seconds 60)))))
        (+ (if (< m 10) (+ "0" m) m) ":" (if (< s 10) (+ "0" s) s))))
    
    (defun start-countdown-ticker ()
      (when *countdown-interval*
        (clear-interval *countdown-interval*))
      (setf *countdown-interval*
            (set-interval
             (lambda ()
               (let ((el (ps:chain document (get-element-by-id "track-countdown"))))
                 (when el
                   (if (and *track-remaining-seconds* (> *track-remaining-seconds* 0))
                       (progn
                         (decf *track-remaining-seconds*)
                         (setf (ps:@ el text-content) (+ "[" (format-countdown *track-remaining-seconds*) "]")))
                       (setf (ps:@ el text-content) "")))))
             1000)))
    
    ;; Check if notifications are enabled in localStorage
    (defun notifications-enabled-p ()
      (= (ps:chain local-storage (get-item "notifications-enabled")) "true"))
    
    ;; Check if browser supports notifications
    (defun notifications-supported-p ()
      (not (= (typeof (ps:@ window -notification)) "undefined")))
    
    ;; Get notification permission status
    (defun get-notification-permission ()
      (if (notifications-supported-p)
          (ps:@ -notification permission)
          "denied"))
    
    ;; Request notification permission from user
    (defun request-notification-permission ()
      (when (notifications-supported-p)
        (ps:chain -notification (request-permission)
                  (then (lambda (permission)
                          (if (= permission "granted")
                              (progn
                                (ps:chain local-storage (set-item "notifications-enabled" "true"))
                                (update-notification-toggle-ui)
                                (show-track-notification "Notifications Enabled" "You'll now receive track change notifications"))
                              (progn
                                (ps:chain local-storage (set-item "notifications-enabled" "false"))
                                (update-notification-toggle-ui))))))))
    
    ;; Toggle notifications on/off
    (defun toggle-notifications ()
      (let ((permission (get-notification-permission)))
        (cond
          ;; Not supported
          ((not (notifications-supported-p))
           (alert "Your browser does not support notifications"))
          ;; Permission denied - can't do anything
          ((= permission "denied")
           (alert "Notifications are blocked. Please enable them in your browser settings."))
          ;; Permission not yet requested
          ((= permission "default")
           (request-notification-permission))
          ;; Permission granted - toggle the setting
          ((= permission "granted")
           (if (notifications-enabled-p)
               (progn
                 (ps:chain local-storage (set-item "notifications-enabled" "false"))
                 (update-notification-toggle-ui))
               (progn
                 (ps:chain local-storage (set-item "notifications-enabled" "true"))
                 (update-notification-toggle-ui)
                 (show-track-notification "Notifications Enabled" "You'll now receive track change notifications")))))))
    
    ;; Update the notification toggle button UI
    (defun update-notification-toggle-ui ()
      (let ((btn (ps:chain document (get-element-by-id "notification-toggle"))))
        (when btn
          (let ((permission (get-notification-permission))
                (enabled (notifications-enabled-p)))
            (cond
              ((not (notifications-supported-p))
               (setf (ps:@ btn text-content) "🔕")
               (setf (ps:@ btn title) "Notifications not supported"))
              ((= permission "denied")
               (setf (ps:@ btn text-content) "🔕")
               (setf (ps:@ btn title) "Notifications blocked - enable in browser settings"))
              ((and (= permission "granted") enabled)
               (setf (ps:@ btn text-content) "🔔")
               (setf (ps:@ btn title) "Track notifications ON - click to disable"))
              (t
               (setf (ps:@ btn text-content) "🔕")
               (setf (ps:@ btn title) "Track notifications OFF - click to enable")))))))
    
    ;; Show a system notification for track change
    (defun show-track-notification (title body)
      (ps:chain console (log "[NOTIFY] show-track-notification called:"
                             "supported=" (notifications-supported-p)
                             "permission=" (get-notification-permission)
                             "enabled=" (notifications-enabled-p)
                             "last=" *last-notified-title*
                             "title=" title))
      (when (and (notifications-supported-p)
                 (= (get-notification-permission) "granted")
                 (notifications-enabled-p)
                 (not (= title *last-notified-title*)))
        (setf *last-notified-title* title)
        (ps:try
         (let ((notification (ps:new (-notification title
                                       (ps:create :body body
                                                  :icon "/asteroid/static/asteroid.png"
                                                  :tag "asteroid-track-change"
                                                  :renotify true
                                                  :silent false)))))
           (ps:chain console (log "[NOTIFY] Notification created successfully"))
           ;; Auto-close after 5 seconds
           (set-timeout (lambda () (ps:chain notification (close))) 5000)
           ;; Click to focus the window
           (setf (ps:@ notification onclick)
                 (lambda ()
                   (ps:chain window (focus))
                   (ps:chain notification (close)))))
         (:catch (e)
           (ps:chain console (log "[NOTIFY] Notification error:" e))))))
    
    ;; Notify track change (called from update-mini-now-playing)
    (defun notify-track-change (title)
      (when (and title 
                 (not (= title ""))
                 (not (= title "Loading..."))
                 (not (= title *last-notified-title*)))
        ;; Show full "Artist - Track" in title, "Now Playing" as body
        (show-track-notification title "Now Playing on Asteroid Radio")))
    
    ;; Cache of user's favorite track titles for quick lookup (mini player)
    (defvar *user-favorites-cache-mini* (array))
     
     ;; Load user's favorites into cache (mini player - only if logged in)
     (defun load-favorites-cache-mini ()
       ;; Check global auth state - only call API if logged in
       (when (and (not (= (typeof *auth-state*) "undefined"))
                  (ps:@ *auth-state* logged-in))
         (ps:chain
          (fetch "/api/asteroid/user/favorites" (ps:create :credentials "include"))
          (then (lambda (response)
                  (if (ps:@ response ok)
                      (ps:chain response (json))
                      nil)))
        (then (lambda (data)
                (when data
                  ;; Handle both wrapped (data.data.favorites) and unwrapped (data.favorites) responses
                  (let ((favorites (or (and (ps:@ data data) (ps:@ data data favorites))
                                       (ps:@ data favorites))))
                    (when favorites
                      (setf *user-favorites-cache-mini* 
                            (ps:chain favorites (map (lambda (f) (ps:@ f title)))))
                      ;; Update UI after cache is loaded
                      (check-favorite-status-mini))))))
          (catch (lambda (error) nil)))))
     
     ;; Check if current track is in favorites and update mini player UI
     (defun check-favorite-status-mini ()
       (let ((title-el (ps:chain document (get-element-by-id "mini-now-playing")))
             (btn (ps:chain document (get-element-by-id "favorite-btn-mini"))))
         (when (and title-el btn)
           (let* ((track-title (ps:@ title-el text-content))
                  (star-icon (ps:chain btn (query-selector ".star-icon")))
                  (is-in-cache (ps:chain *user-favorites-cache-mini* (includes track-title))))
             (if is-in-cache
                 (progn
                   (ps:chain btn class-list (add "favorited"))
                   (when star-icon (setf (ps:@ star-icon text-content) "★")))
                 (progn
                   (ps:chain btn class-list (remove "favorited"))
                   (when star-icon (setf (ps:@ star-icon text-content) "☆"))))))))
     
     ;; Record track to listening history (only if logged in)
     (defun record-track-listen (title)
       ;; Check global auth state - only call API if logged in
       (when (and (not (= (typeof *auth-state*) "undefined"))
                  (ps:@ *auth-state* logged-in)
                  title (not (= title "")) (not (= title "Loading...")) (not (= title *last-recorded-title*)))
         (setf *last-recorded-title* title)
         (ps:chain
          (fetch (+ "/api/asteroid/user/history/record?title=" (encode-u-r-i-component title))
                 (ps:create :method "POST"))
          (then (lambda (response)
                  (ps:@ response ok)))
          (catch (lambda (error) nil)))))
     
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
                        ;; Check if track changed and record to history + notify
                        (when (not (= (ps:@ el text-content) title))
                          (ps:chain console (log "[STREAM-SYNC] Title changed:" title))
                          (record-track-listen title)
                          (notify-track-change title))
                        (setf (ps:@ el text-content) title)
                        (check-favorite-status-mini))
                      (update-media-session title)
                      (when track-id-el
                        (let ((track-id (or (ps:@ data data track_id) (ps:@ data track_id))))
                          (setf (ps:@ track-id-el value) (or track-id ""))))
                      (let ((count-el (ps:chain document (get-element-by-id "favorite-count-mini")))
                            (fav-count (or (ps:@ data data favorite_count) (ps:@ data favorite_count) 0)))
                        (when count-el
                          (cond
                            ((= fav-count 0) (setf (ps:@ count-el text-content) ""))
                            ((= fav-count 1) (setf (ps:@ count-el text-content) "1 ❤️"))
                            (t (setf (ps:@ count-el text-content) (+ fav-count " ❤️"))))))
                      ;; Sync countdown timer from server
                      (let ((remaining (or (ps:@ data data remaining) (ps:@ data remaining))))
                        (when remaining
                          (setf *track-remaining-seconds* remaining)))
                      (let ((mb-link (ps:chain document (get-element-by-id "mini-musicbrainz-link")))
                            (search-url (or (ps:@ data data search_url) (ps:@ data search_url))))
                        (when mb-link
                          (if search-url
                              (progn
                                (setf (ps:@ mb-link href) search-url)
                                (setf (ps:@ mb-link style display) "inline"))
                              (setf (ps:@ mb-link style display) "none"))))))))
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
                           (ps:create :method "POST" :credentials "include"))
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
                              (setf (ps:@ (ps:chain btn (query-selector ".star-icon")) text-content) "☆")
                              ;; Reload cache to update favorite count (don't call update-mini-now-playing
                              ;; as it would check the old cache before reload completes)
                              (load-favorites-cache-mini))))
                    (catch (lambda (error)
                             (ps:chain console (error "Error removing favorite:" error)))))
                   ;; Add favorite
                   (ps:chain
                    (fetch (+ "/api/asteroid/user/favorites/add?" params)
                           (ps:create :method "POST" :credentials "include"))
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
                              (setf (ps:@ (ps:chain btn (query-selector ".star-icon")) text-content) "★")
                              ;; Reload cache to update favorite count (don't call update-mini-now-playing
                              ;; as it would check the old cache before reload completes)
                              (load-favorites-cache-mini))))
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
                            (setf (ps:@ artist-el text-content) "Asteroid Radio")))))
                  (update-media-session track-text))))
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
     (defvar *stall-count* 0)
     (defvar *reconnect-timeout* nil)
     (defvar *is-reconnecting* false)
     (defvar *user-paused* false)
     
     ;; Reconnect stream - reuses existing audio element to preserve user gesture context
     (defun reconnect-stream ()
       (ps:chain console (log "Reconnecting stream..."))
       (show-status "🔄 Reconnecting..." false)
       
       (let* ((audio (ps:chain document (get-element-by-id "persistent-audio")))
              (source (ps:chain document (get-element-by-id "audio-source")))
              (stream-base-url (ps:@ (ps:chain document (get-element-by-id "stream-base-url")) value))
              (stream-channel (get-current-channel))
              (stream-quality (get-current-quality))
              (config (get-stream-config stream-base-url stream-channel stream-quality)))
         
         (unless audio
           (show-status "❌ Could not reconnect - reload page" true)
           (setf *is-reconnecting* false)
           (return-from reconnect-stream nil))
         
         (ps:chain console (log "Saving volume:" (ps:@ audio volume) "muted:" (ps:@ audio muted)))
         
         ;; Reset spectrum analyzer if it exists
         (when (ps:@ window reset-spectrum-analyzer)
           (ps:chain window (reset-spectrum-analyzer)))
         
         ;; Reload source on existing element (preserves user gesture context)
         (ps:chain audio (pause))
         (if source
             ;; Update existing source element
             (progn
               (setf (ps:@ source src) (+ (ps:@ config url) "?t=" (ps:chain (ps:new (*Date)) (get-time))))
               (setf (ps:@ source type) (ps:@ config type)))
             ;; Create source if missing
             (let ((new-source (ps:chain document (create-element "source"))))
               (setf (ps:@ new-source id) "audio-source")
               (setf (ps:@ new-source src) (+ (ps:@ config url) "?t=" (ps:chain (ps:new (*Date)) (get-time))))
               (setf (ps:@ new-source type) (ps:@ config type))
               (ps:chain audio (append-child new-source))))
         
         ;; Reload and play — keep *is-reconnecting* true until 'playing' fires
         (ps:chain audio (load))
         (set-timeout
          (lambda ()
            (ps:chain audio (play)
                      (catch (lambda (error)
                               (ps:chain console (log "Reconnect play failed:" error))
                               ;; play() rejected — reset so next stall/error can retry
                               (setf *is-reconnecting* false)))))
          500)))
     
     ;; Simple reconnect for popout player (just reload and play)
     (defun simple-reconnect (audio-element)
       (ps:chain audio-element (load))
       (ps:chain audio-element (play)
                 (catch (lambda (err)
                          (ps:chain console (log "Reconnect failed:" err))))))
     
     ;; Buffer bloat detection and reset
     (defvar *max-buffer-seconds* 15)
     (defvar *buffer-check-interval* nil)
     
     (defun get-buffer-ahead (audio-element)
       "Return seconds of audio buffered ahead of current playback position."
       (ps:try
        (when (and (ps:@ audio-element buffered)
                   (> (ps:@ audio-element buffered length) 0))
          (- (ps:chain audio-element buffered (end (- (ps:@ audio-element buffered length) 1)))
             (ps:@ audio-element current-time)))
        (:catch (e) 0)))
     
     (defun start-buffer-monitor (audio-element)
       (when *buffer-check-interval*
         (clear-interval *buffer-check-interval*))
       (setf *buffer-check-interval*
             (set-interval
              (lambda ()
                (when (and (not (ps:@ audio-element paused))
                           (not *is-reconnecting*))
                  (let ((ahead (get-buffer-ahead audio-element)))
                    (when (and ahead (> ahead 0))
                      (ps:chain console (log (+ "[BUFFER] " (ps:chain ahead (to-fixed 1)) "s ahead")))
                      (when (> ahead *max-buffer-seconds*)
                        (ps:chain console (log (+ "[BUFFER] Bloat detected (" (ps:chain ahead (to-fixed 1)) "s), resetting stream")))
                        (reconnect-stream))))))
              10000)))

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
                                       ;; Initialize and start mini spectrum on first play
                                       (init-mini-spectrum audio-element)
                                       (start-mini-spectrum)
                                       (hide-status)
                                       (setf *stream-error-count* 0)
                                       (setf *stall-count* 0)
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
                                        (setf *stall-count* (+ *stall-count* 1))
                                        ;; Exponential backoff: 5s, 10s, 20s, max 60s
                                        (let ((delay (ps:chain -math (min (* 5000 (ps:chain -math (pow 2 (- *stall-count* 1)))) 60000))))
                                          (if (> *stall-count* 10)
                                              ;; Give up after 10 stall attempts — show manual retry
                                              (progn
                                                (ps:chain console (log "Too many stall retries, giving up auto-reconnect"))
                                                (show-status "⚠️ Stream unavailable - click play to retry" true))
                                              (progn
                                                (ps:chain console (log (+ "Audio stalled (attempt " *stall-count* "), reconnecting in " (/ delay 1000) "s...")))
                                                (show-status (+ "⚠️ Stream stalled - reconnecting (" *stall-count* ")...") true)
                                                (setf *is-reconnecting* true)
                                                (setf *reconnect-timeout*
                                                      (set-timeout
                                                       (lambda ()
                                                         (reconnect-stream))
                                                       delay)))))))))
       
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
                                       ;; Stop mini spectrum when paused
                                       (stop-mini-spectrum)
                                       ;; Only treat as throttle if: muted, not reconnecting, and not user-initiated
                                       (when (and (ps:@ audio-element muted)
                                                  (not *is-reconnecting*)
                                                  (not *user-paused*))
                                         (ps:chain console (log "Stream paused while muted (possible browser throttling), will reconnect in 5 seconds..."))
                                         (show-status "⚠️ Stream paused - reconnecting..." true)
                                         (setf *is-reconnecting* true)
                                         (set-timeout (lambda () (reconnect-stream)) 5000))))))
     
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
           ;; Load user's favorites for highlight feature
           (load-favorites-cache-mini)
           
           ;; Try to enable low-latency mode if supported
           (when (ps:@ navigator media-session)
             (setf (ps:@ navigator media-session metadata)
                   (ps:new (-media-metadata
                           (ps:create :title "Asteroid Radio Live Stream"
                                      :artist "Asteroid Radio"
                                      :album "Live Broadcast")))))
           
           ;; Attach event listeners and buffer monitor
           (attach-audio-listeners audio-element)
           (start-buffer-monitor audio-element)
           
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
           
           ;; Initialize notification toggle UI
           (update-notification-toggle-ui)
           
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
               15000))  ;; Poll every 15 seconds
           
           ;; Start now playing updates and countdown ticker
           (set-timeout update-mini-now-playing 1000)
           (set-interval update-mini-now-playing 15000)
           (start-countdown-ticker))))
     
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
           (set-interval update-popout-now-playing 15000)
           
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
    (setf (ps:@ window toggle-notifications) toggle-notifications)
    (setf (ps:@ window update-notification-toggle-ui) update-notification-toggle-ui)
    (setf (ps:@ window notify-popout-opened) notify-popout-opened)
    (setf (ps:@ window notify-popout-closing) notify-popout-closing)
     
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
                    (init-popout-player)))))
     
     ;; Listen for messages from parent frame (e.g., favorites cache reload)
     (ps:chain window (add-event-listener
                       "message"
                       (lambda (event)
                         (when (= (ps:@ event data) "reload-favorites")
                           (load-favorites-cache-mini))))))
   )
  "Compiled JavaScript for stream player - generated at load time")

(defun generate-stream-player-js ()
  "Generate JavaScript code for the stream player"
  (ps-join
    *common-player-js*
    *stream-player-js*))
