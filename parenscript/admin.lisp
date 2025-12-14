;;;; admin.lisp - ParenScript version of admin.js
;;;; Admin Dashboard functionality including track management, queue controls, and player

(in-package #:asteroid)

(defparameter *admin-js*
  (ps:ps*
   '(progn
    
    ;; Global variables
    (defvar *tracks* (array))
    (defvar *current-track-id* nil)
    (defvar *current-page* 1)
    (defvar *tracks-per-page* 20)
    (defvar *filtered-tracks* (array))
    (defvar *stream-queue* (array))
    (defvar *queue-search-timeout* nil)
    (defvar *audio-player* nil)
    
    ;; Initialize admin dashboard on page load
    (ps:chain document
              (add-event-listener
               "DOMContentLoaded"
               (lambda ()
                 (load-tracks)
                 (setup-event-listeners)
                 (load-playlist-list)
                 (load-current-queue)
                 (refresh-liquidsoap-status)
                 (setup-stats-refresh)
                 ;; Update Liquidsoap status every 10 seconds
                 (set-interval refresh-liquidsoap-status 10000))))
    
    ;; Setup all event listeners
    (defun setup-event-listeners ()
      ;; Main controls
      (let ((scan-btn (ps:chain document (get-element-by-id "scan-library")))
            (refresh-btn (ps:chain document (get-element-by-id "refresh-tracks")))
            (search-input (ps:chain document (get-element-by-id "track-search")))
            (sort-select (ps:chain document (get-element-by-id "sort-tracks")))
            (copy-btn (ps:chain document (get-element-by-id "copy-files")))
            (open-btn (ps:chain document (get-element-by-id "open-incoming"))))
        
        (when scan-btn
          (ps:chain scan-btn (add-event-listener "click" scan-library)))
        (when refresh-btn
          (ps:chain refresh-btn (add-event-listener "click" load-tracks)))
        (when search-input
          (ps:chain search-input (add-event-listener "input" filter-tracks)))
        (when sort-select
          (ps:chain sort-select (add-event-listener "change" sort-tracks)))
        (when copy-btn
          (ps:chain copy-btn (add-event-listener "click" copy-files)))
        (when open-btn
          (ps:chain open-btn (add-event-listener "click" open-incoming-folder))))
      
      ;; Player controls
      (let ((play-btn (ps:chain document (get-element-by-id "player-play")))
            (pause-btn (ps:chain document (get-element-by-id "player-pause")))
            (stop-btn (ps:chain document (get-element-by-id "player-stop")))
            (resume-btn (ps:chain document (get-element-by-id "player-resume"))))
        
        (when play-btn
          (ps:chain play-btn (add-event-listener "click" 
                                                 (lambda () (play-track *current-track-id*)))))
        (when pause-btn
          (ps:chain pause-btn (add-event-listener "click" pause-player)))
        (when stop-btn
          (ps:chain stop-btn (add-event-listener "click" stop-player)))
        (when resume-btn
          (ps:chain resume-btn (add-event-listener "click" resume-player))))
      
      ;; Queue controls
      (let ((refresh-queue-btn (ps:chain document (get-element-by-id "refresh-queue")))
            (clear-queue-btn (ps:chain document (get-element-by-id "clear-queue-btn")))
            (save-queue-btn (ps:chain document (get-element-by-id "save-queue-btn")))
            (save-as-btn (ps:chain document (get-element-by-id "save-as-btn")))
            (add-random-btn (ps:chain document (get-element-by-id "add-random-tracks")))
            (queue-search-input (ps:chain document (get-element-by-id "queue-track-search")))
            ;; Playlist controls
            (playlist-select (ps:chain document (get-element-by-id "playlist-select")))
            (load-playlist-btn (ps:chain document (get-element-by-id "load-playlist-btn")))
            (refresh-playlists-btn (ps:chain document (get-element-by-id "refresh-playlists-btn"))))
        
        (when refresh-queue-btn
          (ps:chain refresh-queue-btn (add-event-listener "click" load-current-queue)))
        (when clear-queue-btn
          (ps:chain clear-queue-btn (add-event-listener "click" clear-stream-queue)))
        (when save-queue-btn
          (ps:chain save-queue-btn (add-event-listener "click" save-stream-queue)))
        (when save-as-btn
          (ps:chain save-as-btn (add-event-listener "click" save-queue-as-new)))
        (when add-random-btn
          (ps:chain add-random-btn (add-event-listener "click" add-random-tracks)))
        (when queue-search-input
          (ps:chain queue-search-input (add-event-listener "input" search-tracks-for-queue)))
        ;; Playlist controls
        (when load-playlist-btn
          (ps:chain load-playlist-btn (add-event-listener "click" load-selected-playlist)))
        (when refresh-playlists-btn
          (ps:chain refresh-playlists-btn (add-event-listener "click" load-playlist-list))))
      
      ;; Liquidsoap controls
      (let ((ls-refresh-btn (ps:chain document (get-element-by-id "ls-refresh-status")))
            (ls-skip-btn (ps:chain document (get-element-by-id "ls-skip")))
            (ls-reload-btn (ps:chain document (get-element-by-id "ls-reload")))
            (ls-restart-btn (ps:chain document (get-element-by-id "ls-restart"))))
        (when ls-refresh-btn
          (ps:chain ls-refresh-btn (add-event-listener "click" refresh-liquidsoap-status)))
        (when ls-skip-btn
          (ps:chain ls-skip-btn (add-event-listener "click" liquidsoap-skip)))
        (when ls-reload-btn
          (ps:chain ls-reload-btn (add-event-listener "click" liquidsoap-reload)))
        (when ls-restart-btn
          (ps:chain ls-restart-btn (add-event-listener "click" liquidsoap-restart))))
      
      ;; Icecast restart
      (let ((icecast-restart-btn (ps:chain document (get-element-by-id "icecast-restart"))))
        (when icecast-restart-btn
          (ps:chain icecast-restart-btn (add-event-listener "click" icecast-restart)))))
    
    ;; Load tracks from API
    (defun load-tracks ()
      (ps:chain
       (fetch "/api/asteroid/admin/tracks")
       (then (lambda (response) (ps:chain response (json))))
       (then (lambda (result)
               ;; Handle Radiance API response format
               (let ((data (or (ps:@ result data) result)))
                 (when (= (ps:@ data status) "success")
                   (setf *tracks* (or (ps:@ data tracks) (array)))
                   (let ((count-el (ps:chain document (get-element-by-id "track-count"))))
                     (when count-el
                       (setf (ps:@ count-el text-content) (ps:@ *tracks* length))))
                   (display-tracks *tracks*)))))
       (catch (lambda (error)
                (ps:chain console (error "Error loading tracks:" error))
                (let ((container (ps:chain document (get-element-by-id "tracks-container"))))
                  (when container
                    (setf (ps:@ container inner-h-t-m-l) 
                          "<div class=\"error\">Error loading tracks</div>")))))))
    
    ;; Display tracks with pagination
    (defun display-tracks (track-list)
      (setf *filtered-tracks* track-list)
      (setf *current-page* 1)
      (render-page))
    
    ;; Render current page of tracks
    (defun render-page ()
      (let ((container (ps:chain document (get-element-by-id "tracks-container")))
            (pagination-controls (ps:chain document (get-element-by-id "pagination-controls"))))
        
        (when (= (ps:@ *filtered-tracks* length) 0)
          (when container
            (setf (ps:@ container inner-h-t-m-l) 
                  "<div class=\"no-tracks\">No tracks found. Click \"Scan Library\" to add tracks.</div>"))
          (when pagination-controls
            (setf (ps:@ pagination-controls style display) "none"))
          (return))
        
        ;; Calculate pagination
        (let* ((total-pages (ps:chain -math (ceil (/ (ps:@ *filtered-tracks* length) *tracks-per-page*))))
               (start-index (* (- *current-page* 1) *tracks-per-page*))
               (end-index (+ start-index *tracks-per-page*))
               (tracks-to-show (ps:chain *filtered-tracks* (slice start-index end-index))))
          
          ;; Render tracks for current page
          (let ((tracks-html 
                 (ps:chain tracks-to-show
                           (map (lambda (track)
                                  (+ "<div class=\"track-item\" data-track-id=\"" (ps:@ track id) "\">"
                                     "<div class=\"track-info\">"
                                     "<div class=\"track-title\">" (or (ps:@ track title) "Unknown Title") "</div>"
                                     "<div class=\"track-artist\">" (or (ps:@ track artist) "Unknown Artist") "</div>"
                                     "<div class=\"track-album\">" (or (ps:@ track album) "Unknown Album") "</div>"
                                     "</div>"
                                     "<div class=\"track-actions\">"
                                     "<button onclick=\"addToQueue(" (ps:@ track id) ", 'end')\" class=\"btn btn-sm btn-primary\">➕ Add to Queue</button>"
                                     "<button onclick=\"deleteTrack(" (ps:@ track id) ")\" class=\"btn btn-sm btn-danger\">🗑️ Delete</button>"
                                     "</div>"
                                     "</div>")))
                           (join ""))))
            
            (when container
              (setf (ps:@ container inner-h-t-m-l) tracks-html)))
          
          ;; Update pagination controls
          (let ((page-info (ps:chain document (get-element-by-id "page-info"))))
            (when page-info
              (setf (ps:@ page-info text-content) 
                    (+ "Page " *current-page* " of " total-pages " (" (ps:@ *filtered-tracks* length) " tracks)"))))
          
          (when pagination-controls
            (setf (ps:@ pagination-controls style display) 
                  (if (> total-pages 1) "block" "none"))))))
    
    ;; Pagination functions
    (defun go-to-page (page)
      (let ((total-pages (ps:chain -math (ceil (/ (ps:@ *filtered-tracks* length) *tracks-per-page*)))))
        (when (and (>= page 1) (<= page total-pages))
          (setf *current-page* page)
          (render-page))))
    
    (defun previous-page ()
      (when (> *current-page* 1)
        (setf *current-page* (- *current-page* 1))
        (render-page)))
    
    (defun next-page ()
      (let ((total-pages (ps:chain -math (ceil (/ (ps:@ *filtered-tracks* length) *tracks-per-page*)))))
        (when (< *current-page* total-pages)
          (setf *current-page* (+ *current-page* 1))
          (render-page))))
    
    (defun go-to-last-page ()
      (let ((total-pages (ps:chain -math (ceil (/ (ps:@ *filtered-tracks* length) *tracks-per-page*)))))
        (setf *current-page* total-pages)
        (render-page)))
    
    (defun change-tracks-per-page ()
      (let ((select-el (ps:chain document (get-element-by-id "tracks-per-page"))))
        (when select-el
          (setf *tracks-per-page* (parse-int (ps:@ select-el value)))
          (setf *current-page* 1)
          (render-page))))
    
    ;; Scan music library
    (defun scan-library ()
      (let ((status-el (ps:chain document (get-element-by-id "scan-status")))
            (scan-btn (ps:chain document (get-element-by-id "scan-library"))))
        
        (when status-el
          (setf (ps:@ status-el text-content) "Scanning..."))
        (when scan-btn
          (setf (ps:@ scan-btn disabled) t))
        
        (ps:chain
         (fetch "/api/asteroid/admin/scan-library" (ps:create :method "POST"))
         (then (lambda (response) (ps:chain response (json))))
         (then (lambda (result)
                 (let ((data (or (ps:@ result data) result)))
                   (if (= (ps:@ data status) "success")
                       (progn
                         (when status-el
                           (setf (ps:@ status-el text-content) 
                                 (+ "✅ Added " (ps:getprop data "tracks-added") " tracks")))
                         (load-tracks))
                       (when status-el
                         (setf (ps:@ status-el text-content) "❌ Scan failed"))))))
         (catch (lambda (error)
                  (when status-el
                    (setf (ps:@ status-el text-content) "❌ Scan error"))
                  (ps:chain console (error "Error scanning library:" error))))
         (finally (lambda ()
                    (when scan-btn
                      (setf (ps:@ scan-btn disabled) nil))
                    (set-timeout (lambda ()
                                   (when status-el
                                     (setf (ps:@ status-el text-content) "")))
                                 3000))))))
    
    ;; Filter tracks based on search
    (defun filter-tracks ()
      (let* ((search-input (ps:chain document (get-element-by-id "track-search")))
             (query (when search-input (ps:chain (ps:@ search-input value) (to-lower-case))))
             (filtered (ps:chain *tracks*
                                 (filter (lambda (track)
                                           (or (ps:chain (or (ps:@ track title) "") (to-lower-case) (includes query))
                                               (ps:chain (or (ps:@ track artist) "") (to-lower-case) (includes query))
                                               (ps:chain (or (ps:@ track album) "") (to-lower-case) (includes query))))))))
        (display-tracks filtered)))
    
    ;; Sort tracks
    (defun sort-tracks ()
      (let* ((sort-select (ps:chain document (get-element-by-id "sort-tracks")))
             (sort-by (when sort-select (ps:@ sort-select value)))
             (sorted (ps:chain *tracks*
                               (slice)
                               (sort (lambda (a b)
                                       (let ((a-val (or (ps:getprop a sort-by) ""))
                                             (b-val (or (ps:getprop b sort-by) "")))
                                         (ps:chain a-val (locale-compare b-val))))))))
        (display-tracks sorted)))
    
    ;; Initialize audio player
    (defun init-audio-player ()
      (unless *audio-player*
        (setf *audio-player* (new (-audio)))
        (ps:chain *audio-player*
                  (add-event-listener "ended" (lambda ()
                                                (setf *current-track-id* nil)
                                                (update-player-status))))
        (ps:chain *audio-player*
                  (add-event-listener "error" (lambda (e)
                                                (ps:chain console (error "Audio playback error:" e))
                                                (alert "Error playing audio file")))))
      *audio-player*)
    
    ;; Player functions
    (defun play-track (track-id)
      (unless track-id
        (alert "Please select a track to play")
        (return))
      
      (ps:chain
       (-promise (lambda (resolve reject)
                   (let ((player (init-audio-player)))
                     (setf (ps:@ player src) (+ "/asteroid/tracks/" track-id "/stream"))
                     (ps:chain player (play))
                     (setf *current-track-id* track-id)
                     (update-player-status)
                     (resolve))))
       (catch (lambda (error)
                (ps:chain console (error "Play error:" error))
                (alert "Error playing track")))))
    
    (defun pause-player ()
      (ps:chain
       (-promise (lambda (resolve reject)
                   (when (and *audio-player* (not (ps:@ *audio-player* paused)))
                     (ps:chain *audio-player* (pause))
                     (update-player-status))
                   (resolve)))
       (catch (lambda (error)
                (ps:chain console (error "Pause error:" error))))))
    
    (defun stop-player ()
      (ps:chain
       (-promise (lambda (resolve reject)
                   (when *audio-player*
                     (ps:chain *audio-player* (pause))
                     (setf (ps:@ *audio-player* current-time) 0)
                     (setf *current-track-id* nil)
                     (update-player-status))
                   (resolve)))
       (catch (lambda (error)
                (ps:chain console (error "Stop error:" error))))))
    
    (defun resume-player ()
      (ps:chain
       (-promise (lambda (resolve reject)
                   (when (and *audio-player* (ps:@ *audio-player* paused) *current-track-id*)
                     (ps:chain *audio-player* (play))
                     (update-player-status))
                   (resolve)))
       (catch (lambda (error)
                (ps:chain console (error "Resume error:" error))))))
    
    (defun update-player-status ()
      (ps:chain
       (fetch "/api/asteroid/player/status")
       (then (lambda (response) (ps:chain response (json))))
       (then (lambda (data)
               (when (= (ps:@ data status) "success")
                 (let ((player (ps:@ data player))
                       (state-el (ps:chain document (get-element-by-id "player-state")))
                       (track-el (ps:chain document (get-element-by-id "current-track"))))
                   (when state-el
                     (setf (ps:@ state-el text-content) (ps:@ player state)))
                   (when track-el
                     (setf (ps:@ track-el text-content) (or (ps:getprop player "current-track") "None")))))))
       (catch (lambda (error)
                (ps:chain console (error "Error updating player status:" error))))))
    
    ;; Utility functions
    (defun stream-track (track-id)
      (ps:chain window (open (+ "/asteroid/tracks/" track-id "/stream") "_blank")))
    
    (defun delete-track (track-id)
      (when (confirm "Are you sure you want to delete this track?")
        (alert "Track deletion not yet implemented")))
    
    (defun copy-files ()
      (ps:chain
       (fetch "/admin/copy-files")
       (then (lambda (response) (ps:chain response (json))))
       (then (lambda (data)
               (if (= (ps:@ data status) "success")
                   (progn
                     (alert (ps:@ data message))
                     (load-tracks))
                   (alert (+ "Error: " (ps:@ data message))))))
       (catch (lambda (error)
                (ps:chain console (error "Error copying files:" error))
                (alert "Failed to copy files")))))
    
    (defun open-incoming-folder ()
      (alert "Copy your MP3 files to: /home/glenn/Projects/Code/asteroid/music/incoming/\n\nThen click \"Copy Files to Library\" to add them to your music collection."))
    
    ;; ========================================
    ;; Stream Queue Management
    ;; ========================================
    
    ;; Load current stream queue
    (defun load-stream-queue ()
      (ps:chain
       (fetch "/api/asteroid/stream/queue")
       (then (lambda (response) (ps:chain response (json))))
       (then (lambda (result)
               (let ((data (or (ps:@ result data) result)))
                 (when (= (ps:@ data status) "success")
                   (setf *stream-queue* (or (ps:@ data queue) (array)))
                   (display-stream-queue)))))
       (catch (lambda (error)
                (ps:chain console (error "Error loading stream queue:" error))
                (let ((container (ps:chain document (get-element-by-id "stream-queue-container"))))
                  (when container
                    (setf (ps:@ container inner-h-t-m-l) 
                          "<div class=\"error\">Error loading queue</div>")))))))
    
    ;; Display stream queue
    (defun display-stream-queue ()
      (let ((container (ps:chain document (get-element-by-id "stream-queue-container"))))
        (when container
          (if (= (ps:@ *stream-queue* length) 0)
              (setf (ps:@ container inner-h-t-m-l) 
                    "<div class=\"empty-state\">Queue is empty. Add tracks below.</div>")
              (let ((html "<div class=\"queue-items\">"))
                (ps:chain *stream-queue*
                          (for-each (lambda (item index)
                                      (when item
                                        (let ((is-first (= index 0))
                                              (is-last (= index (- (ps:@ *stream-queue* length) 1))))
                                          (setf html 
                                                (+ html
                                                   "<div class=\"queue-item\" data-track-id=\"" (ps:@ item id) "\" data-index=\"" index "\">"
                                                   "<span class=\"queue-position\">" (+ index 1) "</span>"
                                                   "<div class=\"queue-track-info\">"
                                                   "<div class=\"track-title\">" (or (ps:@ item title) "Unknown") "</div>"
                                                   "<div class=\"track-artist\">" (or (ps:@ item artist) "Unknown Artist") "</div>"
                                                   "</div>"
                                                   "<div class=\"queue-actions\">"
                                                   "<button class=\"btn btn-sm btn-secondary\" onclick=\"moveTrackUp(" index ")\" " (if is-first "disabled" "") ">⬆️</button>"
                                                   "<button class=\"btn btn-sm btn-secondary\" onclick=\"moveTrackDown(" index ")\" " (if is-last "disabled" "") ">⬇️</button>"
                                                   "<button class=\"btn btn-sm btn-danger\" onclick=\"removeFromQueue(" (ps:@ item id) ")\">Remove</button>"
                                                   "</div>"
                                                   "</div>")))))))
                (setf html (+ html "</div>"))
                (setf (ps:@ container inner-h-t-m-l) html))))))
    
    ;; Move track up in queue
    (defun move-track-up (index)
      (when (= index 0) (return))
      
      ;; Swap with previous track
      (let ((new-queue (ps:chain *stream-queue* (slice))))
        (let ((temp (ps:getprop new-queue (- index 1))))
          (setf (ps:getprop new-queue (- index 1)) (ps:getprop new-queue index))
          (setf (ps:getprop new-queue index) temp))
        (reorder-queue new-queue)))
    
    ;; Move track down in queue
    (defun move-track-down (index)
      (when (= index (- (ps:@ *stream-queue* length) 1)) (return))
      
      ;; Swap with next track
      (let ((new-queue (ps:chain *stream-queue* (slice))))
        (let ((temp (ps:getprop new-queue index)))
          (setf (ps:getprop new-queue index) (ps:getprop new-queue (+ index 1)))
          (setf (ps:getprop new-queue (+ index 1)) temp))
        (reorder-queue new-queue)))
    
    ;; Reorder the queue
    (defun reorder-queue (new-queue)
      (let ((track-ids (ps:chain new-queue
                                 (map (lambda (track) (ps:@ track id)))
                                 (join ","))))
        (ps:chain
         (fetch (+ "/api/asteroid/stream/queue/reorder?track-ids=" track-ids)
                (ps:create :method "POST"))
         (then (lambda (response) (ps:chain response (json))))
         (then (lambda (result)
                 (let ((data (or (ps:@ result data) result)))
                   (if (= (ps:@ data status) "success")
                       (load-stream-queue)
                       (alert (+ "Error reordering queue: " (or (ps:@ data message) "Unknown error")))))))
         (catch (lambda (error)
                  (ps:chain console (error "Error reordering queue:" error))
                  (alert "Error reordering queue"))))))
    
    ;; Remove track from queue
    (defun remove-from-queue (track-id)
      (ps:chain
       (fetch "/api/asteroid/stream/queue/remove"
              (ps:create :method "POST"
                         :headers (ps:create "Content-Type" "application/x-www-form-urlencoded")
                         :body (+ "track-id=" track-id)))
       (then (lambda (response) (ps:chain response (json))))
       (then (lambda (result)
               (let ((data (or (ps:@ result data) result)))
                 (if (= (ps:@ data status) "success")
                     (load-stream-queue)
                     (alert (+ "Error removing track: " (or (ps:@ data message) "Unknown error")))))))
       (catch (lambda (error)
                (ps:chain console (error "Error removing track:" error))
                (alert "Error removing track")))))
    
    ;; Add track to queue
    (defun add-to-queue (track-id &optional (position "end") (show-notification t))
      (ps:chain
       (fetch "/api/asteroid/stream/queue/add"
              (ps:create :method "POST"
                         :headers (ps:create "Content-Type" "application/x-www-form-urlencoded")
                         :body (+ "track-id=" track-id "&position=" position)))
       (then (lambda (response) (ps:chain response (json))))
       (then (lambda (result)
               (let ((data (or (ps:@ result data) result)))
                 (if (= (ps:@ data status) "success")
                     (progn
                       ;; Only reload queue if we're in the queue management section
                       (let ((queue-container (ps:chain document (get-element-by-id "stream-queue-container"))))
                         (when (and queue-container (not (= (ps:@ queue-container offset-parent) nil)))
                           (load-stream-queue)))
                       
                       ;; Show brief success notification
                       (when show-notification
                         (show-toast "✓ Added to queue"))
                       t)
                     (progn
                       (alert (+ "Error adding track: " (or (ps:@ data message) "Unknown error")))
                       nil)))))
       (catch (lambda (error)
                (ps:chain console (error "Error adding track:" error))
                (alert "Error adding track")
                nil))))
    
    ;; Simple toast notification
    (defun show-toast (message)
      (let ((toast (ps:chain document (create-element "div"))))
        (setf (ps:@ toast text-content) message)
        (setf (ps:@ toast style css-text)
              "position: fixed; bottom: 20px; right: 20px; background: #00ff00; color: #000; padding: 12px 20px; border-radius: 4px; font-weight: bold; z-index: 10000; animation: slideIn 0.3s ease-out;")
        (ps:chain document body (append-child toast))
        
        (set-timeout (lambda ()
                       (setf (ps:@ toast style opacity) "0")
                       (setf (ps:@ toast style transition) "opacity 0.3s")
                       (set-timeout (lambda () (ps:chain toast (remove))) 300))
                     2000)))
    
    ;; Add random tracks to queue
    (defun add-random-tracks ()
      (when (= (ps:@ *tracks* length) 0)
        (alert "No tracks available. Please scan the library first.")
        (return))
      
      (let* ((count 10)
             (shuffled (ps:chain *tracks* (slice) (sort (lambda () (- (ps:chain -math (random)) 0.5)))))
             (selected (ps:chain shuffled (slice 0 (ps:chain -math (min count (ps:@ *tracks* length)))))))
        
        (ps:chain selected
                  (for-each (lambda (track)
                              (add-to-queue (ps:@ track id) "end" nil))))
        
        (show-toast (+ "✓ Added " (ps:@ selected length) " random tracks to queue"))))
    
    ;; Search tracks for adding to queue
    (defun search-tracks-for-queue (event)
      (clear-timeout *queue-search-timeout*)
      (let ((query (ps:chain (ps:@ event target value) (to-lower-case))))
        
        (when (< (ps:@ query length) 2)
          (let ((results-container (ps:chain document (get-element-by-id "queue-track-results"))))
            (when results-container
              (setf (ps:@ results-container inner-h-t-m-l) "")))
          (return))
        
        (setf *queue-search-timeout*
              (set-timeout (lambda ()
                             (let ((results (ps:chain *tracks*
                                                       (filter (lambda (track)
                                                                 (or (and (ps:@ track title) 
                                                                          (ps:chain (ps:@ track title) (to-lower-case) (includes query)))
                                                                     (and (ps:@ track artist) 
                                                                          (ps:chain (ps:@ track artist) (to-lower-case) (includes query)))
                                                                     (and (ps:@ track album) 
                                                                          (ps:chain (ps:@ track album) (to-lower-case) (includes query))))))
                                                       (slice 0 20))))
                               (display-queue-search-results results)))
                           300))))
    
    ;; Display search results for queue
    (defun display-queue-search-results (results)
      (let ((container (ps:chain document (get-element-by-id "queue-track-results"))))
        (when container
          (if (= (ps:@ results length) 0)
              (setf (ps:@ container inner-h-t-m-l) 
                    "<div class=\"empty-state\">No tracks found</div>")
              (let ((html "<div class=\"search-results\">"))
                (ps:chain results
                          (for-each (lambda (track)
                                      (setf html
                                            (+ html
                                               "<div class=\"search-result-item\">"
                                               "<div class=\"track-info\">"
                                               "<div class=\"track-title\">" (or (ps:@ track title) "Unknown") "</div>"
                                               "<div class=\"track-artist\">" (or (ps:@ track artist) "Unknown") " - " (or (ps:@ track album) "Unknown Album") "</div>"
                                               "</div>"
                                               "<div class=\"track-actions\">"
                                               "<button class=\"btn btn-sm btn-primary\" onclick=\"addToQueue(" (ps:@ track id) ", 'end')\">Add to End</button>"
                                               "<button class=\"btn btn-sm btn-success\" onclick=\"addToQueue(" (ps:@ track id) ", 'next')\">Play Next</button>"
                                               "</div>"
                                               "</div>")))))
                (setf html (+ html "</div>"))
                (setf (ps:@ container inner-h-t-m-l) html))))))
    
    ;; ========================================
    ;; Playlist File Management
    ;; ========================================
    
    ;; Load list of available playlists into dropdown
    (defun load-playlist-list ()
      (ps:chain
       (fetch "/api/asteroid/stream/playlists")
       (then (lambda (response) (ps:chain response (json))))
       (then (lambda (result)
               (let ((data (or (ps:@ result data) result)))
                 (when (= (ps:@ data status) "success")
                   (let ((select (ps:chain document (get-element-by-id "playlist-select")))
                         (playlists (or (ps:@ data playlists) (array))))
                     (when select
                       ;; Clear existing options except the first one
                       (setf (ps:@ select inner-h-t-m-l) 
                             "<option value=\"\">-- Select a playlist --</option>")
                       ;; Add playlist options
                       (ps:chain playlists
                                 (for-each (lambda (name)
                                             (let ((option (ps:chain document (create-element "option"))))
                                               (setf (ps:@ option value) name)
                                               (setf (ps:@ option text-content) name)
                                               (ps:chain select (append-child option))))))))))))
       (catch (lambda (error)
                (ps:chain console (error "Error loading playlists:" error))))))
    
    ;; Load selected playlist
    (defun load-selected-playlist ()
      (let* ((select (ps:chain document (get-element-by-id "playlist-select")))
             (name (ps:@ select value)))
        (when (= name "")
          (alert "Please select a playlist first")
          (return))
        
        (unless (confirm (+ "Load playlist '" name "'? This will replace the current stream queue."))
          (return))
        
        (ps:chain
         (fetch (+ "/api/asteroid/stream/playlists/load?name=" (encode-u-r-i-component name))
                (ps:create :method "POST"))
         (then (lambda (response) (ps:chain response (json))))
         (then (lambda (result)
                 (let ((data (or (ps:@ result data) result)))
                   (if (= (ps:@ data status) "success")
                       (progn
                         (show-toast (+ "✓ Loaded " (ps:@ data count) " tracks from " name))
                         (load-current-queue))
                       (alert (+ "Error loading playlist: " (or (ps:@ data message) "Unknown error")))))))
         (catch (lambda (error)
                  (ps:chain console (error "Error loading playlist:" error))
                  (alert "Error loading playlist"))))))
    
    ;; Load current queue contents (from stream-queue.m3u)
    (defun load-current-queue ()
      (ps:chain
       (fetch "/api/asteroid/stream/playlists/current")
       (then (lambda (response) (ps:chain response (json))))
       (then (lambda (result)
               (let ((data (or (ps:@ result data) result)))
                 (when (= (ps:@ data status) "success")
                   (let ((tracks (or (ps:@ data tracks) (array)))
                         (count (or (ps:@ data count) 0)))
                     ;; Update count display
                     (let ((count-el (ps:chain document (get-element-by-id "queue-count"))))
                       (when count-el
                         (setf (ps:@ count-el text-content) count)))
                     ;; Display tracks
                     (display-current-queue tracks))))))
       (catch (lambda (error)
                (ps:chain console (error "Error loading current queue:" error))
                (let ((container (ps:chain document (get-element-by-id "stream-queue-container"))))
                  (when container
                    (setf (ps:@ container inner-h-t-m-l) 
                          "<div class=\"error\">Error loading queue</div>")))))))
    
    ;; Display current queue contents
    (defun display-current-queue (tracks)
      (let ((container (ps:chain document (get-element-by-id "stream-queue-container"))))
        (when container
          (if (= (ps:@ tracks length) 0)
              (setf (ps:@ container inner-h-t-m-l) 
                    "<div class=\"empty-state\">Queue is empty. Liquidsoap will use random playback from the music library.</div>")
              (let ((html "<div class=\"queue-items\">"))
                (ps:chain tracks
                          (for-each (lambda (track index)
                                      (setf html 
                                            (+ html
                                               "<div class=\"queue-item\" data-index=\"" index "\">"
                                               "<span class=\"queue-position\">" (+ index 1) "</span>"
                                               "<div class=\"queue-track-info\">"
                                               "<div class=\"track-title\">" (or (ps:@ track title) "Unknown") "</div>"
                                               "<div class=\"track-artist\">" (or (ps:@ track artist) "Unknown Artist") 
                                               (if (ps:@ track album) (+ " - " (ps:@ track album)) "") "</div>"
                                               "</div>"
                                               "</div>")))))
                (setf html (+ html "</div>"))
                (setf (ps:@ container inner-h-t-m-l) html))))))
    
    ;; Save current queue to stream-queue.m3u
    (defun save-stream-queue ()
      (ps:chain
       (fetch "/api/asteroid/stream/playlists/save" (ps:create :method "POST"))
       (then (lambda (response) (ps:chain response (json))))
       (then (lambda (result)
               (let ((data (or (ps:@ result data) result)))
                 (if (= (ps:@ data status) "success")
                     (show-toast "✓ Queue saved")
                     (alert (+ "Error saving queue: " (or (ps:@ data message) "Unknown error")))))))
       (catch (lambda (error)
                (ps:chain console (error "Error saving queue:" error))
                (alert "Error saving queue")))))
    
    ;; Save queue as new playlist
    (defun save-queue-as-new ()
      (let* ((input (ps:chain document (get-element-by-id "save-as-name")))
             (name (ps:chain (ps:@ input value) (trim))))
        (when (= name "")
          (alert "Please enter a name for the new playlist")
          (return))
        
        (ps:chain
         (fetch (+ "/api/asteroid/stream/playlists/save-as?name=" (encode-u-r-i-component name))
                (ps:create :method "POST"))
         (then (lambda (response) (ps:chain response (json))))
         (then (lambda (result)
                 (let ((data (or (ps:@ result data) result)))
                   (if (= (ps:@ data status) "success")
                       (progn
                         (show-toast (+ "✓ Saved as " name))
                         (setf (ps:@ input value) "")
                         (load-playlist-list))
                       (alert (+ "Error saving playlist: " (or (ps:@ data message) "Unknown error")))))))
         (catch (lambda (error)
                  (ps:chain console (error "Error saving playlist:" error))
                  (alert "Error saving playlist"))))))
    
    ;; Clear stream queue (updated to use new API)
    (defun clear-stream-queue ()
      (unless (confirm "Clear the stream queue? Liquidsoap will fall back to random playback from the music library.")
        (return))
      
      (ps:chain
       (fetch "/api/asteroid/stream/playlists/clear" (ps:create :method "POST"))
       (then (lambda (response) (ps:chain response (json))))
       (then (lambda (result)
               (let ((data (or (ps:@ result data) result)))
                 (if (= (ps:@ data status) "success")
                     (progn
                       (show-toast "✓ Queue cleared")
                       (load-current-queue))
                     (alert (+ "Error clearing queue: " (or (ps:@ data message) "Unknown error")))))))
       (catch (lambda (error)
                (ps:chain console (error "Error clearing queue:" error))
                (alert "Error clearing queue")))))
    
    ;; ========================================
    ;; Liquidsoap Control Functions
    ;; ========================================
    
    ;; Refresh Liquidsoap status
    (defun refresh-liquidsoap-status ()
      (ps:chain
       (fetch "/api/asteroid/liquidsoap/status")
       (then (lambda (response) (ps:chain response (json))))
       (then (lambda (result)
               (let ((data (or (ps:@ result data) result)))
                 (when (= (ps:@ data status) "success")
                   (let ((uptime-el (ps:chain document (get-element-by-id "ls-uptime")))
                         (remaining-el (ps:chain document (get-element-by-id "ls-remaining")))
                         (metadata-el (ps:chain document (get-element-by-id "ls-metadata"))))
                     (when uptime-el
                       (setf (ps:@ uptime-el text-content) (or (ps:@ data uptime) "--")))
                     (when remaining-el
                       (setf (ps:@ remaining-el text-content) (or (ps:@ data remaining) "--")))
                     (when metadata-el
                       (setf (ps:@ metadata-el text-content) (or (ps:@ data metadata) "--"))))))))
       (catch (lambda (error)
                (ps:chain console (error "Error fetching Liquidsoap status:" error))))))
    
    ;; Skip current track
    (defun liquidsoap-skip ()
      (ps:chain
       (fetch "/api/asteroid/liquidsoap/skip" (ps:create :method "POST"))
       (then (lambda (response) (ps:chain response (json))))
       (then (lambda (result)
               (let ((data (or (ps:@ result data) result)))
                 (if (= (ps:@ data status) "success")
                     (progn
                       (show-toast "⏭️ Track skipped")
                       (set-timeout refresh-liquidsoap-status 1000))
                     (alert (+ "Error skipping track: " (or (ps:@ data message) "Unknown error")))))))
       (catch (lambda (error)
                (ps:chain console (error "Error skipping track:" error))
                (alert "Error skipping track")))))
    
    ;; Reload playlist
    (defun liquidsoap-reload ()
      (ps:chain
       (fetch "/api/asteroid/liquidsoap/reload" (ps:create :method "POST"))
       (then (lambda (response) (ps:chain response (json))))
       (then (lambda (result)
               (let ((data (or (ps:@ result data) result)))
                 (if (= (ps:@ data status) "success")
                     (show-toast "📂 Playlist reloaded")
                     (alert (+ "Error reloading playlist: " (or (ps:@ data message) "Unknown error")))))))
       (catch (lambda (error)
                (ps:chain console (error "Error reloading playlist:" error))
                (alert "Error reloading playlist")))))
    
    ;; Restart Liquidsoap container
    (defun liquidsoap-restart ()
      (unless (confirm "Restart Liquidsoap container? This will cause a brief interruption to the stream.")
        (return))
      
      (show-toast "🔄 Restarting Liquidsoap...")
      (ps:chain
       (fetch "/api/asteroid/liquidsoap/restart" (ps:create :method "POST"))
       (then (lambda (response) (ps:chain response (json))))
       (then (lambda (result)
               (let ((data (or (ps:@ result data) result)))
                 (if (= (ps:@ data status) "success")
                     (progn
                       (show-toast "✓ Liquidsoap restarting")
                       ;; Refresh status after a delay to let container restart
                       (set-timeout refresh-liquidsoap-status 5000))
                     (alert (+ "Error restarting Liquidsoap: " (or (ps:@ data message) "Unknown error")))))))
       (catch (lambda (error)
                (ps:chain console (error "Error restarting Liquidsoap:" error))
                (alert "Error restarting Liquidsoap")))))
    
    ;; Restart Icecast container
    (defun icecast-restart ()
      (unless (confirm "Restart Icecast container? This will disconnect all listeners temporarily.")
        (return))
      
      (show-toast "🔄 Restarting Icecast...")
      (ps:chain
       (fetch "/api/asteroid/icecast/restart" (ps:create :method "POST"))
       (then (lambda (response) (ps:chain response (json))))
       (then (lambda (result)
               (let ((data (or (ps:@ result data) result)))
                 (if (= (ps:@ data status) "success")
                     (show-toast "✓ Icecast restarting - listeners will reconnect automatically")
                     (alert (+ "Error restarting Icecast: " (or (ps:@ data message) "Unknown error")))))))
       (catch (lambda (error)
                (ps:chain console (error "Error restarting Icecast:" error))
                (alert "Error restarting Icecast")))))
    
    ;; ========================================
    ;; Listener Statistics
    ;; ========================================
    
    ;; Refresh listener stats from API
    (defun refresh-listener-stats ()
      (let ((status-el (ps:chain document (get-element-by-id "stats-status"))))
        (when status-el
          (setf (ps:@ status-el text-content) "Loading...")))
      
      (ps:chain
       (fetch "/api/asteroid/stats/current")
       (then (lambda (response) (ps:chain response (json))))
       (then (lambda (result)
               (let ((data (or (ps:@ result data) result)))
                 (if (and (= (ps:@ data status) "success") (ps:@ data listeners))
                     (progn
                       ;; Process listener data - get most recent for each mount
                       (let ((mounts (ps:create)))
                         (ps:chain (ps:@ data listeners)
                                   (for-each (lambda (item)
                                               ;; item is [mount, "/asteroid.mp3", listeners, 1, timestamp, 123456]
                                               (let ((mount (ps:getprop item 1))
                                                     (listeners (ps:getprop item 3))
                                                     (timestamp (ps:getprop item 5)))
                                                 (when (or (not (ps:getprop mounts mount))
                                                           (> timestamp (ps:@ (ps:getprop mounts mount) timestamp)))
                                                   (setf (ps:getprop mounts mount)
                                                         (ps:create :listeners listeners :timestamp timestamp)))))))
                         
                         ;; Update UI
                         (let ((mp3 (or (and (ps:getprop mounts "/asteroid.mp3")
                                             (ps:@ (ps:getprop mounts "/asteroid.mp3") listeners)) 0))
                               (aac (or (and (ps:getprop mounts "/asteroid.aac")
                                             (ps:@ (ps:getprop mounts "/asteroid.aac") listeners)) 0))
                               (low (or (and (ps:getprop mounts "/asteroid-low.mp3")
                                             (ps:@ (ps:getprop mounts "/asteroid-low.mp3") listeners)) 0)))
                           
                           (let ((mp3-el (ps:chain document (get-element-by-id "listeners-mp3")))
                                 (aac-el (ps:chain document (get-element-by-id "listeners-aac")))
                                 (low-el (ps:chain document (get-element-by-id "listeners-low")))
                                 (total-el (ps:chain document (get-element-by-id "listeners-total")))
                                 (updated-el (ps:chain document (get-element-by-id "stats-updated")))
                                 (status-el (ps:chain document (get-element-by-id "stats-status"))))
                             
                             (when mp3-el (setf (ps:@ mp3-el text-content) mp3))
                             (when aac-el (setf (ps:@ aac-el text-content) aac))
                             (when low-el (setf (ps:@ low-el text-content) low))
                             (when total-el (setf (ps:@ total-el text-content) (+ mp3 aac low)))
                             (when updated-el
                               (setf (ps:@ updated-el text-content)
                                     (ps:chain (ps:new (-date)) (to-locale-time-string))))
                             (when status-el (setf (ps:@ status-el text-content) ""))))))
                     (let ((status-el (ps:chain document (get-element-by-id "stats-status"))))
                       (when status-el
                         (setf (ps:@ status-el text-content) "No data available")))))))
       (catch (lambda (error)
                (ps:chain console (error "Error fetching stats:" error))
                (let ((status-el (ps:chain document (get-element-by-id "stats-status"))))
                  (when status-el
                    (setf (ps:@ status-el text-content) "Error loading stats")))))))
    
    ;; ========================================
    ;; Geo Statistics
    ;; ========================================
    
    ;; Track expanded countries
    (defvar *expanded-countries* (ps:new (-set)))
    
    ;; Convert country code to flag emoji
    (defun country-to-flag (country-code)
      (if (or (not country-code) (not (= (ps:@ country-code length) 2)))
          "🌍"
          (let ((code-points (ps:chain (ps:chain country-code (to-upper-case))
                                       (split "")
                                       (map (lambda (char)
                                              (+ 127397 (ps:chain char (char-code-at 0))))))))
            (ps:chain -string (from-code-point (ps:@ code-points 0) (ps:@ code-points 1))))))
    
    ;; Refresh geo stats from API
    (defun refresh-geo-stats ()
      (ps:chain
       (fetch "/api/asteroid/stats/geo?days=7")
       (then (lambda (response) (ps:chain response (json))))
       (then (lambda (result)
               (let ((data (or (ps:@ result data) result))
                     (tbody (ps:chain document (get-element-by-id "geo-stats-body"))))
                 (when tbody
                   (if (and (= (ps:@ data status) "success")
                            (ps:@ data geo)
                            (> (ps:@ (ps:@ data geo) length) 0))
                       (progn
                         (setf (ps:@ tbody inner-h-t-m-l)
                               (ps:chain (ps:@ data geo)
                                         (map (lambda (item)
                                                (let* ((country (or (ps:@ item country_code) (ps:getprop item 0)))
                                                       (listeners (or (ps:@ item total_listeners) (ps:getprop item 1) 0))
                                                       (minutes (or (ps:@ item total_minutes) (ps:getprop item 2) 0))
                                                       (is-expanded (ps:chain *expanded-countries* (has country)))
                                                       (arrow (if is-expanded "▼" "▶")))
                                                  (+ "<tr class=\"country-row\" data-country=\"" country "\" style=\"cursor: pointer;\" onclick=\"toggleCountryCities('" country "')\">"
                                                     "<td><span class=\"expand-arrow\">" arrow "</span> " (country-to-flag country) " " country "</td>"
                                                     "<td>" listeners "</td>"
                                                     "<td>" minutes "</td>"
                                                     "</tr>"
                                                     "<tr class=\"city-rows\" id=\"cities-" country "\" style=\"display: " (if is-expanded "table-row" "none") ";\">"
                                                     "<td colspan=\"3\" style=\"padding: 0;\"><div class=\"city-container\" id=\"city-container-" country "\"></div></td>"
                                                     "</tr>"))))
                                         (join "")))
                         ;; Re-fetch cities for expanded countries
                         (ps:chain *expanded-countries*
                                   (for-each (lambda (country)
                                               (fetch-cities country)))))
                       (setf (ps:@ tbody inner-h-t-m-l)
                             "<tr><td colspan=\"3\" style=\"color: #888;\">No geo data yet</td></tr>"))))))
       (catch (lambda (error)
                (ps:chain console (error "Error fetching geo stats:" error))
                (let ((tbody (ps:chain document (get-element-by-id "geo-stats-body"))))
                  (when tbody
                    (setf (ps:@ tbody inner-h-t-m-l)
                          "<tr><td colspan=\"3\" style=\"color: #ff6666;\">Error loading geo data</td></tr>")))))))
    
    ;; Toggle city display for a country
    (defun toggle-country-cities (country)
      (let ((city-row (ps:chain document (get-element-by-id (+ "cities-" country))))
            (country-row (ps:chain document (query-selector (+ "tr[data-country=\"" country "\"]"))))
            (arrow (when country-row (ps:chain country-row (query-selector ".expand-arrow")))))
        
        (if (ps:chain *expanded-countries* (has country))
            (progn
              (ps:chain *expanded-countries* (delete country))
              (when city-row (setf (ps:@ city-row style display) "none"))
              (when arrow (setf (ps:@ arrow text-content) "▶")))
            (progn
              (ps:chain *expanded-countries* (add country))
              (when city-row (setf (ps:@ city-row style display) "table-row"))
              (when arrow (setf (ps:@ arrow text-content) "▼"))
              (fetch-cities country)))))
    
    ;; Fetch cities for a country
    (defun fetch-cities (country)
      (let ((container (ps:chain document (get-element-by-id (+ "city-container-" country)))))
        (when container
          (setf (ps:@ container inner-h-t-m-l)
                "<div style=\"padding: 5px 20px; color: #888;\">Loading cities...</div>")
          
          (ps:chain
           (fetch (+ "/api/asteroid/stats/geo/cities?country=" country "&days=7"))
           (then (lambda (response) (ps:chain response (json))))
           (then (lambda (result)
                   (let ((data (or (ps:@ result data) result)))
                     (if (and (= (ps:@ data status) "success")
                              (ps:@ data cities)
                              (> (ps:@ (ps:@ data cities) length) 0))
                         (setf (ps:@ container inner-h-t-m-l)
                               (+ "<table style=\"width: 100%; margin-left: 20px;\">"
                                  (ps:chain (ps:@ data cities)
                                            (map (lambda (city)
                                                   (+ "<tr style=\"background: rgba(0,255,0,0.05);\">"
                                                      "<td style=\"padding: 3px 10px;\">└ " (ps:@ city city) "</td>"
                                                      "<td style=\"padding: 3px 10px;\">" (ps:@ city listeners) "</td>"
                                                      "<td style=\"padding: 3px 10px;\">" (ps:@ city minutes) "</td>"
                                                      "</tr>")))
                                            (join ""))
                                  "</table>"))
                         (setf (ps:@ container inner-h-t-m-l)
                               "<div style=\"padding: 5px 20px; color: #888;\">No city data</div>")))))
           (catch (lambda (error)
                    (ps:chain console (error "Error fetching cities:" error))
                    (setf (ps:@ container inner-h-t-m-l)
                          "<div style=\"padding: 5px 20px; color: #ff6666;\">Error loading cities</div>")))))))
    
    ;; ========================================
    ;; Admin Password Reset
    ;; ========================================
    
    (defun reset-user-password (event)
      (ps:chain event (prevent-default))
      
      (let ((username (ps:@ (ps:chain document (get-element-by-id "reset-username")) value))
            (new-password (ps:@ (ps:chain document (get-element-by-id "reset-new-password")) value))
            (confirm-password (ps:@ (ps:chain document (get-element-by-id "reset-confirm-password")) value))
            (message-div (ps:chain document (get-element-by-id "reset-password-message"))))
        
        ;; Client-side validation
        (when (< (ps:@ new-password length) 8)
          (setf (ps:@ message-div text-content) "New password must be at least 8 characters")
          (setf (ps:@ message-div class-name) "message error")
          (return nil))
        
        (when (not (= new-password confirm-password))
          (setf (ps:@ message-div text-content) "Passwords do not match")
          (setf (ps:@ message-div class-name) "message error")
          (return nil))
        
        ;; Send request to API
        (let ((form-data (ps:new (-form-data))))
          (ps:chain form-data (append "username" username))
          (ps:chain form-data (append "new-password" new-password))
          
          (ps:chain
           (fetch "/api/asteroid/admin/reset-password"
                  (ps:create :method "POST" :body form-data))
           (then (lambda (response) (ps:chain response (json))))
           (then (lambda (data)
                   (if (or (= (ps:@ data status) "success")
                           (and (ps:@ data data) (= (ps:@ (ps:@ data data) status) "success")))
                       (progn
                         (setf (ps:@ message-div text-content)
                               (+ "Password reset successfully for user: " username))
                         (setf (ps:@ message-div class-name) "message success")
                         (ps:chain (ps:chain document (get-element-by-id "admin-reset-password-form")) (reset)))
                       (progn
                         (setf (ps:@ message-div text-content)
                               (or (ps:@ data message)
                                   (and (ps:@ data data) (ps:@ (ps:@ data data) message))
                                   "Failed to reset password"))
                         (setf (ps:@ message-div class-name) "message error")))))
           (catch (lambda (error)
                    (ps:chain console (error "Error resetting password:" error))
                    (setf (ps:@ message-div text-content) "Error resetting password")
                    (setf (ps:@ message-div class-name) "message error"))))))
      
      nil)
    
    ;; ========================================
    ;; Auto-refresh and Initialization for Stats
    ;; ========================================
    
    ;; Setup stats auto-refresh (called from DOMContentLoaded)
    (defun setup-stats-refresh ()
      ;; Initial load
      (refresh-listener-stats)
      (refresh-geo-stats)
      ;; Auto-refresh intervals
      (set-interval refresh-listener-stats 30000)
      (set-interval refresh-geo-stats 60000))
    
    ;; Make functions globally accessible for onclick handlers
    (setf (ps:@ window go-to-page) go-to-page)
    (setf (ps:@ window previous-page) previous-page)
    (setf (ps:@ window next-page) next-page)
    (setf (ps:@ window go-to-last-page) go-to-last-page)
    (setf (ps:@ window change-tracks-per-page) change-tracks-per-page)
    (setf (ps:@ window stream-track) stream-track)
    (setf (ps:@ window delete-track) delete-track)
    (setf (ps:@ window move-track-up) move-track-up)
    (setf (ps:@ window move-track-down) move-track-down)
    (setf (ps:@ window remove-from-queue) remove-from-queue)
    (setf (ps:@ window add-to-queue) add-to-queue)
    (setf (ps:@ window toggle-country-cities) toggle-country-cities)
    (setf (ps:@ window reset-user-password) reset-user-password)
    (setf (ps:@ window refresh-listener-stats) refresh-listener-stats)
    (setf (ps:@ window refresh-geo-stats) refresh-geo-stats)
    (setf (ps:@ window setup-stats-refresh) setup-stats-refresh)
    ))
  "Compiled JavaScript for admin dashboard - generated at load time")

(defun generate-admin-js ()
  "Return the pre-compiled JavaScript for admin dashboard"
  *admin-js*)
