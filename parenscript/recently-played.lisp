;;;; recently-played.lisp - ParenScript version of recently-played.js
;;;; Recently Played Tracks functionality

(in-package #:asteroid)

(defparameter *recently-played-js*
  (ps:ps
   (progn
    
    ;; Get current mount from stream quality selection
    ;; Checks local selector first, then sibling player-frame (for frameset mode)
    (defun get-current-mount-for-recently-played ()
      (let* ((selector (ps:chain document (get-element-by-id "stream-quality")))
             ;; If no local selector, try to get from sibling player-frame (frameset mode)
             (player-frame-selector 
              (when (and (not selector)
                         (not (= (ps:@ window parent) window)))
                (ps:try
                 (let ((player-frame (ps:@ (ps:@ window parent) frames "player-frame")))
                   (when player-frame
                     (ps:chain player-frame document (get-element-by-id "stream-quality"))))
                 (:catch (e) nil))))
             (effective-selector (or selector player-frame-selector))
             (quality (or (when effective-selector (ps:@ effective-selector value))
                          (ps:chain local-storage (get-item "stream-quality"))
                          "aac")))
        (cond
          ((= quality "shuffle") "asteroid-shuffle.mp3")
          ((= quality "low") "asteroid-low.mp3")
          ((= quality "mp3") "asteroid.mp3")
          (t "asteroid.aac"))))
    
    ;; Update recently played tracks display
    (defun update-recently-played ()
      (let ((mount (get-current-mount-for-recently-played)))
        (ps:chain
         (fetch (+ "/api/asteroid/recently-played?mount=" mount))
         (then (lambda (response) (ps:chain response (json))))
         (then (lambda (result)
                 ;; Radiance wraps API responses in a data envelope
                 (let ((data (or (ps:@ result data) result)))
                   (if (and (equal (ps:@ data status) "success")
                            (ps:@ data tracks)
                            (> (ps:@ data tracks length) 0))
                       (let ((list-el (ps:chain document (get-element-by-id "recently-played-list"))))
                         (when list-el
                           ;; Build HTML for tracks
                           (let ((html "<ul class=\"track-list\">"))
                             (ps:chain (ps:@ data tracks)
                                       (for-each (lambda (track index)
                                                   (let ((time-ago (format-time-ago (ps:@ track timestamp))))
                                                     (setf html
                                                           (+ html
                                                              "<li class=\"track-item\">"
                                                              "<div class=\"track-info\">"
                                                              "<div class=\"track-title\">"
                                                              "<a href=\"" (ps:@ track search_url) "\" target=\"_blank\" rel=\"noopener noreferrer\" class=\"track-link\">"
                                                              (escape-html (ps:@ track song))
                                                              "<svg width=\"14\" height=\"14\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" class=\"external-icon\">"
                                                              "<path d=\"M18 13v6a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h6\"></path>"
                                                              "<polyline points=\"15 3 21 3 21 9\"></polyline>"
                                                              "<line x1=\"10\" y1=\"14\" x2=\"21\" y2=\"3\"></line>"
                                                              "</svg>"
                                                              "</a>"
                                                              "</div>"
                                                              "<div class=\"track-artist\">" (escape-html (ps:@ track artist)) "</div>"
                                                              "<span class=\"track-time\">" time-ago "</span>"
                                                              "</div>"
                                                              "</li>"))))))
                             (setf html (+ html "</ul>"))
                             (setf (aref list-el "innerHTML") html))))
                       (let ((list-el (ps:chain document (get-element-by-id "recently-played-list"))))
                         (when list-el
                           (setf (aref list-el "innerHTML") "<p class=\"no-tracks\">No tracks played yet</p>")))))))
         (catch (lambda (error)
                  (ps:chain console (error "Error fetching recently played:" error))
                  (let ((list-el (ps:chain document (get-element-by-id "recently-played-list"))))
                    (when list-el
                      (setf (aref list-el "innerHTML") "<p class=\"error\">Error loading recently played tracks</p>"))))))))
    
    ;; Format timestamp as relative time
    (defun format-time-ago (timestamp)
      (let* ((now (floor (/ (ps:chain *date (now)) 1000)))
             (diff (- now timestamp)))
        (cond
          ((< diff 60) "Just now")
          ((< diff 3600) (+ (floor (/ diff 60)) "m ago"))
          ((< diff 86400) (+ (floor (/ diff 3600)) "h ago"))
          (t (+ (floor (/ diff 86400)) "d ago")))))
    
    ;; Escape HTML to prevent XSS
    (defun escape-html (text)
      (when (ps:@ window document)
        (let ((div (ps:chain document (create-element "div"))))
          (setf (ps:@ div text-content) text)
          (aref div "innerHTML"))))
    
    ;; Initialize on page load  
    (when (ps:@ window document)
      (ps:chain document
                (add-event-listener
                 "DOMContentLoaded"
                 (lambda ()
                   (let ((panel (ps:chain document (get-element-by-id "recently-played-panel"))))
                     (if panel
                         (progn
                           (update-recently-played)
                           ;; Refresh immediately when user switches streams
                           (let ((selector (ps:chain document (get-element-by-id "stream-quality"))))
                             (when (not selector)
                               (setf selector
                                     (ps:try
                                      (let ((player-frame (ps:@ (ps:@ window parent) frames "player-frame")))
                                        (when player-frame
                                          (ps:chain player-frame document (get-element-by-id "stream-quality"))))
                                      (:catch (e) nil))))
                             (when selector
                               (ps:chain selector
                                         (add-event-listener
                                          "change"
                                          (lambda (_ev)
                                            ;; Small delay so localStorage / UI state settles
                                            (ps:chain window (set-timeout update-recently-played 50)))))))
                           ;; Update every 30 seconds
                           (set-interval update-recently-played 30000))
                         (let ((list (ps:chain document (get-element-by-id "recently-played-list"))))
                           (when list
                             (update-recently-played)
                             (set-interval update-recently-played 30000))))))))))
   )
  "Compiled JavaScript for recently played tracks - generated at load time"
)

(defun generate-recently-played-js ()
  "Generate JavaScript code for recently played tracks"
  *recently-played-js*)
