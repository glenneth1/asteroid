(in-package #:asteroid)

;;; Spectrum Analyzer - Parenscript Implementation
;;; Generates JavaScript for real-time audio visualization

(define-api asteroid/spectrum-analyzer.js () ()
  "Serve the spectrum analyzer JavaScript generated from Parenscript"
  (setf (content-type *response*) "application/javascript")
  (ps:ps
    (defvar *audio-context* nil)
    (defvar *analyser* nil)
    (defvar *canvas* nil)
    (defvar *canvas-ctx* nil)
    (defvar *animation-id* nil)
    
    (defun init-spectrum-analyzer ()
      "Initialize the spectrum analyzer"
      (let ((audio-element nil)
            (canvas-element (ps:chain document (get-element-by-id "spectrum-canvas"))))
        
        ;; Try to find audio element in current frame first
        (setf audio-element (or (ps:chain document (get-element-by-id "live-audio"))
                                (ps:chain document (get-element-by-id "persistent-audio"))))
        
        ;; If not found and we're in a frame, try to access from parent frameset
        (when (and (not audio-element)
                   (ps:@ window parent)
                   (not (eq (ps:@ window parent) window)))
          (ps:chain console (log "Trying to access audio from parent frame..."))
          (ps:try
           (progn
             ;; Try accessing via parent.frames
             (let ((player-frame (ps:getprop (ps:@ window parent) "player-frame")))
               (when player-frame
                 (setf audio-element (ps:chain player-frame document (get-element-by-id "persistent-audio")))
                 (ps:chain console (log "Found audio in player-frame:" audio-element)))))
           (:catch (e)
             (ps:chain console (log "Cross-frame access error:" e)))))
        
        (when (and audio-element canvas-element (not *audio-context*))
          ;; Create Audio Context
          (setf *audio-context* (ps:new (or (ps:@ window |AudioContext|)
                                            (ps:@ window |webkitAudioContext|))))
          
          ;; Create Analyser Node
          (setf *analyser* (ps:chain *audio-context* (create-analyser)))
          (setf (ps:@ *analyser* |fftSize|) 256)
          (setf (ps:@ *analyser* |smoothingTimeConstant|) 0.8)
          
          ;; Connect audio source to analyser
          (let ((source (ps:chain *audio-context* (create-media-element-source audio-element))))
            (ps:chain source (connect *analyser*))
            (ps:chain *analyser* (connect (ps:@ *audio-context* destination))))
          
          ;; Setup canvas
          (setf *canvas* canvas-element)
          (setf *canvas-ctx* (ps:chain *canvas* (get-context "2d")))
          
          ;; Start visualization
          (draw-spectrum))))
    
    (defun draw-spectrum ()
      "Draw the spectrum analyzer visualization"
      (setf *animation-id* (request-animation-frame draw-spectrum))
      
      (let* ((buffer-length (ps:@ *analyser* |frequencyBinCount|))
             (data-array (ps:new (|Uint8Array| buffer-length)))
             (width (ps:@ *canvas* width))
             (height (ps:@ *canvas* height))
             (bar-width (/ width buffer-length))
             (bar-height 0)
             (x 0))
        
        (ps:chain *analyser* (get-byte-frequency-data data-array))
        
        ;; Clear canvas with fade effect
        (setf (ps:@ *canvas-ctx* |fillStyle|) "rgba(0, 0, 0, 0.2)")
        (ps:chain *canvas-ctx* (fill-rect 0 0 width height))
        
        ;; Draw bars
        (dotimes (i buffer-length)
          (setf bar-height (/ (* (aref data-array i) height) 256))
          
          ;; Create gradient for each bar
          (let ((gradient (ps:chain *canvas-ctx* 
                                (create-linear-gradient 0 (- height bar-height) 0 height))))
            (ps:chain gradient (add-color-stop 0 "#00ff00"))
            (ps:chain gradient (add-color-stop 0.5 "#00aa00"))
            (ps:chain gradient (add-color-stop 1 "#005500"))
            
            (setf (ps:@ *canvas-ctx* |fillStyle|) gradient)
            (ps:chain *canvas-ctx* (fill-rect x (- height bar-height) bar-width bar-height))
            
            (incf x bar-width)))))
    
    (defun stop-spectrum-analyzer ()
      "Stop the spectrum analyzer"
      (when *animation-id*
        (cancel-animation-frame *animation-id*)
        (setf *animation-id* nil)))
    
    ;; Initialize when audio starts playing
    (ps:chain document (add-event-listener "DOMContentLoaded"
      (lambda ()
        (let ((audio-element (or (ps:chain document (get-element-by-id "live-audio"))
                                 (ps:chain document (get-element-by-id "persistent-audio")))))
          
          ;; If not found and we're in a frame, try parent
          (when (and (not audio-element)
                     (ps:@ window parent)
                     (not (eq (ps:@ window parent) window)))
            (ps:try
             (let ((player-frame (ps:getprop (ps:@ window parent) "player-frame")))
               (when player-frame
                 (setf audio-element (ps:chain player-frame document (get-element-by-id "persistent-audio")))))
             (:catch (e)
               (ps:chain console (log "Event listener cross-frame error:" e)))))
          
          (when audio-element
            (ps:chain audio-element (add-event-listener "play" init-spectrum-analyzer))
            (ps:chain audio-element (add-event-listener "pause" stop-spectrum-analyzer)))))))))
