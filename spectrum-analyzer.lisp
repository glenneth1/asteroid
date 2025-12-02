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
      (let ((audio-element (chain document (get-element-by-id "live-audio")))
            (canvas-element (chain document (get-element-by-id "spectrum-canvas"))))
        
        (when (and audio-element canvas-element)
          ;; Create Audio Context
          (setf *audio-context* (new (or (@ window -audio-context)
                                         (@ window -webkit-audio-context))))
          
          ;; Create Analyser Node
          (setf *analyser* (chain *audio-context* (create-analyser)))
          (setf (@ *analyser* fft-size) 256)
          (setf (@ *analyser* smoothing-time-constant) 0.8)
          
          ;; Connect audio source to analyser
          (let ((source (chain *audio-context* (create-media-element-source audio-element))))
            (chain source (connect *analyser*))
            (chain *analyser* (connect (@ *audio-context* destination))))
          
          ;; Setup canvas
          (setf *canvas* canvas-element)
          (setf *canvas-ctx* (chain *canvas* (get-context "2d")))
          
          ;; Start visualization
          (draw-spectrum))))
    
    (defun draw-spectrum ()
      "Draw the spectrum analyzer visualization"
      (setf *animation-id* (request-animation-frame draw-spectrum))
      
      (let* ((buffer-length (@ *analyser* frequency-bin-count))
             (data-array (new (-uint8-array buffer-length)))
             (width (@ *canvas* width))
             (height (@ *canvas* height))
             (bar-width (/ width buffer-length))
             (bar-height 0)
             (x 0))
        
        (chain *analyser* (get-byte-frequency-data data-array))
        
        ;; Clear canvas with fade effect
        (setf (@ *canvas-ctx* fill-style) "rgba(0, 0, 0, 0.2)")
        (chain *canvas-ctx* (fill-rect 0 0 width height))
        
        ;; Draw bars
        (dotimes (i buffer-length)
          (setf bar-height (/ (* (aref data-array i) height) 256))
          
          ;; Create gradient for each bar
          (let ((gradient (chain *canvas-ctx* 
                                (create-linear-gradient 0 (- height bar-height) 0 height))))
            (chain gradient (add-color-stop 0 "#00ff00"))
            (chain gradient (add-color-stop 0.5 "#00aa00"))
            (chain gradient (add-color-stop 1 "#005500"))
            
            (setf (@ *canvas-ctx* fill-style) gradient)
            (chain *canvas-ctx* (fill-rect x (- height bar-height) bar-width bar-height))
            
            (incf x bar-width)))))
    
    (defun stop-spectrum-analyzer ()
      "Stop the spectrum analyzer"
      (when *animation-id*
        (cancel-animation-frame *animation-id*)
        (setf *animation-id* nil)))
    
    ;; Initialize when audio starts playing
    (chain document (add-event-listener "DOMContentLoaded"
      (lambda ()
        (let ((audio-element (chain document (get-element-by-id "live-audio"))))
          (when audio-element
            (chain audio-element (add-event-listener "play" init-spectrum-analyzer))
            (chain audio-element (add-event-listener "pause" stop-spectrum-analyzer))))))))
