;;;; limiter.lisp - Rate limiter definitions for the application
;;;;
;;;; Includes monkey-patches for r-simple-rate's sliding-window bug:
;;;; upstream tax-rate updates the timestamp on EVERY request, which
;;;; prevents the window from ever resetting while polling is active.
;;;; Our overrides use a proper fixed window — the timestamp is only
;;;; updated when the window expires and the counter resets.

(in-package :asteroid)

(defun cleanup-corrupted-rate-limits ()
  "Clean up corrupted rate limit entries with negative amounts.
   The r-simple-rate library has a bug where the reset condition only triggers
   when amount >= 0, so negative amounts never reset. This function deletes
   any corrupted entries so they can be recreated fresh."
  (handler-case
      (let ((deleted (db:remove 'simple-rate::tracking 
                                (db:query (:< 'amount 0)))))
        (when (and deleted (> deleted 0))
          (l:info :rate-limiter "Cleaned up ~a corrupted rate limit entries" deleted)))
    (error (e)
      (l:warn :rate-limiter "Failed to cleanup rate limits: ~a" e))))

;;; ——— r-simple-rate fixed-window overrides ———

(defun simple-rate::tax-rate (limit &key (ip (remote *request*)))
  "Fixed-window version of tax-rate.
   Only updates the timestamp when the window resets, not on every request.
   This prevents the sliding-window bug where continuous polling starves
   the counter because the reset condition never triggers."
  (let* ((limit (simple-rate::limit limit))
         (tracking (dm:get-one 'simple-rate::tracking
                               (db:query (:and (:= 'limit (simple-rate::name limit))
                                               (:= 'ip ip))))))
    (cond (tracking
           ;; If the window has expired, reset counter AND timestamp
           (when (<= (+ (dm:field tracking "time")
                        (simple-rate::timeout limit))
                     (get-universal-time))
             (setf (dm:field tracking "amount") (simple-rate::amount limit))
             (setf (dm:field tracking "time") (get-universal-time)))
           ;; Tax it (do NOT touch timestamp here — fixed window)
           (decf (dm:field tracking "amount"))
           (dm:save tracking))
          (t
           (db:insert 'simple-rate::tracking
                      `((limit . ,(simple-rate::name limit))
                        (time . ,(get-universal-time))
                        (amount . ,(simple-rate::amount limit))
                        (ip . ,ip)))))))

(defun rate:left (limit &key (ip (remote *request*)))
  "Fixed-window version of rate:left.
   Returns correct remaining amount even for expired windows, so that
   with-limitation does not block on stale tracking entries."
  (let* ((limit (simple-rate::limit limit))
         (tracking (dm:get-one 'simple-rate::tracking
                               (db:query (:and (:= 'limit (simple-rate::name limit))
                                               (:= 'ip ip))))))
    (if tracking
        (let ((window-end (+ (dm:field tracking "time")
                             (simple-rate::timeout limit)))
              (now (get-universal-time)))
          (if (<= window-end now)
              ;; Window expired — report full budget
              (values (simple-rate::amount limit) 0)
              ;; Window still active
              (values (dm:field tracking "amount")
                      (- window-end now))))
        ;; No tracking entry yet — full budget
        (values (simple-rate::amount limit)
                (simple-rate::timeout limit)))))

(define-trigger db:connected ()
  "Clean up any corrupted rate limit entries on startup"
  (cleanup-corrupted-rate-limits))

(defun render-rate-limit-error-page()
  (clip:process-to-string
   (load-template "error")
   :error-message "It seems that your acceleration has elevated your orbit out of your designated path."
   :error-action "Please wait a moment for it to stabilize and try your request again."))

(defun api-limit-error-output ()
  (api-output `(("status" . "error")
                ("message" .  "It seems that your acceleration has elevated your orbit out of your designated path."))
              :message   "It seems that your acceleration has elevated your orbit out of your designated path."
              :status 429))

(defun extract-limit-options (options)
  "Extracts the rate-limit options and forwards the reamaining radiance route options"
  (let ((limit   (getf options :limit))
        (timeout (getf options :timeout))
        (group (getf options :limit-group))
        (rest (loop for (k v) on options by #'cddr
                    unless (member k '(:limit :timeout :limit-group))
                      append (list k v))))
    (values limit timeout group rest)))


(defmacro define-page-with-limit (name uri options &body body)
  "Rate limit for a page route. Defaults to 30 requests per minute."
  (multiple-value-bind (limit timeout group rest) (extract-limit-options options)
    (let* ((limit-name (string-upcase (format nil "~a-route-limit" (or group name))))
           (limit-sym (intern limit-name))
           (limit (or limit 30))
           (timeout (or timeout 60)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (rate:define-limit ,limit-sym (time-left :limit ,limit :timeout ,timeout)
           ;; (format t "Route limit '~a' hit. Wait ~a seconds and retry.~%" ,(string name) time-left)
           (render-rate-limit-error-page))
         (define-page ,name ,uri ,rest
           (rate:with-limitation (,limit-sym)
             ,@body))))))

(defmacro define-api-with-limit (name args options &body body)
  "Rate limit for api routes. Defaults to 60 requests per minute."
  (multiple-value-bind (limit timeout group rest) (extract-limit-options options)
    (let* ((limit-name (string-upcase (format nil "~a-api-limit" (or group name))))
           (limit-sym (intern limit-name))
           (limit (or limit 60))
           (timeout (or timeout 60)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (rate:define-limit ,limit-sym (time-left :limit ,limit :timeout ,timeout)
           ;; (format t "API Rate limit '~a' hit. Wait ~a seconds and retry.~%" ,(string name) time-left)
           (api-limit-error-output))
         (define-api ,name ,args ,rest
           (rate:with-limitation (,limit-sym)
             ,@body))))))
