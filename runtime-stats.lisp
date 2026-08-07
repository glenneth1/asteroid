;;;; runtime-stats.lisp - SBCL runtime monitoring for Asteroid Radio
;;;; Collects memory, GC, and thread stats periodically and exposes via API.

(in-package #:asteroid)

(defparameter *runtime-stats-history* nil
  "Ring buffer of runtime stat snapshots, newest first.")
(defparameter *runtime-stats-lock* (bt:make-lock "runtime-stats-lock"))
(defparameter *runtime-stats-max-samples* 360
  "Maximum number of samples to keep (360 * 10s = 1 hour of history).")
(defparameter *runtime-stats-thread* nil
  "Background thread for collecting runtime stats.")
(defparameter *runtime-stats-running* nil
  "Flag to control the stats collection loop.")
(defparameter *runtime-stats-interval* 10
  "Seconds between runtime stat samples.")
(defparameter *runtime-start-time* (get-universal-time)
  "When the runtime stats collector started (for uptime calculation).")

(defun collect-runtime-stats ()
  "Collect a single snapshot of SBCL runtime statistics.
   Returns a plist suitable for JSON encoding."
  (let ((now (get-universal-time)))
    (list :timestamp now
          :uptime (- now *runtime-start-time*)
          :dynamic-usage (sb-kernel:dynamic-usage)
          :bytes-consed (sb-ext:get-bytes-consed)
          :gc-count (car sb-kernel::*gc-epoch*)
          :thread-count (length (sb-thread:list-all-threads))
          :threads (mapcar (lambda (th)
                             (list :name (sb-thread:thread-name th)
                                   :alive (sb-thread:thread-alive-p th)
                                   :os-tid (sb-thread:thread-os-tid th)))
                           (sb-thread:list-all-threads)))))

(defun store-runtime-stats (stats)
  "Store a stats snapshot in the ring buffer (thread-safe)."
  (bt:with-lock-held (*runtime-stats-lock*)
    (push stats *runtime-stats-history*)
    (when (> (length *runtime-stats-history*) *runtime-stats-max-samples*)
      (setf *runtime-stats-history*
            (subseq *runtime-stats-history* 0 *runtime-stats-max-samples*)))))

(defun get-runtime-stats-history (&optional (limit 60))
  "Get recent stats snapshots from the ring buffer (thread-safe).
   LIMIT is the max number of samples to return (default 60 = 10 min)."
  (bt:with-lock-held (*runtime-stats-lock*)
    (subseq *runtime-stats-history* 0 (min limit (length *runtime-stats-history*)))))

(defun runtime-stats-loop ()
  "Background loop that collects runtime stats at regular intervals."
  (log:info "Runtime stats collector started (interval=~As, max-samples=~A)"
            *runtime-stats-interval* *runtime-stats-max-samples*)
  (loop while *runtime-stats-running*
        do (sleep *runtime-stats-interval*)
           (when *runtime-stats-running*
             (handler-case
                 (store-runtime-stats (collect-runtime-stats))
               (error (e)
                 (log:warn "Runtime stats collection error: ~A" e)))))
  (log:info "Runtime stats collector stopped"))

(defun start-runtime-stats-collection ()
  "Start the background runtime stats collector."
  (when *runtime-stats-thread*
    (log:warn "Runtime stats collector already running")
    (return-from start-runtime-stats-collection))
  (setf *runtime-stats-running* t)
  (setf *runtime-start-time* (get-universal-time))
  ;; Collect an immediate sample so we have data right away
  (handler-case
      (store-runtime-stats (collect-runtime-stats))
    (error (e)
      (log:warn "Initial runtime stats collection failed: ~A" e)))
  (setf *runtime-stats-thread*
        (bt:make-thread #'runtime-stats-loop
                        :name "runtime-stats-collector")))

(defun stop-runtime-stats-collection ()
  "Stop the background runtime stats collector."
  (setf *runtime-stats-running* nil)
  (when *runtime-stats-thread*
    (let ((thread *runtime-stats-thread*))
      (setf *runtime-stats-thread* nil)
      (handler-case
          (bt:join-thread thread)
        (error () nil)))))

(defun format-bytes (bytes)
  "Format bytes as human-readable string."
  (unless bytes (return-from format-bytes "0 B"))
  (cond
    ((>= bytes (* 1024 1024 1024))
     (format nil "~,1F GB" (/ bytes (* 1024 1024 1024))))
    ((>= bytes (* 1024 1024))
     (format nil "~,1F MB" (/ bytes (* 1024 1024))))
    ((>= bytes 1024)
     (format nil "~,1F KB" (/ bytes 1024)))
    (t
     (format nil "~A B" bytes))))

(defun format-uptime (seconds)
  "Format uptime seconds as human-readable string."
  (unless (and seconds (numberp seconds))
    (return-from format-uptime "0d 0h 0m 0s"))
  (multiple-value-bind (d rem) (floor seconds (* 60 60 24))
    (multiple-value-bind (h rem2) (floor rem (* 60 60))
      (multiple-value-bind (m s) (floor rem2 60)
        (format nil "~Ad ~Ah ~Am ~As" d h m s)))))

(defun runtime-stats-to-json (stats &key (include-threads t))
  "Convert a stats snapshot plist to an alist suitable for JSON encoding."
  `(("timestamp" . ,(getf stats :timestamp))
    ("uptime_seconds" . ,(getf stats :uptime))
    ("uptime_human" . ,(format-uptime (getf stats :uptime)))
    ("dynamic_usage_bytes" . ,(getf stats :dynamic-usage))
    ("dynamic_usage_human" . ,(format-bytes (getf stats :dynamic-usage)))
    ("bytes_consed_total" . ,(getf stats :bytes-consed))
    ("bytes_consed_human" . ,(format-bytes (getf stats :bytes-consed)))
    ("gc_count" . ,(getf stats :gc-count))
    ("thread_count" . ,(getf stats :thread-count))
    ,@(when include-threads
        `(("threads" . ,(mapcar (lambda (th)
                                  `(("name" . ,(getf th :name))
                                    ("alive" . ,(getf th :alive))
                                    ("os_tid" . ,(getf th :os-tid))))
                                (getf stats :threads)))))))

;; API endpoint: current + historical runtime stats
(define-api asteroid/admin/runtime-stats (&optional (limit "60")) ()
  "Get SBCL runtime statistics (memory, GC, threads) with historical data.
   LIMIT controls how many historical samples to return (default 60, max 360)."
  (require-role :admin)
  (handler-case
      (let* ((limit-int (min (max (parse-integer limit :junk-allowed t) 1) 360))
             (history (get-runtime-stats-history limit-int)))
        ;; If no history yet, collect a sample on demand
        (unless history
          (store-runtime-stats (collect-runtime-stats))
          (setf history (get-runtime-stats-history limit-int)))
        (let ((current (first history)))
          (if current
              (api-output
               `(("status" . "success")
                 ("current" . ,(runtime-stats-to-json current))
                 ("history" . ,(mapcar (lambda (s)
                                         (runtime-stats-to-json s :include-threads nil))
                                       history))
                 ("config" . (("interval_seconds" . ,*runtime-stats-interval*)
                              ("max_samples" . ,*runtime-stats-max-samples*)
                              ("collected_samples" . ,(length history))))))
              (api-output
               `(("status" . "error")
                 ("message" . "No runtime stats collected yet"))
               :status 503))))
    (error (e)
      (format t "runtime-stats API error: ~A~%" e)
      (api-output `(("status" . "error")
                    ("message" . ,(format nil "~A" e)))
                  :status 500))))
