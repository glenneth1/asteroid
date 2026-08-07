;;;; runtime-stats-js.lisp - ParenScript for runtime monitoring dashboard

(in-package #:asteroid)

(defparameter *runtime-stats-js*
  (ps:ps*
   '(progn
     (defvar *rs-memory-chart* nil)
     (defvar *rs-alloc-chart* nil)
     (defvar *rs-threads-chart* nil)
     (defvar *rs-autorefresh-timer* nil)

     (defun rs-format-time (timestamp)
       (let ((d (ps:new (-date (* timestamp 1000)))))
         (+ (ps:chain d (get-hours) (to-string) (pad-start 2 "0"))
            ":" (ps:chain d (get-minutes) (to-string) (pad-start 2 "0"))
            ":" (ps:chain d (get-seconds) (to-string) (pad-start 2 "0")))))

     (defun rs-chart-config (label color)
       (ps:create
        :type "line"
        :data (ps:create
               :labels (array)
               :datasets (array
                          (ps:create
                           :label label
                           :data (array)
                           :borderColor color
                           :backgroundColor "rgba(76,175,80,0.1)"
                           :fill t
                           :tension 0.3)))
        :options (ps:create
                  :responsive t
                  :maintainAspectRatio false
                  :scales (ps:create :y (ps:create :beginAtZero t))
                  :plugins (ps:create :legend (ps:create :display t)))))

     (defun rs-init-charts ()
       (setf *rs-memory-chart*
             (ps:new (-chart (ps:chain document (get-element-by-id "rs-memory-chart"))
                         (rs-chart-config "Heap Memory (MB)" "#4CAF50"))))
       (setf *rs-alloc-chart*
             (ps:new (-chart (ps:chain document (get-element-by-id "rs-alloc-chart"))
                         (rs-chart-config "Allocation Rate (KB/s)" "#FF9800"))))
       (setf *rs-threads-chart*
             (ps:new (-chart (ps:chain document (get-element-by-id "rs-threads-chart"))
                         (rs-chart-config "Thread Count" "#2196F3")))))

     (defun rs-update-charts (data)
       (let ((history (ps:getprop data "history"))
             (labels (array))
             (mem-data (array))
             (alloc-data (array))
             (thread-data (array)))
         (ps:chain history
                   (for-each
                    (lambda (entry i)
                      (let ((prev (when (> i 0) (aref history (- i 1)))))
                        (ps:chain labels (push (rs-format-time (ps:getprop entry "timestamp"))))
                        (ps:chain mem-data (push (ps:chain (/ (ps:getprop entry "dynamic_usage_bytes") (* 1024 1024)) (to-fixed 1))))
                        (ps:chain thread-data (push (ps:getprop entry "thread_count")))
                        (if prev
                            (let ((delta (- (ps:getprop entry "bytes_consed_total")
                                            (ps:getprop prev "bytes_consed_total")))
                                  (time-delta (- (ps:getprop entry "timestamp")
                                                 (ps:getprop prev "timestamp"))))
                              (if (> time-delta 0)
                                  (ps:chain alloc-data (push (ps:chain (/ (/ delta time-delta) 1024) (to-fixed 1))))
                                  (ps:chain alloc-data (push 0))))
                            (ps:chain alloc-data (push 0)))))))
         (setf (ps:@ *rs-memory-chart* data labels) labels)
         (setf (ps:@ *rs-memory-chart* data datasets 0 data) mem-data)
         (ps:chain *rs-memory-chart* (update))
         (setf (ps:@ *rs-alloc-chart* data labels) labels)
         (setf (ps:@ *rs-alloc-chart* data datasets 0 data) alloc-data)
         (ps:chain *rs-alloc-chart* (update))
         (setf (ps:@ *rs-threads-chart* data labels) labels)
         (setf (ps:@ *rs-threads-chart* data datasets 0 data) thread-data)
         (ps:chain *rs-threads-chart* (update))))

     (defun rs-update-current (current)
       (setf (ps:@ (ps:chain document (get-element-by-id "rs-heap-memory")) inner-text)
             (ps:getprop current "dynamic_usage_human"))
       (setf (ps:@ (ps:chain document (get-element-by-id "rs-bytes-consed")) inner-text)
             (ps:getprop current "bytes_consed_human"))
       (setf (ps:@ (ps:chain document (get-element-by-id "rs-gc-count")) inner-text)
             (ps:getprop current "gc_count"))
       (setf (ps:@ (ps:chain document (get-element-by-id "rs-thread-count")) inner-text)
             (ps:getprop current "thread_count"))
       (setf (ps:@ (ps:chain document (get-element-by-id "rs-uptime")) inner-text)
             (ps:getprop current "uptime_human")))

     (defun rs-update-threads (threads)
       (let ((tbody (ps:chain document (get-element-by-id "rs-threads-table"))))
         (setf (ps:@ tbody inner-h-t-m-l) "")
         (ps:chain threads
                   (for-each
                    (lambda (th)
                      (let ((row (ps:chain document (create-element "tr"))))
                        (setf (ps:@ row inner-h-t-m-l)
                              (+ "<td>" (ps:getprop th "name") "</td>"
                                 "<td>" (if (ps:getprop th "alive") "Yes" "No") "</td>"
                                 "<td>" (ps:getprop th "os_tid") "</td>"))
                        (ps:chain tbody (append-child row))))))))

     (defun rs-refresh ()
       (ps:chain
        (fetch "/api/asteroid/admin/runtime-stats?limit=60"
               (ps:create :credentials "same-origin"))
        (then (lambda (response) (ps:chain response (json))))
        (then (lambda (data)
                (let ((data (or (ps:@ data data) data)))
                  (if (= (ps:getprop data "status") "success")
                      (let ((current (ps:getprop data "current")))
                        (rs-update-current current)
                        (rs-update-threads (ps:getprop current "threads"))
                        (rs-update-charts data)
                        (setf (ps:@ (ps:chain document (get-element-by-id "rs-last-update")) inner-text)
                              (+ "Updated: " (rs-format-time (ps:getprop current "timestamp")))))
                      (setf (ps:@ (ps:chain document (get-element-by-id "rs-last-update")) inner-text)
                            (ps:getprop data "message"))))))
        (catch (lambda (err)
                 (setf (ps:@ (ps:chain document (get-element-by-id "rs-last-update")) inner-text)
                       (+ "Error: " err))))))

     (defun rs-toggle-autorefresh (seconds)
       (when *rs-autorefresh-timer*
         (clear-interval *rs-autorefresh-timer*)
         (setf *rs-autorefresh-timer* nil))
       (when (> (parse-int seconds) 0)
         (setf *rs-autorefresh-timer*
               (set-interval rs-refresh (* (parse-int seconds) 1000)))))

     (ps:chain document
               (add-event-listener
                "DOMContentLoaded"
                (lambda ()
                  (rs-init-charts)
                  (rs-refresh))))))

  "Compiled JavaScript for runtime stats dashboard")

(defun generate-runtime-stats-js ()
  "Return the pre-compiled JavaScript for runtime stats dashboard"
  *runtime-stats-js*)
