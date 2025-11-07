(in-package :asteroid)

(defun icecast-now-playing (icecast-base-url)
    (let* ((icecast-url (format nil "~a/admin/stats.xml" icecast-base-url))
           (response (drakma:http-request icecast-url
                                         :want-stream nil
                                         :basic-authorization '("admin" "asteroid_admin_2024"))))
      (format t "DEBUG: Fetching Icecast stats from ~a~%" icecast-url)
      (when response
              (let ((xml-string (if (stringp response)
                                    response
                                    (babel:octets-to-string response :encoding :utf-8))))
                ;; Simple XML parsing to extract source information and aggregate listeners
                ;; Get title from main mp3 stream
                (let* ((mp3-match (cl-ppcre:scan "<source mount=\"/asteroid\\.mp3\">" xml-string))
                       (title (if mp3-match
                                  (let* ((source-section (subseq xml-string mp3-match
                                                                (or (cl-ppcre:scan "</source>" xml-string :start mp3-match)
                                                                    (length xml-string))))
                                         (titlep (cl-ppcre:all-matches "<title>" source-section)))
                                    (if titlep
                                        (cl-ppcre:regex-replace-all ".*<title>(.*?)</title>.*" source-section "\\1")
                                        "Unknown"))
                                  "Unknown"))
                       ;; Aggregate listeners from all three streams
                       (total-listeners 0))
                  ;; Count listeners from each mount point
                  (dolist (mount '("/asteroid\\.mp3" "/asteroid\\.aac" "/asteroid-low\\.mp3"))
                    (let ((match-pos (cl-ppcre:scan (format nil "<source mount=\"~a\">" mount) xml-string)))
                      (when match-pos
                        (let* ((source-section (subseq xml-string match-pos
                                                      (or (cl-ppcre:scan "</source>" xml-string :start match-pos)
                                                          (length xml-string))))
                               (listenersp (cl-ppcre:all-matches "<listeners>" source-section)))
                          (when listenersp
                            (let ((listener-count-str (cl-ppcre:regex-replace-all ".*<listeners>(.*?)</listeners>.*" source-section "\\1"))
                                  (count (parse-integer (cl-ppcre:regex-replace-all ".*<listeners>(.*?)</listeners>.*" source-section "\\1") :junk-allowed t)))
                              (format t "DEBUG: Mount ~a has ~a listeners~%" mount count)
                              (incf total-listeners count)))))))
                  (let ((result `((:listenurl . ,(format nil "~a/asteroid.mp3" *stream-base-url*))
                                  (:title . ,title)
                                  (:listeners . ,total-listeners))))
                    (format t "DEBUG: Parsed title=~a, total-listeners=~a~%" title total-listeners)
                    result))))))

(define-api asteroid/partial/now-playing () ()
  "Get Partial HTML with live status from Icecast server"
  (handler-case
    (let ((now-playing-stats (icecast-now-playing *stream-base-url*)))
      (if now-playing-stats
          (progn
            ;; TODO: it should be able to define a custom api-output for this
            ;; (api-output <clip-parser> :format "html"))
            (setf (header "Content-Type") "text/html")
            (clip:process-to-string
             (load-template "partial/now-playing")
             :stats now-playing-stats))
          (progn
            (setf (header "Content-Type") "text/html")
            (clip:process-to-string
             (load-template "partial/now-playing")
             :connection-error t
             :stats nil))))
    (error ()
      (format t "Error in now-playing endpoint~%")
      (setf (header "Content-Type") "text/html")
      (clip:process-to-string
       (load-template "partial/now-playing")
       :connection-error t
       :stats nil))))

(define-api asteroid/partial/now-playing-inline () ()
  "Get inline text with now playing info (for admin dashboard and widgets)"
  (with-error-handling
    (let ((now-playing-stats (icecast-now-playing *stream-base-url*)))
      (if now-playing-stats
          (progn
            (setf (header "Content-Type") "text/plain")
            (cdr (assoc :title now-playing-stats)))
          (progn
            (setf (header "Content-Type") "text/plain")
            "Stream Offline")))))
