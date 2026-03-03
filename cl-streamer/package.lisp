(defpackage #:cl-streamer
  (:use #:cl #:alexandria)
  (:export
   ;; Conditions
   #:streamer-error
   #:connection-error
   #:encoding-error
   
   ;; Buffer
   #:ring-buffer
   #:make-ring-buffer
   #:buffer-write
   #:buffer-read
   #:buffer-available
   #:buffer-clear
   
   ;; ICY Protocol
   #:icy-metadata
   #:make-icy-metadata
   #:icy-metadata-title
   #:icy-metadata-url
   #:encode-icy-metadata
   #:icy-metaint
   
   ;; Stream Server
   #:stream-server
   #:make-stream-server
   #:start-server
   #:stop-server
   #:server-running-p
   #:add-mount
   #:remove-mount
   #:update-metadata
   #:listener-count
   
   ;; Main API
   #:*default-port*
   #:*default-metaint*))
