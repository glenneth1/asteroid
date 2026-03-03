(in-package #:cl-streamer)

(defclass ring-buffer ()
  ((data :initarg :data :accessor buffer-data)
   (size :initarg :size :reader buffer-size)
   (read-pos :initform 0 :accessor buffer-read-pos)
   (write-pos :initform 0 :accessor buffer-write-pos)
   (lock :initform (bt:make-lock "ring-buffer-lock") :reader buffer-lock)
   (not-empty :initform (bt:make-condition-variable :name "buffer-not-empty")
              :reader buffer-not-empty)
   (not-full :initform (bt:make-condition-variable :name "buffer-not-full")
             :reader buffer-not-full)))

(defun make-ring-buffer (size)
  "Create a ring buffer with SIZE bytes capacity."
  (make-instance 'ring-buffer
                 :data (make-array size :element-type '(unsigned-byte 8))
                 :size size))

(defun buffer-available (buffer)
  "Return the number of bytes available to read."
  (bt:with-lock-held ((buffer-lock buffer))
    (let ((write (buffer-write-pos buffer))
          (read (buffer-read-pos buffer))
          (size (buffer-size buffer)))
      (mod (- write read) size))))

(defun buffer-free-space (buffer)
  "Return the number of bytes available to write."
  (- (buffer-size buffer) (buffer-available buffer) 1))

(defun buffer-write (buffer data &key (start 0) (end (length data)))
  "Write bytes from DATA to BUFFER. Blocks if buffer is full."
  (let ((len (- end start)))
    (bt:with-lock-held ((buffer-lock buffer))
      (loop while (< (buffer-free-space buffer) len)
            do (bt:condition-wait (buffer-not-full buffer) (buffer-lock buffer)))
      (let ((write-pos (buffer-write-pos buffer))
            (size (buffer-size buffer))
            (buf-data (buffer-data buffer)))
        (loop for i from start below end
              for j = write-pos then (mod (1+ j) size)
              do (setf (aref buf-data j) (aref data i))
              finally (setf (buffer-write-pos buffer) (mod (1+ j) size))))
      (bt:condition-notify (buffer-not-empty buffer))))
  len)

(defun buffer-read (buffer output &key (start 0) (end (length output)) (blocking t))
  "Read bytes from BUFFER into OUTPUT. Returns number of bytes read.
   If BLOCKING is T, waits for data. Otherwise returns 0 if empty."
  (let ((requested (- end start)))
    (bt:with-lock-held ((buffer-lock buffer))
      (when blocking
        (loop while (zerop (buffer-available buffer))
              do (bt:condition-wait (buffer-not-empty buffer) (buffer-lock buffer))))
      (let* ((available (buffer-available buffer))
             (to-read (min requested available))
             (read-pos (buffer-read-pos buffer))
             (size (buffer-size buffer))
             (buf-data (buffer-data buffer)))
        (loop for i from start below (+ start to-read)
              for j = read-pos then (mod (1+ j) size)
              do (setf (aref output i) (aref buf-data j))
              finally (setf (buffer-read-pos buffer) (mod (1+ j) size)))
        (bt:condition-notify (buffer-not-full buffer))
        to-read))))

(defun buffer-clear (buffer)
  "Clear all data from the buffer."
  (bt:with-lock-held ((buffer-lock buffer))
    (setf (buffer-read-pos buffer) 0
          (buffer-write-pos buffer) 0)
    (bt:condition-notify (buffer-not-full buffer))))
