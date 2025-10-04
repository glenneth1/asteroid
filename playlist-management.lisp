;;;; playlist-management.lisp - Playlist Management for Asteroid Radio
;;;; Database operations and functions for user playlists

(in-package :asteroid)

;; Playlist management functions

(defun create-playlist (user-id name &optional description)
  "Create a new playlist for a user"
  (unless (db:collection-exists-p "playlists")
    (error "Playlists collection does not exist in database"))
  
  (let ((playlist-data `(("user-id" ,user-id)
                         ("name" ,name)
                         ("description" ,(or description ""))
                         ("track-ids" "")  ; Empty string for text field
                         ("created-date" ,(local-time:timestamp-to-unix (local-time:now))))))
    (format t "Creating playlist with user-id: ~a (type: ~a)~%" user-id (type-of user-id))
    (format t "Playlist data: ~a~%" playlist-data)
    (db:insert "playlists" playlist-data)
    t))

(defun get-user-playlists (user-id)
  "Get all playlists for a user"
  (format t "Querying playlists with user-id: ~a (type: ~a)~%" user-id (type-of user-id))
  (let ((all-playlists (db:select "playlists" (db:query :all))))
    (format t "Total playlists in database: ~a~%" (length all-playlists))
    (when (> (length all-playlists) 0)
      (let ((first-playlist (first all-playlists)))
        (format t "First playlist user-id: ~a (type: ~a)~%" 
                (gethash "user-id" first-playlist)
                (type-of (gethash "user-id" first-playlist)))))
    ;; Filter manually since DB stores user-id as a list (2) instead of 2
    (remove-if-not (lambda (playlist)
                     (let ((stored-user-id (gethash "user-id" playlist)))
                       (or (equal stored-user-id user-id)
                           (and (listp stored-user-id) 
                                (equal (first stored-user-id) user-id)))))
                   all-playlists)))

(defun get-playlist-by-id (playlist-id)
  "Get a specific playlist by ID"
  (format t "get-playlist-by-id called with: ~a (type: ~a)~%" playlist-id (type-of playlist-id))
  ;; Try direct query first
  (let ((playlists (db:select "playlists" (db:query (:= "_id" playlist-id)))))
    (if (> (length playlists) 0)
        (progn
          (format t "Found via direct query~%")
          (first playlists))
        ;; If not found, search manually (ID might be stored as list)
        (let ((all-playlists (db:select "playlists" (db:query :all))))
          (format t "Searching through ~a playlists manually~%" (length all-playlists))
          (find-if (lambda (playlist)
                     (let ((stored-id (gethash "_id" playlist)))
                       (format t "Checking playlist _id: ~a (type: ~a)~%" stored-id (type-of stored-id))
                       (or (equal stored-id playlist-id)
                           (and (listp stored-id) (equal (first stored-id) playlist-id)))))
                   all-playlists)))))

(defun add-track-to-playlist (playlist-id track-id)
  "Add a track to a playlist"
  (let ((playlist (get-playlist-by-id playlist-id)))
    (when playlist
      (let* ((current-track-ids-raw (gethash "track-ids" playlist))
             ;; Handle database storing as list - extract string
             (current-track-ids (if (listp current-track-ids-raw)
                                   (first current-track-ids-raw)
                                   current-track-ids-raw))
             ;; Parse comma-separated string into list
             (tracks-list (if (and current-track-ids 
                                  (stringp current-track-ids)
                                  (not (string= current-track-ids "")))
                             (mapcar #'parse-integer 
                                    (cl-ppcre:split "," current-track-ids))
                             nil))
             (new-tracks (append tracks-list (list track-id)))
             ;; Convert back to comma-separated string
             (track-ids-str (format nil "~{~a~^,~}" new-tracks)))
        (format t "Adding track ~a to playlist ~a~%" track-id playlist-id)
        (format t "Current track-ids raw: ~a (type: ~a)~%" current-track-ids-raw (type-of current-track-ids-raw))
        (format t "Current track-ids: ~a~%" current-track-ids)
        (format t "Tracks list: ~a~%" tracks-list)
        (format t "New tracks: ~a~%" new-tracks)
        (format t "Track IDs string: ~a~%" track-ids-str)
        ;; Update using track-ids field (defined in schema)
        (db:update "playlists"
                   (db:query (:= "_id" playlist-id))
                   `(("track-ids" ,track-ids-str)))
        (format t "Update complete~%")
        t))))

(defun remove-track-from-playlist (playlist-id track-id)
  "Remove a track from a playlist"
  (let ((playlist (get-playlist-by-id playlist-id)))
    (when playlist
      (let* ((current-track-ids-raw (gethash "track-ids" playlist))
             ;; Handle database storing as list - extract string
             (current-track-ids (if (listp current-track-ids-raw)
                                   (first current-track-ids-raw)
                                   current-track-ids-raw))
             ;; Parse comma-separated string into list
             (tracks-list (if (and current-track-ids 
                                  (stringp current-track-ids)
                                  (not (string= current-track-ids "")))
                             (mapcar #'parse-integer 
                                    (cl-ppcre:split "," current-track-ids))
                             nil))
             (new-tracks (remove track-id tracks-list :test #'equal))
             ;; Convert back to comma-separated string
             (track-ids-str (format nil "~{~a~^,~}" new-tracks)))
        (db:update "playlists"
                   (db:query (:= "_id" playlist-id))
                   `(("track-ids" ,track-ids-str)))
        t))))

(defun delete-playlist (playlist-id)
  "Delete a playlist"
  (db:remove "playlists" (db:query (:= "_id" playlist-id)))
  t)

(defun ensure-playlists-collection ()
  "Ensure playlists collection exists in database"
  (unless (db:collection-exists-p "playlists")
    (format t "Creating playlists collection...~%")
    (db:create "playlists"))
  
  ;; Debug: Print the actual structure
  (format t "~%=== PLAYLISTS COLLECTION STRUCTURE ===~%")
  (format t "Structure: ~a~%~%" (db:structure "playlists"))
  
  ;; Debug: Check existing playlists
  (let ((playlists (db:select "playlists" (db:query :all))))
    (when playlists
      (format t "Sample playlist fields: ~{~a~^, ~}~%~%" 
              (alexandria:hash-table-keys (first playlists))))))
