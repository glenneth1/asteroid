(in-package :asteroid)

;; Database connection parameters for direct postmodern queries
(defun get-db-connection-params ()
  "Get database connection parameters for postmodern"
  (list (or (uiop:getenv "ASTEROID_DB_NAME") "asteroid")
        (or (uiop:getenv "ASTEROID_DB_USER") "asteroid")
        (or (uiop:getenv "ASTEROID_DB_PASSWORD") "asteroid_db_2025")
        (or (uiop:getenv "ASTEROID_DB_HOST") "localhost")
        :port (parse-integer (or (uiop:getenv "ASTEROID_DB_PORT") "5432"))))

(defmacro with-db (&body body)
  "Execute body with database connection"
  `(postmodern:with-connection (get-db-connection-params)
     ,@body))

;; Database initialization - must be in db:connected trigger because
;; the system could load before the database is ready.

(define-trigger db:connected ()
  "Initialize database collections when database connects"
  (unless (db:collection-exists-p "tracks")
    (db:create "tracks" '((title :text)
                          (artist :text)
                          (album :text)
                          (duration :integer)
                          (file-path :text)
                          (format :text)
                          (bitrate :integer)
                          (added-date :integer)
                          (play-count :integer))))
  
  (unless (db:collection-exists-p "playlists")
    (db:create "playlists" '((name :text)
                             (description :text)
                             (created-date :integer)
                             (user-id :integer)
                             (track-ids :text))))
  
  (unless (db:collection-exists-p "USERS")
    (db:create "USERS" '((username :text)
                         (email :text)
                         (password-hash :text)
                         (role :text)
                         (active :integer)
                         (created-date :integer)
                         (last-login :integer))))

  (unless (db:collection-exists-p "playlist_tracks")
    (db:create "playlist_tracks" '((track_id :integer)
                                   (position :integer)
                                   (added_date :integer))))

  (unless (db:collection-exists-p "user_favorites")
    (db:create "user_favorites" '((user-id :integer)
                                  (track-id :integer)
                                  (rating :integer)
                                  (created-date :integer))))

  (unless (db:collection-exists-p "listening_history")
    (db:create "listening_history" '((user-id :integer)
                                     (track-id :integer)
                                     (listened-at :integer)
                                     (listen-duration :integer)
                                     (completed :integer))))

  ;; TODO: the radiance db interface is too basic to contain anything
  ;; but strings, integers, booleans, and maybe timestamps... we will
  ;; need to rethink this. currently track/playlist relationships are
  ;; defined in the SQL file 'init-db.sql' referenced in the docker
  ;; config for postgresql, but our lisp code doesn't leverage it.
  
  ;; (unless (db:collection-exists-p "sessions")
  ;;   (db:create "sessions" '(())))
  
  (format t "~2&Database collections initialized~%"))

(defun data-model-as-alist (model)
  "Converts a radiance data-model instance into a alist"
  (unless (dm:hull-p model)
    (loop for field in (dm:fields model)
          collect (cons field (dm:field model field)))))

(defun lambdalite-db-p ()
  "Checks if application is using lambdalite as database backend"
  (string= (string-upcase (package-name (db:implementation)))
           "I-LAMBDALITE"))

(defun data-model-save (data-model)
  "Wrapper on data-model save method to bypass error using dm:save on lambdalite.
It uses the same approach as dm:save under the hood through db:save."
  (if (lambdalite-db-p)
      (progn
        (format t "Updating lambdalite collection '~a'~%" (dm:collection data-model))
        (db:update (dm:collection data-model)
                   (db:query (:= '_id (dm:id data-model)))
                   (dm:field-table data-model)))
      (progn
        (format t "Updating database table '~a'~%" (dm:collection data-model))
        (dm:save data-model))))
