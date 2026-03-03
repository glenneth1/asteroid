;;;; user-management.lisp - User Management System for Asteroid Radio
;;;; Core user management functionality and database operations

(in-package :asteroid)

;; User roles and permissions
(defparameter *user-roles* '(:listener :dj :admin))

;; User management functions
(defun create-user (username email password &key (role :listener) (active t))
  "Create a new user account"
  (let ((user (dm:hull "USERS"))
        (password-hash (hash-password password)))
    (setf (dm:field user "username") username
          (dm:field user "email") email
          (dm:field user "password-hash") password-hash
          (dm:field user "role") (string-downcase (symbol-name role))
          (dm:field user "active") (if active 1 0)
          ;; Let database defaults handle created-date (CURRENT_TIMESTAMP)
          (dm:field user "last-login") nil)
    (handler-case
        (db:with-transaction ()
          (let ((result (dm:insert user)))
            (log:info "User created: ~a (~a)" username role)
            t))
      (error (e)
        (log:warn "Error creating user ~a: ~a" username e)
        nil))))

(defun find-user-by-username (username)
  "Find a user by username"
  (let ((user (dm:get-one "USERS" (db:query (:= 'username username)))))
    user))

(defun find-user-by-id (user-id)
  "Find a user by ID"
  (let* ((user-id (if (stringp user-id)
                      (parse-integer user-id)
                      user-id))
         (user (dm:get-one "USERS" (db:query (:= '_id user-id)))))
    user))


(defun authenticate-user (username password)
  "Authenticate a user with username and password"
  (log:info "Authentication attempt for user: ~a" username)
  (let ((user (find-user-by-username username)))
    (when user
      (let ((user-active (dm:field user "active"))
            (user-password (dm:field user "password-hash")))
        (handler-case
            (verify-password password user-password)
          (error (e)
            (log:warn "Error during user authentication: ~a" e)))
        (when (and (= 1 user-active)
                   (verify-password password user-password))
          ;; Update last login using data-model (database agnostic)
          (handler-case
              (progn
                (setf (dm:field user "last-login") 
                      (format-timestamp-iso8601 (local-time:now)))
                ;; Use data-model-save to normalize all timestamp fields before saving
                (data-model-save user))
            (error (e)
              (log:warn "Could not update last-login: ~a" e)))
          user)))))

(defun hash-password (password)
  "Hash a password using ironclad"
  (let ((digest (ironclad:make-digest :sha256)))
    (ironclad:update-digest digest (babel:string-to-octets password))
    (ironclad:byte-array-to-hex-string (ironclad:produce-digest digest))))

(defun verify-password (password hash)
  "Verify a password against its hash"
  (let ((computed-hash (hash-password password)))
    (string= computed-hash hash)))

(defun reset-user-password (username new-password)
  "Reset a user's password"
  (let ((user (find-user-by-username username)))
    (if user
        (handler-case
            (let ((new-hash (hash-password new-password))
                  (user-id (dm:id user)))
              (log:info "Resetting password for user: ~a" username)
              ;; Try direct update with uppercase field name to match stored case
              (setf (dm:field user "password-hash") new-hash)
              ;; (dm:save user)
              (data-model-save user)
              ;; Verify the update worked
              (let ((updated-user (find-user-by-username username)))
                (let ((updated-hash (dm:field updated-user "password-hash")))
                  (if (string= updated-hash new-hash)
                      (progn
                        (log:info "Password reset successful for user: ~a" username)
                        t)
                      (progn
                        (log:warn "Password reset FAILED for user: ~a" username)
                        nil)))))
          (error (e)
            (log:warn "Error resetting password for ~a: ~a" username e)
            nil))
        (progn
          (log:warn "User not found for password reset: ~a" username)
          nil))))

(defun user-has-role-p (user role)
  "Check if user has the specified role"
  (when user
    (let* ((role-value (dm:field user "role"))
           (user-role (intern (string-upcase role-value) :keyword)))
      (or (eq user-role role)
          (and (eq role :listener) (member user-role '(:dj :admin)))
          (and (eq role :dj) (eq user-role :admin))))))

(defun get-current-user ()
  "Get the currently authenticated user from session"
  (handler-case
      (let ((user-id (session:field "user-id")))
        (when user-id
          (find-user-by-id user-id)))
    (error (e)
      (log:warn "Error getting current user: ~a" e)
      nil)))

(defun get-current-user-id ()
  "Get the currently authenticated user's ID from session"
  (session:field "user-id"))

(defun get-auth-state-js-var ()
  "Builds a JavaScript variable definition with the current authentication state
   for a request. The variable definition is a string ready to be injected in a
   template file."
  (let ((user (get-current-user)))
    (format nil "var AUTHSTATE = ~a"
            (cl-json:encode-json-to-string
             `(("loggedIn" .  ,(when user t))
               ("isAdmin" . ,(when (and user (user-has-role-p user :admin)) t))
               ("username" . ,(when user (dm:field user "username"))))))))

(defun require-authentication (&key (api nil))
  "Require user to be authenticated. 
   Returns T if authenticated, NIL if not (after emitting error response).
   If :api t, returns JSON error (401). Otherwise redirects to login page.
   Auto-detects API routes if not specified."
  (let* ((user-id (session:field "user-id"))
         (uri (radiance:path (radiance:uri *request*)))
         ;; Use explicit flag if provided, otherwise auto-detect from URI
         ;; Check for "api/" anywhere in the path
         (is-api-request (if api t (or (search "/api/" uri)
                                       (search "api/" uri)))))
    (if user-id
        t  ; Authenticated - return T to continue
        ;; Not authenticated - emit error and signal to stop processing
        (progn
          (if is-api-request
              ;; API request - return JSON error with 401 status using api-output
              (progn
                (api-output `(("status" . "error")
                              ("message" . "Authentication required"))
                            :status 401))
              ;; Page request - redirect to login
              (radiance:redirect "/login"))))))

(defun require-role (role &key (api nil))
  "Require user to have a specific role.
   Returns T if authorized, NIL if not (after emitting error response).
   If :api t, returns JSON error (403). Otherwise redirects to login page.
   Auto-detects API routes if not specified."
  (let* ((current-user (get-current-user))
         (uri (radiance:path (radiance:uri *request*)))
         ;; Use explicit flag if provided, otherwise auto-detect from URI
         ;; Check both "/api/" and "api/" since path may or may not have leading slash
         (is-api-request (if api t (or (search "/api/" uri)
                                       (and (>= (length uri) 4)
                                            (string= "api/" (subseq uri 0 4)))))))
    (if (and current-user (user-has-role-p current-user role))
        t  ; Authorized - return T to continue
        ;; Not authorized - emit error
        (if is-api-request
            ;; API request - return NIL (caller will handle JSON error)
            nil
            ;; Page request - redirect to login (redirect doesn't return)
            (radiance:redirect "/asteroid/login")))))

(defun update-user-role (user-id new-role)
  "Update a user's role"
  (handler-case
      (let ((user (find-user-by-id user-id)))
        (if user
            (progn
              (setf (dm:field user "role") (string-downcase (symbol-name new-role)))
              ;; (dm:save user)
              (data-model-save user)
              t)
            (log:warn "Could not find user with id #~a" user-id)))
    (error (e)
      (log:warn "Error updating user role: ~a" e)
      nil)))

(defun deactivate-user (user-id)
  "Deactivate a user account"
  (handler-case
      (let ((user (find-user-by-id user-id)))
        (setf (dm:field user "active") 0)
        ;; (dm:save user)
        (data-model-save user)
        (log:info "Deactivated user ~a" user-id)
        t)
    (error (e)
      (log:warn "Error deactivating user: ~a" e)
      nil)))

(defun activate-user (user-id)
  "Activate a user account"
  (handler-case
      (let ((user (find-user-by-id user-id)))
        (setf (dm:field user "active") 1)
        ;; (dm:save user)
        (data-model-save user)
        (log:info "Activated user ~a" user-id)
        t)
    (error (e)
      (log:warn "Error activating user: ~a" e)
      nil)))

(defun get-all-users ()
  "Get all users from database"
  (dm:get "USERS" (db:query :all)))

(defun get-user-stats ()
  "Get user statistics"
  (let ((all-users (get-all-users)))
    `(("total-users" . ,(length all-users))
      ("active-users" . ,(count-if (lambda (user) (= 1 (dm:field user "active"))) all-users))
      ("listeners" . ,(count-if (lambda (user) 
                                  (let ((role (dm:field user "role")))
                                    (string= role "listener"))) all-users))
      ("djs" . ,(count-if (lambda (user) 
                            (let ((role (dm:field user "role")))
                              (string= role "dj"))) all-users))
      ("admins" . ,(count-if (lambda (user) 
                               (let ((role (dm:field user "role")))
                                 (string= role "admin"))) all-users)))))

(defun create-default-admin ()
  "Create default admin user if no admin exists"
  (let ((existing-admins (remove-if-not 
                          (lambda (user) 
                            (let ((role (dm:field user "role")))
                              (string= role "admin")))
                          (get-all-users))))
    (unless existing-admins
      (log:info "Creating default admin user (admin / asteroid123) - change password after first login!")
      (create-user "admin" "admin@asteroid.radio" "asteroid123" :role :admin :active t))))

(defun initialize-user-system ()
  "Initialize the user management system"
  (log:info "Initializing user management system")
  ;; Try immediate initialization first
  #+nil
  (handler-case
      (progn
        (create-default-admin)
        (log:info "User management initialization complete."))
    (error (e)
      (log:info "Database not ready, will retry in background: ~a" e)
      ;; Fallback to delayed initialization
      (bt:make-thread 
       (lambda ()
         (dotimes (a 5)
           (unless (db:connected-p)
             (sleep 3)) ; Give database more time to initialize
           (handler-case
               (progn
                 (create-default-admin)
                 (log:info "User management initialization complete.")
                 (return))
             (error (e)
               (log:warn "Error initializing user system: ~a" e)))))
       :name "user-init"))))

(defun dump-users (users)
  (with-open-file (s "userdump.csv" :direction :output :if-exists :supersede)
    (loop for user in users
          do
             (let* ((_id (dm:field user "_id"))
                    (username (dm:field user "username"))
                    (email (dm:field user "email"))
                    (password-hash (dm:field user "password-hash"))
                    (role (dm:field user "role"))
                    (active (dm:field user "active"))
                    (created-date (dm:field user "created-date"))
                    (last-login (dm:field user "last-login")))
               (format s "~&~{~A~^,~}~%" (list _id username email password-hash role active created-date last-login))
               (finish-output s)))))

(defun get-users-from-csv (filename)
  (with-open-file (s filename :direction :input)
    (let ((csv-data (cl-csv:read-csv s)))
      csv-data)))

(defun ensure-users (filename)
  (let* ((users (get-users-from-csv filename)))
    (princ users)
    (loop 
      for (_id username email password-hash role active created-date last-login) in users
      do
         (progn
           (format t "~&_id: ~A, username: ~A, email: ~A password-hash: ~A role: ~A active: ~A created-date: ~A  last-login: ~A" _id username email password-hash role active created-date last-login)
           (let ((user (dm:hull "USERS")))
             (setf (dm:field user "username") username)
             (setf (dm:field user "email") email)
             (setf (dm:field user "password-hash") password-hash)
             (setf (dm:field user "role") role)
             (setf (dm:field user "active") active)
             (setf (dm:field user "created-date") created-date)
             (setf (dm:field user "last-login") nil)

             (handler-case
                 (db:with-transaction ()
                   (format t "Inserting user: ~A~%" user)
                   (let ((result (dm:insert user)))
                     (format t "Insert result: ~A~%" result)
                     (format t "User created: ~A (~A)~%" username role)))
               (error (e)
                 (format t "Error creating user ~A: ~A~%" username e))))))))
