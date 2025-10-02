;;;; user-management.lisp - User Management System for Asteroid Radio
;;;; Core user management functionality and database operations

(in-package :asteroid)

;; User roles and permissions
(defparameter *user-roles* '(:listener :dj :admin))

;; User management functions
(defun create-user (username email password &key (role :listener) (active t))
  "Create a new user account"
  (let* ((password-hash (hash-password password))
         (user-data `(("username" ,username)
                      ("email" ,email)
                      ("password-hash" ,password-hash)
                      ("role" ,(string-downcase (symbol-name role)))
                      ("active" ,(if active 1 0))
                      ("created-date" ,(local-time:timestamp-to-unix (local-time:now)))
                      ("last-login" nil))))
    (handler-case
        (db:with-transaction ()
          (format t "Inserting user data: ~a~%" user-data)
          (let ((result (db:insert "USERS" user-data)))
            (format t "Insert result: ~a~%" result)
            (format t "User created: ~a (~a)~%" username role)
            t))
      (error (e)
        (format t "Error creating user ~a: ~a~%" username e)
        nil))))

(defun find-user-by-username (username)
  "Find a user by username"
  (format t "Searching for user: ~a~%" username)
  (format t "Available collections: ~a~%" (db:collections))
  (format t "Trying to select from USERS collection...~%")
  (let ((all-users-test (db:select "USERS" (db:query :all))))
    (format t "Total users in USERS collection: ~a~%" (length all-users-test))
    (dolist (user all-users-test)
      (format t "User data: ~a~%" user)
      (format t "Username field: ~a~%" (gethash "username" user))))
  (let ((all-users (db:select "USERS" (db:query :all)))
        (users nil))
    (dolist (user all-users)
      (format t "Comparing ~a with ~a~%" (gethash "username" user) username)
      (let ((stored-username (gethash "username" user)))
        (when (equal (if (listp stored-username) (first stored-username) stored-username) username)
          (push user users))))
    (format t "Query returned ~a users~%" (length users))
    (when users 
      (format t "First user: ~a~%" (first users))
      (first users))))

(defun find-user-by-id (user-id)
  "Find a user by ID"
  (format t "Looking for user with ID: ~a (type: ~a)~%" user-id (type-of user-id))
  ;; Handle both integer and BIT types by iterating through all users
  (let ((all-users (db:select "USERS" (db:query :all)))
        (target-id (if (numberp user-id) user-id (parse-integer (format nil "~a" user-id)))))
    (format t "Searching through ~a users for ID ~a~%" (length all-users) target-id)
    (dolist (user all-users)
      (let ((db-id (gethash "_id" user)))
        (format t "Checking user with _id: ~a (type: ~a)~%" db-id (type-of db-id))
        (when (equal db-id target-id)
          (format t "Found matching user!~%")
          (return user))))))

(defun authenticate-user (username password)
  "Authenticate a user with username and password"
  (format t "Attempting to authenticate user: ~a~%" username)
  (let ((user (find-user-by-username username)))
    (format t "User found: ~a~%" (if user "YES" "NO"))
    (when user
      (handler-case
          (progn
            (format t "User active: ~a~%" (gethash "active" user))
            (format t "Password hash from DB: ~a~%" (gethash "password-hash" user))
            (format t "Password verification: ~a~%" 
                    (verify-password password (first (gethash "password-hash" user)))))
        (error (e)
          (format t "Error during user data access: ~a~%" e))))
    (when (and user
               (= (first (gethash "active" user)) 1)
               (verify-password password (first (gethash "password-hash" user))))
      ;; Update last login
      (db:update "USERS" 
                 (db:query (:= "_id" (gethash "_id" user)))
                 `(("last-login" ,(local-time:timestamp-to-unix (local-time:now)))))
      user)))

(defun hash-password (password)
  "Hash a password using ironclad"
  (let ((digest (ironclad:make-digest :sha256)))
    (ironclad:update-digest digest (babel:string-to-octets password))
    (ironclad:byte-array-to-hex-string (ironclad:produce-digest digest))))

(defun verify-password (password hash)
  "Verify a password against its hash"
  (string= (hash-password password) hash))

(defun user-has-role-p (user role)
  "Check if user has the specified role"
  (when user
    (let* ((role-field (gethash "role" user))
           (role-string (if (listp role-field) (first role-field) role-field))
           (user-role (intern (string-upcase role-string) :keyword)))
      (format t "User role: ~a, checking against: ~a~%" user-role role)
      (or (eq user-role role)
          (and (eq role :listener) (member user-role '(:dj :admin)))
          (and (eq role :dj) (eq user-role :admin))))))

(defun get-current-user ()
  "Get the currently authenticated user from session"
  (handler-case
      (let ((user-id (session:field "user-id")))
        (format t "Session user-id: ~a~%" user-id)
        (when user-id
          (let ((user (find-user-by-id user-id)))
            (format t "Found user: ~a~%" (if user "YES" "NO"))
            user)))
    (error (e)
      (format t "Error getting current user: ~a~%" e)
      nil)))

(defun require-authentication ()
  "Require user to be authenticated"
  (handler-case
      (unless (session:field "user-id")
        (radiance:redirect "/asteroid/login"))
    (error (e)
      (format t "Authentication error: ~a~%" e)
      (radiance:redirect "/asteroid/login"))))

(defun require-role (role)
  "Require user to have a specific role"
  (handler-case
      (let ((current-user (get-current-user)))
        (format t "Current user for role check: ~a~%" (if current-user "FOUND" "NOT FOUND"))
        (when current-user
          (format t "User has role ~a: ~a~%" role (user-has-role-p current-user role)))
        (unless (and current-user (user-has-role-p current-user role))
          (format t "Role check failed - redirecting to login~%")
          (radiance:redirect "/asteroid/login")))
    (error (e)
      (format t "Role check error: ~a~%" e)
      (radiance:redirect "/asteroid/login"))))

(defun update-user-role (user-id new-role)
  "Update a user's role"
  (handler-case
      (progn
        (db:update "USERS"
                   (db:query (:= "_id" user-id))
                   `(("role" ,(string-downcase (symbol-name new-role)))))
        (format t "Updated user ~a role to ~a~%" user-id new-role)
        t)
    (error (e)
      (format t "Error updating user role: ~a~%" e)
      nil)))

(defun deactivate-user (user-id)
  "Deactivate a user account"
  (handler-case
      (progn
        (db:update "USERS"
                   (db:query (:= "_id" user-id))
                   `(("active" 0)))
        (format t "Deactivated user ~a~%" user-id)
        t)
    (error (e)
      (format t "Error deactivating user: ~a~%" e)
      nil)))

(defun activate-user (user-id)
  "Activate a user account"
  (handler-case
      (progn
        (db:update "USERS"
                   (db:query (:= "_id" user-id))
                   `(("active" 1)))
        (format t "Activated user ~a~%" user-id)
        t)
    (error (e)
      (format t "Error activating user: ~a~%" e)
      nil)))

(defun get-all-users ()
  "Get all users from database"
  (format t "Getting all users from database...~%")
  (let ((users (db:select "USERS" (db:query :all))))
    (format t "Total users in database: ~a~%" (length users))
    (dolist (user users)
      (format t "User: ~a~%" user)
      (format t "User _id field: ~a (type: ~a)~%" (gethash "_id" user) (type-of (gethash "_id" user))))
    users))

(defun get-user-stats ()
  "Get user statistics"
  (let ((all-users (get-all-users)))
    `(("total-users" . ,(length all-users))
      ("active-users" . ,(count-if (lambda (user) (gethash "active" user)) all-users))
      ("listeners" . ,(count-if (lambda (user) 
                                  (let ((role (gethash "role" user)))
                                    (string= (if (listp role) (first role) role) "listener"))) all-users))
      ("djs" . ,(count-if (lambda (user) 
                            (let ((role (gethash "role" user)))
                              (string= (if (listp role) (first role) role) "dj"))) all-users))
      ("admins" . ,(count-if (lambda (user) 
                               (let ((role (gethash "role" user)))
                                 (string= (if (listp role) (first role) role) "admin"))) all-users)))))

(defun create-default-admin ()
  "Create default admin user if no admin exists"
  (let ((existing-admins (remove-if-not 
                          (lambda (user) 
                            (let ((role (gethash "role" user)))
                              (string= (if (listp role) (first role) role) "admin")))
                          (get-all-users))))
    (unless existing-admins
      (format t "~%Creating default admin user...~%")
      (format t "Username: admin~%")
      (format t "Password: asteroid123~%")
      (format t "Please change this password after first login!~%~%")
      (create-user "admin" "admin@asteroid.radio" "asteroid123" :role :admin :active t))))

(defun initialize-user-system ()
  "Initialize the user management system"
  (format t "Initializing user management system...~%")
  ;; Try immediate initialization first
  (handler-case
      (progn
        (format t "Setting up user management...~%")
        (create-default-admin)
        (format t "User management initialization complete.~%"))
    (error (e)
      (format t "Database not ready, will retry in background: ~a~%" e)
      ;; Fallback to delayed initialization
      (bt:make-thread 
       (lambda ()
         (sleep 3) ; Give database more time to initialize
         (handler-case
             (progn
               (format t "Retrying user management setup...~%")
               (create-default-admin)
               (format t "User management initialization complete.~%"))
           (error (e)
             (format t "Error initializing user system: ~a~%" e))))
       :name "user-init"))))
