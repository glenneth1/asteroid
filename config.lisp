;; -*-lisp-*-
;; Configuration management for Asteroid Radio
;; This file centralizes all configuration parameters and provides
;; mechanisms to load them from environment variables or config files

(in-package :asteroid)

;;; Configuration Structure
;;; All configuration is loaded from environment variables with sensible defaults

(defclass asteroid-config ()
  ((server-port
    :initarg :server-port
    :accessor config-server-port
    :initform 8080
    :documentation "HTTP server port")
   
   (music-library-path
    :initarg :music-library-path
    :accessor config-music-library-path
    :initform nil
    :documentation "Path to music library directory")
   
   (stream-base-url
    :initarg :stream-base-url
    :accessor config-stream-base-url
    :initform "http://localhost:8000"
    :documentation "Base URL for Icecast stream server")
   
   (icecast-admin-user
    :initarg :icecast-admin-user
    :accessor config-icecast-admin-user
    :initform "admin"
    :documentation "Icecast admin username")
   
   (icecast-admin-password
    :initarg :icecast-admin-password
    :accessor config-icecast-admin-password
    :initform nil
    :documentation "Icecast admin password (MUST be set via environment)")
   
   (supported-formats
    :initarg :supported-formats
    :accessor config-supported-formats
    :initform '("mp3" "flac" "ogg" "wav")
    :documentation "List of supported audio formats")
   
   (max-history-size
    :initarg :max-history-size
    :accessor config-max-history-size
    :initform 50
    :documentation "Maximum number of tracks in stream history")
   
   (database-backend
    :initarg :database-backend
    :accessor config-database-backend
    :initform :i-lambdalite
    :documentation "Database backend to use (:i-lambdalite or :postgresql)")
   
   (postgres-host
    :initarg :postgres-host
    :accessor config-postgres-host
    :initform "localhost"
    :documentation "PostgreSQL host")
   
   (postgres-port
    :initarg :postgres-port
    :accessor config-postgres-port
    :initform 5432
    :documentation "PostgreSQL port")
   
   (postgres-database
    :initarg :postgres-database
    :accessor config-postgres-database
    :initform "asteroid"
    :documentation "PostgreSQL database name")
   
   (postgres-user
    :initarg :postgres-user
    :accessor config-postgres-user
    :initform "asteroid"
    :documentation "PostgreSQL username")
   
   (postgres-password
    :initarg :postgres-password
    :accessor config-postgres-password
    :initform nil
    :documentation "PostgreSQL password (MUST be set via environment)")
   
   (tls-enabled
    :initarg :tls-enabled
    :accessor config-tls-enabled
    :initform nil
    :documentation "Whether TLS/HTTPS is enabled")
   
   (tls-certificate-path
    :initarg :tls-certificate-path
    :accessor config-tls-certificate-path
    :initform nil
    :documentation "Path to TLS certificate file")
   
   (tls-key-path
    :initarg :tls-key-path
    :accessor config-tls-key-path
    :initform nil
    :documentation "Path to TLS private key file"))
  (:documentation "Configuration object for Asteroid Radio"))

;;; Global configuration instance
(defvar *config* nil
  "Global configuration instance")

;;; Environment variable helpers
(defun getenv (name &optional default)
  "Get environment variable NAME, or DEFAULT if not set"
  (let ((value (uiop:getenv name)))
    (if (and value (not (string= value "")))
        value
        default)))

(defun parse-integer-safe (string &optional default)
  "Parse STRING as integer, return DEFAULT on error"
  (handler-case
      (parse-integer string)
    (error () default)))

(defun parse-boolean (string)
  "Parse STRING as boolean (true/false, yes/no, 1/0)"
  (when string
    (member (string-downcase string) 
            '("true" "yes" "1" "t" "y")
            :test #'string=)))

;;; Configuration loading
(defun load-config-from-env ()
  "Load configuration from environment variables"
  (make-instance 'asteroid-config
    :server-port (parse-integer-safe 
                  (getenv "ASTEROID_SERVER_PORT") 
                  8080)
    
    :music-library-path (or (getenv "ASTEROID_MUSIC_PATH")
                            (merge-pathnames "music/library/" 
                                           (asdf:system-source-directory :asteroid)))
    
    :stream-base-url (getenv "ASTEROID_STREAM_URL" "http://localhost:8000")
    
    :icecast-admin-user (getenv "ICECAST_ADMIN_USER" "admin")
    
    :icecast-admin-password (getenv "ICECAST_ADMIN_PASSWORD")
    
    :supported-formats '("mp3" "flac" "ogg" "wav")
    
    :max-history-size (parse-integer-safe 
                       (getenv "ASTEROID_MAX_HISTORY") 
                       50)
    
    :database-backend (let ((backend (getenv "ASTEROID_DB_BACKEND")))
                        (cond
                          ((string-equal backend "postgresql") :postgresql)
                          ((string-equal backend "postgres") :postgresql)
                          (t :i-lambdalite)))
    
    :postgres-host (getenv "POSTGRES_HOST" "localhost")
    
    :postgres-port (parse-integer-safe 
                    (getenv "POSTGRES_PORT") 
                    5432)
    
    :postgres-database (getenv "POSTGRES_DB" "asteroid")
    
    :postgres-user (getenv "POSTGRES_USER" "asteroid")
    
    :postgres-password (getenv "POSTGRES_PASSWORD")
    
    :tls-enabled (parse-boolean (getenv "ASTEROID_TLS_ENABLED"))
    
    :tls-certificate-path (getenv "ASTEROID_TLS_CERT")
    
    :tls-key-path (getenv "ASTEROID_TLS_KEY")))

(defun init-config ()
  "Initialize global configuration"
  (unless *config*
    (setf *config* (load-config-from-env)))
  
  ;; Validate critical configuration
  (validate-config *config*)
  
  *config*)

(defun validate-config (config)
  "Validate configuration and warn about missing critical values"
  (unless (config-icecast-admin-password config)
    (warn "ICECAST_ADMIN_PASSWORD not set! Icecast status checks will fail."))
  
  (when (eq (config-database-backend config) :postgresql)
    (unless (config-postgres-password config)
      (warn "POSTGRES_PASSWORD not set! PostgreSQL connection will fail.")))
  
  (when (config-tls-enabled config)
    (unless (and (config-tls-certificate-path config)
                 (config-tls-key-path config))
      (error "TLS enabled but certificate or key path not configured!")))
  
  t)

;;; Convenience accessors for backward compatibility
(defun get-config-value (key)
  "Get configuration value by key (for backward compatibility)"
  (unless *config*
    (init-config))
  
  (case key
    (:server-port (config-server-port *config*))
    (:music-library-path (config-music-library-path *config*))
    (:stream-base-url (config-stream-base-url *config*))
    (:icecast-admin-user (config-icecast-admin-user *config*))
    (:icecast-admin-password (config-icecast-admin-password *config*))
    (:supported-formats (config-supported-formats *config*))
    (otherwise (error "Unknown configuration key: ~a" key))))

;;; Export configuration for display (without sensitive data)
(defun config-summary ()
  "Return configuration summary (without passwords)"
  (unless *config*
    (init-config))
  
  `(("Server Port" . ,(config-server-port *config*))
    ("Music Library" . ,(namestring (config-music-library-path *config*)))
    ("Stream URL" . ,(config-stream-base-url *config*))
    ("Icecast Admin User" . ,(config-icecast-admin-user *config*))
    ("Icecast Password Set" . ,(if (config-icecast-admin-password *config*) "Yes" "No"))
    ("Supported Formats" . ,(format nil "~{~a~^, ~}" (config-supported-formats *config*)))
    ("Database Backend" . ,(config-database-backend *config*))
    ("PostgreSQL Host" . ,(config-postgres-host *config*))
    ("PostgreSQL Port" . ,(config-postgres-port *config*))
    ("PostgreSQL Database" . ,(config-postgres-database *config*))
    ("PostgreSQL User" . ,(config-postgres-user *config*))
    ("PostgreSQL Password Set" . ,(if (config-postgres-password *config*) "Yes" "No"))
    ("TLS Enabled" . ,(if (config-tls-enabled *config*) "Yes" "No"))))
