;;;; user-profile.lisp - User profile features: favorites, listening history
;;;; Part of Asteroid Radio

(in-package #:asteroid)

;;; ==========================================================================
;;; User Favorites - Track likes/ratings
;;; ==========================================================================

(defun add-favorite (user-id track-id &optional (rating 1) track-title)
  "Add a track to user's favorites with optional rating (1-5).
   If track-id is nil but track-title is provided, stores by title."
  (let ((rating-val (max 1 (min 5 (or rating 1)))))
    (with-db
      (if track-id
          (postmodern:query
           (:raw (format nil "INSERT INTO user_favorites (\"user-id\", \"track-id\", track_title, rating) VALUES (~a, ~a, ~a, ~a)"
                         user-id track-id 
                         (if track-title (format nil "$$~a$$" track-title) "NULL")
                         rating-val)))
          ;; No track-id, store by title only
          (when track-title
            (postmodern:query
             (:raw (format nil "INSERT INTO user_favorites (\"user-id\", track_title, rating) VALUES (~a, $$~a$$, ~a)"
                           user-id track-title rating-val))))))))

(defun remove-favorite (user-id track-id &optional track-title)
  "Remove a track from user's favorites by track-id or title"
  (with-db
    (if track-id
        (postmodern:query
         (:raw (format nil "DELETE FROM user_favorites WHERE \"user-id\" = ~a AND \"track-id\" = ~a"
                       user-id track-id)))
        (when track-title
          (postmodern:query
           (:raw (format nil "DELETE FROM user_favorites WHERE \"user-id\" = ~a AND track_title = $$~a$$"
                         user-id track-title)))))))

(defun update-favorite-rating (user-id track-id rating)
  "Update the rating for a favorited track"
  (let ((rating-val (max 1 (min 5 rating))))
    (with-db
      (postmodern:query
       (:update 'user_favorites
        :set 'rating rating-val
        :where (:and (:= '"user-id" user-id)
                     (:= '"track-id" track-id)))))))

(defun get-user-favorites (user-id &key (limit 50) (offset 0))
  "Get user's favorite tracks - works with both track-id and title-based favorites"
  (with-db
    (postmodern:query
     (:raw (format nil "SELECT _id, rating, \"created-date\", track_title, \"track-id\" FROM user_favorites WHERE \"user-id\" = ~a ORDER BY \"created-date\" DESC LIMIT ~a OFFSET ~a"
                   user-id limit offset))
     :alists)))

(defun is-track-favorited (user-id track-id)
  "Check if a track is in user's favorites, returns rating or nil"
  (with-db
    (postmodern:query
     (:raw (format nil "SELECT rating FROM user_favorites WHERE \"user-id\" = ~a AND \"track-id\" = ~a"
                   user-id track-id))
     :single)))

(defun get-favorites-count (user-id)
  "Get total count of user's favorites"
  (with-db
    (postmodern:query
     (:raw (format nil "SELECT COUNT(*) FROM user_favorites WHERE \"user-id\" = ~a" user-id))
     :single)))

;;; ==========================================================================
;;; Listening History - Per-user track play history
;;; ==========================================================================

(defun sql-escape-string (str)
  "Escape a string for SQL by doubling single quotes"
  (if str
      (cl-ppcre:regex-replace-all "'" str "''")
      ""))

(defun record-listen (user-id &key track-id track-title (duration 0) (completed nil))
  "Record a track listen in user's history. Can use track-id or track-title."
  (with-db
    (if track-id
        (postmodern:query
         (:raw (format nil "INSERT INTO listening_history (\"user-id\", \"track-id\", track_title, \"listen-duration\", completed) VALUES (~a, ~a, ~a, ~a, ~a)"
                       user-id track-id
                       (if track-title (format nil "'~a'" (sql-escape-string track-title)) "NULL")
                       duration (if completed 1 0))))
        (when track-title
          (postmodern:query
           (:raw (format nil "INSERT INTO listening_history (\"user-id\", track_title, \"listen-duration\", completed) VALUES (~a, '~a', ~a, ~a)"
                         user-id (sql-escape-string track-title) duration (if completed 1 0))))))))

(defun get-listening-history (user-id &key (limit 20) (offset 0))
  "Get user's listening history - works with title-based history"
  (with-db
    (postmodern:query
     (:raw (format nil "SELECT _id, \"listened-at\", \"listen-duration\", completed, track_title, \"track-id\" FROM listening_history WHERE \"user-id\" = ~a ORDER BY \"listened-at\" DESC LIMIT ~a OFFSET ~a"
                   user-id limit offset))
     :alists)))

(defun get-listening-stats (user-id)
  "Get aggregate listening statistics for a user"
  (with-db
    (let ((stats (postmodern:query
                  (:raw (format nil "SELECT COUNT(*), COALESCE(SUM(\"listen-duration\"), 0) FROM listening_history WHERE \"user-id\" = ~a" user-id))
                  :row)))
      (list :tracks-played (or (first stats) 0)
            :total-listen-time (or (second stats) 0)))))

(defun get-top-artists (user-id &key (limit 5))
  "Get user's most listened artists - extracts artist from track_title"
  (with-db
    ;; Extract artist from 'Artist - Title' format in track_title
    (postmodern:query
     (:raw (format nil "SELECT SPLIT_PART(track_title, ' - ', 1) as artist, COUNT(*) as play_count FROM listening_history WHERE \"user-id\" = ~a AND track_title IS NOT NULL GROUP BY SPLIT_PART(track_title, ' - ', 1) ORDER BY play_count DESC LIMIT ~a"
                   user-id limit))
     :alists)))

(defun clear-listening-history (user-id)
  "Clear all listening history for a user"
  (with-db
    (postmodern:query
     (:raw (format nil "DELETE FROM listening_history WHERE \"user-id\" = ~a" user-id)))))

(defun get-listening-activity (user-id &key (days 30))
  "Get listening activity aggregated by day for the last N days"
  (with-db
    (postmodern:query
     (:raw (format nil "SELECT DATE(\"listened-at\") as day, COUNT(*) as track_count FROM listening_history WHERE \"user-id\" = ~a AND \"listened-at\" >= NOW() - INTERVAL '~a days' GROUP BY DATE(\"listened-at\") ORDER BY day ASC"
                   user-id days))
     :alists)))

;;; ==========================================================================
;;; API Endpoints for User Favorites
;;; ==========================================================================

(define-api asteroid/user/favorites () ()
  "Get current user's favorite tracks"
  (require-authentication)
  (with-error-handling
    (let* ((user-id (session:field "user-id"))
           (favorites (get-user-favorites user-id)))
      (api-output `(("status" . "success")
                    ("favorites" . ,(mapcar (lambda (fav)
                                              `(("id" . ,(cdr (assoc :_id fav)))
                                                ("track_id" . ,(cdr (assoc :track-id fav)))
                                                ("title" . ,(or (cdr (assoc :track-title fav))
                                                                (cdr (assoc :track_title fav))))
                                                ("rating" . ,(cdr (assoc :rating fav)))))
                                            favorites))
                    ("count" . ,(get-favorites-count user-id)))))))

(define-api asteroid/user/favorites/add (&optional track-id rating title) ()
  "Add a track to user's favorites. Can use track-id or title."
  (require-authentication)
  (with-error-handling
    (let* ((user-id-raw (session:field "user-id"))
           (user-id (if (stringp user-id-raw) 
                        (parse-integer user-id-raw :junk-allowed t)
                        user-id-raw))
           (track-id-int (when (and track-id (not (string= track-id "")))
                           (parse-integer track-id :junk-allowed t)))
           (rating-int (if rating (parse-integer rating :junk-allowed t) 1)))
      (format t "Adding favorite: user-id=~a track-id=~a title=~a~%" user-id track-id-int title)
      (add-favorite user-id track-id-int (or rating-int 1) title)
      (api-output `(("status" . "success")
                    ("message" . "Track added to favorites"))))))

(define-api asteroid/user/favorites/remove (&optional track-id title) ()
  "Remove a track from user's favorites by track-id or title"
  (require-authentication)
  (with-error-handling
    (let* ((user-id (session:field "user-id"))
           (track-id-int (when (and track-id (not (string= track-id "")))
                           (parse-integer track-id :junk-allowed t))))
      (remove-favorite user-id track-id-int title)
      (api-output `(("status" . "success")
                    ("message" . "Track removed from favorites"))))))

(define-api asteroid/user/favorites/rate (track-id rating) ()
  "Update rating for a favorited track"
  (require-authentication)
  (with-error-handling
    (let* ((user-id (session:field "user-id"))
           (track-id-int (parse-integer track-id))
           (rating-int (parse-integer rating)))
      (update-favorite-rating user-id track-id-int rating-int)
      (api-output `(("status" . "success")
                    ("message" . "Rating updated"))))))

(define-api asteroid/user/favorites/check (track-id) ()
  "Check if a track is in user's favorites"
  (require-authentication)
  (with-error-handling
    (let* ((user-id (session:field "user-id"))
           (track-id-int (parse-integer track-id))
           (rating (is-track-favorited user-id track-id-int)))
      (api-output `(("status" . "success")
                    ("favorited" . ,(if rating t nil))
                    ("rating" . ,rating))))))

;;; ==========================================================================
;;; API Endpoints for Listening History
;;; ==========================================================================

(define-api asteroid/user/history () ()
  "Get current user's listening history"
  (require-authentication)
  (with-error-handling
    (let* ((user-id (session:field "user-id"))
           (history (get-listening-history user-id)))
      (api-output `(("status" . "success")
                    ("history" . ,(mapcar (lambda (h)
                                            `(("id" . ,(cdr (assoc :_id h)))
                                              ("track_id" . ,(cdr (assoc :track-id h)))
                                              ("title" . ,(or (cdr (assoc :track-title h))
                                                              (cdr (assoc :track_title h))))
                                              ("listened_at" . ,(cdr (assoc :listened-at h)))
                                              ("listen_duration" . ,(cdr (assoc :listen-duration h)))
                                              ("completed" . ,(let ((c (cdr (assoc :completed h))))
                                                                (and c (= 1 c))))))
                                          history)))))))

(define-api asteroid/user/history/record (&optional track-id title duration completed) ()
  "Record a track listen (called by player). Can use track-id or title."
  (require-authentication)
  (with-error-handling
    (let* ((user-id-raw (session:field "user-id"))
           (user-id (if (stringp user-id-raw)
                        (parse-integer user-id-raw :junk-allowed t)
                        user-id-raw))
           (track-id-int (when (and track-id (not (string= track-id "")))
                           (parse-integer track-id :junk-allowed t)))
           (duration-int (if duration (parse-integer duration :junk-allowed t) 0))
           (completed-bool (and completed (string-equal completed "true"))))
      (format t "Recording listen: user-id=~a title=~a~%" user-id title)
      (when (and user-id title)
        (record-listen user-id :track-id track-id-int :track-title title 
                       :duration (or duration-int 0) :completed completed-bool))
      (api-output `(("status" . "success")
                    ("message" . "Listen recorded"))))))

(define-api asteroid/user/history/clear () ()
  "Clear user's listening history"
  (require-authentication)
  (with-error-handling
    (let ((user-id (session:field "user-id")))
      (clear-listening-history user-id)
      (api-output `(("status" . "success")
                    ("message" . "Listening history cleared"))))))

(define-api asteroid/user/activity (&optional (days "30")) ()
  "Get listening activity by day for the last N days"
  (require-authentication)
  (with-error-handling
    (let* ((user-id (session:field "user-id"))
           (days-int (or (parse-integer days :junk-allowed t) 30))
           (activity (get-listening-activity user-id :days days-int)))
      (api-output `(("status" . "success")
                    ("activity" . ,(mapcar (lambda (a)
                                             `(("day" . ,(cdr (assoc :day a)))
                                               ("track_count" . ,(cdr (assoc :track-count a)))))
                                           activity)))))))

;;; ==========================================================================
;;; Avatar Management
;;; ==========================================================================

(defun get-avatars-directory ()
  "Get the path to the avatars directory"
  (merge-pathnames "static/avatars/" (asdf:system-source-directory :asteroid)))

(defun save-avatar (user-id temp-file-path original-filename)
  "Save an avatar file from temp path and return the relative path"
  (let* ((extension (pathname-type original-filename))
         (safe-ext (if (member extension '("png" "jpg" "jpeg" "gif" "webp") :test #'string-equal)
                       extension
                       "png"))
         (new-filename (format nil "~a.~a" user-id safe-ext))
         (full-path (merge-pathnames new-filename (get-avatars-directory)))
         (relative-path (format nil "/asteroid/static/avatars/~a" new-filename)))
    ;; Copy from temp file to avatars directory
    (uiop:copy-file temp-file-path full-path)
    ;; Update database
    (with-db
      (postmodern:query
       (:raw (format nil "UPDATE \"USERS\" SET avatar_path = '~a' WHERE _id = ~a"
                     relative-path user-id))))
    relative-path))

(defun get-user-avatar (user-id)
  "Get the avatar path for a user"
  (with-db
    (postmodern:query
     (:raw (format nil "SELECT avatar_path FROM \"USERS\" WHERE _id = ~a" user-id))
     :single)))

(define-api asteroid/user/avatar/upload () ()
  "Upload a new avatar image"
  (require-authentication)
  (with-error-handling
    (let* ((user-id (session:field "user-id"))
           ;; Radiance wraps hunchentoot - post-var returns (path filename content-type) for files
           (file-info (radiance:post-var "avatar"))
           (temp-path (when (listp file-info) (first file-info)))
           (original-name (when (listp file-info) (second file-info))))
      (format t "Avatar upload: file-info=~a temp-path=~a original-name=~a~%" file-info temp-path original-name)
      (if (and temp-path (probe-file temp-path))
          (let ((avatar-path (save-avatar user-id temp-path (or original-name "avatar.png"))))
            (api-output `(("status" . "success")
                          ("message" . "Avatar uploaded successfully")
                          ("avatar_path" . ,avatar-path))))
          (api-output `(("status" . "error")
                        ("message" . "No file provided"))
                      :status 400)))))

(define-api asteroid/user/avatar () ()
  "Get current user's avatar path"
  (require-authentication)
  (with-error-handling
    (let* ((user-id (session:field "user-id"))
           (avatar-path (get-user-avatar user-id)))
      (api-output `(("status" . "success")
                    ("avatar_path" . ,avatar-path))))))
