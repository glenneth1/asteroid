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

(defun record-listen (user-id track-id &key (duration 0) (completed nil))
  "Record a track listen in user's history"
  (with-db
    (postmodern:query
     (:insert-into 'listening_history
      :set '"user-id" user-id
           '"track-id" track-id
           '"listened-at" (:current_timestamp)
           '"listen-duration" duration
           'completed (if completed 1 0)))))

(defun get-listening-history (user-id &key (limit 20) (offset 0))
  "Get user's listening history with track details"
  (with-db
    (postmodern:query
     (:limit
      (:order-by
       (:select 'lh._id 'lh.listened-at 'lh.listen-duration 'lh.completed
                't.title 't.artist 't.album 't.duration 't._id
        :from (:as 'listening_history 'lh)
        :inner-join (:as 'tracks 't) :on (:= 'lh.track-id 't._id)
        :where (:= 'lh.user-id user-id))
       (:desc 'lh.listened-at))
      limit offset)
     :alists)))

(defun get-listening-stats (user-id)
  "Get aggregate listening statistics for a user"
  (with-db
    (let ((stats (postmodern:query
                  (:select (:count '*) (:sum 'listen-duration)
                   :from 'listening_history
                   :where (:= '"user-id" user-id))
                  :row)))
      (list :tracks-played (or (first stats) 0)
            :total-listen-time (or (second stats) 0)))))

(defun get-top-artists (user-id &key (limit 5))
  "Get user's most listened artists"
  (with-db
    (postmodern:query
     (:limit
      (:order-by
       (:select 't.artist (:as (:count '*) 'play_count)
        :from (:as 'listening_history 'lh)
        :inner-join (:as 'tracks 't) :on (:= 'lh.track-id 't._id)
        :where (:= 'lh.user-id user-id)
        :group-by 't.artist)
       (:desc 'play_count))
      limit)
     :alists)))

(defun clear-listening-history (user-id)
  "Clear all listening history for a user"
  (with-db
    (postmodern:query
     (:delete-from 'listening_history
      :where (:= '"user-id" user-id)))))

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
                                              ("track_id" . ,(cdr (assoc :_id h)))
                                              ("title" . ,(cdr (assoc :title h)))
                                              ("artist" . ,(cdr (assoc :artist h)))
                                              ("album" . ,(cdr (assoc :album h)))
                                              ("duration" . ,(cdr (assoc :duration h)))
                                              ("listened_at" . ,(cdr (assoc :listened-at h)))
                                              ("completed" . ,(= 1 (cdr (assoc :completed h))))))
                                          history)))))))

(define-api asteroid/user/history/record (track-id &optional duration completed) ()
  "Record a track listen (called by player)"
  (require-authentication)
  (with-error-handling
    (let* ((user-id (session:field "user-id"))
           (track-id-int (parse-integer track-id))
           (duration-int (if duration (parse-integer duration) 0))
           (completed-bool (and completed (string-equal completed "true"))))
      (record-listen user-id track-id-int :duration duration-int :completed completed-bool)
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
