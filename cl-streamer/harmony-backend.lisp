(defpackage #:cl-streamer/harmony
  (:use #:cl #:alexandria)
  (:local-nicknames (#:harmony #:org.shirakumo.fraf.harmony)
                    (#:mixed #:org.shirakumo.fraf.mixed))
  (:export #:audio-pipeline
           #:make-audio-pipeline
           #:add-pipeline-output
           #:start-pipeline
           #:stop-pipeline
           #:play-file
           #:play-list
           #:pipeline-server
           #:make-streaming-server
           ;; Track state & control
           #:pipeline-current-track
           #:pipeline-on-track-change
           #:pipeline-running-p
           #:pipeline-skip
           #:pipeline-queue-files
           #:pipeline-get-queue
           #:pipeline-clear-queue
           #:pipeline-pending-playlist-path
           #:pipeline-on-playlist-change
           ;; Metadata helpers
           #:read-audio-metadata
           #:format-display-title
           #:update-all-mounts-metadata
           ;; DJ support
           #:pipeline-harmony-server
           #:volume-ramp
           #:pipeline-stop-all-voices))

(in-package #:cl-streamer/harmony)

;;; ---- Streaming Drain ----
;;; Custom drain that captures PCM from Harmony's pack buffer
;;; and feeds it to the encoder/stream server, replacing the
;;; dummy drain which just discards audio data.

(defclass streaming-drain (mixed:drain)
  ((outputs :initarg :outputs :accessor drain-outputs :initform nil
            :documentation "List of (encoder . mount-path) pairs")
   (channels :initarg :channels :accessor drain-channels :initform 2)))

(defun drain-add-output (drain encoder mount-path)
  "Add an encoder/mount pair to the drain."
  (push (cons encoder mount-path) (drain-outputs drain)))

(defun drain-remove-output (drain mount-path)
  "Remove an encoder/mount pair by mount path."
  (setf (drain-outputs drain)
        (remove mount-path (drain-outputs drain) :key #'cdr :test #'string=)))

(defmethod mixed:free ((drain streaming-drain)))

(defmethod mixed:start ((drain streaming-drain)))

(declaim (inline float-to-s16))
(defun float-to-s16 (sample)
  "Convert a float sample (-1.0 to 1.0) to signed 16-bit integer."
  (let ((clamped (max -1.0 (min 1.0 sample))))
    (the (signed-byte 16) (round (* clamped 32767.0)))))

(defmethod mixed:mix ((drain streaming-drain))
  "Read interleaved float PCM from the pack buffer, encode to all outputs.
   The pack buffer is (unsigned-byte 8) with IEEE 754 single-floats (4 bytes each).
   Layout: L0b0 L0b1 L0b2 L0b3 R0b0 R0b1 R0b2 R0b3 L1b0 ... (interleaved stereo)"
  (mixed:with-buffer-tx (data start size (mixed:pack drain))
    (when (> size 0)
      (let* ((channels (drain-channels drain))
             (bytes-per-sample 4)  ; single-float = 4 bytes
             (total-floats (floor size bytes-per-sample))
             (num-samples (floor total-floats channels))
             (pcm-buffer (make-array (* num-samples channels)
                                     :element-type '(signed-byte 16))))
        ;; Convert raw bytes -> single-float -> signed-16
        (cffi:with-pointer-to-vector-data (ptr data)
          (loop for i below (* num-samples channels)
                for byte-offset = (+ start (* i bytes-per-sample))
                for sample = (cffi:mem-ref ptr :float byte-offset)
                do (setf (aref pcm-buffer i) (float-to-s16 sample))))
        ;; Feed PCM to all encoder/mount pairs
        (dolist (output (drain-outputs drain))
          (let ((encoder (car output))
                (mount-path (cdr output)))
            (handler-case
                (let ((encoded (encode-for-output encoder pcm-buffer num-samples)))
                  (when (> (length encoded) 0)
                    (cl-streamer:write-audio-data mount-path encoded)))
              (error (e)
                (log:warn "Encode error for ~A: ~A" mount-path e)))))))
    ;; Sleep for most of the audio duration (leave headroom for encoding)
    (let* ((channels (drain-channels drain))
           (bytes-per-frame (* channels 4))
           (frames (floor size bytes-per-frame))
           (samplerate (mixed:samplerate (mixed:pack drain))))
      (when (> frames 0)
        (sleep (* 0.9 (/ frames samplerate)))))
    (mixed:finish size)))

(defgeneric encode-for-output (encoder pcm-buffer num-samples)
  (:documentation "Encode PCM samples using the given encoder. Returns byte vector."))

(defmethod encode-for-output ((encoder cl-streamer::mp3-encoder) pcm-buffer num-samples)
  (cl-streamer:encode-pcm-interleaved encoder pcm-buffer num-samples))

(defmethod encode-for-output ((encoder cl-streamer::aac-encoder) pcm-buffer num-samples)
  (cl-streamer:encode-aac-pcm encoder pcm-buffer num-samples))

(defmethod mixed:end ((drain streaming-drain)))

;;; ---- Audio Pipeline ----

(defclass audio-pipeline ()
  ((harmony-server :initform nil :accessor pipeline-harmony-server)
   (drain :initform nil :accessor pipeline-drain)
   (stream-server :initarg :stream-server :accessor pipeline-server)
   (mount-path :initarg :mount-path :accessor pipeline-mount-path :initform "/stream.mp3")
   (sample-rate :initarg :sample-rate :accessor pipeline-sample-rate :initform 44100)
   (channels :initarg :channels :accessor pipeline-channels :initform 2)
   (running :initform nil :accessor pipeline-running-p)
   ;; Track state
   (current-track :initform nil :accessor pipeline-current-track
                  :documentation "Plist of current track: (:title :artist :album :file :display-title)")
   (on-track-change :initarg :on-track-change :initform nil
                    :accessor pipeline-on-track-change
                    :documentation "Callback (lambda (pipeline track-info)) called on track change")
   ;; Playlist queue & skip control
   (file-queue :initform nil :accessor pipeline-file-queue
               :documentation "List of file entries to play after current playlist")
   (queue-lock :initform (bt:make-lock "pipeline-queue-lock")
               :reader pipeline-queue-lock)
   (skip-flag :initform nil :accessor pipeline-skip-flag
              :documentation "Set to T to skip the current track")
   (pending-playlist-path :initform nil :accessor pipeline-pending-playlist-path
                          :documentation "Playlist path queued by scheduler, applied when tracks start playing")
   (on-playlist-change :initarg :on-playlist-change :initform nil
                       :accessor pipeline-on-playlist-change
                       :documentation "Callback (lambda (pipeline playlist-path)) called when scheduler playlist starts")))

(defun make-audio-pipeline (&key encoder stream-server (mount-path "/stream.mp3")
                                 (sample-rate 44100) (channels 2))
  "Create an audio pipeline connecting Harmony to the stream server via an encoder.
   The initial encoder/mount-path pair is added as the first output.
   Additional outputs can be added with add-pipeline-output."
  (let ((pipeline (make-instance 'audio-pipeline
                                 :stream-server stream-server
                                 :mount-path mount-path
                                 :sample-rate sample-rate
                                 :channels channels)))
    (when encoder
      (setf (slot-value pipeline 'drain)
            (make-instance 'streaming-drain :channels channels))
      (drain-add-output (pipeline-drain pipeline) encoder mount-path))
    pipeline))

(defun add-pipeline-output (pipeline encoder mount-path)
  "Add an additional encoder/mount output to the pipeline.
   Can be called before or after start-pipeline."
  (unless (pipeline-drain pipeline)
    (setf (pipeline-drain pipeline)
          (make-instance 'streaming-drain :channels (pipeline-channels pipeline))))
  (drain-add-output (pipeline-drain pipeline) encoder mount-path))

(defun start-pipeline (pipeline)
  "Start the audio pipeline - initializes Harmony with our streaming drain."
  (when (pipeline-running-p pipeline)
    (error "Pipeline already running"))
  (mixed:init)
  (let* ((server (harmony:make-simple-server
                  :name "CL-Streamer"
                  :samplerate (pipeline-sample-rate pipeline)
                  :latency 0.05
                  :drain :dummy
                  :output-channels (pipeline-channels pipeline)))
         (output (harmony:segment :output server))
         (old-drain (harmony:segment :drain output))
         (pack (mixed:pack old-drain))
         (drain (pipeline-drain pipeline)))
    ;; TODO: Investigate setting (mixed:encoding pack) :int16 to let cl-mixed
    ;; handle float→s16 in C. Currently causes static — may need to be set
    ;; before server start, or pack may need recreation with correct encoding.
    ;; Wire our streaming drain to the same pack buffer
    (setf (mixed:pack drain) pack)
    ;; Swap: withdraw old dummy drain, add our streaming drain
    (mixed:withdraw old-drain output)
    (mixed:add drain output)
    (setf (pipeline-harmony-server pipeline) server)
    (mixed:start server))
  (setf (pipeline-running-p pipeline) t)
  (log:info "Audio pipeline started with streaming drain (~A outputs)"
            (length (drain-outputs (pipeline-drain pipeline))))
  pipeline)

(defun stop-pipeline (pipeline)
  "Stop the audio pipeline."
  (setf (pipeline-running-p pipeline) nil)
  (when (pipeline-harmony-server pipeline)
    (mixed:end (pipeline-harmony-server pipeline))
    (setf (pipeline-harmony-server pipeline) nil))
  (log:info "Audio pipeline stopped")
  pipeline)

;;; ---- Pipeline Control ----

(defun pipeline-skip (pipeline)
  "Skip the current track. The play-list loop will detect this and advance."
  (setf (pipeline-skip-flag pipeline) t)
  (log:info "Skip requested"))

(defun pipeline-queue-files (pipeline file-entries &key (position :end))
  "Add file entries to the pipeline queue.
   Each entry is a string (path) or plist (:file path :title title).
   POSITION is :end (append) or :next (prepend)."
  (bt:with-lock-held ((pipeline-queue-lock pipeline))
    (case position
      (:next (setf (pipeline-file-queue pipeline)
                   (append file-entries (pipeline-file-queue pipeline))))
      (t (setf (pipeline-file-queue pipeline)
               (append (pipeline-file-queue pipeline) file-entries)))))
  (log:info "Queued ~A files (~A)" (length file-entries) position))

(defun pipeline-get-queue (pipeline)
  "Get the current file queue (copy)."
  (bt:with-lock-held ((pipeline-queue-lock pipeline))
    (copy-list (pipeline-file-queue pipeline))))

(defun pipeline-clear-queue (pipeline)
  "Clear the file queue."
  (bt:with-lock-held ((pipeline-queue-lock pipeline))
    (setf (pipeline-file-queue pipeline) nil))
  (log:info "Queue cleared"))

(defun pipeline-stop-all-voices (pipeline)
  "Immediately stop all active voices on the Harmony mixer.
   Used by DJ session to silence the auto-playlist before mixing."
  (let ((server (pipeline-harmony-server pipeline)))
    (when server
      (let ((harmony:*server* server))
        (dolist (voice (harmony:voices server))
          (handler-case
              (progn
                (setf (mixed:volume voice) 0.0)
                (harmony:stop voice))
            (error (e)
              (log:debug "Error stopping voice: ~A" e))))
        (log:info "All voices stopped on mixer")))))

(defun pipeline-pop-queue (pipeline)
  "Pop the next entry from the file queue (internal use)."
  (bt:with-lock-held ((pipeline-queue-lock pipeline))
    (pop (pipeline-file-queue pipeline))))

;;; ---- Metadata ----

(defun ensure-simple-string (s)
  "Coerce S to a simple-string if it's a string, or return NIL.
   Coerce first to guarantee simple-string before any string operations,
   since SBCL's string-trim may require simple-string input."
  (when (stringp s)
    (let ((simple (coerce s 'simple-string)))
      (string-trim '(#\Space #\Nul) simple))))

(defun safe-tag (fn audio-file)
  "Safely read a tag field, coercing to simple-string. Returns NIL on any error."
  (handler-case
      (ensure-simple-string (funcall fn audio-file))
    (error () nil)))

(defun read-audio-metadata (file-path)
  "Read metadata (artist, title, album) from an audio file using taglib.
   Returns a plist (:artist ... :title ... :album ...) or NIL on failure."
  (handler-case
      (let ((audio-file (audio-streams:open-audio-file (namestring file-path))))
        (list :artist (safe-tag #'abstract-tag:artist audio-file)
              :title (safe-tag #'abstract-tag:title audio-file)
              :album (safe-tag #'abstract-tag:album audio-file)))
    (error (e)
      (log:debug "Could not read tags from ~A: ~A" file-path e)
      nil)))

(defun format-display-title (file-path &optional explicit-title)
  "Build a display title for ICY metadata.
   If EXPLICIT-TITLE is given, use it.
   Otherwise read tags from the file: 'Artist - Title' or fall back to filename."
  (or explicit-title
      (let ((tags (read-audio-metadata file-path)))
        (if tags
            (let ((artist (getf tags :artist))
                  (title (getf tags :title)))
              (cond ((and artist title (not (string= artist ""))
                          (not (string= title "")))
                     (format nil "~A - ~A" artist title))
                    (title title)
                    (artist artist)
                    (t (pathname-name (pathname file-path)))))
            (pathname-name (pathname file-path))))))

(defun update-all-mounts-metadata (pipeline display-title)
  "Update ICY metadata on all mount points."
  (dolist (output (drain-outputs (pipeline-drain pipeline)))
    (cl-streamer:set-now-playing (cdr output) display-title)))

(defun notify-track-change (pipeline track-info)
  "Update pipeline state and fire the on-track-change callback."
  (setf (pipeline-current-track pipeline) track-info)
  (when (pipeline-on-track-change pipeline)
    (handler-case
        (funcall (pipeline-on-track-change pipeline) pipeline track-info)
      (error (e)
        (log:warn "Track change callback error: ~A" e)))))

(defun play-file (pipeline file-path &key (mixer :music) title (on-end :free)
                                          (update-metadata t))
  "Play an audio file through the pipeline.
   The file will be decoded by Harmony and encoded for streaming.
   If TITLE is given, update ICY metadata with it.
   Otherwise reads tags from the file via taglib.
   FILE-PATH can be a string or pathname.
   ON-END is passed to harmony:play (default :free).
   UPDATE-METADATA controls whether ICY metadata is updated immediately."
  (let* ((path-string (etypecase file-path
                        (string file-path)
                        (pathname (namestring file-path))))
         ;; Use parse-native-namestring to prevent SBCL from interpreting
         ;; brackets as wildcard patterns. Standard (pathname ...) turns
         ;; "[FLAC]" into a wild component with non-simple strings, which
         ;; causes SIMPLE-ARRAY errors in cl-flac's CFFI calls.
         (path (sb-ext:parse-native-namestring path-string))
         (server (pipeline-harmony-server pipeline))
         (harmony:*server* server)
         (tags (read-audio-metadata path))
         (display-title (format-display-title path title))
         (track-info (list :file path-string
                           :display-title display-title
                           :artist (getf tags :artist)
                           :title (getf tags :title)
                           :album (getf tags :album))))
    (when update-metadata
      (update-all-mounts-metadata pipeline display-title)
      (notify-track-change pipeline track-info))
    (let ((voice (harmony:play path :mixer mixer :on-end on-end)))
      (if update-metadata
          (log:info "Now playing: ~A" display-title)
          (log:info "Loading next: ~A" display-title))
      (values voice display-title track-info))))

(defun voice-remaining-seconds (voice)
  "Return estimated seconds remaining for a voice, or NIL if unknown."
  (handler-case
      (let ((pos (mixed:frame-position voice))
            (total (mixed:frame-count voice))
            (sr (mixed:samplerate voice)))
        (when (and pos total sr (> total 0) (> sr 0))
          (/ (- total pos) sr)))
    (error () nil)))

(defun volume-ramp (voice target-volume duration &key (steps 20))
  "Smoothly ramp a voice's volume to TARGET-VOLUME over DURATION seconds.
   Runs in the calling thread (blocks for DURATION seconds)."
  (let* ((start-volume (mixed:volume voice))
         (delta (- target-volume start-volume))
         (step-time (/ duration steps)))
    (loop for i from 1 to steps
          for fraction = (/ i steps)
          for vol = (+ start-volume (* delta fraction))
          do (setf (mixed:volume voice) (max 0.0 (min 1.0 (float vol))))
             (sleep step-time))))

(defun drain-queue-into-remaining (pipeline remaining-ref current-list-ref)
  "If the scheduler has queued tracks, drain them all into remaining-ref,
   replacing any current remaining tracks. Also update current-list-ref
   so loop-queue replays the scheduler's playlist, not the original.
   Returns T if new tracks were loaded, NIL otherwise."
  (let ((first (pipeline-pop-queue pipeline)))
    (when first
      (let ((all-queued (list first)))
        ;; Drain remaining queue entries
        (loop for item = (pipeline-pop-queue pipeline)
              while item do (push item all-queued))
        (setf all-queued (nreverse all-queued))
        (log:info "Scheduler playlist taking over: ~A tracks" (length all-queued))
        ;; Replace remaining list and update current for loop-queue
        (setf (car remaining-ref) all-queued)
        (setf (car current-list-ref) (copy-list all-queued))
        ;; Fire playlist-change callback so app layer updates metadata
        (when (pipeline-on-playlist-change pipeline)
          (let ((playlist-path (pipeline-pending-playlist-path pipeline)))
            (when playlist-path
              (handler-case
                  (funcall (pipeline-on-playlist-change pipeline)
                           pipeline playlist-path)
                (error (e)
                  (log:warn "Playlist change callback error: ~A" e)))
              (setf (pipeline-pending-playlist-path pipeline) nil))))
        t))))

(defun next-entry (pipeline remaining-ref current-list-ref)
  "Get the next entry to play. Checks scheduler queue first (drains all into remaining),
   then pops from remaining-ref.
   REMAINING-REF is a cons cell whose car is the remaining file list.
   CURRENT-LIST-REF is a cons cell whose car is the full current playlist (for loop-queue)."
  (drain-queue-into-remaining pipeline remaining-ref current-list-ref)
  (pop (car remaining-ref)))

(defun play-list (pipeline file-list &key (crossfade-duration 3.0)
                                          (fade-in 2.0)
                                          (fade-out 2.0)
                                          (loop-queue nil))
  "Play a list of file paths sequentially through the pipeline.
   Each entry can be a string (path) or a plist (:file path :title title).
   CROSSFADE-DURATION is how early to start the next track (seconds).
   FADE-IN/FADE-OUT control the volume ramp durations.
   Both voices play simultaneously through the mixer during crossfade.
   When LOOP-QUEUE is T, repeats the playlist from the start when tracks run out.
   Scheduler-queued tracks take priority over the repeat cycle."
  (bt:make-thread
   (lambda ()
     (handler-case
         (let ((prev-voice nil)
               (idx 0)
               (remaining-list (list (copy-list file-list)))
               (current-list (list (copy-list file-list))))
           (loop while (pipeline-running-p pipeline)
                 for entry = (next-entry pipeline remaining-list current-list)
                 do (cond
                      ;; No entry and loop mode: re-queue current playlist
                      ((and (null entry) loop-queue)
                       (log:info "Playlist ended, repeating from start (~A tracks)"
                                 (length (car current-list)))
                       (setf (car remaining-list) (copy-list (car current-list))))
                      ;; No entry: done
                      ((null entry)
                       (return))
                      ;; Play the entry
                      (t
                   (multiple-value-bind (path title)
                       (if (listp entry)
                           (values (getf entry :file) (getf entry :title))
                           (values entry nil))
                     (handler-case
                         (let* ((server (pipeline-harmony-server pipeline))
                                (harmony:*server* server))
                           (multiple-value-bind (voice display-title track-info)
                               (handler-case
                                   (play-file pipeline path :title title
                                              :on-end :disconnect
                                              :update-metadata (null prev-voice))
                                 (error (retry-err)
                                   ;; Retry once after brief delay for transient FLAC init errors
                                   (log:debug "Retrying ~A after init error: ~A"
                                              (pathname-name (pathname path)) retry-err)
                                   (sleep 0.2)
                                   (play-file pipeline path :title title
                                              :on-end :disconnect
                                              :update-metadata (null prev-voice))))
                             (when voice
                               ;; If this isn't the first track, crossfade
                               (when (and prev-voice (> idx 0))
                                 (setf (mixed:volume voice) 0.0)
                                 (let ((fade-thread
                                         (bt:make-thread
                                          (lambda ()
                                            (volume-ramp prev-voice 0.0 fade-out)
                                            (harmony:stop prev-voice))
                                          :name "cl-streamer-fadeout")))
                                   (volume-ramp voice 1.0 fade-in)
                                   (bt:join-thread fade-thread))
                                 ;; Crossfade done — brief pause so listeners perceive
                                 ;; the new track before UI updates
                                 (sleep 1.0)
                                 (update-all-mounts-metadata pipeline display-title)
                                 (notify-track-change pipeline track-info))
                               ;; Wait for track to approach its end (or skip)
                               (setf (pipeline-skip-flag pipeline) nil)
                               (sleep 0.5)
                               ;; Log initial track duration info
                               (let ((initial-remaining (voice-remaining-seconds voice)))
                                 (log:info "Track duration check: remaining=~A pos=~A total=~A sr=~A"
                                           initial-remaining
                                           (ignore-errors (mixed:frame-position voice))
                                           (ignore-errors (mixed:frame-count voice))
                                           (ignore-errors (mixed:samplerate voice))))
                               (loop while (and (pipeline-running-p pipeline)
                                                (not (mixed:done-p voice))
                                                (not (pipeline-skip-flag pipeline)))
                                     for remaining = (voice-remaining-seconds voice)
                                     when (and remaining
                                               (<= remaining crossfade-duration)
                                               (not (mixed:done-p voice)))
                                       do (log:info "Crossfade trigger: ~,1Fs remaining" remaining)
                                          (setf prev-voice voice)
                                          (return)
                                     do (sleep 0.1))
                               ;; Handle skip
                               (when (pipeline-skip-flag pipeline)
                                 (setf (pipeline-skip-flag pipeline) nil)
                                 (setf prev-voice voice)
                                 (log:info "Skipping current track"))
                               ;; If track ended naturally (no crossfade), clean up
                               (when (mixed:done-p voice)
                                 (harmony:stop voice)
                                 (setf prev-voice nil))
                               (incf idx))))
                       (error (e)
                         (log:warn "Error playing ~A: ~A" path e)
                         (sleep 1)))))))
           ;; Clean up last voice
           (when prev-voice
             (let ((harmony:*server* (pipeline-harmony-server pipeline)))
               (volume-ramp prev-voice 0.0 fade-out)
               (harmony:stop prev-voice))))
       (error (e)
         (log:error "play-list thread crashed: ~A" e))))
   :name "cl-streamer-playlist"))

