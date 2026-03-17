;;;; src/crucible/rate-limit.lisp — Per-IP rate limiting plug
(in-package :cauldron.crucible)

;;; --- Rate limit store ---

(defstruct rate-limit-store
  "Per-IP request tracking with sliding window."
  (table (make-hash-table :test 'equal) :type hash-table)
  (lock (cauldron.runtime:make-lock "rate-limit"))
  (max-requests 100 :type integer)
  (window-seconds 60 :type integer))

(defstruct rate-limit-entry
  "Tracks requests for a single IP."
  (count 0 :type integer)
  (window-start 0 :type integer))

(defun current-epoch ()
  "Current time in seconds since some epoch (internal real time based)."
  (floor (get-internal-real-time) internal-time-units-per-second))

;;; --- Rate limit plug ---

(defun make-plug-rate-limit (&key (max-requests 100) (window-seconds 60) store)
  "Create rate limiting plug. Per-IP tracking via :remote-addr assign.
Returns 429 with Retry-After header when exceeded.
STORE is an optional pre-created rate-limit-store."
  (let ((store (or store (make-rate-limit-store :max-requests max-requests
                                                 :window-seconds window-seconds))))
    (lambda (conn)
      (let* ((ip (or (conn-get-assign conn :remote-addr) "unknown"))
             (now (current-epoch))
             (allowed t)
             (retry-after 0))
        (cauldron.runtime:with-lock ((rate-limit-store-lock store))
          (let* ((table (rate-limit-store-table store))
                 (entry (gethash ip table))
                 (window (rate-limit-store-window-seconds store))
                 (max-req (rate-limit-store-max-requests store)))
            (cond
              ;; No entry or window expired → reset
              ((or (null entry)
                   (>= (- now (rate-limit-entry-window-start entry)) window))
               (setf (gethash ip table)
                     (make-rate-limit-entry :count 1 :window-start now)))
              ;; Under limit → increment
              ((< (rate-limit-entry-count entry) max-req)
               (incf (rate-limit-entry-count entry)))
              ;; Over limit
              (t
               (setf allowed nil)
               (setf retry-after
                     (max 1 (- window (- now (rate-limit-entry-window-start entry)))))))))
        (if allowed
            conn
            (progn
              (conn-put-resp-header conn "Retry-After" (format nil "~D" retry-after))
              (conn-put-resp-header conn "Content-Type" "text/plain")
              (halt-conn conn :status 429 :body "Too Many Requests")))))))

;;; --- Cleanup ---

(defun rate-limit-cleanup (store)
  "Remove expired entries from STORE. Thread-safe."
  (let ((now (current-epoch))
        (window (rate-limit-store-window-seconds store)))
    (cauldron.runtime:with-lock ((rate-limit-store-lock store))
      (let ((table (rate-limit-store-table store))
            (expired '()))
        (maphash (lambda (ip entry)
                   (when (>= (- now (rate-limit-entry-window-start entry)) window)
                     (push ip expired)))
                 table)
        (dolist (ip expired)
          (remhash ip table))
        (length expired)))))
