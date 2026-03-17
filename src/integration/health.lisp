;;;; src/integration/health.lisp — In-memory health tracking for integrations
(in-package :cauldron.integration)

(defstruct health-stats
  "Health statistics for an integration."
  (total-calls 0 :type integer)
  (success-count 0 :type integer)
  (failure-count 0 :type integer)
  (total-duration-ms 0 :type integer)
  (last-call-at 0 :type integer)
  (last-error nil)
  (last-status nil))

(defvar *health-registry* (make-hash-table :test 'eq)
  "Integration name → health-stats.")

(defvar *health-lock* (cauldron.runtime:make-lock "integration-health")
  "Lock for thread-safe health registry access.")

(defun record-health (integration-name status duration-ms &key error)
  "Record a health data point for INTEGRATION-NAME. Thread-safe."
  (cauldron.runtime:with-lock (*health-lock*)
    (let ((stats (or (gethash integration-name *health-registry*)
                     (setf (gethash integration-name *health-registry*)
                           (make-health-stats)))))
      (incf (health-stats-total-calls stats))
      (incf (health-stats-total-duration-ms stats) duration-ms)
      (setf (health-stats-last-call-at stats) (get-universal-time))
      (setf (health-stats-last-status stats) status)
      (if (and status (< status 400))
          (incf (health-stats-success-count stats))
          (progn
            (incf (health-stats-failure-count stats))
            (when error
              (setf (health-stats-last-error stats) error)))))))

(defun integration-health (integration-name)
  "Return health-stats for INTEGRATION-NAME, or NIL. Thread-safe."
  (cauldron.runtime:with-lock (*health-lock*)
    (gethash integration-name *health-registry*)))

(defun all-integration-health ()
  "Return alist of (name . health-stats) for all tracked integrations. Thread-safe."
  (cauldron.runtime:with-lock (*health-lock*)
    (let ((result '()))
      (maphash (lambda (name stats) (push (cons name stats) result))
               *health-registry*)
      (nreverse result))))

(defun reset-health (&optional name)
  "Reset health stats. If NAME is given, reset only that integration.
Otherwise reset all. Thread-safe."
  (cauldron.runtime:with-lock (*health-lock*)
    (if name
        (remhash name *health-registry*)
        (clrhash *health-registry*))))
