;;;; src/runtime/timer.lisp — Timer and scheduler with background thread
(in-package :cauldron.runtime)

(defvar *next-timer-id* 0
  "Monotonically increasing counter for timer IDs.")
(defvar *timer-id-lock* (sb-thread:make-mutex :name "timer-id-lock"))

(defun %next-timer-id ()
  "Generate a unique timer ID."
  (sb-thread:with-mutex (*timer-id-lock*)
    (incf *next-timer-id*)))

(defstruct timer-entry
  "A single scheduled timer."
  (id        0     :type fixnum)
  (fire-time 0.0d0 :type double-float)
  (interval  nil)   ; nil for one-shot, seconds (number) for recurring
  (callback  nil   :type (or null function)))

(defstruct (scheduler (:constructor %make-scheduler))
  "A scheduler that runs timer callbacks on a background thread."
  (lock       (make-lock "scheduler-lock"))
  (entries    '()   :type list)
  (thread     nil)
  (running-p  t     :type boolean))

(defun %now ()
  "Return the current time as a double-float (internal real time in seconds)."
  (/ (coerce (get-internal-real-time) 'double-float)
     (coerce internal-time-units-per-second 'double-float)))

(defun %scheduler-tick (scheduler)
  "Process one tick of the scheduler: fire any entries whose time has come."
  (let ((now (%now))
        (to-fire '())
        (to-reschedule '()))
    ;; Collect entries that need to fire under the lock
    (with-lock ((scheduler-lock scheduler))
      (let ((remaining '()))
        (dolist (entry (scheduler-entries scheduler))
          (if (<= (timer-entry-fire-time entry) now)
              (progn
                (push entry to-fire)
                ;; If recurring, schedule next firing
                (when (timer-entry-interval entry)
                  (push (make-timer-entry
                         :id (timer-entry-id entry)
                         :fire-time (+ now (coerce (timer-entry-interval entry) 'double-float))
                         :interval (timer-entry-interval entry)
                         :callback (timer-entry-callback entry))
                        to-reschedule)))
              (push entry remaining)))
        (setf (scheduler-entries scheduler)
              (nconc remaining to-reschedule))))
    ;; Fire callbacks outside the lock to avoid deadlocks
    (dolist (entry to-fire)
      (handler-case
          (funcall (timer-entry-callback entry))
        (error (c)
          (format *error-output*
                  "~&[cauldron] Timer ~D callback error: ~A~%"
                  (timer-entry-id entry) c))))))

(defun %scheduler-loop (scheduler)
  "Main loop for the scheduler background thread."
  (loop while (scheduler-running-p scheduler)
        do (%scheduler-tick scheduler)
           (sleep 0.1)))

(defun make-scheduler ()
  "Create and start a scheduler. The scheduler runs a background thread
that checks for due timers every 100ms."
  (let ((sched (%make-scheduler)))
    (setf (scheduler-thread sched)
          (spawn "cauldron-scheduler"
                 (lambda () (%scheduler-loop sched))))
    sched))

(defun schedule-once (scheduler delay callback)
  "Schedule CALLBACK to run once after DELAY seconds.
Returns a timer-id that can be used with CANCEL-TIMER."
  (let* ((id (%next-timer-id))
         (entry (make-timer-entry
                 :id id
                 :fire-time (+ (%now) (coerce delay 'double-float))
                 :interval nil
                 :callback callback)))
    (with-lock ((scheduler-lock scheduler))
      (push entry (scheduler-entries scheduler)))
    id))

(defun schedule-recurring (scheduler interval callback)
  "Schedule CALLBACK to run every INTERVAL seconds, starting after the
first INTERVAL elapses. Returns a timer-id for CANCEL-TIMER."
  (let* ((id (%next-timer-id))
         (entry (make-timer-entry
                 :id id
                 :fire-time (+ (%now) (coerce interval 'double-float))
                 :interval interval
                 :callback callback)))
    (with-lock ((scheduler-lock scheduler))
      (push entry (scheduler-entries scheduler)))
    id))

(defun cancel-timer (scheduler timer-id)
  "Remove the timer identified by TIMER-ID from SCHEDULER.
Returns T if a timer was found and removed, NIL otherwise."
  (with-lock ((scheduler-lock scheduler))
    (let ((before-len (length (scheduler-entries scheduler))))
      (setf (scheduler-entries scheduler)
            (remove timer-id (scheduler-entries scheduler)
                    :key #'timer-entry-id))
      (/= before-len (length (scheduler-entries scheduler))))))

(defun shutdown-scheduler (scheduler)
  "Stop the scheduler background thread and discard all pending timers."
  (with-lock ((scheduler-lock scheduler))
    (setf (scheduler-running-p scheduler) nil)
    (setf (scheduler-entries scheduler) '()))
  ;; Wait for the background thread to exit
  (when (and (scheduler-thread scheduler)
             (sb-thread:thread-alive-p (scheduler-thread scheduler)))
    (handler-case
        (sb-thread:join-thread (scheduler-thread scheduler) :timeout 2)
      (error () nil)))
  (values))
