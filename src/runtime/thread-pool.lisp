;;;; src/runtime/thread-pool.lisp — Thread pool with worker management
(in-package :cauldron.runtime)

;;; Sentinel value used to signal workers to shut down.
(defvar *pool-shutdown-sentinel* (gensym "POOL-SHUTDOWN"))

(defstruct (thread-pool (:constructor %make-thread-pool))
  "A pool of worker threads that process tasks from a shared queue."
  (workers    #()  :type simple-vector)
  (task-queue nil   :type concurrent-queue)
  (size       0     :type fixnum)
  (running-p  t     :type boolean)
  (completed  0     :type fixnum)
  (failed     0     :type fixnum)
  (lock       (make-lock "pool-lock")))

(defun %pool-worker-loop (pool worker-id)
  "Main loop for a pool worker thread. Dequeues and executes tasks until
the shutdown sentinel is received."
  (declare (ignore worker-id))
  (loop
    (let ((task (dequeue (thread-pool-task-queue pool))))
      ;; Check for shutdown sentinel
      (when (eq task *pool-shutdown-sentinel*)
        (return))
      ;; Execute the task with error handling
      (handler-case
          (progn
            (funcall task)
            (with-lock ((thread-pool-lock pool))
              (incf (thread-pool-completed pool))))
        (error (c)
          (with-lock ((thread-pool-lock pool))
            (incf (thread-pool-failed pool)))
          (format *error-output*
                  "~&[cauldron] Pool worker error: ~A~%" c))))))

(defun make-thread-pool (&key (size 4))
  "Create a thread pool with SIZE worker threads that immediately begin
waiting for tasks. Each worker runs in its own thread."
  (let* ((queue (make-queue "pool-task-queue"))
         (pool (%make-thread-pool
                :workers (make-array size)
                :task-queue queue
                :size size
                :running-p t)))
    ;; Spawn worker threads
    (loop for i from 0 below size
          do (setf (svref (thread-pool-workers pool) i)
                   (spawn (format nil "pool-worker-~D" i)
                          (let ((id i))
                            (lambda () (%pool-worker-loop pool id))))))
    pool))

(defun submit-task (pool function)
  "Submit FUNCTION to POOL for asynchronous execution.
Signals an error if the pool has been shut down."
  (unless (thread-pool-running-p pool)
    (error "Cannot submit task to a shut-down thread pool"))
  (enqueue (thread-pool-task-queue pool) function)
  (values))

(defun shutdown-pool (pool &key (timeout 5))
  "Shut down POOL gracefully. Sends shutdown sentinels to all workers and
joins them. TIMEOUT (in seconds) controls how long to wait for each worker
before giving up."
  (with-lock ((thread-pool-lock pool))
    (setf (thread-pool-running-p pool) nil))
  ;; Send one sentinel per worker so each worker receives exactly one
  (dotimes (i (thread-pool-size pool))
    (enqueue (thread-pool-task-queue pool) *pool-shutdown-sentinel*))
  ;; Join all worker threads
  (loop for worker across (thread-pool-workers pool)
        do (handler-case
               (if timeout
                   (sb-thread:join-thread worker :timeout timeout)
                   (sb-thread:join-thread worker))
             (sb-thread:join-thread-error ()
               ;; Worker may have already exited
               nil)
             (error (c)
               (format *error-output*
                       "~&[cauldron] Error joining pool worker ~A: ~A~%"
                       (sb-thread:thread-name worker) c))))
  (values))

(defun thread-pool-active-count (pool)
  "Return the count of worker threads that are currently alive."
  (count-if #'sb-thread:thread-alive-p (thread-pool-workers pool)))

(defun thread-pool-queued-count (pool)
  "Return the number of tasks waiting in the pool's queue."
  (queue-length (thread-pool-task-queue pool)))
