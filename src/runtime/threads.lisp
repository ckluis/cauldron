;;;; src/runtime/threads.lisp — Threading primitives wrapping sb-thread
(in-package :cauldron.runtime)

(defun spawn (name thunk)
  "Create a named thread that executes THUNK.
Captures any errors that occur during execution and wraps them
so they can be inspected via JOIN-THREAD. Returns the thread object."
  (sb-thread:make-thread
   (lambda ()
     (handler-case (funcall thunk)
       (error (c)
         (format *error-output* "~&[cauldron] Thread ~S caught error: ~A~%" name c)
         (values nil c))))
   :name name))

(defun current-thread ()
  "Return the current thread object."
  sb-thread:*current-thread*)

(defun thread-alive-p (thread)
  "Return T if THREAD is still running."
  (sb-thread:thread-alive-p thread))

(defun thread-name (thread)
  "Return the name string of THREAD."
  (sb-thread:thread-name thread))

(defun kill-thread (thread)
  "Terminate THREAD if it is still alive. Safe to call on dead threads."
  (when (sb-thread:thread-alive-p thread)
    (sb-thread:terminate-thread thread)))

(defun join-thread (thread)
  "Wait for THREAD to complete and return its values."
  (sb-thread:join-thread thread))

(defun make-lock (&optional name)
  "Create a new mutex lock with optional NAME."
  (sb-thread:make-mutex :name (or name "cauldron-lock")))

(defmacro with-lock ((lock) &body body)
  "Execute BODY while holding LOCK."
  `(sb-thread:with-mutex (,lock)
     ,@body))

(defun make-semaphore (&key (count 0) name)
  "Create a new counting semaphore with initial COUNT."
  (sb-thread:make-semaphore :name (or name "cauldron-semaphore") :count count))

(defun signal-semaphore (semaphore)
  "Increment the semaphore count, potentially waking a waiting thread."
  (sb-thread:signal-semaphore semaphore))

(defun wait-semaphore (semaphore &key timeout)
  "Decrement the semaphore count, blocking if zero.
If TIMEOUT (in seconds) is provided, return NIL on timeout instead of blocking
forever. Returns T on successful acquisition."
  (if timeout
      (sb-thread:wait-on-semaphore semaphore :timeout timeout)
      (sb-thread:wait-on-semaphore semaphore)))
