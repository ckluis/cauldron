;;;; src/runtime/shutdown.lisp — Graceful shutdown support
(in-package :cauldron.runtime)

;;; --- Shutdown hook registry ---

(defvar *shutdown-hooks* '()
  "List of functions to call during graceful shutdown.
Functions are called in the order they were registered.")

(defvar *shutdown-lock* (sb-thread:make-mutex :name "shutdown-lock")
  "Lock protecting *shutdown-hooks* modifications.")

(defvar *shutting-down-p* nil
  "T when a graceful shutdown is in progress.")

(defun register-shutdown-hook (fn)
  "Register FN as a shutdown hook. FN is a function of zero arguments
that will be called during graceful-shutdown. Hooks run in registration order.
Returns FN."
  (sb-thread:with-mutex (*shutdown-lock*)
    (setf *shutdown-hooks* (append *shutdown-hooks* (list fn))))
  fn)

(defun graceful-shutdown ()
  "Execute all registered shutdown hooks in order.
Each hook runs in a handler-case so one failing hook does not prevent
subsequent hooks from executing. Logs errors to *error-output*.
Returns a list of (:ok fn) or (:error fn condition) for each hook."
  (when *shutting-down-p*
    (format *error-output* "~&[cauldron] Shutdown already in progress, ignoring.~%")
    (return-from graceful-shutdown nil))
  (setf *shutting-down-p* t)
  (let ((hooks (sb-thread:with-mutex (*shutdown-lock*)
                 (copy-list *shutdown-hooks*)))
        (results '()))
    (format t "~&[cauldron] Graceful shutdown: ~D hooks to run~%" (length hooks))
    (dolist (hook hooks)
      (handler-case
          (progn
            (funcall hook)
            (push (list :ok hook) results))
        (error (c)
          (format *error-output*
                  "~&[cauldron] Shutdown hook error: ~A~%" c)
          (push (list :error hook c) results))))
    (setf *shutting-down-p* nil)
    (nreverse results)))

(defun install-signal-handlers ()
  "Install SIGTERM and SIGINT handlers that call graceful-shutdown.
Uses sb-posix when available; logs a warning otherwise."
  (flet ((handle-signal (signum)
           (declare (ignore signum))
           (format t "~&[cauldron] Signal received, initiating graceful shutdown...~%")
           (graceful-shutdown)
           (sb-ext:exit :code 0)))
    (handler-case
        (progn
          ;; SIGTERM = 15, SIGINT = 2
          (sb-sys:enable-interrupt sb-unix:sigterm #'handle-signal)
          (sb-sys:enable-interrupt sb-unix:sigint #'handle-signal)
          (format t "~&[cauldron] Signal handlers installed (SIGTERM, SIGINT)~%")
          t)
      (error (c)
        (format *error-output*
                "~&[cauldron] Warning: Could not install signal handlers: ~A~%" c)
        nil))))
