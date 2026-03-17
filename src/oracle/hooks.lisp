;;;; src/oracle/hooks.lisp — Action hook system
;;;; Provides :before, :after-success, :after-failure, :around hooks
;;;; for Reagent actions.

(in-package :cauldron.oracle)

;;; --- Hook storage ---

(defstruct action-hook
  "A registered action hook."
  (name     nil :type symbol)
  (phase    :before :type keyword)    ; :before, :after-success, :after-failure, :around
  (priority 0 :type fixnum)           ; Higher runs first
  (action   nil)                       ; Action name filter (nil = all actions)
  (test     nil)                       ; Predicate (nil = always run)
  (function nil :type (or null function)))

(defvar *action-hooks* '()
  "Global list of registered action hooks, sorted by priority (descending).")

(defun install-action-hook (name function &key (phase :before) (priority 0)
                                               action test)
  "Install a hook named NAME that calls FUNCTION.
PHASE is one of :before, :after-success, :after-failure, :around.
PRIORITY determines ordering (higher runs first).
ACTION filters to a specific action name (nil = all).
TEST is an optional predicate called with (context) — hook runs only if truthy."
  (remove-action-hook name)
  (let ((hook (make-action-hook :name name
                                :phase phase
                                :priority priority
                                :action action
                                :test test
                                :function function)))
    (setf *action-hooks*
          (sort (cons hook *action-hooks*)
                #'> :key #'action-hook-priority))
    hook))

(defun remove-action-hook (name)
  "Remove the hook named NAME."
  (setf *action-hooks*
        (remove name *action-hooks* :key #'action-hook-name)))

(defun clear-action-hooks ()
  "Remove all registered hooks."
  (setf *action-hooks* '()))

(defun hooks-for-phase (phase action-name)
  "Return hooks matching PHASE and ACTION-NAME, in priority order."
  (loop for hook in *action-hooks*
        when (and (eq (action-hook-phase hook) phase)
                  (or (null (action-hook-action hook))
                      (eq (action-hook-action hook) action-name)))
          collect hook))

(defun run-action-hooks (phase action-name context &optional body-fn)
  "Run all hooks for PHASE and ACTION-NAME with CONTEXT.
For :around hooks, BODY-FN is the function to wrap.
Returns the result of the last hook or body-fn."
  (let ((hooks (hooks-for-phase phase action-name)))
    (case phase
      (:around
       (if (null hooks)
           (when body-fn (funcall body-fn context))
           ;; Build the chain: innermost = body-fn, wrap with each :around hook
           (let ((chain body-fn))
             (dolist (hook (reverse hooks))
               (let ((outer-fn (action-hook-function hook))
                     (inner chain))
                 (when (or (null (action-hook-test hook))
                           (funcall (action-hook-test hook) context))
                   (setf chain
                         (lambda (ctx)
                           (funcall outer-fn ctx inner))))))
             (funcall chain context))))
      (otherwise
       ;; :before, :after-success, :after-failure — run sequentially
       (let ((result nil))
         (dolist (hook hooks result)
           (when (or (null (action-hook-test hook))
                     (funcall (action-hook-test hook) context))
             (setf result
                   (funcall (action-hook-function hook) context)))))))))
