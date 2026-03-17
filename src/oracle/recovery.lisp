;;;; src/oracle/recovery.lisp — Error recovery using CL restarts
;;;; Provides structured recovery strategies for Cauldron actions.

(in-package :cauldron.oracle)

;;; --- Restart helpers ---

(defun retry-action (&optional (max-retries 3))
  "Invoke the RETRY-ACTION restart if available. Signals error after MAX-RETRIES."
  (let ((restart (find-restart 'retry-action)))
    (when restart
      (invoke-restart restart max-retries))))

(defun skip-action ()
  "Invoke the SKIP-ACTION restart if available. Returns NIL from the action."
  (let ((restart (find-restart 'skip-action)))
    (when restart
      (invoke-restart restart))))

(defun use-value (value)
  "Invoke the USE-VALUE restart with VALUE if available."
  (let ((restart (find-restart 'use-value)))
    (when restart
      (invoke-restart restart value))))

(defun abort-action ()
  "Invoke the ABORT-ACTION restart if available."
  (let ((restart (find-restart 'abort-action)))
    (when restart
      (invoke-restart restart))))

;;; --- with-recovery macro ---

(defmacro with-recovery ((&rest handler-specs) &body body)
  "Execute BODY with error recovery restarts and condition handlers.

Each handler-spec is (CONDITION-TYPE &key retry skip use-value abort handler).
  :retry N      — automatically retry up to N times on this condition
  :skip T       — skip and return NIL on this condition
  :use-value V  — return V on this condition
  :abort T      — signal abort on this condition
  :handler FN   — custom handler function called with the condition

Example:
  (with-recovery ((timeout-error :retry 3)
                  (not-found-error :skip t)
                  (validation-error :handler #'log-and-continue))
    (do-something-risky))"
  (let ((retry-count (gensym "RETRY-COUNT"))
        (block-name (gensym "RECOVERY-BLOCK")))
    `(block ,block-name
       (let ((,retry-count 0))
         (declare (ignorable ,retry-count))
         (tagbody
          retry-point
          (restart-case
              (handler-bind
                  (,@(loop for spec in handler-specs
                           collect (destructuring-bind (condition-type &key retry skip use-value abort handler)
                                       spec
                                     (declare (ignorable use-value))
                                     `(,condition-type
                                       (lambda (c)
                                         (declare (ignorable c))
                                         ,@(cond
                                             (retry
                                              `((when (< ,retry-count ,retry)
                                                  (incf ,retry-count)
                                                  (go retry-point))))
                                             (skip
                                              `((return-from ,block-name nil)))
                                             (use-value
                                              `((return-from ,block-name ,use-value)))
                                             (abort
                                              `((error c)))
                                             (handler
                                              `((funcall ,handler c)))))))))
                (return-from ,block-name
                  (progn ,@body)))
            (retry-action (&optional max-retries)
              :report "Retry the action"
              (when (and max-retries (>= ,retry-count max-retries))
                (error 'cauldron-error
                       :message (format nil "Max retries (~D) exceeded" max-retries)))
              (incf ,retry-count)
              (go retry-point))
            (skip-action ()
              :report "Skip the action and return NIL"
              (return-from ,block-name nil))
            (use-value (value)
              :report "Use a specified value"
              :interactive (lambda () (list (read)))
              (return-from ,block-name value))
            (abort-action ()
              :report "Abort the action"
              (return-from ,block-name
                (error 'cauldron-error
                       :message "Action aborted")))))))))
