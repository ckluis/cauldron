;;;; src/oracle/packages.lisp — Package definitions for the condition/error system

(defpackage :cauldron.oracle
  (:use :cl)
  (:shadow #:use-value)
  (:export
   ;; Condition hierarchy
   #:cauldron-error
   #:cauldron-error-message
   #:validation-error
   #:validation-error-field
   #:validation-error-value
   #:validation-error-errors
   #:authorization-error
   #:authorization-error-action
   #:authorization-error-resource
   #:not-found-error
   #:not-found-error-resource
   #:not-found-error-id
   #:timeout-error
   #:timeout-error-duration
   #:timeout-error-operation
   #:conflict-error
   #:conflict-error-resource
   #:database-error
   #:database-error-code
   #:database-error-detail
   ;; Action hooks
   #:install-action-hook
   #:remove-action-hook
   #:clear-action-hooks
   #:run-action-hooks
   #:*action-hooks*
   ;; Recovery
   #:with-recovery
   #:retry-action
   #:skip-action
   #:use-value
   #:abort-action))
