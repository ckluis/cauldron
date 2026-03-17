;;;; src/reagent/registry.lisp — Global resource registry
(in-package :cauldron.reagent)

;;; -------------------------------------------------------
;;; Resource Registry
;;;
;;; *resource-registry* is defined in metaclass.lisp (forward
;;; declaration) so it's available during class initialization.
;;; This file provides the public query/mutation API.
;;; -------------------------------------------------------

(defun register-resource (name class)
  "Register resource CLASS under NAME in the global registry."
  (setf (gethash name *resource-registry*) class))

(defun find-resource-class (name)
  "Look up a resource class by NAME (symbol). Returns the metaclass instance or NIL."
  (gethash name *resource-registry*))

(defun all-resources ()
  "Return a list of (name . class) pairs for all registered resources."
  (let ((result '()))
    (maphash (lambda (name class)
               (push (cons name class) result))
             *resource-registry*)
    (nreverse result)))

(defun resource-registry ()
  "Return the global resource registry hash-table."
  *resource-registry*)
