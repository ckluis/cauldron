;;;; src/reagent/metaclass.lisp — Resource metaclass using sb-mop
(in-package :cauldron.reagent)

;;; -------------------------------------------------------
;;; Resource Metaclass
;;;
;;; A metaclass (subclass of standard-class) that stores
;;; resource-specific metadata: attributes, actions, policies,
;;; calculations, relationships, and the database table name.
;;; -------------------------------------------------------

(defclass resource-metaclass (standard-class)
  ((resource-attributes
    :initarg :resource-attributes
    :initform nil
    :accessor resource-attributes
    :documentation "List of attribute definitions for this resource.")
   (resource-actions
    :initarg :resource-actions
    :initform nil
    :accessor resource-actions
    :documentation "List of action definitions for this resource.")
   (resource-policies
    :initarg :resource-policies
    :initform nil
    :accessor resource-policies
    :documentation "List of policy rules for this resource.")
   (resource-calculations
    :initarg :resource-calculations
    :initform nil
    :accessor resource-calculations
    :documentation "List of calculated field definitions.")
   (resource-relationships
    :initarg :resource-relationships
    :initform nil
    :accessor resource-relationships
    :documentation "List of relationship definitions.")
   (resource-table-name
    :initarg :resource-table-name
    :initform nil
    :accessor resource-table-name
    :documentation "PostgreSQL table name (string). Derived from class name if not specified."))
  (:documentation "Metaclass for Cauldron resources. Stores attribute, action,
policy, calculation, and relationship metadata alongside the standard CLOS class."))

;;; Allow resource-metaclass instances to have standard-class as superclass
(defmethod sb-mop:validate-superclass ((class resource-metaclass)
                                       (superclass standard-class))
  t)

;;; Pluralize a resource name for the table name.
;;; Naive English pluralization — handles common suffixes.
(defun pluralize-name (name)
  "Pluralize a symbol name into a lowercase plural string.
Handles: -y → -ies, -s/-x/-sh/-ch → -es, otherwise append -s."
  (let ((s (string-downcase (symbol-name name))))
    (cond
      ;; words ending in consonant + y → ies
      ((and (> (length s) 1)
            (char= (char s (1- (length s))) #\y)
            (not (member (char s (- (length s) 2))
                         '(#\a #\e #\i #\o #\u))))
       (concatenate 'string (subseq s 0 (1- (length s))) "ies"))
      ;; words ending in s, x, sh, ch → es
      ((or (char= (char s (1- (length s))) #\s)
           (char= (char s (1- (length s))) #\x)
           (and (>= (length s) 2)
                (string= (subseq s (- (length s) 2)) "sh"))
           (and (>= (length s) 2)
                (string= (subseq s (- (length s) 2)) "ch")))
       (concatenate 'string s "es"))
      ;; default: append s
      (t (concatenate 'string s "s")))))

;;; Forward-declare the registry so metaclass initialization can register.
;;; The full registry API is in registry.lisp.
(defvar *resource-registry* (make-hash-table :test 'eq)
  "Global registry mapping resource name symbols to their resource-metaclass instances.")

(defun %register-resource (name class)
  "Internal: register resource CLASS under NAME. Called during class init."
  (setf (gethash name *resource-registry*) class))

;;; After class initialization, derive table name if not set and register.
(defmethod initialize-instance :after ((class resource-metaclass) &rest initargs)
  (declare (ignore initargs))
  (unless (resource-table-name class)
    (when (class-name class)
      (setf (resource-table-name class)
            (pluralize-name (class-name class)))))
  (when (class-name class)
    (%register-resource (class-name class) class)))

(defmethod reinitialize-instance :after ((class resource-metaclass) &rest initargs)
  (declare (ignore initargs))
  (unless (resource-table-name class)
    (when (class-name class)
      (setf (resource-table-name class)
            (pluralize-name (class-name class)))))
  (when (class-name class)
    (%register-resource (class-name class) class)))
