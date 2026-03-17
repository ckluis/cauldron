;;;; src/reagent/attributes.lisp — Resource attribute definitions and defresource macro
(in-package :cauldron.reagent)

;;; -------------------------------------------------------
;;; Attribute Type System
;;;
;;; Maps Cauldron attribute types to PostgreSQL column types
;;; and default form field types for Alembic.
;;; -------------------------------------------------------

(defparameter *attribute-pg-types*
  '((:string    . "VARCHAR(255)")
    (:text      . "TEXT")
    (:integer   . "INTEGER")
    (:float     . "DOUBLE PRECISION")
    (:boolean   . "BOOLEAN")
    (:keyword   . "VARCHAR(255)")
    (:uuid      . "UUID")
    (:timestamp . "TIMESTAMP WITH TIME ZONE")
    (:date      . "DATE")
    (:jsonb     . "JSONB"))
  "Alist mapping Cauldron attribute types to PostgreSQL column type strings.")

(defparameter *attribute-form-fields*
  '((:string    . :text-input)
    (:text      . :textarea)
    (:integer   . :number-input)
    (:float     . :number-input)
    (:boolean   . :checkbox-input)
    (:keyword   . :select-input)
    (:uuid      . :text-input)
    (:timestamp . :text-input)
    (:date      . :text-input)
    (:jsonb     . :textarea))
  "Alist mapping Cauldron attribute types to default form field types.")

(defun attribute-pg-type (attr-type)
  "Return the PostgreSQL column type string for ATTR-TYPE (a keyword like :string)."
  (or (cdr (assoc attr-type *attribute-pg-types*))
      (error "Unknown attribute type: ~S" attr-type)))

(defun attribute-form-field (attr-type)
  "Return the default form field type keyword for ATTR-TYPE."
  (or (cdr (assoc attr-type *attribute-form-fields*))
      :text-input))

;;; -------------------------------------------------------
;;; Attribute Definition Structure
;;; -------------------------------------------------------

(defstruct attribute-def
  "Definition of a single resource attribute."
  (name nil :type symbol)
  (type :string :type keyword)
  (required nil :type boolean)
  (default nil)
  (max-length nil :type (or null integer))
  (min-length nil :type (or null integer))
  (one-of nil :type list)
  (format-re nil :type (or null string))
  (pg-type nil :type (or null string))
  (form-field nil :type (or null keyword)))

(defun parse-attribute-spec (spec)
  "Parse an attribute specification list into an attribute-def struct.
SPEC is (name &key type required default max-length min-length one-of format-re)."
  (destructuring-bind (name &key (type :string) required default
                                 max-length min-length one-of format-re)
      spec
    (make-attribute-def
     :name name
     :type type
     :required required
     :default default
     :max-length max-length
     :min-length min-length
     :one-of one-of
     :format-re format-re
     :pg-type (attribute-pg-type type)
     :form-field (attribute-form-field type))))

;;; -------------------------------------------------------
;;; Spec parsers — used at macroexpansion time by defresource.
;;; Defined before the macro so they are available when the
;;; macro is expanded within the same file during compilation.
;;; -------------------------------------------------------

(defun parse-action-spec (spec)
  "Parse an action spec (name &key accept validate authorize) into a plist."
  (destructuring-bind (name &key accept validate authorize) spec
    (list :name name :accept accept :validate validate :authorize authorize)))

(defun parse-policy-spec (spec)
  "Parse a policy spec (allow/deny action condition) into a plist."
  (destructuring-bind (disposition action condition &rest rest) spec
    (declare (ignore rest))
    (list :disposition disposition :action action :condition condition)))

(defun parse-relationship-spec (spec)
  "Parse a relationship spec (type target &key ...) into a plist."
  (destructuring-bind (rel-type target &key foreign-key through) spec
    (list :type rel-type :target target :foreign-key foreign-key :through through)))

;;; -------------------------------------------------------
;;; defresource macro
;;;
;;; The main DSL macro for defining Cauldron resources.
;;; Expands into a defclass with resource-metaclass and
;;; stores all metadata on the metaclass instance.
;;; -------------------------------------------------------

(defun make-slot-spec (attr-def)
  "Convert an attribute-def into a CLOS slot specification."
  (let ((name (attribute-def-name attr-def))
        (default (attribute-def-default attr-def)))
    `(,name
      :initarg ,(intern (symbol-name name) :keyword)
      :initform ,default
      :accessor ,name)))

(defmacro defresource (name &key attributes actions policies
                                 calculations relationships table-name)
  "Define a resource named NAME with attribute, action, policy, calculation,
and relationship metadata. Expands into a CLOS class with resource-metaclass.

Example:
  (defresource user
    :attributes ((name :type string :required t :max-length 255)
                 (email :type string :required t)
                 (role :type keyword :one-of (:user :admin) :default :user))
    :actions ((create :accept (name email))
              (update :accept (name role)))
    :policies ((allow :read :always)
               (allow :create :when authenticated-p)))"
  (let* ((attr-defs (mapcar #'parse-attribute-spec attributes))
         (slot-specs (mapcar #'make-slot-spec attr-defs))
         (table (or table-name (pluralize-name name))))
    `(progn
       ;; Define the class with resource-metaclass
       (defclass ,name ()
         (,@slot-specs)
         (:metaclass resource-metaclass))

       ;; Store parsed metadata on the metaclass
       (let ((class (find-class ',name)))
         (setf (resource-attributes class)
               (list ,@(mapcar (lambda (spec)
                                 `(parse-attribute-spec ',spec))
                               attributes)))
         (setf (resource-actions class)
               ',(mapcar #'parse-action-spec actions))
         (setf (resource-policies class)
               ',(mapcar #'parse-policy-spec policies))
         (setf (resource-calculations class)
               ',calculations)
         (setf (resource-relationships class)
               ',(mapcar #'parse-relationship-spec (or relationships nil)))
         (setf (resource-table-name class) ,table)

         ;; Ensure registered
         (register-resource ',name class))

       ;; Return the class name
       ',name)))
