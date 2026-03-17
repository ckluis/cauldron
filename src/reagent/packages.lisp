;;;; src/reagent/packages.lisp — Package definitions for the resource layer
(defpackage :cauldron.reagent
  (:use :cl)
  (:export
   ;; Metaclass
   #:resource-metaclass
   #:resource-attributes
   #:resource-actions
   #:resource-policies
   #:resource-calculations
   #:resource-relationships
   #:resource-table-name
   ;; DSL macros
   #:defresource
   #:defaction
   #:defcalculation
   ;; Attributes
   #:attribute-def
   #:make-attribute-def
   #:attribute-def-name
   #:attribute-def-type
   #:attribute-def-required
   #:attribute-def-default
   #:attribute-def-one-of
   #:attribute-def-max-length
   #:attribute-def-min-length
   #:attribute-def-format-re
   #:attribute-def-pg-type
   #:attribute-def-form-field
   #:parse-attribute-spec
   #:attribute-pg-type
   #:attribute-form-field
   ;; Actions
   #:action-def
   #:action-error
   #:run-action
   #:find-action
   ;; Policies
   #:allow
   #:deny
   ;; Relationships
   #:belongs-to
   #:has-many
   #:many-to-many
   #:preload
   ;; Registry
   #:*resource-registry*
   #:resource-registry
   #:register-resource
   #:find-resource-class
   #:all-resources))
