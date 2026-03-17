;;;; src/oracle/conditions.lisp — Cauldron condition hierarchy
;;;; Uses CL's condition system to provide structured, recoverable errors.

(in-package :cauldron.oracle)

;;; --- Base condition ---

(define-condition cauldron-error (error)
  ((message :initarg :message
            :reader cauldron-error-message
            :initform "An error occurred"))
  (:report (lambda (c s)
             (format s "Cauldron error: ~A" (cauldron-error-message c)))))

;;; --- Validation ---

(define-condition validation-error (cauldron-error)
  ((field  :initarg :field  :reader validation-error-field  :initform nil)
   (value  :initarg :value  :reader validation-error-value  :initform nil)
   (errors :initarg :errors :reader validation-error-errors :initform nil))
  (:report (lambda (c s)
             (if (validation-error-field c)
                 (format s "Validation failed on ~A: ~A"
                         (validation-error-field c)
                         (cauldron-error-message c))
                 (format s "Validation failed: ~A"
                         (cauldron-error-message c))))))

;;; --- Authorization ---

(define-condition authorization-error (cauldron-error)
  ((action   :initarg :action   :reader authorization-error-action   :initform nil)
   (resource :initarg :resource :reader authorization-error-resource :initform nil))
  (:report (lambda (c s)
             (format s "Not authorized~@[ to ~A~]~@[ on ~A~]"
                     (authorization-error-action c)
                     (authorization-error-resource c)))))

;;; --- Not found ---

(define-condition not-found-error (cauldron-error)
  ((resource :initarg :resource :reader not-found-error-resource :initform nil)
   (id       :initarg :id       :reader not-found-error-id       :initform nil))
  (:report (lambda (c s)
             (format s "~@[~A ~]not found~@[ (id: ~A)~]"
                     (not-found-error-resource c)
                     (not-found-error-id c)))))

;;; --- Timeout ---

(define-condition timeout-error (cauldron-error)
  ((duration  :initarg :duration  :reader timeout-error-duration  :initform nil)
   (operation :initarg :operation :reader timeout-error-operation :initform nil))
  (:report (lambda (c s)
             (format s "~@[~A ~]timed out~@[ after ~As~]"
                     (timeout-error-operation c)
                     (timeout-error-duration c)))))

;;; --- Conflict ---

(define-condition conflict-error (cauldron-error)
  ((resource :initarg :resource :reader conflict-error-resource :initform nil))
  (:report (lambda (c s)
             (format s "Conflict~@[ on ~A~]: ~A"
                     (conflict-error-resource c)
                     (cauldron-error-message c)))))

;;; --- Database ---

(define-condition database-error (cauldron-error)
  ((code   :initarg :code   :reader database-error-code   :initform nil)
   (detail :initarg :detail :reader database-error-detail :initform nil))
  (:report (lambda (c s)
             (format s "Database error~@[ (~A)~]: ~A~@[~%Detail: ~A~]"
                     (database-error-code c)
                     (cauldron-error-message c)
                     (database-error-detail c)))))
