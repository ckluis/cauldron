;;;; src/reagent/actions.lisp — Resource action definitions and pipeline
(in-package :cauldron.reagent)

;;; -------------------------------------------------------
;;; Action Definition
;;;
;;; Actions are named operations on resources (create, update,
;;; delete, etc.) with a pipeline: validate → authorize → persist → hooks.
;;; -------------------------------------------------------

(defstruct action-def
  "Definition of a resource action."
  (name nil :type symbol)
  (resource nil :type symbol)
  (accept nil :type list)
  (validate nil)
  (authorize nil)
  (before-hooks nil :type list)
  (after-hooks nil :type list)
  (handler nil))

(defvar *action-registry* (make-hash-table :test 'equal)
  "Global registry of (resource-name . action-name) → action-def.")

(defun action-key (resource action)
  "Create a hash key for the action registry."
  (cons resource action))

(defun find-action (resource action)
  "Look up an action-def by RESOURCE and ACTION names."
  (gethash (action-key resource action) *action-registry*))

(defmacro defaction (resource-name action-name &key accept validate authorize
                                                     before after handler)
  "Define an action ACTION-NAME on resource RESOURCE-NAME.

:accept    — list of attribute names this action accepts
:validate  — function (params) → (values valid-p errors)
:authorize — function (actor params) → boolean
:before    — list of functions called before persist
:after     — list of functions called after persist
:handler   — custom handler function (actor params) → result

Example:
  (defaction user create
    :accept (name email)
    :validate #'validate-user-create
    :authorize #'require-admin)"
  `(progn
     (setf (gethash (action-key ',resource-name ',action-name)
                     *action-registry*)
           (make-action-def
            :name ',action-name
            :resource ',resource-name
            :accept ',accept
            :validate ,validate
            :authorize ,authorize
            :before-hooks (list ,@before)
            :after-hooks (list ,@after)
            :handler ,handler))
     ;; Also update the metaclass action list if class exists
     (let ((class (find-class ',resource-name nil)))
       (when (and class (typep class 'resource-metaclass))
         (let* ((actions (resource-actions class))
                (existing (find ',action-name actions
                                :key (lambda (a) (getf a :name)))))
           (if existing
               ;; Update existing
               (setf (getf existing :accept) ',accept
                     (getf existing :validate) ,validate
                     (getf existing :authorize) ,authorize)
               ;; Add new
               (push (list :name ',action-name
                           :accept ',accept
                           :validate ,validate
                           :authorize ,authorize)
                     (resource-actions class))))))
     ',action-name))

;;; -------------------------------------------------------
;;; Action Execution Pipeline
;;; -------------------------------------------------------

(define-condition action-error (error)
  ((action :initarg :action :reader action-error-action)
   (phase :initarg :phase :reader action-error-phase)
   (detail :initarg :detail :reader action-error-detail))
  (:report (lambda (c stream)
             (format stream "Action ~S failed in ~A phase: ~A"
                     (action-error-action c)
                     (action-error-phase c)
                     (action-error-detail c)))))

(defun filter-params (params accept-list)
  "Filter PARAMS plist to only include keys in ACCEPT-LIST.
ACCEPT-LIST is a list of symbols; they are matched as keyword arguments."
  (if (null accept-list)
      params
      (let ((accepted-keys (mapcar (lambda (s) (intern (symbol-name s) :keyword))
                                   accept-list))
            (result nil))
        (loop for (key val) on params by #'cddr
              when (member key accepted-keys)
              do (push key result)
                 (push val result))
        (nreverse result))))

(defun run-action (resource-name action-name actor params &key persist-fn)
  "Execute the action pipeline for RESOURCE-NAME / ACTION-NAME.

Pipeline:
  1. Filter params through :accept list
  2. Run :validate function → signals action-error on failure
  3. Run :authorize function → signals action-error on denial
  4. Run :before hooks
  5. Call PERSIST-FN (or :handler) with filtered params
  6. Run :after hooks
  7. Return result

ACTOR is the current user/context. PARAMS is a plist.
PERSIST-FN is an optional function (filtered-params) → result for database persistence."
  (let ((action-def (find-action resource-name action-name)))
    (unless action-def
      (error 'action-error :action action-name :phase :lookup
                           :detail (format nil "No action ~S defined for ~S"
                                           action-name resource-name)))
    (let ((filtered (filter-params params (action-def-accept action-def))))
      ;; 1. Validate
      (when (action-def-validate action-def)
        (multiple-value-bind (valid-p errors)
            (funcall (action-def-validate action-def) filtered)
          (unless valid-p
            (error 'action-error :action action-name :phase :validate
                                 :detail errors))))
      ;; 2. Authorize
      (when (action-def-authorize action-def)
        (unless (funcall (action-def-authorize action-def) actor filtered)
          (error 'action-error :action action-name :phase :authorize
                               :detail "Authorization denied")))
      ;; 3. Before hooks
      (dolist (hook (action-def-before-hooks action-def))
        (funcall hook actor filtered))
      ;; 4. Persist / handle
      (let ((result (cond
                      ((action-def-handler action-def)
                       (funcall (action-def-handler action-def) actor filtered))
                      (persist-fn
                       (funcall persist-fn filtered))
                      (t filtered))))
        ;; 5. After hooks
        (dolist (hook (action-def-after-hooks action-def))
          (funcall hook actor result))
        ;; 6. Return
        result))))

;;; -------------------------------------------------------
;;; defcalculation macro
;;; -------------------------------------------------------

(defstruct calculation-def
  "Definition of a calculated (virtual) field on a resource."
  (name nil :type symbol)
  (resource nil :type symbol)
  (dependencies nil :type list)
  (compute nil))

(defvar *calculation-registry* (make-hash-table :test 'equal)
  "Global registry of (resource-name . calc-name) → calculation-def.")

(defmacro defcalculation (resource-name calc-name &key dependencies compute)
  "Define a calculated field CALC-NAME on RESOURCE-NAME.

:dependencies — list of attribute names this calculation reads
:compute      — function (resource-instance) → value

Example:
  (defcalculation user full-name
    :dependencies (first-name last-name)
    :compute (lambda (u)
               (format nil \"~A ~A\" (first-name u) (last-name u))))"
  `(progn
     (setf (gethash (cons ',resource-name ',calc-name) *calculation-registry*)
           (make-calculation-def
            :name ',calc-name
            :resource ',resource-name
            :dependencies ',dependencies
            :compute ,compute))
     ',calc-name))

(defun compute-calculation (resource-name calc-name instance)
  "Compute the calculated field CALC-NAME for INSTANCE of RESOURCE-NAME."
  (let ((calc-def (gethash (cons resource-name calc-name) *calculation-registry*)))
    (unless calc-def
      (error "No calculation ~S defined for ~S" calc-name resource-name))
    (funcall (calculation-def-compute calc-def) instance)))
