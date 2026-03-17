;;;; src/grimoire/changeset.lisp — Data validation and transformation
;;;; Changesets validate and transform data before persistence.
;;;; Inspired by Ecto changesets.

(in-package :cauldron.grimoire)

;;; --- Changeset struct ---

(defstruct changeset
  "Represents a set of changes to be validated and applied."
  (data nil)                        ; Original data (hash-table or nil for create)
  (changes (make-hash-table :test 'equal) :type hash-table)  ; Pending changes
  (errors '() :type list)           ; List of (field . message) pairs
  (valid-p t :type boolean)         ; Computed from errors
  (action nil :type symbol)         ; :insert or :update
  (source nil :type (or null string))) ; Table name

;;; --- Changeset creation ---

(defun cast (data params allowed-fields &key (action :insert) source)
  "Create a changeset by casting PARAMS onto DATA.
Only ALLOWED-FIELDS from PARAMS are included.
DATA is a hash-table (for update) or NIL (for insert).
PARAMS is a hash-table or alist of incoming values."
  (let ((cs (make-changeset :data data :action action :source source))
        (param-table (if (hash-table-p params)
                         params
                         ;; Convert alist to hash-table
                         (let ((ht (make-hash-table :test 'equal)))
                           (dolist (pair params ht)
                             (setf (gethash (car pair) ht) (cdr pair)))))))
    (dolist (field allowed-fields)
      (let ((field-name (if (stringp field) field (string-downcase (symbol-name field)))))
        (multiple-value-bind (val present-p)
            (gethash field-name param-table)
          (when present-p
            ;; Only include if different from original (for updates)
            (let ((changed (or (null data)
                               (not (equal val (gethash field-name data))))))
              (when changed
                (setf (gethash field-name (changeset-changes cs)) val)))))))
    cs))

;;; --- Validations ---

(defun add-error (changeset field message)
  "Add a validation error to CHANGESET."
  (push (cons field message) (changeset-errors changeset))
  (setf (changeset-valid-p changeset) nil)
  changeset)

(defun get-field-value (changeset field)
  "Get the value of FIELD — from changes first, then original data."
  (let ((field-name (if (stringp field) field (string-downcase (symbol-name field)))))
    (multiple-value-bind (val present) (gethash field-name (changeset-changes changeset))
      (if present
          (values val t)
          (when (changeset-data changeset)
            (gethash field-name (changeset-data changeset)))))))

(defun validate-required (changeset &rest fields)
  "Validate that FIELDS are present and non-empty."
  (dolist (field fields changeset)
    (let ((val (get-field-value changeset field)))
      (when (or (null val)
                (and (stringp val) (= 0 (length val))))
        (add-error changeset field "is required")))))

(defun validate-length (changeset field &key min max)
  "Validate string length of FIELD."
  (let ((val (get-field-value changeset field)))
    (when (and val (stringp val))
      (when (and min (< (length val) min))
        (add-error changeset field
                   (format nil "must be at least ~D characters" min)))
      (when (and max (> (length val) max))
        (add-error changeset field
                   (format nil "must be at most ~D characters" max)))))
  changeset)

(defun validate-format (changeset field pattern &key (message "has invalid format"))
  "Validate that FIELD matches a simple pattern.
PATTERN is a function (string) → boolean."
  (let ((val (get-field-value changeset field)))
    (when (and val (stringp val) (not (funcall pattern val)))
      (add-error changeset field message)))
  changeset)

(defun validate-inclusion (changeset field valid-values &key (message nil))
  "Validate that FIELD value is in VALID-VALUES."
  (let ((val (get-field-value changeset field)))
    (when (and val (not (member val valid-values :test #'equal)))
      (add-error changeset field
                 (or message
                     (format nil "must be one of: ~{~A~^, ~}" valid-values)))))
  changeset)

(defun validate-number (changeset field &key min max)
  "Validate that FIELD is a number within range."
  (let ((val (get-field-value changeset field)))
    (when val
      (unless (numberp val)
        (add-error changeset field "must be a number")
        (return-from validate-number changeset))
      (when (and min (< val min))
        (add-error changeset field (format nil "must be at least ~A" min)))
      (when (and max (> val max))
        (add-error changeset field (format nil "must be at most ~A" max)))))
  changeset)

;;; --- Changeset query accessors ---

(defun changeset-errors-for (changeset field)
  "Get all error messages for FIELD."
  (let ((field-name (if (stringp field) field (string-downcase (symbol-name field)))))
    (mapcar #'cdr
            (remove-if-not (lambda (pair) (string= (car pair) field-name))
                           (changeset-errors changeset)))))

;;; --- Application (generates SQL) ---

(defun apply-changeset (changeset &key table returning)
  "Generate SQL query from a valid changeset.
Returns a query struct ready for execution.
Signals QUERY-ERROR if changeset is invalid."
  (unless (changeset-valid-p changeset)
    (error 'query-error
           :message (format nil "Cannot apply invalid changeset. Errors: ~{~A: ~A~^; ~}"
                            (loop for (field . msg) in (changeset-errors changeset)
                                  collect field collect msg))))
  (let ((source (or table (changeset-source changeset)))
        (changes (changeset-changes changeset)))
    (unless source
      (error 'query-error :message "No table specified for changeset"))
    (let ((cols '()) (vals '()))
      (maphash (lambda (k v)
                 (push k cols)
                 (push v vals))
               changes)
      (setf cols (nreverse cols))
      (setf vals (nreverse vals))
      (let ((q (ecase (changeset-action changeset)
                 (:insert
                  (%make-query :source source
                               :operation :insert
                               :columns cols
                               :values-list vals))
                 (:update
                  (%make-query :source source
                               :operation :update
                               :columns cols
                               :values-list vals)))))
        (when returning
          (setf (query-returning q) (if (listp returning) returning (list returning))))
        q))))
