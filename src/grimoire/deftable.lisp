;;;; src/grimoire/deftable.lisp — Declarative table definitions and DDL generation
;;;;
;;;; Provides deftable macro for defining database tables as Lisp data.
;;;; All DDL is generated from declarations — no hand-written SQL.
;;;;
;;;; Usage:
;;;;   (deftable contacts
;;;;     (:column first_name :type text :required t)
;;;;     (:column email :type email)
;;;;     (:column company_id :type integer :references crm_companies)
;;;;     (:index email)
;;;;     (:unique email))
;;;;
;;;; Every table auto-gets: id SERIAL PRIMARY KEY, created_at, updated_at.

(in-package :cauldron.grimoire)

;;; --- Table spec data structures ---

(defstruct table-spec
  "A declarative table definition."
  (name nil :type symbol)
  (columns nil :type list)     ; list of column-spec
  (indexes nil :type list)     ; list of (:columns (col ...) :unique-p bool)
  (seed-sql nil :type list))   ; list of raw SQL strings for seed data

(defstruct column-spec
  "A column definition within a table."
  (name nil :type (or symbol string))
  (field-type "text" :type string)
  (required-p nil :type boolean)
  (default nil)
  (references nil :type (or null symbol string))
  (on-delete nil :type (or null string))  ; CASCADE, SET NULL, etc.
  (unique-p nil :type boolean))

;;; --- Registry ---

(defvar *table-registry* (make-hash-table :test 'equal)
  "Global registry of table definitions: name-string -> table-spec.")

(defun find-table-spec (name)
  "Look up a table spec by name (string or symbol)."
  (gethash (table-name-string name) *table-registry*))

(defun table-name-string (name)
  "Normalize a table name to a lowercase string with underscores."
  (substitute #\_ #\- (string-downcase (string name))))

;;; --- Field type to PostgreSQL type mapping ---

(defun field-type-to-pg-type (field-type)
  "Map a field type string to a PostgreSQL column type.
Supports: text, email, phone, url, textarea, number, integer, boolean,
date, datetime, enum, money, json."
  (let ((ft (string-downcase field-type)))
    (cond
      ((string= ft "text") "TEXT")
      ((string= ft "email") "TEXT")
      ((string= ft "phone") "TEXT")
      ((string= ft "url") "TEXT")
      ((string= ft "textarea") "TEXT")
      ((string= ft "number") "NUMERIC")
      ((string= ft "integer") "INTEGER")
      ((string= ft "boolean") "BOOLEAN")
      ((string= ft "date") "DATE")
      ((string= ft "datetime") "TIMESTAMPTZ")
      ((string= ft "enum") "TEXT")
      ((string= ft "money") "NUMERIC(12,2)")
      ((string= ft "json") "JSONB")
      ((string= ft "serial") "SERIAL")
      (t "TEXT"))))

;;; --- deftable macro ---

(defmacro deftable (name &body clauses)
  "Define a table declaratively and register it in the global table registry.

CLAUSES can be:
  (:column col-name :type type :required t :default val :references table :on-delete action :unique t)
  (:index col1 col2 ...)
  (:unique col1 col2 ...)
  (:seed \"SQL string\")

Every table automatically includes id, created_at, and updated_at columns."
  (let ((name-str (table-name-string name)))
    `(setf (gethash ,name-str *table-registry*)
           (parse-table-clauses ',name ',clauses))))

(defun parse-table-clauses (name clauses)
  "Parse deftable clauses into a table-spec struct."
  (let ((columns nil)
        (indexes nil)
        (seeds nil))
    (dolist (clause clauses)
      (let ((kind (first clause)))
        (case kind
          (:column
           (push (parse-column-clause (rest clause)) columns))
          (:index
           (push (list :columns (mapcar #'table-name-string (rest clause))
                       :unique-p nil)
                 indexes))
          (:unique
           (push (list :columns (mapcar #'table-name-string (rest clause))
                       :unique-p t)
                 indexes))
          (:seed
           (push (second clause) seeds))
          (otherwise
           (error "Unknown deftable clause: ~S" clause)))))
    (make-table-spec
     :name name
     :columns (nreverse columns)
     :indexes (nreverse indexes)
     :seed-sql (nreverse seeds))))

(defun parse-column-clause (args)
  "Parse a (:column name :type T :required R ...) clause into a column-spec."
  (let ((name (first args))
        (props (rest args)))
    (make-column-spec
     :name name
     :field-type (string-downcase (or (getf props :type) "text"))
     :required-p (getf props :required)
     :default (getf props :default)
     :references (getf props :references)
     :on-delete (getf props :on-delete)
     :unique-p (getf props :unique))))

;;; --- DDL generation ---

(defun generate-table-ddl (table-spec)
  "Generate a list of SQL strings (CREATE TABLE + CREATE INDEX) from a table-spec.
Returns a list of SQL strings to execute in order."
  (let ((table-name (table-name-string (table-spec-name table-spec)))
        (result nil))
    ;; CREATE TABLE
    (push (generate-create-table-sql table-name (table-spec-columns table-spec)) result)
    ;; CREATE INDEX statements
    (dolist (idx (table-spec-indexes table-spec))
      (let ((cols (getf idx :columns))
            (unique-p (getf idx :unique-p)))
        (push (generate-index-sql table-name cols unique-p) result)))
    ;; Column-level unique indexes
    (dolist (col (table-spec-columns table-spec))
      (when (column-spec-unique-p col)
        (let ((col-name (table-name-string (column-spec-name col))))
          (push (generate-index-sql table-name (list col-name) t) result))))
    ;; Seed data
    (dolist (sql (table-spec-seed-sql table-spec))
      (push sql result))
    (nreverse result)))

(defun generate-create-table-sql (table-name columns)
  "Generate a CREATE TABLE statement from column specs."
  (with-output-to-string (s)
    (format s "CREATE TABLE ~A (~%" table-name)
    (format s "  id SERIAL PRIMARY KEY")
    ;; User-defined columns
    (dolist (col columns)
      (let* ((col-name (table-name-string (column-spec-name col)))
             (pg-type (field-type-to-pg-type (column-spec-field-type col)))
             (required (column-spec-required-p col))
             (default (column-spec-default col))
             (references (column-spec-references col))
             (on-delete (column-spec-on-delete col)))
        (format s ",~%  ~A ~A" col-name pg-type)
        (when required (format s " NOT NULL"))
        (when default
          (format s " DEFAULT ~A" (sql-default-value default)))
        (when references
          (let ((ref-table (table-name-string references)))
            (format s " REFERENCES ~A(id)" ref-table)
            (when on-delete
              (format s " ON DELETE ~A" on-delete))))))
    ;; Auto-timestamp columns
    (format s ",~%  created_at TIMESTAMPTZ DEFAULT NOW()")
    (format s ",~%  updated_at TIMESTAMPTZ DEFAULT NOW()")
    (format s "~%)")))

(defun sql-default-value (value)
  "Format a default value for SQL. Strings get quoted, symbols/keywords become uppercase,
numbers stay as-is."
  (cond
    ((stringp value) (format nil "'~A'" value))
    ((numberp value) (format nil "~A" value))
    ((eq value t) "TRUE")
    ((null value) "NULL")
    ((symbolp value) (string-upcase (string value)))
    (t (format nil "'~A'" value))))

(defun generate-index-sql (table-name columns unique-p)
  "Generate a CREATE INDEX statement."
  (let* ((col-str (format nil "~{~A~^, ~}" columns))
         (idx-name (format nil "idx_~A_~{~A~^_~}" table-name columns))
         (unique-str (if unique-p "UNIQUE " "")))
    (format nil "CREATE ~AINDEX ~A ON ~A (~A)"
            unique-str idx-name table-name col-str)))

;;; --- Metadata SQL generation ---

(defun generate-table-metadata-sql (table-spec)
  "Generate INSERT statements for _custom_objects and _custom_fields metadata.
Returns a list of SQL strings."
  (let* ((name (table-name-string (table-spec-name table-spec)))
         (label (string-capitalize (substitute #\Space #\_ name)))
         (result nil))
    ;; Insert into _custom_objects
    (push (format nil
            "INSERT INTO _custom_objects (name, label, table_name, system_p) VALUES ('~A', '~A', '~A', true)"
            name label name)
          result)
    ;; Insert fields
    (let ((position 1))
      (dolist (col (table-spec-columns table-spec))
        (let* ((col-name (table-name-string (column-spec-name col)))
               (col-label (string-capitalize (substitute #\Space #\_ col-name)))
               (field-type (column-spec-field-type col))
               (required (if (column-spec-required-p col) "true" "false")))
          (push (format nil
                  "INSERT INTO _custom_fields (object_id, name, label, field_type, required_p, position) VALUES ((SELECT id FROM _custom_objects WHERE name='~A'), '~A', '~A', '~A', ~A, ~D)"
                  name col-name col-label field-type required position)
                result))
        (incf position)))
    ;; Insert relation metadata for FK columns
    (dolist (col (table-spec-columns table-spec))
      (let ((refs (column-spec-references col)))
        (when refs
          (let* ((col-name (table-name-string (column-spec-name col)))
                 (ref-table (table-name-string refs))
                 (on-delete (or (column-spec-on-delete col) "SET NULL"))
                 (on-delete-rel (cond
                                  ((string-equal on-delete "CASCADE") "cascade")
                                  ((string-equal on-delete "RESTRICT") "restrict")
                                  (t "nullify")))
                 (belongs-label (string-capitalize (substitute #\Space #\_ ref-table)))
                 (has-many-label (string-capitalize (substitute #\Space #\_ name))))
            ;; belongs_to row: from this table to the referenced table
            (push (format nil
                    "INSERT INTO _custom_relations (source_object, target_object, relation_type, foreign_key, label, on_delete, system_p) VALUES ('~A', '~A', 'belongs_to', '~A', '~A', '~A', true) ON CONFLICT DO NOTHING"
                    name ref-table col-name belongs-label on-delete-rel)
                  result)
            ;; has_many inverse: from the referenced table back to this table
            (push (format nil
                    "INSERT INTO _custom_relations (source_object, target_object, relation_type, foreign_key, label, on_delete, system_p) VALUES ('~A', '~A', 'has_many', '~A', '~A', '~A', true) ON CONFLICT DO NOTHING"
                    ref-table name col-name has-many-label on-delete-rel)
                  result)))))
    (nreverse result)))

;;; --- Convenience: generate all DDL + metadata for a table ---

(defun generate-full-table-sql (table-spec)
  "Generate all SQL for a table: CREATE TABLE, indexes, and metadata registration.
Returns a flat list of SQL strings."
  (append (generate-table-ddl table-spec)
          (generate-table-metadata-sql table-spec)))
