;;;; src/db/schema.lisp — Multi-schema database support
;;;; Schema-per-tenant isolation: shared pool, SET search_path on checkout,
;;;; reset on checkin. DDL helpers for schema lifecycle.

(in-package :cauldron.db)

;;; ============================================================
;;; Schema Lifecycle
;;; ============================================================

(defun create-schema (conn schema-name &key (if-not-exists t))
  "Create a PostgreSQL schema. SCHEMA-NAME must be a valid identifier.
Returns the command tag string."
  (validate-schema-name schema-name)
  (execute-sql conn (if if-not-exists
                        (format nil "CREATE SCHEMA IF NOT EXISTS ~A" schema-name)
                        (format nil "CREATE SCHEMA ~A" schema-name))))

(defun drop-schema (conn schema-name &key (cascade nil) (if-exists t))
  "Drop a PostgreSQL schema. Use CASCADE to drop contained objects.
Returns the command tag string."
  (validate-schema-name schema-name)
  (execute-sql conn (format nil "DROP SCHEMA~A ~A~A"
                             (if if-exists " IF EXISTS" "")
                             schema-name
                             (if cascade " CASCADE" ""))))

(defun schema-exists-p (conn schema-name)
  "Return T if SCHEMA-NAME exists in the database."
  (validate-schema-name schema-name)
  (let ((rows (query conn
                     "SELECT 1 FROM information_schema.schemata WHERE schema_name = $1"
                     schema-name)))
    (not (null rows))))

;;; ============================================================
;;; Search Path Management
;;; ============================================================

(defun set-search-path (conn &rest schemas)
  "Set the search_path for CONN to SCHEMAS (list of schema name strings).
Returns the command tag string."
  (unless schemas
    (error "set-search-path: at least one schema name is required"))
  (dolist (s schemas)
    (validate-schema-name s))
  (execute-sql conn (format nil "SET search_path TO ~{~A~^, ~}" schemas)))

(defun get-search-path (conn)
  "Return the current search_path as a string."
  (let ((rows (query conn "SHOW search_path")))
    (when rows
      (gethash "search_path" (first rows)))))

(defun reset-search-path (conn)
  "Reset search_path to the default (public).
Called by pool checkin to prevent tenant data leakage."
  (execute-sql conn "SET search_path TO public"))

(defmacro with-schema ((conn &rest schemas) &body body)
  "Execute BODY with CONN's search_path set to SCHEMAS.
Restores the previous search_path on exit (via unwind-protect)."
  (let ((conn-var (gensym "CONN"))
        (prev-path (gensym "PREV-PATH")))
    `(let* ((,conn-var ,conn)
            (,prev-path (get-search-path ,conn-var)))
       (set-search-path ,conn-var ,@schemas)
       (unwind-protect (progn ,@body)
         (execute-sql ,conn-var
                      (format nil "SET search_path TO ~A" ,prev-path))))))

;;; ============================================================
;;; Schema Introspection
;;; ============================================================

(defun list-schemas (conn &key (exclude-system t))
  "Return a list of schema name strings. By default excludes pg_* and information_schema."
  (let ((rows (if exclude-system
                  (query conn
                         "SELECT schema_name FROM information_schema.schemata
                          WHERE schema_name NOT LIKE 'pg_%'
                            AND schema_name != 'information_schema'
                          ORDER BY schema_name")
                  (query conn
                         "SELECT schema_name FROM information_schema.schemata
                          ORDER BY schema_name"))))
    (mapcar (lambda (row) (gethash "schema_name" row)) rows)))

(defun list-tables (conn &key (schema "public"))
  "Return a list of table name strings in SCHEMA."
  (validate-schema-name schema)
  (let ((rows (query conn
                     "SELECT table_name FROM information_schema.tables
                      WHERE table_schema = $1 AND table_type = 'BASE TABLE'
                      ORDER BY table_name"
                     schema)))
    (mapcar (lambda (row) (gethash "table_name" row)) rows)))

(defun list-columns (conn table-name &key (schema "public"))
  "Return a list of column info alists for TABLE-NAME in SCHEMA.
Each alist has keys: name, type, nullable, default, ordinal."
  (validate-schema-name schema)
  (let ((rows (query conn
                     "SELECT column_name, data_type, is_nullable,
                             column_default, ordinal_position
                      FROM information_schema.columns
                      WHERE table_schema = $1 AND table_name = $2
                      ORDER BY ordinal_position"
                     schema table-name)))
    (mapcar (lambda (row)
              (list (cons "name" (gethash "column_name" row))
                    (cons "type" (gethash "data_type" row))
                    (cons "nullable" (string-equal "YES" (gethash "is_nullable" row)))
                    (cons "default" (gethash "column_default" row))
                    (cons "ordinal" (gethash "ordinal_position" row))))
            rows)))

(defun table-exists-p (conn table-name &key (schema "public"))
  "Return T if TABLE-NAME exists in SCHEMA."
  (validate-schema-name schema)
  (let ((rows (query conn
                     "SELECT 1 FROM information_schema.tables
                      WHERE table_schema = $1 AND table_name = $2"
                     schema table-name)))
    (not (null rows))))

;;; ============================================================
;;; Validation
;;; ============================================================

(defun validate-schema-name (name)
  "Validate that NAME is a safe schema identifier.
Prevents SQL injection by restricting to [a-z0-9_] characters."
  (unless (and (stringp name)
               (> (length name) 0)
               (<= (length name) 63)
               (every (lambda (c)
                        (or (char<= #\a c #\z)
                            (char<= #\0 c #\9)
                            (char= c #\_)))
                      name)
               (not (char= (char name 0) #\_)))
    (error "Invalid schema name: ~S. Must be 1-63 chars of [a-z0-9_], not starting with underscore."
           name)))
