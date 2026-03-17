;;;; src/cli/db-commands.lisp — Framework DB admin CLI commands
;;;; Provides cauldron db {migrate,rollback,status,schemas,tables,columns,sql}
;;;; All commands use *cli-pool* for database access.

(in-package :cauldron.cli)

;;; ============================================================
;;; cauldron db migrate
;;; ============================================================

(defcommand "db migrate"
  (:description "Run pending database migrations")
  (declare (ignore args))
  (cauldron.db:with-pool-connection (conn *cli-pool*)
    (let ((count (cauldron.grimoire:migrate
                  :execute-fn (lambda (sql) (cauldron.db:execute-sql conn sql))
                  :query-fn (lambda (sql) (cauldron.db:query conn sql)))))
      (emit :message (format nil "~D migration~:P applied" count))
      0)))

;;; ============================================================
;;; cauldron db rollback [--step N]
;;; ============================================================

(defcommand "db rollback"
  (:description "Rollback the last migration"
   :options (("--step" :default "1" :description "Number of migrations to roll back")))
  (let ((step (parse-integer (get-arg args "--step" "1") :junk-allowed t)))
    (cauldron.db:with-pool-connection (conn *cli-pool*)
      (let ((count (cauldron.grimoire:rollback
                    :execute-fn (lambda (sql) (cauldron.db:execute-sql conn sql))
                    :query-fn (lambda (sql) (cauldron.db:query conn sql))
                    :step (or step 1))))
        (emit :message (format nil "~D migration~:P rolled back" count))
        0))))

;;; ============================================================
;;; cauldron db status
;;; ============================================================

(defcommand "db status"
  (:description "Show migration status")
  (declare (ignore args))
  (cauldron.db:with-pool-connection (conn *cli-pool*)
    ;; Ensure migrations table exists
    (cauldron.db:execute-sql conn cauldron.grimoire::*schema-migrations-ddl*)
    (let* ((status (cauldron.grimoire:migration-status
                    (lambda (sql) (cauldron.db:query conn sql))))
           (rows (mapcar (lambda (s)
                           (let ((ht (make-hash-table :test 'equal)))
                             (setf (gethash "name" ht) (string (first s)))
                             (setf (gethash "version" ht) (second s))
                             (setf (gethash "status" ht) (string-downcase (string (third s))))
                             ht))
                         status)))
      (emit :data rows)
      0)))

;;; ============================================================
;;; cauldron db schemas
;;; ============================================================

(defcommand "db schemas"
  (:description "List all database schemas")
  (declare (ignore args))
  (cauldron.db:with-pool-connection (conn *cli-pool*)
    (let* ((schemas (cauldron.db:list-schemas conn))
           (rows (mapcar (lambda (s)
                           (let ((ht (make-hash-table :test 'equal)))
                             (setf (gethash "schema_name" ht) s)
                             ht))
                         schemas)))
      (emit :data rows)
      0)))

;;; ============================================================
;;; cauldron db tables [--schema SCHEMA]
;;; ============================================================

(defcommand "db tables"
  (:description "List tables in a schema"
   :options (("--schema" :default "public" :description "Schema name")))
  (let ((schema (get-arg args "--schema" "public")))
    (cauldron.db:with-pool-connection (conn *cli-pool*)
      (let* ((tables (cauldron.db:list-tables conn :schema schema))
             (rows (mapcar (lambda (tbl)
                             (let ((ht (make-hash-table :test 'equal)))
                               (setf (gethash "table_name" ht) tbl)
                               ht))
                           tables)))
        (emit :data rows)
        0))))

;;; ============================================================
;;; cauldron db columns --table TABLE [--schema SCHEMA]
;;; ============================================================

(defcommand "db columns"
  (:description "List columns of a table"
   :options (("--schema" :default "public" :description "Schema name")
             ("--table" :required t :description "Table name")))
  (let ((schema (get-arg args "--schema" "public"))
        (table (get-arg args "--table")))
    (cauldron.db:with-pool-connection (conn *cli-pool*)
      (let* ((columns (cauldron.db:list-columns conn table :schema schema))
             (rows (mapcar (lambda (col)
                             (let ((ht (make-hash-table :test 'equal)))
                               (setf (gethash "name" ht) (cdr (assoc "name" col :test #'string=)))
                               (setf (gethash "type" ht) (cdr (assoc "type" col :test #'string=)))
                               (setf (gethash "nullable" ht) (if (cdr (assoc "nullable" col :test #'string=)) "yes" "no"))
                               (setf (gethash "default" ht) (or (cdr (assoc "default" col :test #'string=)) ""))
                               ht))
                           columns)))
        (emit :data rows)
        0))))

;;; ============================================================
;;; cauldron db sql "SELECT ..." [--schema SCHEMA]
;;; ============================================================

(defcommand "db sql"
  (:description "Execute raw SQL query"
   :options (("--schema" :description "Set search_path to this schema before executing")))
  (let ((sql (get-positional args 0))
        (schema (get-arg args "--schema")))
    (unless sql
      (return-from cli-db-sql
        (emit-error "No SQL provided"
                    :use-hint "cauldron db sql \"SELECT 1\"")))
    (cauldron.db:with-pool-connection (conn *cli-pool*)
      (when schema
        (cauldron.db:set-search-path conn schema "public"))
      (handler-case
          (let ((rows (cauldron.db:query conn sql)))
            (emit :data rows)
            0)
        (cauldron.db:pg-error (e)
          (emit-error (format nil "SQL error: ~A" (cauldron.db:pg-error-message e)))
          1)))))
