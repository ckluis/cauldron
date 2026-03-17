;;;; test/db/schema-integration-test.lisp — Integration tests for schema management
;;;; Requires a live PostgreSQL connection.
(in-package :cauldron.test)

(defsuite :db-schema-integration)

;;; ============================================================
;;; Helpers
;;; ============================================================

(defvar *schema-test-conn* nil
  "Connection used by schema integration tests.")

(defun schema-test-connect ()
  "Connect to the test database for schema tests."
  (cauldron.db:connect :host "127.0.0.1" :port 5432
                       :database "cauldron_test"
                       :user "cauldron" :password "cauldron"))

(defun schema-test-cleanup (conn schema-name)
  "Drop a test schema if it exists, ignoring errors."
  (ignore-errors
    (cauldron.db:drop-schema conn schema-name :cascade t :if-exists t)))

;;; ============================================================
;;; Schema Lifecycle
;;; ============================================================

(deftest test-integration-create-and-drop-schema
  (let ((conn (schema-test-connect))
        (schema "test_schema_lifecycle"))
    (unwind-protect
        (progn
          (schema-test-cleanup conn schema)
          ;; Create
          (cauldron.db:create-schema conn schema)
          (is-true (cauldron.db:schema-exists-p conn schema))
          ;; Drop
          (cauldron.db:drop-schema conn schema)
          (is-false (cauldron.db:schema-exists-p conn schema)))
      (schema-test-cleanup conn schema)
      (cauldron.db:disconnect conn))))

(deftest test-integration-create-schema-if-not-exists
  (let ((conn (schema-test-connect))
        (schema "test_schema_idempotent"))
    (unwind-protect
        (progn
          (schema-test-cleanup conn schema)
          ;; Create twice — should not error
          (cauldron.db:create-schema conn schema)
          (cauldron.db:create-schema conn schema :if-not-exists t)
          (is-true (cauldron.db:schema-exists-p conn schema)))
      (schema-test-cleanup conn schema)
      (cauldron.db:disconnect conn))))

(deftest test-integration-schema-exists-p-false
  (let ((conn (schema-test-connect)))
    (unwind-protect
        (is-false (cauldron.db:schema-exists-p conn "nonexistent_schema_xyz"))
      (cauldron.db:disconnect conn))))

(deftest test-integration-drop-schema-cascade
  (let ((conn (schema-test-connect))
        (schema "test_schema_cascade"))
    (unwind-protect
        (progn
          (schema-test-cleanup conn schema)
          (cauldron.db:create-schema conn schema)
          ;; Create a table in the schema
          (cauldron.db:execute-sql conn
            (format nil "CREATE TABLE ~A.test_table (id serial PRIMARY KEY, name text)" schema))
          ;; Drop with cascade should succeed
          (cauldron.db:drop-schema conn schema :cascade t)
          (is-false (cauldron.db:schema-exists-p conn schema)))
      (schema-test-cleanup conn schema)
      (cauldron.db:disconnect conn))))

;;; ============================================================
;;; Search Path
;;; ============================================================

(deftest test-integration-set-search-path
  (let ((conn (schema-test-connect))
        (schema "test_schema_path"))
    (unwind-protect
        (progn
          (schema-test-cleanup conn schema)
          (cauldron.db:create-schema conn schema)
          (cauldron.db:set-search-path conn schema "public")
          (let ((path (cauldron.db:get-search-path conn)))
            (is-not-nil path)
            (is (search schema path))))
      (cauldron.db:reset-search-path conn)
      (schema-test-cleanup conn schema)
      (cauldron.db:disconnect conn))))

(deftest test-integration-reset-search-path
  (let ((conn (schema-test-connect))
        (schema "test_schema_reset"))
    (unwind-protect
        (progn
          (schema-test-cleanup conn schema)
          (cauldron.db:create-schema conn schema)
          (cauldron.db:set-search-path conn schema)
          (cauldron.db:reset-search-path conn)
          (let ((path (cauldron.db:get-search-path conn)))
            (is-equal "public" path)))
      (schema-test-cleanup conn schema)
      (cauldron.db:disconnect conn))))

(deftest test-integration-with-schema
  (let ((conn (schema-test-connect))
        (schema "test_schema_with"))
    (unwind-protect
        (progn
          (schema-test-cleanup conn schema)
          (cauldron.db:create-schema conn schema)
          ;; Create table in schema
          (cauldron.db:execute-sql conn
            (format nil "CREATE TABLE ~A.items (id serial, name text)" schema))
          ;; with-schema should set and restore path
          (let ((before (cauldron.db:get-search-path conn)))
            (cauldron.db:with-schema (conn schema "public")
              ;; Should be able to query the table without schema prefix
              (cauldron.db:execute-sql conn
                "INSERT INTO items (name) VALUES ('test')"))
            ;; Path should be restored
            (let ((after (cauldron.db:get-search-path conn)))
              (is-equal before after))))
      (schema-test-cleanup conn schema)
      (cauldron.db:disconnect conn))))

;;; ============================================================
;;; Introspection
;;; ============================================================

(deftest test-integration-list-schemas
  (let ((conn (schema-test-connect)))
    (unwind-protect
        (let ((schemas (cauldron.db:list-schemas conn)))
          (is (listp schemas))
          ;; public should always exist
          (is (member "public" schemas :test #'string=)))
      (cauldron.db:disconnect conn))))

(deftest test-integration-list-schemas-excludes-system
  (let ((conn (schema-test-connect)))
    (unwind-protect
        (let ((schemas (cauldron.db:list-schemas conn :exclude-system t)))
          ;; Should not contain pg_catalog or information_schema
          (is-false (member "pg_catalog" schemas :test #'string=))
          (is-false (member "information_schema" schemas :test #'string=)))
      (cauldron.db:disconnect conn))))

(deftest test-integration-list-schemas-includes-system
  (let ((conn (schema-test-connect)))
    (unwind-protect
        (let ((schemas (cauldron.db:list-schemas conn :exclude-system nil)))
          ;; Should contain pg_catalog
          (is (member "pg_catalog" schemas :test #'string=)))
      (cauldron.db:disconnect conn))))

(deftest test-integration-list-tables
  (let ((conn (schema-test-connect))
        (schema "test_schema_tables"))
    (unwind-protect
        (progn
          (schema-test-cleanup conn schema)
          (cauldron.db:create-schema conn schema)
          (cauldron.db:execute-sql conn
            (format nil "CREATE TABLE ~A.users (id serial PRIMARY KEY)" schema))
          (cauldron.db:execute-sql conn
            (format nil "CREATE TABLE ~A.posts (id serial PRIMARY KEY)" schema))
          (let ((tables (cauldron.db:list-tables conn :schema schema)))
            (is (listp tables))
            (is-equal 2 (length tables))
            (is (member "users" tables :test #'string=))
            (is (member "posts" tables :test #'string=))))
      (schema-test-cleanup conn schema)
      (cauldron.db:disconnect conn))))

(deftest test-integration-list-tables-empty-schema
  (let ((conn (schema-test-connect))
        (schema "test_schema_empty"))
    (unwind-protect
        (progn
          (schema-test-cleanup conn schema)
          (cauldron.db:create-schema conn schema)
          (let ((tables (cauldron.db:list-tables conn :schema schema)))
            (is-nil tables)))
      (schema-test-cleanup conn schema)
      (cauldron.db:disconnect conn))))

(deftest test-integration-list-columns
  (let ((conn (schema-test-connect))
        (schema "test_schema_cols"))
    (unwind-protect
        (progn
          (schema-test-cleanup conn schema)
          (cauldron.db:create-schema conn schema)
          (cauldron.db:execute-sql conn
            (format nil "CREATE TABLE ~A.items (
               id serial PRIMARY KEY,
               name text NOT NULL,
               price numeric DEFAULT 0
             )" schema))
          (let ((columns (cauldron.db:list-columns conn "items" :schema schema)))
            (is (listp columns))
            (is-equal 3 (length columns))
            ;; Check first column (id)
            (let ((id-col (first columns)))
              (is-equal "id" (cdr (assoc "name" id-col :test #'string=))))
            ;; Check second column (name)
            (let ((name-col (second columns)))
              (is-equal "name" (cdr (assoc "name" name-col :test #'string=)))
              (is-equal "text" (cdr (assoc "type" name-col :test #'string=))))))
      (schema-test-cleanup conn schema)
      (cauldron.db:disconnect conn))))

(deftest test-integration-table-exists-p
  (let ((conn (schema-test-connect))
        (schema "test_schema_exists"))
    (unwind-protect
        (progn
          (schema-test-cleanup conn schema)
          (cauldron.db:create-schema conn schema)
          (cauldron.db:execute-sql conn
            (format nil "CREATE TABLE ~A.widgets (id serial)" schema))
          (is-true (cauldron.db:table-exists-p conn "widgets" :schema schema))
          (is-false (cauldron.db:table-exists-p conn "nonexistent" :schema schema)))
      (schema-test-cleanup conn schema)
      (cauldron.db:disconnect conn))))

;;; ============================================================
;;; Tenant Isolation via Pool
;;; ============================================================

(deftest test-integration-with-tenant-connection
  (let* ((pool (cauldron.db:make-pool :host "127.0.0.1" :port 5432
                                       :database "cauldron_test"
                                       :user "cauldron" :password "cauldron"
                                       :min-size 1 :max-size 2))
         (schema "test_tenant_pool"))
    (unwind-protect
        (progn
          ;; Setup: create schema and table
          (cauldron.db:with-pool-connection (conn pool)
            (ignore-errors
              (cauldron.db:drop-schema conn schema :cascade t :if-exists t))
            (cauldron.db:create-schema conn schema)
            (cauldron.db:execute-sql conn
              (format nil "CREATE TABLE ~A.items (id serial, name text)" schema)))
          ;; Use with-tenant-connection — search_path should be set
          (cauldron.db:with-tenant-connection (conn pool schema)
            (cauldron.db:execute-sql conn
              "INSERT INTO items (name) VALUES ('tenant-item')")
            (let ((rows (cauldron.db:query conn "SELECT name FROM items")))
              (is-equal 1 (length rows))
              (is-equal "tenant-item"
                        (gethash "name" (first rows)))))
          ;; After checkin, search_path should be reset — items table should not be accessible
          (cauldron.db:with-pool-connection (conn pool)
            (let ((path (cauldron.db:get-search-path conn)))
              (is-equal "public" path))))
      ;; Cleanup
      (cauldron.db:with-pool-connection (conn pool)
        (ignore-errors
          (cauldron.db:drop-schema conn schema :cascade t :if-exists t)))
      (cauldron.db:shutdown-pool pool))))
