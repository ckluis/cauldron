;;;; test/grimoire/migration-integration-test.lisp — Migration system integration tests

(in-package :cauldron.test)

(defsuite :grimoire-migration-integration)

(suite-setup :grimoire-migration-integration
  (lambda ()
    (ensure-test-database)
    ;; Clean up any leftover test migrations and tables
    (with-test-connection (conn)
      (drop-test-tables conn "schema_migrations" "mig_test_items"))))

(suite-teardown :grimoire-migration-integration
  (lambda ()
    (ignore-errors
      (with-test-connection (conn)
        (drop-test-tables conn "schema_migrations" "mig_test_items")))
    ;; Clear test migrations from the registry
    (setf cauldron.grimoire::*migrations*
          (remove 'create-mig-test-items cauldron.grimoire::*migrations*
                  :key #'cauldron.grimoire::migration-def-name))))

;;; Define a test migration (will be registered globally)
(cauldron.grimoire:defmigration create-mig-test-items
  (:version "20260314000001")
  (:up
    "CREATE TABLE mig_test_items (
       id SERIAL PRIMARY KEY,
       name VARCHAR(100) NOT NULL,
       created_at TIMESTAMPTZ DEFAULT NOW()
     )")
  (:down
    "DROP TABLE mig_test_items"))

;;; --- Helper: create execute-fn and query-fn from a connection ---

(defun %make-migration-fns (conn)
  "Return (values execute-fn query-fn) wrapping CONN."
  (values
    (lambda (sql) (cauldron.db:execute-sql conn sql))
    (lambda (sql) (cauldron.db:query conn sql))))

;;; --- Tests ---

(deftest test-ensure-migrations-table-creates-table
  (with-test-connection (conn)
    (drop-test-tables conn "schema_migrations")
    (multiple-value-bind (exec-fn query-fn) (%make-migration-fns conn)
      (declare (ignore query-fn))
      (cauldron.grimoire::ensure-migrations-table exec-fn)
      ;; Verify the table exists
      (let ((rows (cauldron.db:query conn
                    "SELECT tablename FROM pg_tables
                     WHERE schemaname = 'public' AND tablename = 'schema_migrations'")))
        (is-equal 1 (length rows) "schema_migrations table should exist")))))

(deftest test-migrate-up-creates-table
  (with-test-connection (conn)
    (drop-test-tables conn "schema_migrations" "mig_test_items")
    (multiple-value-bind (exec-fn query-fn) (%make-migration-fns conn)
      (let ((count (cauldron.grimoire:migrate
                     :execute-fn exec-fn :query-fn query-fn :direction :up)))
        (is (plusp count) "Should apply at least one migration")
        ;; Verify the table was created
        (let ((rows (cauldron.db:query conn
                      "SELECT tablename FROM pg_tables
                       WHERE schemaname = 'public' AND tablename = 'mig_test_items'")))
          (is-equal 1 (length rows) "mig_test_items table should exist"))))))

(deftest test-version-recorded-in-schema-migrations
  (with-test-connection (conn)
    (drop-test-tables conn "schema_migrations" "mig_test_items")
    (multiple-value-bind (exec-fn query-fn) (%make-migration-fns conn)
      (cauldron.grimoire:migrate :execute-fn exec-fn :query-fn query-fn :direction :up)
      (let ((rows (cauldron.db:query conn
                    "SELECT version FROM schema_migrations WHERE version = '20260314000001'")))
        (is-equal 1 (length rows) "Migration version should be recorded")))))

(deftest test-migrate-down-drops-table
  (with-test-connection (conn)
    (drop-test-tables conn "schema_migrations" "mig_test_items")
    (multiple-value-bind (exec-fn query-fn) (%make-migration-fns conn)
      ;; Apply first
      (cauldron.grimoire:migrate :execute-fn exec-fn :query-fn query-fn :direction :up)
      ;; Roll back
      (cauldron.grimoire:migrate :execute-fn exec-fn :query-fn query-fn :direction :down)
      ;; Verify table is gone
      (let ((rows (cauldron.db:query conn
                    "SELECT tablename FROM pg_tables
                     WHERE schemaname = 'public' AND tablename = 'mig_test_items'")))
        (is-equal 0 (length rows) "mig_test_items should be dropped"))
      ;; Verify version removed
      (let ((rows (cauldron.db:query conn
                    "SELECT version FROM schema_migrations WHERE version = '20260314000001'")))
        (is-equal 0 (length rows) "Migration version should be removed")))))

(deftest test-rollback-with-step-count
  (with-test-connection (conn)
    (drop-test-tables conn "schema_migrations" "mig_test_items")
    (multiple-value-bind (exec-fn query-fn) (%make-migration-fns conn)
      (cauldron.grimoire:migrate :execute-fn exec-fn :query-fn query-fn :direction :up)
      (let ((count (cauldron.grimoire:rollback
                     :execute-fn exec-fn :query-fn query-fn :step 1)))
        (is (plusp count) "Rollback should undo at least one migration")))))

(deftest test-migration-status-reflects-reality
  (with-test-connection (conn)
    (drop-test-tables conn "schema_migrations" "mig_test_items")
    (multiple-value-bind (exec-fn query-fn) (%make-migration-fns conn)
      ;; Before migration
      (cauldron.grimoire::ensure-migrations-table exec-fn)
      (let ((status-before (cauldron.grimoire:migration-status query-fn)))
        (let ((entry (find 'create-mig-test-items status-before :key #'first)))
          (is-not-nil entry "Migration should appear in status")
          (is-equal :pending (third entry) "Should be pending before migrate")))
      ;; After migration
      (cauldron.grimoire:migrate :execute-fn exec-fn :query-fn query-fn :direction :up)
      (let ((status-after (cauldron.grimoire:migration-status query-fn)))
        (let ((entry (find 'create-mig-test-items status-after :key #'first)))
          (is-equal :applied (third entry) "Should be applied after migrate"))))))
