;;;; test/cli/db-commands-test.lisp — Tests for framework DB admin CLI commands
;;;; Verifies command registration (no live DB needed).

(in-package :cauldron.test)

(defsuite :cli-db-commands)

;;; ============================================================
;;; Registration tests — verify each command is in the registry
;;; ============================================================

(deftest test-db-migrate-registered
  (is-not-nil (cauldron.cli:find-command "db migrate")))

(deftest test-db-rollback-registered
  (is-not-nil (cauldron.cli:find-command "db rollback")))

(deftest test-db-status-registered
  (is-not-nil (cauldron.cli:find-command "db status")))

(deftest test-db-schemas-registered
  (is-not-nil (cauldron.cli:find-command "db schemas")))

(deftest test-db-tables-registered
  (is-not-nil (cauldron.cli:find-command "db tables")))

(deftest test-db-columns-registered
  (is-not-nil (cauldron.cli:find-command "db columns")))

(deftest test-db-sql-registered
  (is-not-nil (cauldron.cli:find-command "db sql")))

;;; ============================================================
;;; Required option validation
;;; ============================================================

(deftest test-db-columns-requires-table
  "Dispatching 'db columns' without --table returns exit code 1."
  (let ((*error-output* (make-string-output-stream))
        (*standard-output* (make-string-output-stream)))
    (is-equal 1 (cauldron.cli:dispatch-command '("db" "columns" "--schema" "public")))))
