;;;; test/db/connection-integration-test.lisp — Integration tests for PostgreSQL connections

(in-package :cauldron.test)

(defsuite :db-connection-integration)

;;; Ensure test database exists before the suite runs
(suite-setup :db-connection-integration
  (lambda () (ensure-test-database)))

;;; --- Basic connection lifecycle ---

(deftest test-connect-disconnect
  (with-test-connection (conn)
    (is (cauldron.db::pg-connection-alive-p conn) "Connection should be alive after connect")
    (cauldron.db:disconnect conn)
    (is-false (cauldron.db::pg-connection-alive-p conn) "Connection should be dead after disconnect")
    ;; Prevent double-disconnect in unwind-protect
    (setf conn nil)))

(deftest test-server-parameters-populated
  (with-test-connection (conn)
    (let ((params (cauldron.db:pg-connection-parameters conn)))
      (is (hash-table-p params) "Parameters should be a hash-table")
      (is-not-nil (gethash "server_version" params)
                  "server_version should be present"))))

(deftest test-backend-pid-positive
  (with-test-connection (conn)
    (is (plusp (cauldron.db::pg-connection-backend-pid conn))
        "Backend PID should be positive")))

(deftest test-bad-database-signals-error
  (signals-condition cauldron.db:pg-error
    (cauldron.db:connect :host *test-db-host*
                         :port *test-db-port*
                         :database "nonexistent_db_xyzzy_42"
                         :user *test-db-user*)
    "Connecting to non-existent database should signal pg-error"))

;;; --- Simple query ---

(deftest test-select-one
  (with-test-connection (conn)
    (let ((rows (cauldron.db:query conn "SELECT 1 AS n")))
      (is-equal 1 (length rows) "Should return one row")
      (let ((row (first rows)))
        (is (hash-table-p row) "Row should be a hash-table")
        (is-equal 1 (gethash "n" row) "n should be 1")))))

(deftest test-multiple-rows
  (with-test-connection (conn)
    (let ((rows (cauldron.db:query conn "SELECT generate_series(1,3) AS x")))
      (is-equal 3 (length rows) "Should return 3 rows")
      (is-equal 1 (gethash "x" (first rows)))
      (is-equal 2 (gethash "x" (second rows)))
      (is-equal 3 (gethash "x" (third rows))))))

;;; --- Parameterized query ---

(deftest test-parameterized-query
  (with-test-connection (conn)
    (let ((rows (cauldron.db:query conn "SELECT $1::int + $2::int AS sum" 3 7)))
      (is-equal 1 (length rows))
      (is-equal 10 (gethash "sum" (first rows))))))

;;; --- execute-sql ---

(deftest test-execute-sql-returns-command-tag
  (with-test-connection (conn)
    (let ((tag (cauldron.db:execute-sql conn "SELECT 1")))
      (is (stringp tag) "execute-sql should return a command tag string"))))

;;; --- Prepare + execute-prepared ---

(deftest test-prepare-and-execute
  (with-test-connection (conn)
    (cauldron.db:prepare conn "add_stmt" "SELECT $1::int + $2::int AS result")
    (let ((rows (cauldron.db:execute-prepared conn "add_stmt" '(5 8))))
      (is-equal 1 (length rows))
      (is-equal 13 (gethash "result" (first rows))))))

;;; --- with-connection macro ---

(deftest test-with-connection-cleanup
  (let ((saved-conn nil))
    (cauldron.db:with-connection (conn
                                  :host *test-db-host*
                                  :port *test-db-port*
                                  :database *test-db-name*
                                  :user *test-db-user*)
      (setf saved-conn conn)
      (is (cauldron.db::pg-connection-alive-p conn)))
    (is-false (cauldron.db::pg-connection-alive-p saved-conn)
              "Connection should be cleaned up after with-connection")))

;;; --- Transactions ---

(deftest test-transaction-commit
  (with-test-connection (conn)
    (cauldron.db:execute-sql conn "CREATE TEMP TABLE tx_test (val INT)")
    (cauldron.db:with-transaction (conn)
      (cauldron.db:execute-sql conn "INSERT INTO tx_test (val) VALUES (42)"))
    ;; Row should persist after commit
    (let ((rows (cauldron.db:query conn "SELECT val FROM tx_test")))
      (is-equal 1 (length rows))
      (is-equal 42 (gethash "val" (first rows))))))

(deftest test-transaction-rollback-on-error
  (with-test-connection (conn)
    (cauldron.db:execute-sql conn "CREATE TEMP TABLE tx_test2 (val INT)")
    (ignore-errors
      (cauldron.db:with-transaction (conn)
        (cauldron.db:execute-sql conn "INSERT INTO tx_test2 (val) VALUES (99)")
        (error "deliberate error to trigger rollback")))
    ;; Row should be gone after rollback
    (let ((rows (cauldron.db:query conn "SELECT val FROM tx_test2")))
      (is-equal 0 (length rows) "Row should not persist after rollback"))))

(deftest test-transaction-with-isolation
  (with-test-connection (conn)
    (cauldron.db:execute-sql conn "CREATE TEMP TABLE tx_iso (val INT)")
    (cauldron.db:with-transaction (conn :isolation :serializable)
      (cauldron.db:execute-sql conn "INSERT INTO tx_iso (val) VALUES (1)"))
    (let ((rows (cauldron.db:query conn "SELECT val FROM tx_iso")))
      (is-equal 1 (length rows) "Serializable transaction should commit"))))

;;; --- connection-alive-p after query ---

(deftest test-alive-after-query
  (with-test-connection (conn)
    (cauldron.db:query conn "SELECT 1")
    (is (cauldron.db::connection-alive-p conn)
        "Connection should remain alive after a query")))
