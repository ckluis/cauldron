;;;; test/grimoire/query-integration-test.lisp — Grimoire query execution integration tests

(in-package :cauldron.test)

(defsuite :grimoire-query-integration)

(suite-setup :grimoire-query-integration
  (lambda ()
    (ensure-test-database)
    (with-test-connection (conn)
      (drop-test-tables conn "test_query_users")
      (cauldron.db:execute-sql conn
        "CREATE TABLE test_query_users (
           id SERIAL PRIMARY KEY,
           name VARCHAR(100) NOT NULL,
           email VARCHAR(255) NOT NULL,
           age INTEGER
         )")
      ;; Seed 3 rows
      (cauldron.db:execute-sql conn
        "INSERT INTO test_query_users (name, email, age) VALUES
           ('Alice', 'alice@example.com', 30),
           ('Bob', 'bob@example.com', 25),
           ('Carol', 'carol@example.com', 35)"))))

(suite-teardown :grimoire-query-integration
  (lambda ()
    (ignore-errors
      (with-test-connection (conn)
        (drop-test-tables conn "test_query_users")))))

;;; --- Tests ---

(deftest test-execute-query-select-all
  (with-test-connection (conn)
    (let ((rows (cauldron.grimoire:execute-query conn
                  (cauldron.grimoire:from "test_query_users"))))
      (is-equal 3 (length rows) "Should return 3 rows"))))

(deftest test-execute-query-with-where
  (with-test-connection (conn)
    (let ((rows (cauldron.grimoire:execute-query conn
                  (cauldron.grimoire:where-clause
                    (cauldron.grimoire:from "test_query_users")
                    '(:= "name" "Alice")))))
      (is-equal 1 (length rows) "WHERE name = Alice should return 1 row")
      (is-equal "Alice" (gethash "name" (first rows))))))

(deftest test-execute-query-select-fields
  (with-test-connection (conn)
    (let ((rows (cauldron.grimoire:execute-query conn
                  (cauldron.grimoire:select-fields
                    (cauldron.grimoire:from "test_query_users")
                    "name" "email"))))
      (is-equal 3 (length rows))
      (let ((row (first rows)))
        (is-not-nil (gethash "name" row) "name column should be present")
        (is-not-nil (gethash "email" row) "email column should be present")))))

(deftest test-execute-one-returns-single-row
  (with-test-connection (conn)
    (let ((row (cauldron.grimoire:execute-one conn
                 (cauldron.grimoire:from "test_query_users"))))
      (is (hash-table-p row) "execute-one should return a single hash-table")
      (is-not-nil (gethash "name" row)))))

(deftest test-insert-into-adds-row
  (with-test-connection (conn)
    ;; Insert a new row
    (cauldron.grimoire:execute-query conn
      (cauldron.grimoire:insert-into "test_query_users"
        :name "Dave" :email "dave@example.com" :age 40))
    ;; Verify it's there
    (let ((rows (cauldron.grimoire:execute-query conn
                  (cauldron.grimoire:where-clause
                    (cauldron.grimoire:from "test_query_users")
                    '(:= "name" "Dave")))))
      (is-equal 1 (length rows))
      (is-equal "dave@example.com" (gethash "email" (first rows))))
    ;; Clean up
    (cauldron.db:execute-sql conn "DELETE FROM test_query_users WHERE name = 'Dave'")))

(deftest test-update-set-changes-row
  (with-test-connection (conn)
    (cauldron.grimoire:execute-query conn
      (cauldron.grimoire:where-clause
        (cauldron.grimoire:update-set "test_query_users" :age 31)
        '(:= "name" "Alice")))
    ;; Verify change
    (let ((rows (cauldron.grimoire:execute-query conn
                  (cauldron.grimoire:where-clause
                    (cauldron.grimoire:from "test_query_users")
                    '(:= "name" "Alice")))))
      (is-equal 31 (gethash "age" (first rows)) "Age should be updated to 31"))
    ;; Restore
    (cauldron.db:execute-sql conn "UPDATE test_query_users SET age = 30 WHERE name = 'Alice'")))

(deftest test-delete-from-removes-row
  (with-test-connection (conn)
    ;; Insert a row to delete
    (cauldron.db:execute-sql conn
      "INSERT INTO test_query_users (name, email, age) VALUES ('Temp', 'temp@example.com', 99)")
    (cauldron.grimoire:execute-query conn
      (cauldron.grimoire:where-clause
        (cauldron.grimoire:delete-from "test_query_users")
        '(:= "name" "Temp")))
    ;; Verify it's gone
    (let ((rows (cauldron.grimoire:execute-query conn
                  (cauldron.grimoire:where-clause
                    (cauldron.grimoire:from "test_query_users")
                    '(:= "name" "Temp")))))
      (is-equal 0 (length rows) "Deleted row should be gone"))))

(deftest test-order-by-and-limit
  (with-test-connection (conn)
    (let ((rows (cauldron.grimoire:execute-query conn
                  (cauldron.grimoire:limit-query
                    (cauldron.grimoire:order-by
                      (cauldron.grimoire:from "test_query_users")
                      '("age" :desc))
                    2))))
      (is-equal 2 (length rows) "LIMIT 2 should return 2 rows")
      ;; First row should have highest age
      (is (>= (gethash "age" (first rows))
              (gethash "age" (second rows)))
          "Rows should be ordered by age descending"))))
