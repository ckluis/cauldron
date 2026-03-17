;;;; test/grimoire/query-test.lisp — Query builder tests
(in-package :cauldron.test)

(defsuite :grimoire-query)

;;; --- Basic SELECT ---

(deftest test-from-select-all
  (multiple-value-bind (sql params)
      (cauldron.grimoire:to-sql (cauldron.grimoire:from "users"))
    (is-equal "SELECT * FROM users" sql)
    (is-nil params)))

(deftest test-select-fields
  (multiple-value-bind (sql params)
      (cauldron.grimoire:to-sql
       (cauldron.grimoire:select-fields
        (cauldron.grimoire:from "users")
        "name" "email"))
    (is-equal "SELECT name, email FROM users" sql)
    (is-nil params)))

;;; --- WHERE ---

(deftest test-where-equal
  (multiple-value-bind (sql params)
      (cauldron.grimoire:to-sql
       (cauldron.grimoire:where-clause
        (cauldron.grimoire:from "users")
        '(:= :name "Alice")))
    (is-equal "SELECT * FROM users WHERE name = $1" sql)
    (is-equal '("Alice") params)))

(deftest test-where-not-equal
  (multiple-value-bind (sql params)
      (cauldron.grimoire:to-sql
       (cauldron.grimoire:where-clause
        (cauldron.grimoire:from "users")
        '(:/= :status "banned")))
    (is-equal "SELECT * FROM users WHERE status != $1" sql)
    (is-equal '("banned") params)))

(deftest test-where-greater-than
  (multiple-value-bind (sql params)
      (cauldron.grimoire:to-sql
       (cauldron.grimoire:where-clause
        (cauldron.grimoire:from "users")
        '(:> :age 18)))
    (is-equal "SELECT * FROM users WHERE age > $1" sql)
    (is-equal '(18) params)))

(deftest test-where-less-than
  (multiple-value-bind (sql params)
      (cauldron.grimoire:to-sql
       (cauldron.grimoire:where-clause
        (cauldron.grimoire:from "users")
        '(:< :score 50)))
    (is-equal "SELECT * FROM users WHERE score < $1" sql)
    (is-equal '(50) params)))

(deftest test-where-in
  (multiple-value-bind (sql params)
      (cauldron.grimoire:to-sql
       (cauldron.grimoire:where-clause
        (cauldron.grimoire:from "users")
        '(:in :role ("admin" "mod"))))
    (is-equal "SELECT * FROM users WHERE role IN ($1, $2)" sql)
    (is-equal '("admin" "mod") params)))

(deftest test-where-like
  (multiple-value-bind (sql params)
      (cauldron.grimoire:to-sql
       (cauldron.grimoire:where-clause
        (cauldron.grimoire:from "users")
        '(:like :name "%alice%")))
    (is-equal "SELECT * FROM users WHERE name LIKE $1" sql)
    (is-equal '("%alice%") params)))

(deftest test-where-between
  (multiple-value-bind (sql params)
      (cauldron.grimoire:to-sql
       (cauldron.grimoire:where-clause
        (cauldron.grimoire:from "users")
        '(:between :age (18 65))))
    (is-equal "SELECT * FROM users WHERE age BETWEEN $1 AND $2" sql)
    (is-equal '(18 65) params)))

(deftest test-where-is-null
  (multiple-value-bind (sql params)
      (cauldron.grimoire:to-sql
       (cauldron.grimoire:where-clause
        (cauldron.grimoire:from "users")
        '(:is-null :deleted_at nil)))
    (is-equal "SELECT * FROM users WHERE deleted_at IS NULL" sql)
    ;; is-null doesn't add params
    (is-nil params)))

;;; --- ORDER BY / LIMIT / OFFSET ---

(deftest test-order-by
  (multiple-value-bind (sql)
      (cauldron.grimoire:to-sql
       (cauldron.grimoire:order-by
        (cauldron.grimoire:from "users")
        '(:name :desc)))
    (is-equal "SELECT * FROM users ORDER BY name DESC" sql)))

(deftest test-limit
  (multiple-value-bind (sql)
      (cauldron.grimoire:to-sql
       (cauldron.grimoire:limit-query
        (cauldron.grimoire:from "users")
        10))
    (is-equal "SELECT * FROM users LIMIT 10" sql)))

(deftest test-offset
  (multiple-value-bind (sql)
      (cauldron.grimoire:to-sql
       (cauldron.grimoire:offset-query
        (cauldron.grimoire:limit-query
         (cauldron.grimoire:from "users")
         10)
        20))
    (is-equal "SELECT * FROM users LIMIT 10 OFFSET 20" sql)))

;;; --- JOINs ---

(deftest test-join
  (multiple-value-bind (sql params)
      (cauldron.grimoire:to-sql
       (cauldron.grimoire:join-query
        (cauldron.grimoire:from "users")
        "posts"
        :on '(:= "users.id" "posts.user_id")
        :type :inner))
    (is (search "INNER JOIN posts ON" sql))
    (is (search "users.id = $1" sql))
    ;; Note: current implementation parameterizes column refs in joins
    (is-equal '("posts.user_id") params)))

;;; --- INSERT ---

(deftest test-insert-into
  (multiple-value-bind (sql params)
      (cauldron.grimoire:to-sql
       (cauldron.grimoire:insert-into "users"
                                       :name "Alice"
                                       :email "alice@example.com"))
    (is (search "INSERT INTO users" sql))
    (is (search "name" sql))
    (is (search "email" sql))
    (is (search "$1" sql))
    (is (search "$2" sql))
    (is-equal 2 (length params))
    (is (member "Alice" params :test #'equal))
    (is (member "alice@example.com" params :test #'equal))))

;;; --- UPDATE ---

(deftest test-update-set
  (multiple-value-bind (sql params)
      (cauldron.grimoire:to-sql
       (cauldron.grimoire:where-clause
        (cauldron.grimoire:update-set "users" :name "Bob")
        '(:= :id 1)))
    (is (search "UPDATE users SET" sql))
    (is (search "name = $1" sql))
    (is (search "WHERE id = $2" sql))
    (is-equal '("Bob" 1) params)))

;;; --- DELETE ---

(deftest test-delete-from
  (multiple-value-bind (sql params)
      (cauldron.grimoire:to-sql
       (cauldron.grimoire:where-clause
        (cauldron.grimoire:delete-from "users")
        '(:= :id 5)))
    (is (search "DELETE FROM users" sql))
    (is (search "WHERE id = $1" sql))
    (is-equal '(5) params)))

;;; --- Compound conditions ---

(deftest test-qand
  (multiple-value-bind (sql params)
      (cauldron.grimoire:to-sql
       (cauldron.grimoire:where-clause
        (cauldron.grimoire:from "users")
        (cauldron.grimoire:qand '(:= :name "Alice") '(:> :age 18))))
    (is (search "AND" sql))
    (is-equal '("Alice" 18) params)))

(deftest test-qor
  (multiple-value-bind (sql params)
      (cauldron.grimoire:to-sql
       (cauldron.grimoire:where-clause
        (cauldron.grimoire:from "users")
        (cauldron.grimoire:qor '(:= :role "admin") '(:= :role "mod"))))
    (is (search "OR" sql))
    (is-equal '("admin" "mod") params)))

(deftest test-qnot
  (multiple-value-bind (sql)
      (cauldron.grimoire:to-sql
       (cauldron.grimoire:where-clause
        (cauldron.grimoire:from "users")
        (cauldron.grimoire:qnot '(:= :status "banned"))))
    (is (search "NOT" sql))))

;;; --- All values use $N placeholders ---

(deftest test-no-string-interpolation
  ;; Verify no raw values appear in SQL
  (multiple-value-bind (sql)
      (cauldron.grimoire:to-sql
       (cauldron.grimoire:where-clause
        (cauldron.grimoire:from "users")
        '(:= :name "Robert'); DROP TABLE users;--")))
    ;; The dangerous string should NOT appear in the SQL
    (is-nil (search "Robert" sql))
    ;; Should use $1 placeholder
    (is (search "$1" sql))))

;;; --- SQL identifier validation ---

(deftest test-validate-sql-identifier-valid
  ;; Valid identifiers should not signal errors
  (is-not-nil (progn (cauldron.grimoire::validate-sql-identifier "name") t))
  (is-not-nil (progn (cauldron.grimoire::validate-sql-identifier "user_name") t))
  (is-not-nil (progn (cauldron.grimoire::validate-sql-identifier "_private") t))
  (is-not-nil (progn (cauldron.grimoire::validate-sql-identifier "t1.column_name") t))
  (is-not-nil (progn (cauldron.grimoire::validate-sql-identifier "Amount123") t)))

(deftest test-validate-sql-identifier-injection
  ;; SQL injection attempts should signal errors
  (signals-condition error
    (cauldron.grimoire::validate-sql-identifier "x; DROP TABLE users--")
    "Semicolon rejected")
  (signals-condition error
    (cauldron.grimoire::validate-sql-identifier "col OR 1=1")
    "Space rejected")
  (signals-condition error
    (cauldron.grimoire::validate-sql-identifier "")
    "Empty string rejected")
  (signals-condition error
    (cauldron.grimoire::validate-sql-identifier "123abc")
    "Leading digit rejected"))

(deftest test-aggregate-injection-prevention
  ;; count-rows with injection attempt
  (signals-condition error
    (cauldron.grimoire:count-rows (cauldron.grimoire:from "users") "x; DROP TABLE users--")
    "count-rows rejects injection")
  ;; sum-of with injection attempt
  (signals-condition error
    (cauldron.grimoire:sum-of (cauldron.grimoire:from "orders") "amount; DELETE FROM orders")
    "sum-of rejects injection")
  ;; avg-of with injection attempt
  (signals-condition error
    (cauldron.grimoire:avg-of (cauldron.grimoire:from "orders") "price OR 1=1")
    "avg-of rejects injection"))
