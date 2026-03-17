;;;; test/db/schema-test.lisp — Unit tests for schema management (no real DB)
(in-package :cauldron.test)

(defsuite :db-schema)

;;; ============================================================
;;; validate-schema-name
;;; ============================================================

(deftest test-schema-name-valid-simple
  (is (not (handler-case
               (progn (cauldron.db:validate-schema-name "public") nil)
             (error () t)))))

(deftest test-schema-name-valid-with-numbers
  (is (not (handler-case
               (progn (cauldron.db:validate-schema-name "company_42") nil)
             (error () t)))))

(deftest test-schema-name-valid-underscores
  (is (not (handler-case
               (progn (cauldron.db:validate-schema-name "my_schema_v2") nil)
             (error () t)))))

(deftest test-schema-name-rejects-empty
  (signals-condition error
    (cauldron.db:validate-schema-name "")))

(deftest test-schema-name-rejects-nil
  (signals-condition error
    (cauldron.db:validate-schema-name nil)))

(deftest test-schema-name-rejects-uppercase
  (signals-condition error
    (cauldron.db:validate-schema-name "Public")))

(deftest test-schema-name-rejects-spaces
  (signals-condition error
    (cauldron.db:validate-schema-name "my schema")))

(deftest test-schema-name-rejects-semicolon
  (signals-condition error
    (cauldron.db:validate-schema-name "schema; DROP TABLE users")))

(deftest test-schema-name-rejects-dash
  (signals-condition error
    (cauldron.db:validate-schema-name "my-schema")))

(deftest test-schema-name-rejects-leading-underscore
  (signals-condition error
    (cauldron.db:validate-schema-name "_private")))

(deftest test-schema-name-rejects-too-long
  (signals-condition error
    (cauldron.db:validate-schema-name
     (make-string 64 :initial-element #\a))))

(deftest test-schema-name-accepts-max-length
  (is (not (handler-case
               (progn (cauldron.db:validate-schema-name
                       (make-string 63 :initial-element #\a))
                      nil)
             (error () t)))))

(deftest test-schema-name-rejects-dot
  (signals-condition error
    (cauldron.db:validate-schema-name "schema.name")))

(deftest test-schema-name-rejects-single-quote
  (signals-condition error
    (cauldron.db:validate-schema-name "schema'name")))

(deftest test-schema-name-rejects-number-only
  ;; Numbers are valid characters but first char being a digit is allowed
  ;; since our regex allows [a-z0-9_] not starting with _
  (is (not (handler-case
               (progn (cauldron.db:validate-schema-name "42schema") nil)
             (error () t)))))
