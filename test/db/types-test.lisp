;;;; test/db/types-test.lisp — DB type decoder/encoder tests
(in-package :cauldron.test)

(defsuite :db-types)

;;; --- decode-bool ---

(deftest test-decode-bool-true
  (is-true (cauldron.db::decode-bool "t"))
  (is-true (cauldron.db::decode-bool "true"))
  (is-true (cauldron.db::decode-bool "TRUE")))

(deftest test-decode-bool-false
  (is-false (cauldron.db::decode-bool "f"))
  (is-false (cauldron.db::decode-bool "false"))
  (is-false (cauldron.db::decode-bool "FALSE")))

;;; --- decode-integer ---

(deftest test-decode-integer
  (is-equal 42 (cauldron.db::decode-integer "42"))
  (is-equal -7 (cauldron.db::decode-integer "-7"))
  (is-equal 0 (cauldron.db::decode-integer "0")))

;;; --- decode-float ---

(deftest test-decode-float4
  (let ((result (cauldron.db::decode-float4 "3.14")))
    (is (typep result 'single-float))
    (is (< (abs (- result 3.14)) 0.01))))

(deftest test-decode-float8
  (let ((result (cauldron.db::decode-float8 "3.14159265358979")))
    (is (typep result 'double-float))
    (is (< (abs (- result 3.14159265358979d0)) 1d-6))))

;;; --- decode-text ---

(deftest test-decode-text
  (is-equal "hello" (cauldron.db::decode-text "hello"))
  (is-equal "" (cauldron.db::decode-text "")))

;;; --- decode-bytea-hex ---

(deftest test-decode-bytea-hex
  (let ((result (cauldron.db::decode-bytea-hex "\\x48656c6c6f")))
    (is (typep result '(vector (unsigned-byte 8))))
    (is-equal 5 (length result))
    ;; "Hello" in ASCII
    (is-equal 72 (aref result 0))
    (is-equal 111 (aref result 4))))

;;; --- decode-text-array ---

(deftest test-decode-text-array-simple
  (is-equal '("a" "b" "c") (cauldron.db::decode-text-array "{a,b,c}")))

(deftest test-decode-text-array-empty
  (is-nil (cauldron.db::decode-text-array "{}"))
  (is-nil (cauldron.db::decode-text-array "")))

(deftest test-decode-text-array-quoted
  (let ((result (cauldron.db::decode-text-array "{\"hello world\",\"foo\"}")))
    (is-equal 2 (length result))
    (is-equal "hello world" (first result))
    (is-equal "foo" (second result))))

;;; --- decode-pg-value (dispatch by OID) ---

(deftest test-decode-pg-value-bool
  (is-true (cauldron.db:decode-pg-value 16 "t"))
  (is-false (cauldron.db:decode-pg-value 16 "f")))

(deftest test-decode-pg-value-int4
  (is-equal 123 (cauldron.db:decode-pg-value 23 "123")))

(deftest test-decode-pg-value-text
  (is-equal "hello" (cauldron.db:decode-pg-value 25 "hello")))

(deftest test-decode-pg-value-nil
  (is-nil (cauldron.db:decode-pg-value 23 nil)))

(deftest test-decode-pg-value-unknown-oid
  (is-equal "raw" (cauldron.db:decode-pg-value 99999 "raw")))

;;; --- encode-pg-value ---

(deftest test-encode-pg-value-string
  (is-equal "hello" (cauldron.db:encode-pg-value "hello")))

(deftest test-encode-pg-value-integer
  (is-equal "42" (cauldron.db:encode-pg-value 42)))

(deftest test-encode-pg-value-boolean
  (is-equal "t" (cauldron.db:encode-pg-value t))
  (is-equal "f" (cauldron.db:encode-pg-value :false))
  (is-nil (cauldron.db:encode-pg-value nil)))

(deftest test-encode-pg-value-keyword
  (is-equal "t" (cauldron.db:encode-pg-value :true))
  (is-nil (cauldron.db:encode-pg-value :null)))
