;;;; test/json/decoder-test.lisp — JSON decoder tests
(in-package :cauldron.test)

(defsuite :json-decoder)

;;; --- Literals ---

(deftest test-decode-null
  (is-equal :null (cauldron.json:decode "null")))

(deftest test-decode-true
  (is-equal t (cauldron.json:decode "true")))

(deftest test-decode-false
  (is-equal :false (cauldron.json:decode "false")))

;;; --- Numbers ---

(deftest test-decode-integer
  (is-equal 42 (cauldron.json:decode "42")))

(deftest test-decode-negative
  (is-equal -7 (cauldron.json:decode "-7")))

(deftest test-decode-zero
  (is-equal 0 (cauldron.json:decode "0")))

(deftest test-decode-float
  (let ((result (cauldron.json:decode "3.14")))
    (is (typep result 'double-float))
    (is (< (abs (- result 3.14d0)) 0.001d0))))

(deftest test-decode-exponent
  (let ((result (cauldron.json:decode "1e10")))
    (is (typep result 'double-float))))

(deftest test-decode-leading-zeros-rejected
  (signals-condition cauldron.json:json-parse-error
    (cauldron.json:decode "01")))

;;; --- Strings ---

(deftest test-decode-simple-string
  (is-equal "hello" (cauldron.json:decode "\"hello\"")))

(deftest test-decode-empty-string
  (is-equal "" (cauldron.json:decode "\"\"")))

(deftest test-decode-escape-sequences
  (is-equal (format nil "~C" #\Newline) (cauldron.json:decode "\"\\n\""))
  (is-equal (format nil "~C" #\Tab) (cauldron.json:decode "\"\\t\""))
  (is-equal "\"" (cauldron.json:decode "\"\\\"\""))
  (is-equal "\\" (cauldron.json:decode "\"\\\\\""))
  (is-equal "/" (cauldron.json:decode "\"\\/\"")))

(deftest test-decode-unicode-escape
  ;; \u0041 = 'A'
  (is-equal "A" (cauldron.json:decode "\"\\u0041\"")))

(deftest test-decode-surrogate-pair
  ;; U+1F600 (grinning face) = \uD83D\uDE00
  (let ((result (cauldron.json:decode "\"\\uD83D\\uDE00\"")))
    (is-equal 1 (length result))
    (is-equal #x1F600 (char-code (char result 0)))))

;;; --- Arrays ---

(deftest test-decode-empty-array
  (is-equalp #() (cauldron.json:decode "[]")))

(deftest test-decode-array
  (is-equalp #(1 2 3) (cauldron.json:decode "[1, 2, 3]")))

(deftest test-decode-nested-array
  (let ((result (cauldron.json:decode "[[1, 2], [3, 4]]")))
    (is-equalp #(#(1 2) #(3 4)) result)))

;;; --- Objects ---

(deftest test-decode-empty-object
  (let ((result (cauldron.json:decode "{}")))
    (is (hash-table-p result))
    (is-equal 0 (hash-table-count result))))

(deftest test-decode-object
  (let ((result (cauldron.json:decode "{\"name\": \"Alice\", \"age\": 30}")))
    (is (hash-table-p result))
    (is-equal "Alice" (gethash "name" result))
    (is-equal 30 (gethash "age" result))))

;;; --- Round-trip ---

(deftest test-roundtrip-integer
  (is-equal 42 (cauldron.json:decode (cauldron.json:encode 42))))

(deftest test-roundtrip-string
  (is-equal "hello" (cauldron.json:decode (cauldron.json:encode "hello"))))

(deftest test-roundtrip-array
  (is-equalp #(1 2 3) (cauldron.json:decode (cauldron.json:encode #(1 2 3)))))

(deftest test-roundtrip-object
  (let* ((ht (make-hash-table :test 'equal)))
    (setf (gethash "key" ht) "value")
    (let ((result (cauldron.json:decode (cauldron.json:encode ht))))
      (is-equal "value" (gethash "key" result)))))

;;; --- Error cases ---

(deftest test-decode-trailing-content
  (signals-condition cauldron.json:json-parse-error
    (cauldron.json:decode "42 extra")))

(deftest test-decode-unterminated-string
  (signals-condition cauldron.json:json-parse-error
    (cauldron.json:decode "\"unterminated")))

(deftest test-decode-invalid-escape
  (signals-condition cauldron.json:json-parse-error
    (cauldron.json:decode "\"\\x\"")))

(deftest test-decode-empty-input
  (signals-condition cauldron.json:json-parse-error
    (cauldron.json:decode "")))
