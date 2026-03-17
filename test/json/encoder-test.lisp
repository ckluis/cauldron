;;;; test/json/encoder-test.lisp — JSON encoder tests
(in-package :cauldron.test)

(defsuite :json-encoder)

;;; --- Null/Boolean ---

(deftest test-encode-nil
  (is-equal "null" (cauldron.json:encode nil)))

(deftest test-encode-true
  (is-equal "true" (cauldron.json:encode t)))

(deftest test-encode-false
  (is-equal "false" (cauldron.json:encode :false)))

(deftest test-encode-null-keyword
  (is-equal "null" (cauldron.json:encode :null)))

;;; --- Numbers ---

(deftest test-encode-integer
  (is-equal "42" (cauldron.json:encode 42)))

(deftest test-encode-negative-integer
  (is-equal "-7" (cauldron.json:encode -7)))

(deftest test-encode-zero
  (is-equal "0" (cauldron.json:encode 0)))

(deftest test-encode-float
  (let ((result (cauldron.json:encode 3.14d0)))
    (is (string= "3.14" result))))

(deftest test-encode-float-integer-value
  (let ((result (cauldron.json:encode 1.0d0)))
    (is (string= "1.0" result))))

;;; --- Strings ---

(deftest test-encode-simple-string
  (is-equal "\"hello\"" (cauldron.json:encode "hello")))

(deftest test-encode-empty-string
  (is-equal "\"\"" (cauldron.json:encode "")))

(deftest test-encode-string-escapes
  ;; Double quote
  (is (search "\\\"" (cauldron.json:encode "say \"hi\"")))
  ;; Backslash
  (is (search "\\\\" (cauldron.json:encode "back\\slash")))
  ;; Newline
  (is (search "\\n" (cauldron.json:encode (format nil "line1~%line2"))))
  ;; Tab
  (is (search "\\t" (cauldron.json:encode (format nil "~C" #\Tab)))))

(deftest test-encode-control-chars
  ;; Control characters should be escaped as \uXXXX
  (let ((result (cauldron.json:encode (format nil "~C" (code-char 1)))))
    (is (search "\\u0001" result))))

;;; --- Arrays ---

(deftest test-encode-vector
  (is-equal "[1,2,3]" (cauldron.json:encode #(1 2 3))))

(deftest test-encode-empty-vector
  (is-equal "[]" (cauldron.json:encode #())))

(deftest test-encode-list-as-array
  (is-equal "[1,2,3]" (cauldron.json:encode '(1 2 3))))

(deftest test-encode-empty-list
  ;; nil → null, not empty array
  (is-equal "null" (cauldron.json:encode nil)))

;;; --- Objects ---

(deftest test-encode-hash-table
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "name" ht) "Alice")
    (setf (gethash "age" ht) 30)
    (let ((result (cauldron.json:encode ht)))
      (is (search "\"age\":30" result))
      (is (search "\"name\":\"Alice\"" result)))))

(deftest test-encode-plist
  (let ((result (cauldron.json:encode '(:name "Bob" :age 25))))
    (is (search "\"name\":\"Bob\"" result))
    (is (search "\"age\":25" result))))

;;; --- Nested ---

(deftest test-encode-nested
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "items" ht) #(1 2 3))
    (let ((result (cauldron.json:encode ht)))
      (is (search "\"items\":[1,2,3]" result)))))

;;; --- Error cases ---

(deftest test-encode-nan-signals-error
  ;; Create NaN by masking float traps, then verify encode rejects it
  (let ((nan (sb-int:with-float-traps-masked (:invalid) (/ 0.0d0 0.0d0))))
    (signals-condition cauldron.json:json-encode-error
      (cauldron.json:encode nan))))
