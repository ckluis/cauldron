;;;; test/runtime/utilities-test.lisp — Tests for runtime utility functions
(in-package :cauldron.test)

(defsuite :runtime-utilities)

;;; --- ht (hash-table builder) ---

(deftest test-ht-builds-hash-table
  (let ((ht (cauldron.runtime:ht "a" 1 "b" 2)))
    (is (hash-table-p ht))
    (is-equal 1 (gethash "a" ht))
    (is-equal 2 (gethash "b" ht))))

(deftest test-ht-empty
  (let ((ht (cauldron.runtime:ht)))
    (is (hash-table-p ht))
    (is-equal 0 (hash-table-count ht))))

(deftest test-ht-single-pair
  (let ((ht (cauldron.runtime:ht "key" "value")))
    (is-equal "value" (gethash "key" ht))))

;;; --- ht-get ---

(deftest test-ht-get-from-hash-table
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "name" ht) "Alice")
    (is-equal "Alice" (cauldron.runtime:ht-get ht "name"))))

(deftest test-ht-get-from-alist
  (let ((alist '(("name" . "Bob") ("age" . 30))))
    (is-equal "Bob" (cauldron.runtime:ht-get alist "name"))))

(deftest test-ht-get-default
  (is-equal "fallback" (cauldron.runtime:ht-get (make-hash-table) "missing" "fallback")))

(deftest test-ht-get-nil-input
  (is-equal "default" (cauldron.runtime:ht-get nil "key" "default")))

(deftest test-ht-get-non-table
  (is-equal "default" (cauldron.runtime:ht-get 42 "key" "default")))

;;; --- split-sequence ---

(deftest test-split-by-char
  (is-equal '("foo" "bar" "baz")
            (cauldron.runtime:split-sequence #\/ "foo/bar/baz")))

(deftest test-split-empty-string
  (is-equal '("")
            (cauldron.runtime:split-sequence #\/ "")))

(deftest test-split-no-delimiter
  (is-equal '("hello")
            (cauldron.runtime:split-sequence #\/ "hello")))

(deftest test-split-consecutive-delimiters
  (is-equal '("a" "" "b")
            (cauldron.runtime:split-sequence #\/ "a//b")))

(deftest test-split-trailing-delimiter
  (is-equal '("a" "b" "")
            (cauldron.runtime:split-sequence #\/ "a/b/")))

(deftest test-split-leading-delimiter
  (is-equal '("" "a" "b")
            (cauldron.runtime:split-sequence #\/ "/a/b")))

;;; --- string-prefix-p ---

(deftest test-prefix-match
  (is-true (cauldron.runtime:string-prefix-p "foo" "foobar")))

(deftest test-prefix-no-match
  (is-false (cauldron.runtime:string-prefix-p "bar" "foobar")))

(deftest test-prefix-empty
  (is-true (cauldron.runtime:string-prefix-p "" "anything")))

(deftest test-prefix-longer-than-string
  (is-false (cauldron.runtime:string-prefix-p "foobarqux" "foo")))

;;; --- string-suffix-p ---

(deftest test-suffix-match
  (is-true (cauldron.runtime:string-suffix-p "bar" "foobar")))

(deftest test-suffix-no-match
  (is-false (cauldron.runtime:string-suffix-p "foo" "foobar")))

(deftest test-suffix-empty
  (is-true (cauldron.runtime:string-suffix-p "" "anything")))

;;; --- when-let ---

(deftest test-when-let-truthy
  (is-equal 42
            (cauldron.runtime:when-let (x 42) x)))

(deftest test-when-let-nil
  (is-nil (cauldron.runtime:when-let (x nil) (error "should not execute"))))

;;; --- if-let ---

(deftest test-if-let-truthy
  (is-equal "yes"
            (cauldron.runtime:if-let (x 42) "yes" "no")))

(deftest test-if-let-nil
  (is-equal "no"
            (cauldron.runtime:if-let (x nil) "yes" "no")))

;;; --- ensure-list ---

(deftest test-ensure-list-already-list
  (is-equal '(1 2 3) (cauldron.runtime:ensure-list '(1 2 3))))

(deftest test-ensure-list-atom
  (is-equal '(42) (cauldron.runtime:ensure-list 42)))

(deftest test-ensure-list-nil
  (is-nil (cauldron.runtime:ensure-list nil)))

;;; --- make-keyword ---

(deftest test-make-keyword
  (is-equal :FOO (cauldron.runtime:make-keyword "foo")))

(deftest test-make-keyword-uppercase
  (is-equal :BAR (cauldron.runtime:make-keyword "BAR")))

;;; --- octets-to-integer / integer-to-octets ---

(deftest test-octets-to-integer
  (is-equal #x01020304
            (cauldron.runtime:octets-to-integer
             (make-array 4 :element-type '(unsigned-byte 8)
                           :initial-contents '(1 2 3 4)))))

(deftest test-integer-to-octets
  (is-equalp #(1 2 3 4)
             (cauldron.runtime:integer-to-octets #x01020304 4)))

(deftest test-octet-roundtrip
  (let* ((n #xDEADBEEF)
         (octets (cauldron.runtime:integer-to-octets n 4))
         (result (cauldron.runtime:octets-to-integer octets)))
    (is-equal n result)))

(deftest test-octet-roundtrip-8-bytes
  (let* ((n #x0102030405060708)
         (octets (cauldron.runtime:integer-to-octets n 8))
         (result (cauldron.runtime:octets-to-integer octets)))
    (is-equal n result)))
