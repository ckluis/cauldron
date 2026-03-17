;;;; test/http/edge-cases-test.lisp — HTTP edge case tests
(in-package :cauldron.test)

(defsuite :http-edge-cases)

;;; --- URL decode edge cases ---

(deftest test-url-decode-empty-string
  (is-equal "" (cauldron.http:url-decode "")))

(deftest test-url-decode-no-encoding
  "Plain ASCII passes through unchanged."
  (is-equal "hello-world_123" (cauldron.http:url-decode "hello-world_123")))

(deftest test-url-decode-space-via-percent
  (is-equal "a b" (cauldron.http:url-decode "a%20b")))

(deftest test-url-decode-space-via-plus
  (is-equal "a b" (cauldron.http:url-decode "a+b")))

(deftest test-url-decode-multiple-encoded
  (is-equal "a b&c=d" (cauldron.http:url-decode "a%20b%26c%3Dd")))

(deftest test-url-decode-percent-at-end
  "A trailing percent sign without enough hex digits is passed through."
  (is-equal "test%" (cauldron.http:url-decode "test%")))

(deftest test-url-decode-percent-single-hex
  "A percent with only one hex digit is passed through."
  (is-equal "test%2" (cauldron.http:url-decode "test%2")))

(deftest test-url-decode-uppercase-hex
  (is-equal " " (cauldron.http:url-decode "%20"))
  (is-equal "!" (cauldron.http:url-decode "%21")))

(deftest test-url-decode-lowercase-hex
  (is-equal " " (cauldron.http:url-decode "%20"))
  (is-equal "~" (cauldron.http:url-decode "%7e")))

;;; --- Query string parsing edge cases ---

(deftest test-parse-query-string-empty-string
  (is-nil (cauldron.http:parse-query-string "")))

(deftest test-parse-query-string-nil-input
  (is-nil (cauldron.http:parse-query-string nil)))

(deftest test-parse-query-string-single-param
  (let ((result (cauldron.http:parse-query-string "key=value")))
    (is-equal 1 (length result))
    (is-equal "key" (car (first result)))
    (is-equal "value" (cdr (first result)))))

(deftest test-parse-query-string-duplicate-keys
  "Duplicate keys are preserved as separate entries."
  (let ((result (cauldron.http:parse-query-string "a=1&a=2")))
    (is-equal 2 (length result))
    (is-equal "a" (car (first result)))
    (is-equal "1" (cdr (first result)))
    (is-equal "a" (car (second result)))
    (is-equal "2" (cdr (second result)))))

(deftest test-parse-query-string-empty-value
  (let ((result (cauldron.http:parse-query-string "key=")))
    (is-equal 1 (length result))
    (is-equal "" (cdr (first result)))))

(deftest test-parse-query-string-no-equals
  "Key without = gets empty string value."
  (let ((result (cauldron.http:parse-query-string "alone")))
    (is-equal 1 (length result))
    (is-equal "alone" (car (first result)))
    (is-equal "" (cdr (first result)))))

(deftest test-parse-query-string-trailing-ampersand
  "Trailing & does not produce an extra entry."
  (let ((result (cauldron.http:parse-query-string "a=1&")))
    (is-equal 1 (length result))))

(deftest test-parse-query-string-leading-ampersand
  "Leading & does not produce an extra entry."
  (let ((result (cauldron.http:parse-query-string "&a=1")))
    (is-equal 1 (length result))))

;;; --- parse-request-line edge cases ---

(deftest test-parse-request-line-delete-method
  (multiple-value-bind (method path query version)
      (cauldron.http::parse-request-line "DELETE /api/item/42 HTTP/1.1")
    (is-equal :DELETE method)
    (is-equal "/api/item/42" path)
    (is-nil query)
    (is-equal "HTTP/1.1" version)))

(deftest test-parse-request-line-patch-method
  (multiple-value-bind (method path query version)
      (cauldron.http::parse-request-line "PATCH /api/item/42 HTTP/1.1")
    (is-equal :PATCH method)
    (is-equal "/api/item/42" path)
    (is-nil query)
    (is-equal "HTTP/1.1" version)))

(deftest test-parse-request-line-empty-query
  "Path with ? but no query string."
  (multiple-value-bind (method path query version)
      (cauldron.http::parse-request-line "GET /path? HTTP/1.1")
    (is-equal :GET method)
    (is-equal "/path" path)
    (is-equal "" query)
    (is-equal "HTTP/1.1" version)))

(deftest test-parse-request-line-complex-query
  (multiple-value-bind (method path query version)
      (cauldron.http::parse-request-line "GET /search?q=hello+world&page=2 HTTP/1.1")
    (is-equal :GET method)
    (is-equal "/search" path)
    (is-equal "q=hello+world&page=2" query)
    (is-equal "HTTP/1.1" version)))

;;; --- Request struct and header access ---

(deftest test-request-struct-defaults
  (let ((req (cauldron.http:make-request)))
    (is-equal :GET (cauldron.http:request-method req))
    (is-equal "/" (cauldron.http:request-path req))
    (is-nil (cauldron.http:request-query-string req))
    (is-nil (cauldron.http:request-body req))
    (is-nil (cauldron.http:request-content-type req))
    (is-nil (cauldron.http:request-content-length req))))

(deftest test-request-header-missing
  (let ((req (cauldron.http:make-request :headers '(("X-Foo" . "bar")))))
    (is-nil (cauldron.http:request-header req "X-Missing"))))

(deftest test-request-header-case-insensitive
  (let ((req (cauldron.http:make-request
              :headers '(("Content-Type" . "application/json")))))
    (is-equal "application/json" (cauldron.http:request-header req "content-type"))
    (is-equal "application/json" (cauldron.http:request-header req "CONTENT-TYPE"))
    (is-equal "application/json" (cauldron.http:request-header req "Content-Type"))))

;;; --- split-string ---

(deftest test-split-string-basic
  (is-equal '("a" "b" "c") (cauldron.http::split-string "a&b&c" #\&)))

(deftest test-split-string-no-delimiter
  (is-equal '("abc") (cauldron.http::split-string "abc" #\&)))

(deftest test-split-string-empty-string
  (is-equal '("") (cauldron.http::split-string "" #\&)))

;;; --- hex-digit-value edge cases ---

(deftest test-hex-digit-value-boundaries
  (is-equal 0 (cauldron.http::hex-digit-value #\0))
  (is-equal 9 (cauldron.http::hex-digit-value #\9))
  (is-equal 10 (cauldron.http::hex-digit-value #\a))
  (is-equal 10 (cauldron.http::hex-digit-value #\A))
  (is-equal 15 (cauldron.http::hex-digit-value #\f))
  (is-equal 15 (cauldron.http::hex-digit-value #\F))
  (is-nil (cauldron.http::hex-digit-value #\g))
  (is-nil (cauldron.http::hex-digit-value #\G))
  (is-nil (cauldron.http::hex-digit-value #\Space)))
