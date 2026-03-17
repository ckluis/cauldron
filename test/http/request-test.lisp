;;;; test/http/request-test.lisp — HTTP request parsing tests
(in-package :cauldron.test)

(defsuite :http-request)

;;; --- url-decode ---

(deftest test-url-decode-space
  (is-equal "hello world" (cauldron.http:url-decode "hello%20world"))
  (is-equal "hello world" (cauldron.http:url-decode "hello+world")))

(deftest test-url-decode-slash
  (is-equal "/path/to" (cauldron.http:url-decode "%2Fpath%2Fto")))

(deftest test-url-decode-passthrough
  (is-equal "hello" (cauldron.http:url-decode "hello")))

(deftest test-url-decode-empty
  (is-equal "" (cauldron.http:url-decode "")))

(deftest test-url-decode-special-chars
  (is-equal "a&b=c" (cauldron.http:url-decode "a%26b%3Dc")))

;;; --- hex-digit-value ---

(deftest test-hex-digit-value
  (is-equal 0 (cauldron.http::hex-digit-value #\0))
  (is-equal 9 (cauldron.http::hex-digit-value #\9))
  (is-equal 10 (cauldron.http::hex-digit-value #\a))
  (is-equal 15 (cauldron.http::hex-digit-value #\f))
  (is-equal 10 (cauldron.http::hex-digit-value #\A))
  (is-equal 15 (cauldron.http::hex-digit-value #\F))
  (is-nil (cauldron.http::hex-digit-value #\g)))

;;; --- parse-query-string ---

(deftest test-parse-query-string-basic
  (let ((result (cauldron.http:parse-query-string "a=1&b=2")))
    (is-equal 2 (length result))
    (is-equal "1" (cdr (assoc "a" result :test #'string=)))
    (is-equal "2" (cdr (assoc "b" result :test #'string=)))))

(deftest test-parse-query-string-empty-value
  (let ((result (cauldron.http:parse-query-string "key=")))
    (is-equal 1 (length result))
    (is-equal "" (cdr (first result)))))

(deftest test-parse-query-string-no-value
  (let ((result (cauldron.http:parse-query-string "key")))
    (is-equal 1 (length result))
    (is-equal "" (cdr (first result)))))

(deftest test-parse-query-string-encoded
  (let ((result (cauldron.http:parse-query-string "name=hello%20world")))
    (is-equal "hello world" (cdr (first result)))))

(deftest test-parse-query-string-nil
  (is-nil (cauldron.http:parse-query-string nil))
  (is-nil (cauldron.http:parse-query-string "")))

;;; --- parse-request-line ---

(deftest test-parse-request-line-get
  (multiple-value-bind (method path query version)
      (cauldron.http::parse-request-line "GET /path?q=1 HTTP/1.1")
    (is-equal :GET method)
    (is-equal "/path" path)
    (is-equal "q=1" query)
    (is-equal "HTTP/1.1" version)))

(deftest test-parse-request-line-post
  (multiple-value-bind (method path query version)
      (cauldron.http::parse-request-line "POST /api/users HTTP/1.1")
    (is-equal :POST method)
    (is-equal "/api/users" path)
    (is-nil query)
    (is-equal "HTTP/1.1" version)))

(deftest test-parse-request-line-malformed
  (signals-condition error
    (cauldron.http::parse-request-line "INVALID")))

(deftest test-parse-request-line-nil
  (signals-condition error
    (cauldron.http::parse-request-line nil)))

;;; --- request-header ---

(deftest test-request-header-case-insensitive
  (let ((req (cauldron.http:make-request
              :headers '(("Content-Type" . "text/html")
                          ("X-Custom" . "value")))))
    (is-equal "text/html" (cauldron.http:request-header req "content-type"))
    (is-equal "value" (cauldron.http:request-header req "x-custom"))
    (is-nil (cauldron.http:request-header req "nonexistent"))))
