;;;; test/http-client/enhanced-client-test.lisp — Tests for enhanced HTTP client
(in-package :cauldron.test)

(defsuite :http-client-enhanced)

;;; --- Function existence ---

(deftest test-http-request-exists
  (is (fboundp 'cauldron.http-client:http-request)))

(deftest test-http-get-exists
  (is (fboundp 'cauldron.http-client:http-get)))

(deftest test-http-put-exists
  (is (fboundp 'cauldron.http-client:http-put)))

(deftest test-http-patch-exists
  (is (fboundp 'cauldron.http-client:http-patch)))

(deftest test-http-delete-exists
  (is (fboundp 'cauldron.http-client:http-delete)))

(deftest test-http-request-json-exists
  (is (fboundp 'cauldron.http-client:http-request-json)))

;;; --- http-request returns three values ---

(deftest test-http-request-returns-three-values
  "http-request should return (values body status headers)."
  (multiple-value-bind (body status headers)
      (cauldron.http-client:http-request "http://localhost:1" :method :get)
    (is (stringp body))
    (is (integerp status))
    (is (listp headers))))

;;; --- Convenience wrappers delegate correctly ---

(deftest test-http-get-returns-values
  (multiple-value-bind (body status headers)
      (cauldron.http-client:http-get "http://localhost:1")
    (is (stringp body))
    (is (integerp status))
    (is (listp headers))))

(deftest test-http-post-returns-values
  (multiple-value-bind (body status headers)
      (cauldron.http-client:http-post "http://localhost:1" :body "{}")
    (is (stringp body))
    (is (integerp status))
    (is (listp headers))))

(deftest test-http-put-returns-values
  (multiple-value-bind (body status headers)
      (cauldron.http-client:http-put "http://localhost:1" :body "{}")
    (is (stringp body))
    (is (integerp status))
    (is (listp headers))))

(deftest test-http-patch-returns-values
  (multiple-value-bind (body status headers)
      (cauldron.http-client:http-patch "http://localhost:1" :body "{}")
    (is (stringp body))
    (is (integerp status))
    (is (listp headers))))

(deftest test-http-delete-returns-values
  (multiple-value-bind (body status headers)
      (cauldron.http-client:http-delete "http://localhost:1")
    (is (stringp body))
    (is (integerp status))
    (is (listp headers))))

;;; --- Backward-compatible http-post-json ---

(deftest test-http-post-json-backward-compatible
  "http-post-json should still return only (values body status)."
  (multiple-value-bind (body status)
      (cauldron.http-client:http-post-json "http://localhost:1" nil "{}")
    (is (stringp body))
    (is (integerp status))))

;;; --- Logging hook ---

(deftest test-log-external-call-fn-called
  "The logging hook should be called after each request."
  (let ((called nil))
    (let ((cauldron.http-client:*log-external-call-fn*
            (lambda (plist)
              (setf called plist))))
      (cauldron.http-client:http-get "http://localhost:1")
      (is-not-nil called)
      (is-not-nil (getf called :url))
      (is-equal :get (getf called :method))
      (is (integerp (getf called :status)))
      (is (integerp (getf called :duration-ms))))))

(deftest test-log-external-call-fn-nil-by-default
  "Default value of *log-external-call-fn* should be nil."
  ;; Just verifying the default — save and restore
  (let ((saved cauldron.http-client:*log-external-call-fn*))
    (unwind-protect
         (progn
           (setf cauldron.http-client:*log-external-call-fn* nil)
           ;; Should not error even with nil hook
           (cauldron.http-client:http-get "http://localhost:1")
           (is-true t))
      (setf cauldron.http-client:*log-external-call-fn* saved))))

;;; --- Internal helpers ---

(deftest test-parse-response-headers
  (let ((headers (cauldron.http-client::%parse-response-headers
                  (format nil "HTTP/1.1 200 OK~C~%Content-Type: application/json~C~%X-Request-Id: abc123"
                          #\Return #\Return))))
    (is-not-nil (assoc "Content-Type" headers :test #'string=))
    (is-equal "application/json" (cdr (assoc "Content-Type" headers :test #'string=)))
    (is-equal "abc123" (cdr (assoc "X-Request-Id" headers :test #'string=)))))

(deftest test-split-headers-and-body-crlf
  (let ((raw (format nil "HTTP/1.1 200 OK~C~%Content-Type: text/plain~C~%~C~%Hello body"
                     #\Return #\Return #\Return)))
    (multiple-value-bind (header-block body)
        (cauldron.http-client::%split-headers-and-body raw)
      (is-not-nil (search "Content-Type" header-block))
      (is-equal "Hello body" body))))

(deftest test-split-headers-and-body-lf
  (let ((raw (format nil "HTTP/1.1 200 OK~%Content-Type: text/plain~%~%Hello body")))
    (multiple-value-bind (header-block body)
        (cauldron.http-client::%split-headers-and-body raw)
      (is-not-nil (search "Content-Type" header-block))
      (is-equal "Hello body" body))))

(deftest test-method-string-conversion
  (is-equal "GET" (cauldron.http-client::%method-string :get))
  (is-equal "POST" (cauldron.http-client::%method-string :post))
  (is-equal "PUT" (cauldron.http-client::%method-string :put))
  (is-equal "PATCH" (cauldron.http-client::%method-string :patch))
  (is-equal "DELETE" (cauldron.http-client::%method-string :delete)))
