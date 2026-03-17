;;;; test/http/response-test.lisp — HTTP response builder tests
(in-package :cauldron.test)

(defsuite :http-response)

;;; --- status-text ---

(deftest test-status-text-200
  (is-equal "OK" (cauldron.http:status-text 200)))

(deftest test-status-text-404
  (is-equal "Not Found" (cauldron.http:status-text 404)))

(deftest test-status-text-500
  (is-equal "Internal Server Error" (cauldron.http:status-text 500)))

(deftest test-status-text-302
  (is-equal "Found" (cauldron.http:status-text 302)))

(deftest test-status-text-unknown
  (is-equal "Unknown" (cauldron.http:status-text 999)))

;;; --- make-text-response ---

(deftest test-make-text-response
  (let ((resp (cauldron.http::make-text-response "hello")))
    (is-equal 200 (cauldron.http:response-status resp))
    (is-equal "hello" (cauldron.http:response-body resp))
    (is-equal "text/plain; charset=utf-8"
              (cdr (assoc "Content-Type" (cauldron.http:response-headers resp)
                          :test #'string=)))))

(deftest test-make-text-response-custom-status
  (let ((resp (cauldron.http::make-text-response "error" :status 400)))
    (is-equal 400 (cauldron.http:response-status resp))))

;;; --- make-html-response ---

(deftest test-make-html-response
  (let ((resp (cauldron.http::make-html-response "<h1>Hi</h1>")))
    (is-equal 200 (cauldron.http:response-status resp))
    (is-equal "text/html; charset=utf-8"
              (cdr (assoc "Content-Type" (cauldron.http:response-headers resp)
                          :test #'string=)))))

;;; --- make-json-response ---

(deftest test-make-json-response
  (let ((resp (cauldron.http::make-json-response "{\"key\":\"value\"}")))
    (is-equal "application/json"
              (cdr (assoc "Content-Type" (cauldron.http:response-headers resp)
                          :test #'string=)))))

;;; --- make-redirect ---

(deftest test-make-redirect
  (let ((resp (cauldron.http::make-redirect "/dashboard")))
    (is-equal 302 (cauldron.http:response-status resp))
    (is-equal "/dashboard"
              (cdr (assoc "Location" (cauldron.http:response-headers resp)
                          :test #'string=)))))

(deftest test-make-redirect-301
  (let ((resp (cauldron.http::make-redirect "/new-url" :status 301)))
    (is-equal 301 (cauldron.http:response-status resp))))

;;; --- response-header getter/setter ---

(deftest test-response-header-get
  (let ((resp (cauldron.http:make-response
               :headers '(("X-Custom" . "val")))))
    (is-equal "val" (cauldron.http::response-header resp "X-Custom"))
    (is-equal "val" (cauldron.http::response-header resp "x-custom"))))

(deftest test-response-header-set
  (let ((resp (cauldron.http:make-response)))
    (setf (cauldron.http::response-header resp "X-New") "value")
    (is-equal "value" (cauldron.http::response-header resp "X-New"))))

;;; --- body-to-octets ---

(deftest test-body-to-octets-string
  (let ((result (cauldron.http::body-to-octets "Hi")))
    (is (typep result '(vector (unsigned-byte 8))))
    (is-equal 2 (length result))
    (is-equal 72 (aref result 0))   ; H
    (is-equal 105 (aref result 1)))) ; i

(deftest test-body-to-octets-nil
  (let ((result (cauldron.http::body-to-octets nil)))
    (is-equal 0 (length result))))

(deftest test-body-to-octets-already-octets
  (let* ((input (make-array 3 :element-type '(unsigned-byte 8)
                              :initial-contents '(1 2 3)))
         (result (cauldron.http::body-to-octets input)))
    (is-equalp input result)))
