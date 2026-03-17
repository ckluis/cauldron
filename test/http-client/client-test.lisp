;;;; test/http-client/client-test.lisp — HTTP client tests
(in-package :cauldron.test)

(defsuite :http-client)

(deftest test-http-post-json-exists
  (is (fboundp 'cauldron.http-client:http-post-json)))

(deftest test-http-post-json-returns-two-values
  "http-post-json should return (values body status)."
  (multiple-value-bind (body status)
      (cauldron.http-client:http-post-json
       "http://localhost:1" nil "{}")
    ;; Connection refused — status will be 0, body will be empty
    (is (stringp body))
    (is (integerp status))))
