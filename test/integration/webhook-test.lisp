;;;; test/integration/webhook-test.lisp — Webhook verification tests
(in-package :cauldron.test)

(defsuite :integration-webhook)

;;; --- Setup ---

(suite-setup :integration-webhook
  (lambda ()
    (clrhash cauldron.integration:*integration-registry*)
    (eval '(cauldron.integration:defintegration test-hooks
      (:base-url "https://hooks.example.com")
      (:webhook :events :path "/webhooks/test"
        :signing-key-credential :test-secret
        :verify :hmac-sha256
        :topic "test.events")))))

(suite-teardown :integration-webhook
  (lambda () (clrhash cauldron.integration:*integration-registry*)))

;;; --- HMAC verification ---

(deftest test-verify-webhook-hmac-valid
  (let* ((secret "my-webhook-secret")
         (payload "hello world")
         (hmac (cauldron.crypto:hmac-sha256 secret payload))
         (signature (with-output-to-string (s)
                      (loop for byte across hmac
                            do (format s "~(~2,'0x~)" byte)))))
    (is-true (cauldron.integration::verify-webhook-hmac payload secret signature)
             "Valid HMAC signature accepted")))

(deftest test-verify-webhook-hmac-with-prefix
  ;; Some services prefix with "sha256="
  (let* ((secret "my-webhook-secret")
         (payload "{\"event\":\"test\"}")
         (hmac (cauldron.crypto:hmac-sha256 secret payload))
         (hex (with-output-to-string (s)
                (loop for byte across hmac
                      do (format s "~(~2,'0x~)" byte))))
         (signature (concatenate 'string "sha256=" hex)))
    (is-true (cauldron.integration::verify-webhook-hmac payload secret signature)
             "Signature with sha256= prefix accepted")))

(deftest test-verify-webhook-hmac-invalid
  (let* ((secret "my-webhook-secret")
         (payload "hello world")
         (bad-sig "0000000000000000000000000000000000000000000000000000000000000000"))
    (is-false (cauldron.integration::verify-webhook-hmac payload secret bad-sig)
              "Invalid HMAC signature rejected")))

;;; --- make-webhook-handler ---

(deftest test-make-webhook-handler-returns-function
  (let ((handler (cauldron.integration:make-webhook-handler 'test-hooks :events
                   :get-secret-fn (lambda () "test-secret"))))
    (is (functionp handler) "make-webhook-handler returns a function")))

(deftest test-verify-webhook-function
  ;; Test the verify-webhook function with registered integration
  (let* ((secret "test-verify-secret")
         (payload "{\"type\":\"payment\"}")
         (hmac (cauldron.crypto:hmac-sha256 secret payload))
         (signature (with-output-to-string (s)
                      (loop for byte across hmac
                            do (format s "~(~2,'0x~)" byte)))))
    (is-true (cauldron.integration:verify-webhook 'test-hooks :events
                                                   payload signature secret)
             "verify-webhook accepts valid signature")))

(deftest test-webhook-hmac-uses-raw-body
  ;; Verify HMAC is computed against raw body string, not parsed body
  (let* ((secret "raw-body-test-secret")
         (raw-body "{\"event\":\"payment.completed\"}")
         ;; Compute HMAC against the raw string
         (hmac (cauldron.crypto:hmac-sha256 secret raw-body))
         (signature (with-output-to-string (s)
                      (loop for byte across hmac
                            do (format s "~(~2,'0x~)" byte)))))
    ;; Verification should match raw body
    (is-true (cauldron.integration::verify-webhook-hmac raw-body secret signature)
             "HMAC computed against raw body string matches")))
