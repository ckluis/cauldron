;;;; test/integration/retry-test.lisp — Retry and backoff tests
(in-package :cauldron.test)

(defsuite :integration-retry)

;;; --- Backoff computation ---

(deftest test-backoff-base
  ;; Attempt 0: base delay + jitter (0-25%)
  (let ((delay (cauldron.integration:compute-backoff-ms 0 500 30000)))
    (is (>= delay 500) "Base delay at least 500ms")
    (is (<= delay 625) "Base delay at most 500 + 25% jitter")))

(deftest test-backoff-exponential
  ;; Attempt 2: 500 * 2^2 = 2000 + jitter
  (let ((delay (cauldron.integration:compute-backoff-ms 2 500 30000)))
    (is (>= delay 2000) "Exponential at attempt 2 at least 2000ms")
    (is (<= delay 2500) "Exponential at attempt 2 at most 2000 + 25% jitter")))

(deftest test-backoff-capped
  ;; Attempt 10: 500 * 2^10 = 512000, capped to 30000 + jitter
  (let ((delay (cauldron.integration:compute-backoff-ms 10 500 30000)))
    (is (>= delay 30000) "Capped at max delay")
    (is (<= delay 37500) "Capped + jitter doesn't exceed max + 25%")))

;;; --- Retryable status codes ---

(deftest test-retryable-status-codes
  (is-not-nil (cauldron.integration:retryable-status-p 429) "429 is retryable")
  (is-not-nil (cauldron.integration:retryable-status-p 500) "500 is retryable")
  (is-not-nil (cauldron.integration:retryable-status-p 502) "502 is retryable")
  (is-not-nil (cauldron.integration:retryable-status-p 503) "503 is retryable")
  (is-not-nil (cauldron.integration:retryable-status-p 504) "504 is retryable"))

(deftest test-non-retryable-status-codes
  (is-nil (cauldron.integration:retryable-status-p 200) "200 is not retryable")
  (is-nil (cauldron.integration:retryable-status-p 400) "400 is not retryable")
  (is-nil (cauldron.integration:retryable-status-p 401) "401 is not retryable")
  (is-nil (cauldron.integration:retryable-status-p 403) "403 is not retryable")
  (is-nil (cauldron.integration:retryable-status-p 404) "404 is not retryable"))

;;; --- with-retry execution ---

(deftest test-with-retry-succeeds-immediately
  ;; Body returns success on first try — no retries
  (multiple-value-bind (body status)
      (cauldron.integration:with-retry (:max-retries 3)
        (values "ok" 200))
    (is-equal "ok" body)
    (is-equal 200 status)))

(deftest test-with-retry-retries-then-succeeds
  ;; Body fails twice with 503, then succeeds
  (let ((call-count 0))
    (multiple-value-bind (body status)
        (cauldron.integration:with-retry (:max-retries 3 :base-delay-ms 1 :max-delay-ms 10)
          (incf call-count)
          (if (<= call-count 2)
              (values nil 503)
              (values "recovered" 200)))
      (is-equal "recovered" body)
      (is-equal 200 status)
      (is-equal 3 call-count "Called 3 times (2 retries + success)"))))

(deftest test-with-retry-exhausts-retries
  ;; Body always fails — should exhaust retries and return last result
  (let ((call-count 0))
    (multiple-value-bind (body status)
        (cauldron.integration:with-retry (:max-retries 2 :base-delay-ms 1 :max-delay-ms 10)
          (incf call-count)
          (values nil 500))
      (is-nil body)
      (is-equal 500 status)
      (is-equal 3 call-count "Called 3 times (initial + 2 retries)"))))

(deftest test-with-retry-non-retryable-returns-immediately
  ;; 400 is not retryable — should return immediately
  (let ((call-count 0))
    (multiple-value-bind (body status)
        (cauldron.integration:with-retry (:max-retries 3 :base-delay-ms 1 :max-delay-ms 10)
          (incf call-count)
          (values "bad request" 400))
      (is-equal "bad request" body)
      (is-equal 400 status)
      (is-equal 1 call-count "Called once — 400 is not retryable"))))
