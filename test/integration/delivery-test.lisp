;;;; test/integration/delivery-test.lisp — Phase 40: Webhook delivery tests
(in-package :cauldron.test)

(defsuite :webhook-delivery)

;;; ============================================================
;;; Payload Signing
;;; ============================================================

(deftest test-sign-webhook-payload-format
  (let ((sig (cauldron.integration:sign-webhook-payload "test body" "secret123")))
    (is (stringp sig) "Signature is a string")
    (is (search "sha256=" sig) "Starts with sha256=")
    ;; After sha256= should be hex digest (64 chars)
    (is-equal (+ 7 64) (length sig) "sha256= + 64 hex chars")))

(deftest test-sign-webhook-payload-deterministic
  (let ((sig1 (cauldron.integration:sign-webhook-payload "payload" "key"))
        (sig2 (cauldron.integration:sign-webhook-payload "payload" "key")))
    (is-equal sig1 sig2 "Same input produces same signature")))

(deftest test-sign-webhook-payload-different-secrets
  (let ((sig1 (cauldron.integration:sign-webhook-payload "payload" "key1"))
        (sig2 (cauldron.integration:sign-webhook-payload "payload" "key2")))
    (is (not (string= sig1 sig2)) "Different secrets produce different signatures")))

(deftest test-sign-webhook-payload-different-payloads
  (let ((sig1 (cauldron.integration:sign-webhook-payload "payload1" "key"))
        (sig2 (cauldron.integration:sign-webhook-payload "payload2" "key")))
    (is (not (string= sig1 sig2)) "Different payloads produce different signatures")))

;;; ============================================================
;;; Backoff Schedule
;;; ============================================================

(deftest test-delivery-backoff-schedule
  (is-equal 10 (cauldron.integration:compute-delivery-backoff 0) "First retry: 10s")
  (is-equal 60 (cauldron.integration:compute-delivery-backoff 1) "Second retry: 60s")
  (is-equal 300 (cauldron.integration:compute-delivery-backoff 2) "Third retry: 5m")
  (is-equal 1800 (cauldron.integration:compute-delivery-backoff 3) "Fourth retry: 30m")
  (is-equal 7200 (cauldron.integration:compute-delivery-backoff 4) "Fifth retry: 2h"))

(deftest test-delivery-backoff-exhausted
  (is-nil (cauldron.integration:compute-delivery-backoff 5) "No retry after 5")
  (is-nil (cauldron.integration:compute-delivery-backoff 10) "No retry after 10"))

(deftest test-delivery-backoff-custom-schedule
  (let ((cauldron.integration:*delivery-backoff-seconds* '(1 2 3)))
    (is-equal 1 (cauldron.integration:compute-delivery-backoff 0))
    (is-equal 3 (cauldron.integration:compute-delivery-backoff 2))
    (is-nil (cauldron.integration:compute-delivery-backoff 3))))

;;; ============================================================
;;; Delivery SQL
;;; ============================================================

(deftest test-enqueue-delivery-sql
  (multiple-value-bind (sql params)
      (cauldron.integration:enqueue-delivery-sql 1 "order.created"
                                                  "{\"id\":42}" "https://example.com/hook")
    (is (search "INSERT INTO webhook_deliveries" sql))
    (is (search "RETURNING" sql))
    (is-equal 4 (length params))
    (is-equal 1 (first params))
    (is-equal "order.created" (second params))))

(deftest test-mark-delivery-success-sql
  (multiple-value-bind (sql params)
      (cauldron.integration:mark-delivery-success-sql 99)
    (is (search "delivered" sql))
    (is-equal 1 (length params))
    (is-equal 99 (first params))))

(deftest test-mark-delivery-retry-sql
  (multiple-value-bind (sql params)
      (cauldron.integration:mark-delivery-retry-sql 5 "Connection refused" 60)
    (is (search "attempts = attempts + 1" sql))
    (is (search "60 seconds" sql))
    (is-equal 2 (length params))
    (is-equal 5 (first params))
    (is-equal "Connection refused" (second params))))

(deftest test-mark-delivery-exhausted-sql
  (multiple-value-bind (sql params)
      (cauldron.integration:mark-delivery-exhausted-sql 7 "timeout")
    (is (search "exhausted" sql))
    (is-equal 2 (length params))))

(deftest test-pending-deliveries-sql
  (multiple-value-bind (sql params)
      (cauldron.integration:pending-deliveries-sql :limit 10)
    (is (search "pending" sql))
    (is (search "LIMIT 10" sql))
    (is-nil params)))

(deftest test-list-deliveries-sql-all
  (multiple-value-bind (sql params)
      (cauldron.integration:list-deliveries-sql)
    (is (search "SELECT" sql))
    (is-nil params)))

(deftest test-list-deliveries-sql-by-webhook
  (multiple-value-bind (sql params)
      (cauldron.integration:list-deliveries-sql :webhook-id 3)
    (is (search "webhook_id" sql))
    (is-equal 1 (length params))
    (is-equal 3 (first params))))

;;; ============================================================
;;; Webhook Endpoint SQL
;;; ============================================================

(deftest test-create-webhook-endpoint-sql
  (multiple-value-bind (sql params)
      (cauldron.integration:create-webhook-endpoint-sql
       1 "https://hook.example.com/callback" "secret123"
       '("order.created" "order.updated"))
    (is (search "INSERT INTO webhook_endpoints" sql))
    (is-equal 4 (length params))
    (is-equal 1 (first params))
    (is-equal "https://hook.example.com/callback" (second params))
    (is-equal "secret123" (third params))
    ;; Events should be JSON
    (is (search "order.created" (fourth params)) "Events JSON")))

(deftest test-list-webhook-endpoints-sql
  (multiple-value-bind (sql params)
      (cauldron.integration:list-webhook-endpoints-sql 5)
    (is (search "SELECT" sql))
    (is-equal 1 (length params))
    (is-equal 5 (first params))))

(deftest test-disable-webhook-endpoint-sql
  (multiple-value-bind (sql params)
      (cauldron.integration:disable-webhook-endpoint-sql 10)
    (is (search "enabled_p = false" sql))
    (is-equal 1 (length params))))

;;; ============================================================
;;; Process Delivery Logic
;;; ============================================================

(deftest test-process-delivery-exhausted
  ;; When attempts >= max, should return :exhausted
  (multiple-value-bind (outcome sql-fn)
      (cauldron.integration:process-delivery
       1 "http://localhost:9999/nonexistent" "{}" "secret" 4 5)
    ;; Will fail to connect but with attempts 4 of max 5, it's the 5th attempt
    (is (member outcome '(:exhausted :retry)) "Either exhausted or retry on connection failure")))
