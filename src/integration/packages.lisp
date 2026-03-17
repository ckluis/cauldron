;;;; src/integration/packages.lisp — Package definitions for the integration protocol
(defpackage :cauldron.integration
  (:use :cl)
  (:export
   ;; Spec structs + accessors
   #:integration-spec
   #:make-integration-spec
   #:integration-spec-name
   #:integration-spec-base-url
   #:integration-spec-scopes
   #:integration-spec-auth-type
   #:integration-spec-auth-config
   #:integration-spec-rate-limit
   #:integration-spec-retry-config
   #:integration-spec-canonical-log-p
   #:integration-spec-endpoints
   #:integration-spec-webhooks
   #:integration-spec-description
   #:endpoint-spec
   #:make-endpoint-spec
   #:endpoint-spec-name
   #:endpoint-spec-method
   #:endpoint-spec-path
   #:endpoint-spec-content-type
   #:endpoint-spec-params
   #:endpoint-spec-doc
   #:webhook-spec
   #:make-webhook-spec
   #:webhook-spec-name
   #:webhook-spec-path
   #:webhook-spec-signing-key-credential
   #:webhook-spec-verify-method
   #:webhook-spec-topic
   ;; Registry + macro
   #:defintegration
   #:*integration-registry*
   #:find-integration
   #:list-integrations
   ;; Client
   #:call-integration
   ;; Credentials
   #:store-credential
   #:resolve-credential
   #:delete-credential
   #:list-credentials
   ;; Webhooks
   #:verify-webhook
   #:make-webhook-handler
   ;; Retry
   #:with-retry
   #:compute-backoff-ms
   #:retryable-status-p
   ;; Health
   #:health-stats
   #:make-health-stats
   #:health-stats-total-calls
   #:health-stats-success-count
   #:health-stats-failure-count
   #:health-stats-total-duration-ms
   #:health-stats-last-call-at
   #:health-stats-last-error
   #:health-stats-last-status
   #:integration-health
   #:all-integration-health
   #:reset-health
   #:record-health
   ;; Webhook Delivery
   #:sign-webhook-payload
   #:*delivery-backoff-seconds*
   #:compute-delivery-backoff
   #:enqueue-delivery-sql
   #:mark-delivery-success-sql
   #:mark-delivery-retry-sql
   #:mark-delivery-exhausted-sql
   #:pending-deliveries-sql
   #:list-deliveries-sql
   #:create-webhook-endpoint-sql
   #:list-webhook-endpoints-sql
   #:disable-webhook-endpoint-sql
   #:deliver-webhook
   #:process-delivery))
