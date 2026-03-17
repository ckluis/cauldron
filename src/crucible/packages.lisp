;;;; src/crucible/packages.lisp — Package definitions for the routing layer

(defpackage :cauldron.crucible
  (:use :cl)
  (:export
   ;; Route trie
   #:make-trie
   #:trie-insert
   #:trie-match
   ;; Pipelines
   #:defpipeline
   #:make-pipeline
   #:pipeline-plugs
   #:run-pipeline
   #:halt-conn
   #:conn-halted-p
   ;; Router
   #:defrouter
   #:make-router
   #:add-route
   #:mount
   #:match-route
   #:dispatch
   ;; Conn
   #:make-conn
   #:conn-method
   #:conn-path
   #:conn-params
   #:conn-query-params
   #:conn-body
   #:conn-headers
   #:conn-assigns
   #:conn-status
   #:conn-resp-headers
   #:conn-resp-body
   #:conn-get-assign
   #:conn-put-assign
   #:conn-put-status
   #:conn-put-resp-header
   #:conn-put-resp-body
   ;; Bridge
   #:request-to-conn
   #:conn-to-response
   #:make-app-handler
   ;; Compression
   #:compress-response
   #:*compressible-types*
   #:*compression-min-size*
   ;; CORS
   #:make-plug-cors
   ;; Rate limiting
   #:make-plug-rate-limit
   #:make-rate-limit-store
   #:rate-limit-cleanup
   ;; Logging
   #:plug-request-log
   #:log-request-completion
   #:*request-log-stream*
   #:*request-log-enabled*
   ;; Plugs
   #:plug-parse-body
   #:plug-parse-cookies
   #:plug-set-cookie
   #:plug-session
   #:plug-csrf
   #:plug-flash
   #:plug-security-headers
   #:make-plug-static
   ;; Session config
   #:*session-store*
   #:*session-secret*
   #:*session-cookie-name*
   #:*session-max-age*
   ;; Session helpers
   #:session-get
   #:session-put
   #:session-delete
   #:session-clear
   ;; Flash helpers
   #:put-flash
   ;; Static config
   #:*static-root*
   #:*mime-types*
   #:guess-mime-type
   #:read-file-octets
   #:directory-pathname-p
   ;; Auth
   #:hash-password
   #:verify-password
   #:login
   #:logout
   #:current-user
   #:make-plug-authenticate
   #:plug-require-auth
   #:make-plug-require-role
   ;; Generic plugs
   #:plug-json-content-type
   #:plug-html-content-type
   #:redirect-with-flash
   ;; API response helpers
   #:json-response
   #:json-error
   #:api-base-path
   #:wrap-api-response
   #:role-allows-p
   #:generate-example-value
   #:describe-validation
   #:build-example-payload
   #:build-help-actions
   ;; PG Session Store
   #:pg-session-store
   #:make-pg-session-store-instance
   #:*pg-session-store*
   #:plug-pg-session
   #:plug-pg-session-save
   #:pg-session-cleanup
   ;; API Keys
   #:generate-api-key
   #:create-api-key-sql
   #:revoke-api-key-sql
   #:list-api-keys-sql
   #:find-api-key-by-hash-sql
   #:touch-api-key-sql
   #:rotate-api-key-sql
   #:make-plug-api-key-auth
   #:make-plug-require-scope
   ;; Uploads
   #:*upload-dir*
   #:*max-upload-bytes*
   #:*uploads-ddl*
   #:*attachments-ddl*
   #:*allowed-upload-types*
   #:generate-storage-key
   #:validate-upload-type
   #:save-upload-sql
   #:delete-upload-sql
   #:find-upload-sql
   #:list-uploads-sql
   #:serve-upload-headers
   #:make-plug-upload-limit
   #:attach-upload-sql
   #:list-attachments-sql
   #:detach-upload-sql
   ;; Billing
   #:*billing-plans-ddl*
   #:*subscriptions-ddl*
   #:*invoices-ddl*
   #:create-plan-sql
   #:list-plans-sql
   #:find-plan-sql
   #:create-subscription-sql
   #:update-subscription-status-sql
   #:cancel-subscription-sql
   #:reactivate-subscription-sql
   #:find-subscription-sql
   #:create-invoice-sql
   #:list-invoices-sql
   #:make-plug-require-subscription
   #:make-plug-metered-usage
   ;; Activity Feed
   #:*activity-events-ddl*
   #:record-activity-sql
   #:list-activities-sql
   #:count-activities-sql
   #:activity-feed-sql
   #:make-plug-activity-tracking
   #:activity-ether-topic
   ;; Security Hardening
   #:make-plug-content-security-policy
   #:make-plug-hsts
   #:plug-secure-headers-enhanced
   #:make-plug-ip-allowlist
   #:make-plug-request-size-limit
   #:secure-random-string
   #:regenerate-session-id
   #:validate-sql-identifier))
