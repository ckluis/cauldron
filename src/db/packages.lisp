;;;; src/db/packages.lisp — Package definitions for the database layer
(defpackage :cauldron.db
  (:use :cl)
  (:shadow #:listen)
  (:export
   ;; Protocol
   #:write-int32
   #:write-int16
   #:read-int32
   #:read-int16
   #:read-byte-from-stream
   #:write-cstring
   #:read-cstring
   #:write-message
   #:read-message
   #:write-startup-message
   ;; Types
   #:*pg-type-oids*
   #:decode-pg-value
   #:encode-pg-value
   ;; Auth
   #:handle-auth-response
   #:scram-sha256-auth
   #:md5-auth
   ;; Connection
   #:pg-connection
   #:pg-connection-parameters
   #:pg-connection-in-transaction
   #:connect
   #:disconnect
   #:query
   #:execute-sql
   #:prepare
   #:execute-prepared
   #:with-connection
   #:with-transaction
   #:listen
   #:notify
   #:read-notification
   ;; Conditions
   #:pg-error
   #:pg-error-severity
   #:pg-error-code
   #:pg-error-message
   #:pg-error-detail
   ;; Pool
   #:pg-pool
   #:make-pool
   #:checkout
   #:checkin
   #:with-pool-connection
   #:with-tenant-connection
   #:pool-stats
   #:shutdown-pool
   ;; Schema management
   #:create-schema
   #:drop-schema
   #:schema-exists-p
   #:set-search-path
   #:get-search-path
   #:reset-search-path
   #:with-schema
   #:list-schemas
   #:list-tables
   #:list-columns
   #:table-exists-p
   #:validate-schema-name))
