;;;; src/forge/packages.lisp — Package definitions for the admin interface

(defpackage :cauldron.forge
  (:use :cl)
  (:export
   ;; Config
   #:derive-forge-config
   #:forge-config
   #:forge-config-resources
   ;; Router
   #:forge-router
   #:make-forge-router
   ;; Dashboard
   #:dashboard-view
   ;; CRUD
   #:index-view
   #:show-view
   #:create-view
   #:edit-view
   #:delete-action
   #:batch-action
   ;; SQL Console
   #:sql-console-view
   #:execute-sql-query
   ;; Schema Browser
   #:schema-browser-view
   #:detect-drift
   ;; Audit
   #:audit-log-view
   #:record-audit-event
   #:setup-audit-table
   ;; System views
   #:system-info-view
   ;; Integrations
   #:integrations-view
   ;; Agent dashboard
   #:agents-list-view
   #:agent-detail-view))
