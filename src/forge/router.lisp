;;;; src/forge/router.lisp — Forge admin router
;;;; Mounts at a configurable path, provides routes for all Forge views.

(in-package :cauldron.forge)

(defun make-forge-router (&key (config nil) (mount-path "/forge"))
  "Create the Forge admin router with all routes.
Returns a cauldron.crucible router ready to be mounted."
  (let ((router (cauldron.crucible:make-router :name 'forge-router)))
    ;; Dashboard
    (cauldron.crucible:add-route router :get "/"
      (lambda (conn) (dashboard-view conn config)))

    ;; Resource CRUD routes (for each resource in config)
    (when config
      (dolist (res-config (forge-config-resources config))
        (let* ((rc res-config)
               (base (format nil "/~A" (string-downcase
                                         (symbol-name (forge-resource-config-name rc))))))
          ;; Index
          (cauldron.crucible:add-route router :get base
            (lambda (conn) (index-view conn rc)))
          ;; Show
          (cauldron.crucible:add-route router :get (concatenate 'string base "/:id")
            (lambda (conn) (show-view conn rc)))
          ;; New form
          (cauldron.crucible:add-route router :get (concatenate 'string base "/new")
            (lambda (conn) (create-view conn rc)))
          ;; Create action
          (cauldron.crucible:add-route router :post base
            (lambda (conn) (create-action conn rc)))
          ;; Edit form
          (cauldron.crucible:add-route router :get (concatenate 'string base "/:id/edit")
            (lambda (conn) (edit-view conn rc)))
          ;; Update action
          (cauldron.crucible:add-route router :post (concatenate 'string base "/:id")
            (lambda (conn) (update-action conn rc)))
          ;; Delete action
          (cauldron.crucible:add-route router :delete (concatenate 'string base "/:id")
            (lambda (conn) (delete-action conn rc))))))

    ;; Agent dashboard
    (cauldron.crucible:add-route router :get "/agents"
      (lambda (conn) (agents-list-view conn config)))
    (cauldron.crucible:add-route router :get "/agents/:name"
      (lambda (conn) (agent-detail-view conn config)))

    ;; Integrations
    (cauldron.crucible:add-route router :get "/integrations"
      (lambda (conn) (integrations-view conn config)))

    ;; Tools
    (cauldron.crucible:add-route router :get "/sql"
      (lambda (conn) (sql-console-view conn config)))
    (cauldron.crucible:add-route router :post "/sql/execute"
      (lambda (conn) (execute-sql-query conn config)))
    (cauldron.crucible:add-route router :get "/schema"
      (lambda (conn) (schema-browser-view conn config)))
    (cauldron.crucible:add-route router :get "/audit"
      (lambda (conn) (audit-log-view conn config)))
    (cauldron.crucible:add-route router :get "/system"
      (lambda (conn) (system-info-view conn config)))

    router))

(defun forge-router (&key config)
  "Convenience: create and return a Forge router.
Alias for make-forge-router."
  (make-forge-router :config config))
