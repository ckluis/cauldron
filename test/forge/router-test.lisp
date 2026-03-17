;;;; test/forge/router-test.lisp — Forge router creation tests
(in-package :cauldron.test)

(defsuite :forge-router)

;;; ============================================================
;;; make-forge-router with nil config
;;; ============================================================

(deftest test-forge-router-nil-config
  "With nil config, creates router with only tool routes."
  (let ((router (cauldron.forge:make-forge-router :config nil)))
    (is-not-nil router)
    ;; Should have tool routes: /sql, /sql/execute, /schema, /audit, /system
    ;; and dashboard /
    (is-not-nil (cauldron.crucible:match-route router :get "/"))
    (is-not-nil (cauldron.crucible:match-route router :get "/sql"))
    (is-not-nil (cauldron.crucible:match-route router :post "/sql/execute"))
    (is-not-nil (cauldron.crucible:match-route router :get "/schema"))
    (is-not-nil (cauldron.crucible:match-route router :get "/audit"))
    (is-not-nil (cauldron.crucible:match-route router :get "/system"))))

(deftest test-forge-router-no-resource-routes-without-config
  "Without config, no resource CRUD routes should exist."
  (let ((router (cauldron.forge:make-forge-router :config nil)))
    ;; No resource routes like /users
    (is-nil (cauldron.crucible:match-route router :get "/users"))))

;;; ============================================================
;;; make-forge-router with config
;;; ============================================================

(defun %make-test-forge-config (&rest resource-specs)
  "Build a forge-config from resource specs for testing."
  (cauldron.forge:derive-forge-config
   :resources (mapcar (lambda (spec)
                        (list :name (first spec)
                              :attributes (second spec)
                              :actions '()))
                      resource-specs)))

(deftest test-forge-router-with-single-resource
  "With one resource, should have dashboard + 7 resource routes + 5 tool routes."
  (let* ((config (%make-test-forge-config '(user (name email))))
         (router (cauldron.forge:make-forge-router :config config)))
    ;; Dashboard
    (is-not-nil (cauldron.crucible:match-route router :get "/"))
    ;; Resource routes for user
    (is-not-nil (cauldron.crucible:match-route router :get "/user"))
    (is-not-nil (cauldron.crucible:match-route router :get "/user/123"))
    (is-not-nil (cauldron.crucible:match-route router :get "/user/new"))
    (is-not-nil (cauldron.crucible:match-route router :post "/user"))
    (is-not-nil (cauldron.crucible:match-route router :get "/user/123/edit"))
    (is-not-nil (cauldron.crucible:match-route router :post "/user/123"))
    (is-not-nil (cauldron.crucible:match-route router :delete "/user/123"))
    ;; Tool routes
    (is-not-nil (cauldron.crucible:match-route router :get "/sql"))
    (is-not-nil (cauldron.crucible:match-route router :get "/schema"))
    (is-not-nil (cauldron.crucible:match-route router :get "/audit"))
    (is-not-nil (cauldron.crucible:match-route router :get "/system"))))

(deftest test-forge-router-with-multiple-resources
  "Each resource adds 7 routes."
  (let* ((config (%make-test-forge-config '(user (name email))
                                           '(post (title body))))
         (router (cauldron.forge:make-forge-router :config config)))
    ;; User routes
    (is-not-nil (cauldron.crucible:match-route router :get "/user"))
    (is-not-nil (cauldron.crucible:match-route router :post "/user"))
    ;; Post routes
    (is-not-nil (cauldron.crucible:match-route router :get "/post"))
    (is-not-nil (cauldron.crucible:match-route router :post "/post"))))

(deftest test-forge-router-resource-path-lowercase
  "Resource paths should use lowercase names."
  (let* ((config (%make-test-forge-config '(user (name))))
         (router (cauldron.forge:make-forge-router :config config)))
    (is-not-nil (cauldron.crucible:match-route router :get "/user"))))

;;; ============================================================
;;; forge-router alias
;;; ============================================================

(deftest test-forge-router-alias
  "forge-router should work the same as make-forge-router."
  (let ((router (cauldron.forge:forge-router :config nil)))
    (is-not-nil router)
    (is-not-nil (cauldron.crucible:match-route router :get "/"))))

(deftest test-forge-router-alias-with-config
  (let* ((config (%make-test-forge-config '(item (name price))))
         (router (cauldron.forge:forge-router :config config)))
    (is-not-nil (cauldron.crucible:match-route router :get "/item"))))
