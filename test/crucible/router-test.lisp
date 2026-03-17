;;;; test/crucible/router-test.lisp — Crucible router tests
(in-package :cauldron.test)

(defsuite :crucible-router)

;;; --- strip-prefix ---

(deftest test-strip-prefix-exact-match
  "Exact prefix match returns /."
  (is-equal "/" (cauldron.crucible::strip-prefix "/api" "/api")))

(deftest test-strip-prefix-with-remainder
  (is-equal "/users" (cauldron.crucible::strip-prefix "/api/users" "/api")))

(deftest test-strip-prefix-nested
  (is-equal "/users/42" (cauldron.crucible::strip-prefix "/api/users/42" "/api")))

(deftest test-strip-prefix-no-match
  (is-nil (cauldron.crucible::strip-prefix "/web/users" "/api")))

(deftest test-strip-prefix-partial-segment-no-match
  "Partial segment match should not strip (e.g. /api vs /apiary)."
  (is-nil (cauldron.crucible::strip-prefix "/apiary" "/api")))

(deftest test-strip-prefix-root
  (is-equal "/foo" (cauldron.crucible::strip-prefix "/foo" "")))

(deftest test-strip-prefix-same-path
  (is-equal "/" (cauldron.crucible::strip-prefix "/admin" "/admin")))

;;; --- match-route (with populated router) ---

(defun make-test-router ()
  "Create a router with test routes."
  (let ((router (cauldron.crucible:make-router :name 'test-router)))
    (cauldron.crucible:add-route router :get "/" (lambda (conn) conn))
    (cauldron.crucible:add-route router :get "/users" (lambda (conn) conn))
    (cauldron.crucible:add-route router :post "/users" (lambda (conn) conn))
    (cauldron.crucible:add-route router :get "/users/:id" (lambda (conn) conn))
    (cauldron.crucible:add-route router :put "/users/:id" (lambda (conn) conn))
    (cauldron.crucible:add-route router :delete "/users/:id" (lambda (conn) conn))
    (cauldron.crucible:add-route router :any "/health" (lambda (conn) conn))
    router))

(deftest test-match-route-get-root
  (let ((router (make-test-router)))
    (is-not-nil (cauldron.crucible:match-route router :get "/"))))

(deftest test-match-route-get-users
  (let ((router (make-test-router)))
    (is-not-nil (cauldron.crucible:match-route router :get "/users"))))

(deftest test-match-route-post-users
  (let ((router (make-test-router)))
    (is-not-nil (cauldron.crucible:match-route router :post "/users"))))

(deftest test-match-route-get-user-by-id
  (let ((router (make-test-router)))
    (multiple-value-bind (handler params)
        (cauldron.crucible:match-route router :get "/users/42")
      (is-not-nil handler)
      (is-equal "42" (cdr (assoc "id" params :test #'string=))))))

(deftest test-match-route-put-user
  (let ((router (make-test-router)))
    (multiple-value-bind (handler params)
        (cauldron.crucible:match-route router :put "/users/7")
      (is-not-nil handler)
      (is-equal "7" (cdr (assoc "id" params :test #'string=))))))

(deftest test-match-route-delete-user
  (let ((router (make-test-router)))
    (is-not-nil (cauldron.crucible:match-route router :delete "/users/99"))))

(deftest test-match-route-any-method
  "Routes registered with :any match any method."
  (let ((router (make-test-router)))
    (is-not-nil (cauldron.crucible:match-route router :get "/health"))
    (is-not-nil (cauldron.crucible:match-route router :post "/health"))
    (is-not-nil (cauldron.crucible:match-route router :delete "/health"))))

(deftest test-match-route-no-match
  (let ((router (make-test-router)))
    (is-nil (cauldron.crucible:match-route router :get "/nonexistent"))))

(deftest test-match-route-wrong-method
  (let ((router (make-test-router)))
    (is-nil (cauldron.crucible:match-route router :patch "/users"))))

;;; --- Mounted sub-routers ---

(deftest test-match-route-mount
  (let ((router (cauldron.crucible:make-router :name 'main))
        (api (cauldron.crucible:make-router :name 'api)))
    (cauldron.crucible:add-route api :get "/items" (lambda (conn) conn))
    (cauldron.crucible:mount router "/api" api)
    (is-not-nil (cauldron.crucible:match-route router :get "/api/items"))))

(deftest test-match-route-mount-with-params
  (let ((router (cauldron.crucible:make-router :name 'main))
        (api (cauldron.crucible:make-router :name 'api)))
    (cauldron.crucible:add-route api :get "/items/:id" (lambda (conn) conn))
    (cauldron.crucible:mount router "/api" api)
    (multiple-value-bind (handler params)
        (cauldron.crucible:match-route router :get "/api/items/5")
      (is-not-nil handler)
      (is-equal "5" (cdr (assoc "id" params :test #'string=))))))

(deftest test-match-route-mount-no-match-outside
  (let ((router (cauldron.crucible:make-router :name 'main))
        (api (cauldron.crucible:make-router :name 'api)))
    (cauldron.crucible:add-route api :get "/items" (lambda (conn) conn))
    (cauldron.crucible:mount router "/api" api)
    (is-nil (cauldron.crucible:match-route router :get "/items"))))
