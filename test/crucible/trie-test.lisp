;;;; test/crucible/trie-test.lisp — Route trie tests
(in-package :cauldron.test)

(defsuite :crucible-trie)

;;; --- Literal match ---

(deftest test-literal-match
  (let ((trie (cauldron.crucible:make-trie)))
    (cauldron.crucible:trie-insert trie :get "/users" :users-handler)
    (multiple-value-bind (handler params)
        (cauldron.crucible:trie-match trie :get "/users")
      (is-equal :users-handler handler)
      (is-nil params))))

(deftest test-literal-no-match
  (let ((trie (cauldron.crucible:make-trie)))
    (cauldron.crucible:trie-insert trie :get "/users" :handler)
    (multiple-value-bind (handler)
        (cauldron.crucible:trie-match trie :get "/posts")
      (is-nil handler))))

;;; --- Param match ---

(deftest test-param-match
  (let ((trie (cauldron.crucible:make-trie)))
    (cauldron.crucible:trie-insert trie :get "/users/:id" :user-handler)
    (multiple-value-bind (handler params)
        (cauldron.crucible:trie-match trie :get "/users/42")
      (is-equal :user-handler handler)
      (is-equal "42" (cdr (assoc "id" params :test #'string=))))))

;;; --- Wildcard match ---

(deftest test-wildcard-match
  (let ((trie (cauldron.crucible:make-trie)))
    (cauldron.crucible:trie-insert trie :get "/files/*path" :files-handler)
    (multiple-value-bind (handler params)
        (cauldron.crucible:trie-match trie :get "/files/docs/readme.md")
      (is-equal :files-handler handler)
      (is-equal "docs/readme.md" (cdr (assoc "path" params :test #'string=))))))

;;; --- Priority: literal > param > wildcard ---

(deftest test-literal-over-param
  (let ((trie (cauldron.crucible:make-trie)))
    (cauldron.crucible:trie-insert trie :get "/users/admin" :admin-handler)
    (cauldron.crucible:trie-insert trie :get "/users/:id" :user-handler)
    (multiple-value-bind (handler)
        (cauldron.crucible:trie-match trie :get "/users/admin")
      (is-equal :admin-handler handler))
    (multiple-value-bind (handler)
        (cauldron.crucible:trie-match trie :get "/users/42")
      (is-equal :user-handler handler))))

;;; --- Method matching ---

(deftest test-method-matching
  (let ((trie (cauldron.crucible:make-trie)))
    (cauldron.crucible:trie-insert trie :get "/users" :get-handler)
    (cauldron.crucible:trie-insert trie :post "/users" :post-handler)
    (multiple-value-bind (handler)
        (cauldron.crucible:trie-match trie :get "/users")
      (is-equal :get-handler handler))
    (multiple-value-bind (handler)
        (cauldron.crucible:trie-match trie :post "/users")
      (is-equal :post-handler handler))))

(deftest test-method-no-match
  (let ((trie (cauldron.crucible:make-trie)))
    (cauldron.crucible:trie-insert trie :get "/users" :handler)
    (multiple-value-bind (handler)
        (cauldron.crucible:trie-match trie :delete "/users")
      (is-nil handler))))

;;; --- Multiple params ---

(deftest test-multiple-params
  (let ((trie (cauldron.crucible:make-trie)))
    (cauldron.crucible:trie-insert trie :get "/users/:user_id/posts/:post_id" :handler)
    (multiple-value-bind (handler params)
        (cauldron.crucible:trie-match trie :get "/users/5/posts/10")
      (is-equal :handler handler)
      (is-equal "5" (cdr (assoc "user_id" params :test #'string=)))
      (is-equal "10" (cdr (assoc "post_id" params :test #'string=))))))

;;; --- Root path ---

(deftest test-root-path
  (let ((trie (cauldron.crucible:make-trie)))
    (cauldron.crucible:trie-insert trie :get "/" :root-handler)
    (multiple-value-bind (handler)
        (cauldron.crucible:trie-match trie :get "/")
      (is-equal :root-handler handler))))
