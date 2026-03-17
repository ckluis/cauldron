;;;; test/crucible/cors-test.lisp — Tests for CORS middleware
(in-package :cauldron.test)

(defsuite :crucible-cors)

(deftest test-cors-preflight-options
  (let* ((plug (cauldron.crucible:make-plug-cors))
         (conn (cauldron.crucible:make-conn
                :method :options :path "/api/data"
                :headers '(("Origin" . "http://example.com"))))
         (result (funcall plug conn)))
    (is-equal 204 (cauldron.crucible:conn-status result))
    (is-not-nil (assoc "Access-Control-Allow-Origin"
                       (cauldron.crucible:conn-resp-headers result) :test #'string=))
    (is-not-nil (assoc "Access-Control-Allow-Methods"
                       (cauldron.crucible:conn-resp-headers result) :test #'string=))
    (is-true (cauldron.crucible:conn-halted-p result))))

(deftest test-cors-simple-get
  (let* ((plug (cauldron.crucible:make-plug-cors))
         (conn (cauldron.crucible:make-conn
                :method :get :path "/api/data"
                :headers '(("Origin" . "http://example.com"))))
         (result (funcall plug conn)))
    (is-equal "*" (cdr (assoc "Access-Control-Allow-Origin"
                              (cauldron.crucible:conn-resp-headers result) :test #'string=)))
    (is-false (cauldron.crucible:conn-halted-p result))))

(deftest test-cors-wildcard-origin
  (let* ((plug (cauldron.crucible:make-plug-cors :allowed-origins '("*")))
         (conn (cauldron.crucible:make-conn
                :method :get :path "/"
                :headers '(("Origin" . "http://any-site.com"))))
         (result (funcall plug conn)))
    (is-equal "*" (cdr (assoc "Access-Control-Allow-Origin"
                              (cauldron.crucible:conn-resp-headers result) :test #'string=)))))

(deftest test-cors-specific-origin-match
  (let* ((plug (cauldron.crucible:make-plug-cors
                :allowed-origins '("http://allowed.com")))
         (conn (cauldron.crucible:make-conn
                :method :get :path "/"
                :headers '(("Origin" . "http://allowed.com"))))
         (result (funcall plug conn)))
    (is-equal "http://allowed.com"
              (cdr (assoc "Access-Control-Allow-Origin"
                          (cauldron.crucible:conn-resp-headers result) :test #'string=)))))

(deftest test-cors-specific-origin-reject
  (let* ((plug (cauldron.crucible:make-plug-cors
                :allowed-origins '("http://allowed.com")))
         (conn (cauldron.crucible:make-conn
                :method :get :path "/"
                :headers '(("Origin" . "http://evil.com"))))
         (result (funcall plug conn)))
    ;; No CORS headers should be added
    (is-nil (assoc "Access-Control-Allow-Origin"
                   (cauldron.crucible:conn-resp-headers result) :test #'string=))))

(deftest test-cors-credentials-header
  (let* ((plug (cauldron.crucible:make-plug-cors :allow-credentials t))
         (conn (cauldron.crucible:make-conn
                :method :get :path "/"
                :headers '(("Origin" . "http://example.com"))))
         (result (funcall plug conn)))
    (is-equal "true"
              (cdr (assoc "Access-Control-Allow-Credentials"
                          (cauldron.crucible:conn-resp-headers result) :test #'string=)))))

(deftest test-cors-no-origin-passthrough
  (let* ((plug (cauldron.crucible:make-plug-cors))
         (conn (cauldron.crucible:make-conn :method :get :path "/"))
         (result (funcall plug conn)))
    ;; No CORS headers when no Origin header
    (is-nil (assoc "Access-Control-Allow-Origin"
                   (cauldron.crucible:conn-resp-headers result) :test #'string=))))

(deftest test-cors-expose-headers
  (let* ((plug (cauldron.crucible:make-plug-cors
                :expose-headers '("X-Custom-Header" "X-Request-Id")))
         (conn (cauldron.crucible:make-conn
                :method :get :path "/"
                :headers '(("Origin" . "http://example.com"))))
         (result (funcall plug conn)))
    (is-not-nil (assoc "Access-Control-Expose-Headers"
                       (cauldron.crucible:conn-resp-headers result) :test #'string=))))
