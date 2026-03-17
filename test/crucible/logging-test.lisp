;;;; test/crucible/logging-test.lisp — Tests for request logging
(in-package :cauldron.test)

(defsuite :crucible-logging)

(deftest test-plug-request-log-sets-start-time
  (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
         (result (cauldron.crucible:plug-request-log conn)))
    (is-not-nil (cauldron.crucible:conn-get-assign result :request-start-time))
    (is-true (integerp (cauldron.crucible:conn-get-assign result :request-start-time)))))

(deftest test-format-request-log-output
  (let ((line (cauldron.crucible::format-request-log :get "/users" 200 12)))
    (is-equal "GET /users 200 12ms" line)))

(deftest test-format-request-log-post
  (let ((line (cauldron.crucible::format-request-log :post "/api/data" 201 45)))
    (is-equal "POST /api/data 201 45ms" line)))

(deftest test-log-request-completion-disabled
  (let ((cauldron.crucible:*request-log-enabled* nil)
        (conn (cauldron.crucible:make-conn :method :get :path "/" :status 200)))
    ;; Should not error when logging is disabled
    (let ((result (cauldron.crucible:log-request-completion conn)))
      (is-not-nil result))))

(deftest test-log-request-completion-writes-to-stream
  (let* ((output (make-string-output-stream))
         (cauldron.crucible:*request-log-stream* output)
         (cauldron.crucible:*request-log-enabled* t)
         (conn (cauldron.crucible:make-conn :method :get :path "/test" :status 200))
         (conn (cauldron.crucible:conn-put-assign conn :request-start-time
                  (get-internal-real-time))))
    (cauldron.crucible:log-request-completion conn)
    (let ((logged (get-output-stream-string output)))
      (is-not-nil (search "GET" logged))
      (is-not-nil (search "/test" logged))
      (is-not-nil (search "200" logged))
      (is-not-nil (search "ms" logged)))))
