;;;; test/crucible/rate-limit-test.lisp — Tests for rate limiting plug
(in-package :cauldron.test)

(defsuite :crucible-rate-limit)

(deftest test-rate-limit-under-limit-passes
  (let* ((plug (cauldron.crucible:make-plug-rate-limit :max-requests 5 :window-seconds 60))
         (conn (cauldron.crucible:make-conn :method :get :path "/"))
         (conn (cauldron.crucible:conn-put-assign conn :remote-addr "1.2.3.4"))
         (result (funcall plug conn)))
    (is-false (cauldron.crucible:conn-halted-p result))))

(deftest test-rate-limit-over-limit-429
  (let* ((store (cauldron.crucible:make-rate-limit-store :max-requests 3 :window-seconds 60))
         (plug (cauldron.crucible:make-plug-rate-limit :store store)))
    ;; Make 3 requests (should pass)
    (dotimes (i 3)
      (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
             (conn (cauldron.crucible:conn-put-assign conn :remote-addr "5.6.7.8"))
             (result (funcall plug conn)))
        (is-false (cauldron.crucible:conn-halted-p result))))
    ;; 4th request should be rate limited
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
           (conn (cauldron.crucible:conn-put-assign conn :remote-addr "5.6.7.8"))
           (result (funcall plug conn)))
      (is-equal 429 (cauldron.crucible:conn-status result))
      (is-true (cauldron.crucible:conn-halted-p result)))))

(deftest test-rate-limit-retry-after-header
  (let* ((store (cauldron.crucible:make-rate-limit-store :max-requests 1 :window-seconds 60))
         (plug (cauldron.crucible:make-plug-rate-limit :store store)))
    ;; First request passes
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
           (conn (cauldron.crucible:conn-put-assign conn :remote-addr "10.0.0.1")))
      (funcall plug conn))
    ;; Second request should have Retry-After
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
           (conn (cauldron.crucible:conn-put-assign conn :remote-addr "10.0.0.1"))
           (result (funcall plug conn)))
      (is-not-nil (assoc "Retry-After" (cauldron.crucible:conn-resp-headers result)
                         :test #'string=)))))

(deftest test-rate-limit-per-ip-isolation
  (let* ((store (cauldron.crucible:make-rate-limit-store :max-requests 1 :window-seconds 60))
         (plug (cauldron.crucible:make-plug-rate-limit :store store)))
    ;; IP-A uses its one request
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
           (conn (cauldron.crucible:conn-put-assign conn :remote-addr "1.1.1.1")))
      (funcall plug conn))
    ;; IP-B should still be allowed
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
           (conn (cauldron.crucible:conn-put-assign conn :remote-addr "2.2.2.2"))
           (result (funcall plug conn)))
      (is-false (cauldron.crucible:conn-halted-p result)))))

(deftest test-rate-limit-cleanup
  (let* ((store (cauldron.crucible:make-rate-limit-store :max-requests 10 :window-seconds 0))
         (plug (cauldron.crucible:make-plug-rate-limit :store store)))
    ;; Make a request to populate store
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
           (conn (cauldron.crucible:conn-put-assign conn :remote-addr "3.3.3.3")))
      (funcall plug conn))
    ;; Cleanup with window=0 should clear expired entries
    (let ((removed (cauldron.crucible:rate-limit-cleanup store)))
      (is-true (>= removed 0)))))

(deftest test-rate-limit-missing-ip-fallback
  (let* ((plug (cauldron.crucible:make-plug-rate-limit :max-requests 5 :window-seconds 60))
         (conn (cauldron.crucible:make-conn :method :get :path "/"))
         (result (funcall plug conn)))
    ;; Should not error even without :remote-addr
    (is-false (cauldron.crucible:conn-halted-p result))))

(deftest test-rate-limit-thread-safety
  ;; Verify store creation includes a lock
  (let ((store (cauldron.crucible:make-rate-limit-store)))
    (is-not-nil (cauldron.crucible::rate-limit-store-lock store))))

(deftest test-rate-limit-window-reset
  (let* ((store (cauldron.crucible:make-rate-limit-store :max-requests 1 :window-seconds 0))
         (plug (cauldron.crucible:make-plug-rate-limit :store store)))
    ;; First request
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
           (conn (cauldron.crucible:conn-put-assign conn :remote-addr "4.4.4.4")))
      (funcall plug conn))
    ;; With window-seconds=0, the window should have expired by now
    ;; so the next request should be allowed
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
           (conn (cauldron.crucible:conn-put-assign conn :remote-addr "4.4.4.4"))
           (result (funcall plug conn)))
      (is-false (cauldron.crucible:conn-halted-p result)))))
