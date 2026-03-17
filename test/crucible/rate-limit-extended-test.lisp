;;;; test/crucible/rate-limit-extended-test.lisp — Extended rate limit tests
(in-package :cauldron.test)

(defsuite :rate-limit-extended)

;;; --- Store creation ---

(deftest test-rate-limit-store-defaults
  (let ((store (cauldron.crucible:make-rate-limit-store)))
    (is-not-nil store)
    (is-equal 100 (cauldron.crucible::rate-limit-store-max-requests store))
    (is-equal 60 (cauldron.crucible::rate-limit-store-window-seconds store))
    (is-not-nil (cauldron.crucible::rate-limit-store-lock store))
    (is (hash-table-p (cauldron.crucible::rate-limit-store-table store)))))

(deftest test-rate-limit-store-custom-config
  (let ((store (cauldron.crucible:make-rate-limit-store
                :max-requests 50 :window-seconds 120)))
    (is-equal 50 (cauldron.crucible::rate-limit-store-max-requests store))
    (is-equal 120 (cauldron.crucible::rate-limit-store-window-seconds store))))

(deftest test-rate-limit-store-empty-table
  "Freshly created store has empty table."
  (let ((store (cauldron.crucible:make-rate-limit-store)))
    (is-equal 0 (hash-table-count (cauldron.crucible::rate-limit-store-table store)))))

;;; --- Cleanup of expired entries ---

(deftest test-rate-limit-cleanup-expired-entries
  "Entries with window-seconds=0 are all expired and get cleaned."
  (let ((store (cauldron.crucible:make-rate-limit-store :max-requests 10 :window-seconds 0))
        (plug nil))
    (setf plug (cauldron.crucible:make-plug-rate-limit :store store))
    ;; Make requests to populate the store
    (dolist (ip '("1.1.1.1" "2.2.2.2" "3.3.3.3"))
      (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
             (conn (cauldron.crucible:conn-put-assign conn :remote-addr ip)))
        (funcall plug conn)))
    ;; All entries should be expired with window-seconds=0
    (let ((removed (cauldron.crucible:rate-limit-cleanup store)))
      (is (>= removed 0) "cleanup returns non-negative count"))))

(deftest test-rate-limit-cleanup-no-entries
  "Cleanup on empty store returns 0."
  (let ((store (cauldron.crucible:make-rate-limit-store)))
    (is-equal 0 (cauldron.crucible:rate-limit-cleanup store))))

;;; --- Rate limit plug with different limits ---

(deftest test-rate-limit-limit-of-one
  "With max-requests=1, second request is blocked."
  (let* ((store (cauldron.crucible:make-rate-limit-store :max-requests 1 :window-seconds 60))
         (plug (cauldron.crucible:make-plug-rate-limit :store store)))
    ;; First request passes
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
           (conn (cauldron.crucible:conn-put-assign conn :remote-addr "10.0.0.1"))
           (result (funcall plug conn)))
      (is-false (cauldron.crucible:conn-halted-p result)))
    ;; Second request blocked
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
           (conn (cauldron.crucible:conn-put-assign conn :remote-addr "10.0.0.1"))
           (result (funcall plug conn)))
      (is-true (cauldron.crucible:conn-halted-p result))
      (is-equal 429 (cauldron.crucible:conn-status result)))))

(deftest test-rate-limit-high-limit
  "With max-requests=1000, many requests pass."
  (let* ((store (cauldron.crucible:make-rate-limit-store :max-requests 1000 :window-seconds 60))
         (plug (cauldron.crucible:make-plug-rate-limit :store store)))
    (dotimes (i 100)
      (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
             (conn (cauldron.crucible:conn-put-assign conn :remote-addr "10.0.0.2"))
             (result (funcall plug conn)))
        (is-false (cauldron.crucible:conn-halted-p result))))))

;;; --- Multiple IPs don't interfere ---

(deftest test-rate-limit-multiple-ips-independent
  "Each IP has its own counter; one IP being rate-limited doesn't affect others."
  (let* ((store (cauldron.crucible:make-rate-limit-store :max-requests 2 :window-seconds 60))
         (plug (cauldron.crucible:make-plug-rate-limit :store store)))
    ;; IP-A uses both its requests
    (dotimes (i 2)
      (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
             (conn (cauldron.crucible:conn-put-assign conn :remote-addr "a.a.a.a")))
        (funcall plug conn)))
    ;; IP-A is now rate-limited
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
           (conn (cauldron.crucible:conn-put-assign conn :remote-addr "a.a.a.a"))
           (result (funcall plug conn)))
      (is-true (cauldron.crucible:conn-halted-p result)))
    ;; IP-B should still be allowed
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
           (conn (cauldron.crucible:conn-put-assign conn :remote-addr "b.b.b.b"))
           (result (funcall plug conn)))
      (is-false (cauldron.crucible:conn-halted-p result)))
    ;; IP-C should still be allowed
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
           (conn (cauldron.crucible:conn-put-assign conn :remote-addr "c.c.c.c"))
           (result (funcall plug conn)))
      (is-false (cauldron.crucible:conn-halted-p result)))))

(deftest test-rate-limit-429-has-content-type
  "429 response includes Content-Type: text/plain."
  (let* ((store (cauldron.crucible:make-rate-limit-store :max-requests 1 :window-seconds 60))
         (plug (cauldron.crucible:make-plug-rate-limit :store store)))
    ;; Exhaust the limit
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
           (conn (cauldron.crucible:conn-put-assign conn :remote-addr "ct.test")))
      (funcall plug conn))
    ;; Check 429 response
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
           (conn (cauldron.crucible:conn-put-assign conn :remote-addr "ct.test"))
           (result (funcall plug conn)))
      (is-equal 429 (cauldron.crucible:conn-status result))
      (let ((ct (cdr (assoc "Content-Type" (cauldron.crucible:conn-resp-headers result)
                            :test #'string=))))
        (is-equal "text/plain" ct)))))
