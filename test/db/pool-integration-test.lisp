;;;; test/db/pool-integration-test.lisp — Connection pool integration tests

(in-package :cauldron.test)

(defsuite :db-pool-integration)

(suite-setup :db-pool-integration
  (lambda () (ensure-test-database)))

;;; --- Pool lifecycle ---

(deftest test-pool-warms-min-size
  (let ((pool (make-test-pool :min-size 2 :max-size 5)))
    (unwind-protect
        (let ((stats (cauldron.db:pool-stats pool)))
          (is (>= (gethash "idle" stats) 2)
              "Pool should warm at least min-size connections")
          (is-equal 0 (gethash "active" stats)
                    "No connections should be active initially"))
      (cauldron.db:shutdown-pool pool))))

(deftest test-checkout-checkin-cycle
  (let ((pool (make-test-pool :min-size 1 :max-size 5)))
    (unwind-protect
        (let ((conn (cauldron.db:checkout pool)))
          (is-not-nil conn "Checkout should return a connection")
          (is (cauldron.db::pg-connection-alive-p conn))
          ;; Use the connection
          (let ((rows (cauldron.db:query conn "SELECT 1 AS n")))
            (is-equal 1 (gethash "n" (first rows))))
          ;; Check stats
          (let ((stats (cauldron.db:pool-stats pool)))
            (is-equal 1 (gethash "active" stats)))
          ;; Return it
          (cauldron.db:checkin pool conn)
          (let ((stats (cauldron.db:pool-stats pool)))
            (is-equal 0 (gethash "active" stats))))
      (cauldron.db:shutdown-pool pool))))

(deftest test-with-pool-connection-macro
  (let ((pool (make-test-pool :min-size 1 :max-size 5)))
    (unwind-protect
        (progn
          (cauldron.db:with-pool-connection (conn pool)
            (let ((rows (cauldron.db:query conn "SELECT 42 AS answer")))
              (is-equal 42 (gethash "answer" (first rows)))))
          ;; After macro exits, connection should be checked in
          (let ((stats (cauldron.db:pool-stats pool)))
            (is-equal 0 (gethash "active" stats))))
      (cauldron.db:shutdown-pool pool))))

(deftest test-recheckout-after-checkin
  (let ((pool (make-test-pool :min-size 1 :max-size 5)))
    (unwind-protect
        (progn
          (let ((conn (cauldron.db:checkout pool)))
            (cauldron.db:checkin pool conn))
          ;; Should be able to check out again
          (let ((conn2 (cauldron.db:checkout pool)))
            (is-not-nil conn2 "Should get a connection on re-checkout")
            (let ((rows (cauldron.db:query conn2 "SELECT 1 AS n")))
              (is-equal 1 (gethash "n" (first rows))))
            (cauldron.db:checkin pool conn2)))
      (cauldron.db:shutdown-pool pool))))

(deftest test-concurrent-checkouts
  (let ((pool (make-test-pool :min-size 2 :max-size 5))
        (results (make-array 5 :initial-element nil))
        (threads '()))
    (unwind-protect
        (progn
          ;; Spawn 5 threads, each checks out, queries, checks in
          (dotimes (i 5)
            (push (cauldron.runtime:spawn
                    (format nil "pool-test-~D" i)
                    (let ((idx i))
                      (lambda ()
                        (cauldron.db:with-pool-connection (conn pool)
                          (let ((rows (cauldron.db:query conn
                                        (format nil "SELECT ~D AS n" idx))))
                            (setf (aref results idx)
                                  (gethash "n" (first rows))))))))
                  threads))
          ;; Wait for all threads
          (dolist (th threads)
            (cauldron.runtime:join-thread th))
          ;; Verify all got correct results
          (dotimes (i 5)
            (is-equal i (aref results i)
                      (format nil "Thread ~D should have gotten ~D" i i))))
      (cauldron.db:shutdown-pool pool))))

(deftest test-pool-stats-structure
  (let ((pool (make-test-pool :min-size 1 :max-size 5)))
    (unwind-protect
        (let ((stats (cauldron.db:pool-stats pool)))
          (is (hash-table-p stats) "pool-stats should return a hash-table")
          (is-not-nil (gethash "max-size" stats))
          (is-not-nil (gethash "min-size" stats))
          (is (integerp (gethash "total-created" stats)))
          (is (integerp (gethash "idle" stats)))
          (is (integerp (gethash "active" stats))))
      (cauldron.db:shutdown-pool pool))))

(deftest test-shutdown-pool-disconnects
  (let* ((pool (make-test-pool :min-size 2 :max-size 5))
         (pre-stats (cauldron.db:pool-stats pool)))
    (is (>= (gethash "idle" pre-stats) 2))
    (cauldron.db:shutdown-pool pool)
    (let ((post-stats (cauldron.db:pool-stats pool)))
      (is-equal 0 (gethash "idle" post-stats)
                "All idle connections should be gone after shutdown"))))

(deftest test-max-size-blocking-timeout
  (let ((pool (make-test-pool :min-size 1 :max-size 2)))
    (unwind-protect
        (let ((c1 (cauldron.db:checkout pool))
              (c2 (cauldron.db:checkout pool)))
          ;; Pool is at capacity. Checkout with short timeout should fail.
          (signals-condition error
            (cauldron.db:checkout pool :timeout 0.5)
            "Checkout beyond max-size should signal an error or timeout")
          (cauldron.db:checkin pool c1)
          (cauldron.db:checkin pool c2))
      (cauldron.db:shutdown-pool pool))))
