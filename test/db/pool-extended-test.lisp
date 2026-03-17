;;;; test/db/pool-extended-test.lisp — Extended pool struct + state tests (no real DB)
(in-package :cauldron.test)

(defsuite :db-pool-extended)

;;; --- Pool struct creation and accessors ---

(deftest test-pg-pool-struct-all-accessors
  "Verify all pg-pool struct accessors return expected values."
  (let ((pool (cauldron.db::make-pg-pool
               :host "db.example.com"
               :port 5433
               :database "mydb"
               :user "admin"
               :password "secret"
               :min-size 3
               :max-size 15
               :leak-threshold-seconds 45)))
    (is-equal "db.example.com" (cauldron.db::pg-pool-host pool))
    (is-equal 5433 (cauldron.db::pg-pool-port pool))
    (is-equal "mydb" (cauldron.db::pg-pool-database pool))
    (is-equal "admin" (cauldron.db::pg-pool-user pool))
    (is-equal "secret" (cauldron.db::pg-pool-password pool))
    (is-equal 3 (cauldron.db::pg-pool-min-size pool))
    (is-equal 15 (cauldron.db::pg-pool-max-size pool))
    (is-equal 45 (cauldron.db::pg-pool-leak-threshold-seconds pool))
    (is-false (cauldron.db::pg-pool-shutdown-p pool))))

(deftest test-pg-pool-default-leak-threshold
  "Default leak threshold is 30 seconds."
  (let ((pool (cauldron.db::make-pg-pool)))
    (is-equal 30 (cauldron.db::pg-pool-leak-threshold-seconds pool))))

;;; --- Pool state machine: idle/active transitions ---

(deftest test-pool-idle-connections-push-pop
  "Idle connections list supports push/pop operations."
  (let ((pool (cauldron.db::make-pg-pool))
        (conn1 (cauldron.db::make-pg-connection))
        (conn2 (cauldron.db::make-pg-connection)))
    (cauldron.runtime:with-lock ((cauldron.db::pg-pool-lock pool))
      (push conn1 (cauldron.db::pg-pool-idle-connections pool))
      (push conn2 (cauldron.db::pg-pool-idle-connections pool)))
    (is-equal 2 (length (cauldron.db::pg-pool-idle-connections pool)))
    ;; Pop one
    (let ((popped nil))
      (cauldron.runtime:with-lock ((cauldron.db::pg-pool-lock pool))
        (setf popped (pop (cauldron.db::pg-pool-idle-connections pool))))
      (is-not-nil popped)
      (is-equal 1 (length (cauldron.db::pg-pool-idle-connections pool))))))

(deftest test-pool-active-count-tracking
  "Active count increments and decrements correctly."
  (let ((pool (cauldron.db::make-pg-pool)))
    (is-equal 0 (cauldron.db::pg-pool-active-count pool))
    (cauldron.runtime:with-lock ((cauldron.db::pg-pool-lock pool))
      (incf (cauldron.db::pg-pool-active-count pool)))
    (is-equal 1 (cauldron.db::pg-pool-active-count pool))
    (cauldron.runtime:with-lock ((cauldron.db::pg-pool-lock pool))
      (incf (cauldron.db::pg-pool-active-count pool)))
    (is-equal 2 (cauldron.db::pg-pool-active-count pool))
    (cauldron.runtime:with-lock ((cauldron.db::pg-pool-lock pool))
      (decf (cauldron.db::pg-pool-active-count pool)))
    (is-equal 1 (cauldron.db::pg-pool-active-count pool))))

(deftest test-pool-checkout-checkin-counters
  "Checkout and checkin counters track independently."
  (let ((pool (cauldron.db::make-pg-pool)))
    (cauldron.runtime:with-lock ((cauldron.db::pg-pool-lock pool))
      (incf (cauldron.db::pg-pool-total-checkouts pool))
      (incf (cauldron.db::pg-pool-total-checkouts pool))
      (incf (cauldron.db::pg-pool-total-checkins pool)))
    (is-equal 2 (cauldron.db::pg-pool-total-checkouts pool))
    (is-equal 1 (cauldron.db::pg-pool-total-checkins pool))))

;;; --- Pool size limits ---

(deftest test-make-pool-min-equals-max
  "min-size == max-size is valid."
  ;; make-pool will try to connect, which fails without a DB.
  ;; But we can test the validation logic: min <= max should not error.
  ;; The error happens in create-pool-connection, not in validation.
  ;; Instead, test the struct directly.
  (let ((pool (cauldron.db::make-pg-pool :min-size 5 :max-size 5)))
    (is-equal 5 (cauldron.db::pg-pool-min-size pool))
    (is-equal 5 (cauldron.db::pg-pool-max-size pool))))

(deftest test-make-pool-rejects-max-less-than-min
  "make-pool errors when max-size < min-size."
  (signals-condition error
    (cauldron.db:make-pool :database "testdb" :user "test"
                           :min-size 10 :max-size 5)))

(deftest test-make-pool-rejects-missing-database
  (signals-condition error
    (cauldron.db:make-pool :user "test")))

(deftest test-make-pool-rejects-missing-user
  (signals-condition error
    (cauldron.db:make-pool :database "testdb")))

;;; --- Shutdown behavior ---

(deftest test-shutdown-sets-flag-and-clears-idle
  "Shutdown marks pool as shut down and clears idle connections."
  (let ((pool (cauldron.db::make-pg-pool)))
    ;; Add fake idle connections
    (cauldron.runtime:with-lock ((cauldron.db::pg-pool-lock pool))
      (push (cauldron.db::make-pg-connection) (cauldron.db::pg-pool-idle-connections pool))
      (push (cauldron.db::make-pg-connection) (cauldron.db::pg-pool-idle-connections pool)))
    (is-equal 2 (length (cauldron.db::pg-pool-idle-connections pool)))
    (cauldron.db:shutdown-pool pool)
    (is-true (cauldron.db::pg-pool-shutdown-p pool))
    (is-nil (cauldron.db::pg-pool-idle-connections pool))))

;;; --- pool-stats reflects mutations ---

(deftest test-pool-stats-error-and-timeout-tracking
  "Error and timeout counters are reflected in pool-stats."
  (let ((pool (cauldron.db::make-pg-pool)))
    (cauldron.runtime:with-lock ((cauldron.db::pg-pool-lock pool))
      (incf (cauldron.db::pg-pool-total-errors pool) 3)
      (incf (cauldron.db::pg-pool-total-timeouts pool) 1))
    (let ((stats (cauldron.db:pool-stats pool)))
      (is-equal 3 (gethash "total-errors" stats))
      (is-equal 1 (gethash "total-timeouts" stats)))))
