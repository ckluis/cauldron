;;;; test/db/pool-test.lisp — Pool struct + state machine tests (no real DB)
(in-package :cauldron.test)

(defsuite :db-pool)

;;; ============================================================
;;; pg-pool struct defaults
;;; ============================================================

(deftest test-pg-pool-defaults
  (let ((pool (cauldron.db::make-pg-pool)))
    (is-not-nil pool)
    (is-equal "127.0.0.1" (cauldron.db::pg-pool-host pool))
    (is-equal 5432 (cauldron.db::pg-pool-port pool))
    (is-equal "" (cauldron.db::pg-pool-database pool))
    (is-equal "" (cauldron.db::pg-pool-user pool))
    (is-equal "" (cauldron.db::pg-pool-password pool))
    (is-equal 2 (cauldron.db::pg-pool-min-size pool))
    (is-equal 10 (cauldron.db::pg-pool-max-size pool))
    (is-false (cauldron.db::pg-pool-shutdown-p pool))))

(deftest test-pg-pool-custom-config
  (let ((pool (cauldron.db::make-pg-pool
               :host "10.0.0.5" :port 5433
               :database "testdb" :user "tester" :password "secret"
               :min-size 5 :max-size 20
               :leak-threshold-seconds 60)))
    (is-equal "10.0.0.5" (cauldron.db::pg-pool-host pool))
    (is-equal 5433 (cauldron.db::pg-pool-port pool))
    (is-equal "testdb" (cauldron.db::pg-pool-database pool))
    (is-equal "tester" (cauldron.db::pg-pool-user pool))
    (is-equal "secret" (cauldron.db::pg-pool-password pool))
    (is-equal 5 (cauldron.db::pg-pool-min-size pool))
    (is-equal 20 (cauldron.db::pg-pool-max-size pool))
    (is-equal 60 (cauldron.db::pg-pool-leak-threshold-seconds pool))))

(deftest test-pg-pool-initial-stats-zeroed
  (let ((pool (cauldron.db::make-pg-pool)))
    (is-equal 0 (cauldron.db::pg-pool-total-created pool))
    (is-equal 0 (cauldron.db::pg-pool-active-count pool))
    (is-equal 0 (cauldron.db::pg-pool-total-checkouts pool))
    (is-equal 0 (cauldron.db::pg-pool-total-checkins pool))
    (is-equal 0 (cauldron.db::pg-pool-total-errors pool))
    (is-equal 0 (cauldron.db::pg-pool-total-timeouts pool))))

(deftest test-pg-pool-idle-and-active-empty
  (let ((pool (cauldron.db::make-pg-pool)))
    (is-nil (cauldron.db::pg-pool-idle-connections pool))
    (is-nil (cauldron.db::pg-pool-active-connections pool))))

(deftest test-pg-pool-has-lock-and-semaphore
  (let ((pool (cauldron.db::make-pg-pool)))
    (is-not-nil (cauldron.db::pg-pool-lock pool))
    (is-not-nil (cauldron.db::pg-pool-semaphore pool))))

;;; ============================================================
;;; validate-pool-connection
;;; ============================================================

(deftest test-validate-pool-connection-alive
  (let ((conn (cauldron.db::make-pg-connection :alive-p t)))
    (is-true (cauldron.db::validate-pool-connection conn))))

(deftest test-validate-pool-connection-dead
  (let ((conn (cauldron.db::make-pg-connection :alive-p nil)))
    (is-false (cauldron.db::validate-pool-connection conn))))

;;; ============================================================
;;; pool-stats
;;; ============================================================

(deftest test-pool-stats-fresh-pool
  (let* ((pool (cauldron.db::make-pg-pool))
         (stats (cauldron.db:pool-stats pool)))
    (is (hash-table-p stats))
    (is-equal 0 (gethash "total-created" stats))
    (is-equal 0 (gethash "idle" stats))
    (is-equal 0 (gethash "active" stats))
    (is-equal 0 (gethash "total-checkouts" stats))
    (is-equal 0 (gethash "total-checkins" stats))
    (is-equal 0 (gethash "total-errors" stats))
    (is-equal 0 (gethash "total-timeouts" stats))
    (is-equal 10 (gethash "max-size" stats))
    (is-equal 2 (gethash "min-size" stats))))

(deftest test-pool-stats-all-keys-present
  (let* ((pool (cauldron.db::make-pg-pool))
         (stats (cauldron.db:pool-stats pool))
         (expected-keys '("total-created" "idle" "active" "total-checkouts"
                          "total-checkins" "total-errors" "total-timeouts"
                          "max-size" "min-size")))
    (dolist (key expected-keys)
      (multiple-value-bind (val present) (gethash key stats)
        (declare (ignore val))
        (is present (format nil "Key ~S should be present in stats" key))))))

(deftest test-pool-stats-after-state-mutation
  (let ((pool (cauldron.db::make-pg-pool)))
    ;; Manually mutate state (simulating what checkout/checkin would do)
    (cauldron.runtime:with-lock ((cauldron.db::pg-pool-lock pool))
      (setf (cauldron.db::pg-pool-total-created pool) 5)
      (setf (cauldron.db::pg-pool-active-count pool) 2)
      (setf (cauldron.db::pg-pool-total-checkouts pool) 10)
      (setf (cauldron.db::pg-pool-total-checkins pool) 8)
      (push (cauldron.db::make-pg-connection) (cauldron.db::pg-pool-idle-connections pool))
      (push (cauldron.db::make-pg-connection) (cauldron.db::pg-pool-idle-connections pool))
      (push (cauldron.db::make-pg-connection) (cauldron.db::pg-pool-idle-connections pool)))
    (let ((stats (cauldron.db:pool-stats pool)))
      (is-equal 5 (gethash "total-created" stats))
      (is-equal 3 (gethash "idle" stats))
      (is-equal 2 (gethash "active" stats))
      (is-equal 10 (gethash "total-checkouts" stats))
      (is-equal 8 (gethash "total-checkins" stats)))))

;;; ============================================================
;;; shutdown-pool
;;; ============================================================

(deftest test-shutdown-pool-sets-flag
  (let ((pool (cauldron.db::make-pg-pool)))
    (is-false (cauldron.db::pg-pool-shutdown-p pool))
    (cauldron.db:shutdown-pool pool)
    (is-true (cauldron.db::pg-pool-shutdown-p pool))))

(deftest test-shutdown-pool-clears-idle
  (let ((pool (cauldron.db::make-pg-pool)))
    ;; Add fake idle connections (not real sockets, disconnect will be ignored)
    (cauldron.runtime:with-lock ((cauldron.db::pg-pool-lock pool))
      (push (cauldron.db::make-pg-connection) (cauldron.db::pg-pool-idle-connections pool))
      (push (cauldron.db::make-pg-connection) (cauldron.db::pg-pool-idle-connections pool)))
    (cauldron.db:shutdown-pool pool)
    (is-nil (cauldron.db::pg-pool-idle-connections pool))))

(deftest test-shutdown-pool-empty-pool-no-crash
  (let ((pool (cauldron.db::make-pg-pool)))
    ;; Should not error on empty pool
    (cauldron.db:shutdown-pool pool)
    (is-true (cauldron.db::pg-pool-shutdown-p pool))))

;;; ============================================================
;;; make-pool validation
;;; ============================================================

(deftest test-make-pool-requires-database
  (signals-condition error
    (cauldron.db:make-pool :user "test")))

(deftest test-make-pool-requires-user
  (signals-condition error
    (cauldron.db:make-pool :database "testdb")))

(deftest test-make-pool-max-less-than-min
  (signals-condition error
    (cauldron.db:make-pool :database "testdb" :user "test"
                           :min-size 10 :max-size 5)))
