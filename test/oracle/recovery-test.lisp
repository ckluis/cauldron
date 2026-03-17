;;;; test/oracle/recovery-test.lisp — Recovery macro + restart helper tests
(in-package :cauldron.test)

(defsuite :oracle-recovery)

;;; --- with-recovery + :skip ---

(deftest test-recovery-skip
  "Skip handler returns NIL on condition."
  (let ((result (cauldron.oracle:with-recovery
                    ((error :skip t))
                  (error "boom"))))
    (is-nil result)))

(deftest test-recovery-skip-no-condition
  "Without condition, body result is returned."
  (let ((result (cauldron.oracle:with-recovery
                    ((error :skip t))
                  42)))
    (is-equal 42 result)))

;;; --- with-recovery + :use-value ---

(deftest test-recovery-use-value
  (let ((result (cauldron.oracle:with-recovery
                    ((error :use-value :fallback))
                  (error "boom"))))
    (is-equal :fallback result)))

(deftest test-recovery-use-value-no-error
  (let ((result (cauldron.oracle:with-recovery
                    ((error :use-value :fallback))
                  99)))
    (is-equal 99 result)))

;;; --- with-recovery + :retry ---

(deftest test-recovery-retry-succeeds
  "Retry handler retries and eventually succeeds."
  (let ((attempts 0))
    (let ((result (cauldron.oracle:with-recovery
                      ((error :retry 3))
                    (incf attempts)
                    (when (< attempts 3)
                      (error "not yet"))
                    :done)))
      (is-equal :done result)
      (is-equal 3 attempts))))

(deftest test-recovery-retry-exhausted
  "After max retries, condition propagates."
  (let ((attempts 0))
    (signals-condition error
      (cauldron.oracle:with-recovery
          ((error :retry 2))
        (incf attempts)
        (error "always fails")))))

;;; --- with-recovery no matching handler ---

(deftest test-recovery-unmatched-propagates
  "A condition type not in handler-specs propagates normally."
  (signals-condition simple-error
    (cauldron.oracle:with-recovery
        ((type-error :skip t))
      (error "unhandled error type"))))

;;; --- with-recovery nested ---

(deftest test-recovery-nested
  "Inner recovery handles inner error, outer handles outer."
  (let ((result
          (cauldron.oracle:with-recovery
              ((error :use-value :outer))
            (cauldron.oracle:with-recovery
                ((error :use-value :inner))
              (error "inner error")))))
    (is-equal :inner result)))

;;; --- restart helpers (outside of with-recovery) ---

(deftest test-retry-action-no-restart
  "retry-action returns NIL when no restart is available."
  (is-nil (cauldron.oracle:retry-action)))

(deftest test-skip-action-no-restart
  (is-nil (cauldron.oracle:skip-action)))

(deftest test-use-value-no-restart
  (is-nil (cauldron.oracle:use-value :foo)))

(deftest test-abort-action-no-restart
  (is-nil (cauldron.oracle:abort-action)))

;;; --- restart helpers invoked from with-recovery ---

(deftest test-skip-action-restart
  "skip-action invokes the SKIP-ACTION restart inside with-recovery."
  (let ((result (cauldron.oracle:with-recovery
                    ((error :skip t))
                  (handler-bind ((error (lambda (c)
                                          (declare (ignore c))
                                          (cauldron.oracle:skip-action))))
                    (error "trigger")))))
    (is-nil result)))

(deftest test-use-value-restart
  "use-value restart returns specified value."
  (let ((result (cauldron.oracle:with-recovery
                    ((error :skip t))
                  (handler-bind ((error (lambda (c)
                                          (declare (ignore c))
                                          (cauldron.oracle:use-value :recovered))))
                    (error "trigger")))))
    (is-equal :recovered result)))
