;;;; test/http/timeout-test.lisp — Tests for timeout enforcement
(in-package :cauldron.test)

(defsuite :http-timeout)

(deftest test-timeout-condition-catchable
  ;; Verify sb-ext:timeout is a catchable condition
  (let ((caught nil))
    (handler-case
        (sb-ext:with-timeout 0.01
          (sleep 1))
      (sb-ext:timeout ()
        (setf caught t)))
    (is-true caught)))

(deftest test-timeout-no-timeout-when-fast
  ;; Verify that fast operations don't trigger timeout
  (let ((result nil))
    (handler-case
        (sb-ext:with-timeout 5
          (setf result (+ 1 2)))
      (sb-ext:timeout ()
        (setf result :timed-out)))
    (is-equal 3 result)))
