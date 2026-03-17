;;;; test/runtime/shutdown-test.lisp — Tests for graceful shutdown support
(in-package :cauldron.test)

(defsuite :shutdown)

;;; --- register-shutdown-hook ---

(deftest test-register-hook-adds-to-list
  (let ((cauldron.runtime::*shutdown-hooks* '()))
    (cauldron.runtime:register-shutdown-hook (lambda () :hook-a))
    (is-equal 1 (length cauldron.runtime::*shutdown-hooks*))))

(deftest test-register-hook-returns-fn
  (let ((cauldron.runtime::*shutdown-hooks* '())
        (fn (lambda () :test)))
    (is-equal fn (cauldron.runtime:register-shutdown-hook fn))))

(deftest test-multiple-hooks-registered-in-order
  (let ((cauldron.runtime::*shutdown-hooks* '())
        (order '()))
    (cauldron.runtime:register-shutdown-hook (lambda () (push :first order)))
    (cauldron.runtime:register-shutdown-hook (lambda () (push :second order)))
    (cauldron.runtime:register-shutdown-hook (lambda () (push :third order)))
    (is-equal 3 (length cauldron.runtime::*shutdown-hooks*))
    ;; Run them to verify order
    (dolist (hook cauldron.runtime::*shutdown-hooks*)
      (funcall hook))
    (is-equal '(:third :second :first) order)))

;;; --- graceful-shutdown ---

(deftest test-graceful-shutdown-calls-all-hooks
  (let ((cauldron.runtime::*shutdown-hooks* '())
        (cauldron.runtime::*shutting-down-p* nil)
        (called '()))
    (cauldron.runtime:register-shutdown-hook (lambda () (push :a called)))
    (cauldron.runtime:register-shutdown-hook (lambda () (push :b called)))
    (cauldron.runtime:register-shutdown-hook (lambda () (push :c called)))
    (cauldron.runtime:graceful-shutdown)
    (is-equal '(:c :b :a) called)))

(deftest test-graceful-shutdown-returns-results
  (let ((cauldron.runtime::*shutdown-hooks* '())
        (cauldron.runtime::*shutting-down-p* nil))
    (cauldron.runtime:register-shutdown-hook (lambda () :ok))
    (let ((results (cauldron.runtime:graceful-shutdown)))
      (is-equal 1 (length results))
      (is-equal :ok (first (first results))))))

(deftest test-hook-error-does-not-prevent-other-hooks
  (let ((cauldron.runtime::*shutdown-hooks* '())
        (cauldron.runtime::*shutting-down-p* nil)
        (called '()))
    (cauldron.runtime:register-shutdown-hook (lambda () (push :before called)))
    (cauldron.runtime:register-shutdown-hook (lambda () (error "boom")))
    (cauldron.runtime:register-shutdown-hook (lambda () (push :after called)))
    (let ((results (cauldron.runtime:graceful-shutdown)))
      (is-equal 3 (length results))
      ;; Both non-erroring hooks should have been called
      (is (member :before called) ":before hook called")
      (is (member :after called) ":after hook called"))))

(deftest test-hook-error-recorded-in-results
  (let ((cauldron.runtime::*shutdown-hooks* '())
        (cauldron.runtime::*shutting-down-p* nil))
    (cauldron.runtime:register-shutdown-hook (lambda () (error "test-error")))
    (let* ((results (cauldron.runtime:graceful-shutdown))
           (first-result (first results)))
      (is-equal :error (first first-result)))))

(deftest test-graceful-shutdown-empty-hooks
  (let ((cauldron.runtime::*shutdown-hooks* '())
        (cauldron.runtime::*shutting-down-p* nil))
    (let ((results (cauldron.runtime:graceful-shutdown)))
      (is-true (null results)))))

(deftest test-shutdown-hooks-var-is-list
  (let ((cauldron.runtime::*shutdown-hooks* '()))
    (is-true (listp cauldron.runtime::*shutdown-hooks*))))

(deftest test-double-shutdown-ignored
  (let ((cauldron.runtime::*shutdown-hooks* '())
        (cauldron.runtime::*shutting-down-p* t))
    ;; When already shutting down, should return nil immediately
    (let ((results (cauldron.runtime:graceful-shutdown)))
      (is-true (null results)))))
