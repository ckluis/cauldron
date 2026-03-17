;;;; test/oracle/hooks-test.lisp — Action hook system tests
(in-package :cauldron.test)

(defsuite :oracle-hooks)

;;; --- Install/remove/clear ---

(deftest test-install-hook
  (cauldron.oracle:clear-action-hooks)
  (cauldron.oracle:install-action-hook
   :test-hook
   (lambda (ctx) (declare (ignore ctx)) :ran)
   :phase :before)
  (is-equal 1 (length cauldron.oracle:*action-hooks*))
  (cauldron.oracle:clear-action-hooks))

(deftest test-remove-hook
  (cauldron.oracle:clear-action-hooks)
  (cauldron.oracle:install-action-hook
   :hook-a (lambda (ctx) (declare (ignore ctx)) nil))
  (cauldron.oracle:install-action-hook
   :hook-b (lambda (ctx) (declare (ignore ctx)) nil))
  (is-equal 2 (length cauldron.oracle:*action-hooks*))
  (cauldron.oracle:remove-action-hook :hook-a)
  (is-equal 1 (length cauldron.oracle:*action-hooks*))
  (cauldron.oracle:clear-action-hooks))

(deftest test-clear-hooks
  (cauldron.oracle:clear-action-hooks)
  (cauldron.oracle:install-action-hook
   :h1 (lambda (ctx) (declare (ignore ctx)) nil))
  (cauldron.oracle:install-action-hook
   :h2 (lambda (ctx) (declare (ignore ctx)) nil))
  (cauldron.oracle:clear-action-hooks)
  (is-equal 0 (length cauldron.oracle:*action-hooks*)))

;;; --- Hook execution by phase ---

(deftest test-before-hook-execution
  (cauldron.oracle:clear-action-hooks)
  (let ((ran nil))
    (cauldron.oracle:install-action-hook
     :before-hook
     (lambda (ctx) (declare (ignore ctx)) (setf ran t))
     :phase :before)
    (cauldron.oracle:run-action-hooks :before :test-action nil)
    (is-true ran))
  (cauldron.oracle:clear-action-hooks))

(deftest test-after-success-hook-execution
  (cauldron.oracle:clear-action-hooks)
  (let ((ran nil))
    (cauldron.oracle:install-action-hook
     :after-hook
     (lambda (ctx) (declare (ignore ctx)) (setf ran t))
     :phase :after-success)
    (cauldron.oracle:run-action-hooks :after-success :test-action nil)
    (is-true ran))
  (cauldron.oracle:clear-action-hooks))

;;; --- Priority ordering ---

(deftest test-priority-ordering
  (cauldron.oracle:clear-action-hooks)
  (let ((order '()))
    (cauldron.oracle:install-action-hook
     :low-priority
     (lambda (ctx) (declare (ignore ctx)) (push :low order))
     :phase :before :priority 1)
    (cauldron.oracle:install-action-hook
     :high-priority
     (lambda (ctx) (declare (ignore ctx)) (push :high order))
     :phase :before :priority 10)
    (cauldron.oracle:run-action-hooks :before :test nil)
    ;; High priority runs first
    (is-equal :low (first order))   ; :low was pushed last (ran second)
    (is-equal :high (second order))) ; :high was pushed first (ran first)
  (cauldron.oracle:clear-action-hooks))

;;; --- Conditional hooks ---

(deftest test-conditional-hook
  (cauldron.oracle:clear-action-hooks)
  (let ((ran nil))
    (cauldron.oracle:install-action-hook
     :conditional-hook
     (lambda (ctx) (declare (ignore ctx)) (setf ran t))
     :phase :before
     :test (lambda (ctx) (declare (ignore ctx)) nil)) ; Always false
    (cauldron.oracle:run-action-hooks :before :test nil)
    (is-nil ran))
  (cauldron.oracle:clear-action-hooks))

(deftest test-conditional-hook-passes
  (cauldron.oracle:clear-action-hooks)
  (let ((ran nil))
    (cauldron.oracle:install-action-hook
     :conditional-hook
     (lambda (ctx) (declare (ignore ctx)) (setf ran t))
     :phase :before
     :test (lambda (ctx) (declare (ignore ctx)) t)) ; Always true
    (cauldron.oracle:run-action-hooks :before :test nil)
    (is-true ran))
  (cauldron.oracle:clear-action-hooks))

;;; --- Action filtering ---

(deftest test-action-filtering
  (cauldron.oracle:clear-action-hooks)
  (let ((ran nil))
    (cauldron.oracle:install-action-hook
     :specific-action
     (lambda (ctx) (declare (ignore ctx)) (setf ran t))
     :phase :before
     :action :create)
    ;; Should not run for :delete
    (cauldron.oracle:run-action-hooks :before :delete nil)
    (is-nil ran)
    ;; Should run for :create
    (cauldron.oracle:run-action-hooks :before :create nil)
    (is-true ran))
  (cauldron.oracle:clear-action-hooks))
