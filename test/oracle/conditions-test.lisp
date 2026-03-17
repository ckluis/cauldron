;;;; test/oracle/conditions-test.lisp — Condition hierarchy tests
(in-package :cauldron.test)

(defsuite :oracle-conditions)

;;; --- Each condition type signals and is catchable ---

(deftest test-cauldron-error-signals
  (signals-condition cauldron.oracle:cauldron-error
    (error 'cauldron.oracle:cauldron-error :message "test error")))

(deftest test-validation-error-signals
  (signals-condition cauldron.oracle:validation-error
    (error 'cauldron.oracle:validation-error
           :message "invalid"
           :field "email"
           :value "bad")))

(deftest test-authorization-error-signals
  (signals-condition cauldron.oracle:authorization-error
    (error 'cauldron.oracle:authorization-error
           :action :delete
           :resource "User")))

(deftest test-not-found-error-signals
  (signals-condition cauldron.oracle:not-found-error
    (error 'cauldron.oracle:not-found-error
           :resource "User"
           :id 42)))

(deftest test-timeout-error-signals
  (signals-condition cauldron.oracle:timeout-error
    (error 'cauldron.oracle:timeout-error
           :duration 30
           :operation "connect")))

(deftest test-conflict-error-signals
  (signals-condition cauldron.oracle:conflict-error
    (error 'cauldron.oracle:conflict-error
           :resource "User"
           :message "already exists")))

(deftest test-database-error-signals
  (signals-condition cauldron.oracle:database-error
    (error 'cauldron.oracle:database-error
           :code "23505"
           :detail "Key already exists")))

;;; --- Condition slot readers ---

(deftest test-validation-error-slots
  (handler-case
      (error 'cauldron.oracle:validation-error
             :message "too short"
             :field "name"
             :value "a")
    (cauldron.oracle:validation-error (c)
      (is-equal "too short" (cauldron.oracle:cauldron-error-message c))
      (is-equal "name" (cauldron.oracle:validation-error-field c))
      (is-equal "a" (cauldron.oracle:validation-error-value c)))))

(deftest test-not-found-error-slots
  (handler-case
      (error 'cauldron.oracle:not-found-error
             :resource "Post"
             :id 99)
    (cauldron.oracle:not-found-error (c)
      (is-equal "Post" (cauldron.oracle:not-found-error-resource c))
      (is-equal 99 (cauldron.oracle:not-found-error-id c)))))

(deftest test-database-error-slots
  (handler-case
      (error 'cauldron.oracle:database-error
             :code "42P01"
             :detail "relation does not exist"
             :message "table missing")
    (cauldron.oracle:database-error (c)
      (is-equal "42P01" (cauldron.oracle:database-error-code c))
      (is-equal "relation does not exist" (cauldron.oracle:database-error-detail c))
      (is-equal "table missing" (cauldron.oracle:cauldron-error-message c)))))

;;; --- Inheritance ---

(deftest test-validation-error-is-cauldron-error
  (signals-condition cauldron.oracle:cauldron-error
    (error 'cauldron.oracle:validation-error :message "test")))

(deftest test-not-found-error-is-cauldron-error
  (signals-condition cauldron.oracle:cauldron-error
    (error 'cauldron.oracle:not-found-error :message "test")))

;;; --- Report strings ---

(deftest test-cauldron-error-report
  (handler-case
      (error 'cauldron.oracle:cauldron-error :message "something broke")
    (cauldron.oracle:cauldron-error (c)
      (let ((report (format nil "~A" c)))
        (is (search "something broke" report))))))

(deftest test-authorization-error-report
  (handler-case
      (error 'cauldron.oracle:authorization-error
             :action :delete
             :resource "User")
    (cauldron.oracle:authorization-error (c)
      (let ((report (format nil "~A" c)))
        (is (search "DELETE" report))
        (is (search "User" report))))))
