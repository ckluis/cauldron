;;;; test/runtime/config-test.lisp — Tests for environment config system
(in-package :cauldron.test)

(defsuite :runtime-config)

;; Helper: set/unset env vars via C library
(defun %setenv (name value)
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "setenv"
                          (function sb-alien:int sb-alien:c-string sb-alien:c-string sb-alien:int))
   name value 1))
(defun %unsetenv (name)
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "unsetenv"
                          (function sb-alien:int sb-alien:c-string))
   name))

;;; --- coerce-config-value ---

(deftest test-coerce-string
  (is-equal "hello" (cauldron.runtime:coerce-config-value "hello" :string)))

(deftest test-coerce-integer
  (is-equal 42 (cauldron.runtime:coerce-config-value "42" :integer))
  (is-equal -7 (cauldron.runtime:coerce-config-value "-7" :integer))
  (is-equal 0 (cauldron.runtime:coerce-config-value "0" :integer)))

(deftest test-coerce-boolean-true
  (is-true (cauldron.runtime:coerce-config-value "true" :boolean))
  (is-true (cauldron.runtime:coerce-config-value "1" :boolean))
  (is-true (cauldron.runtime:coerce-config-value "yes" :boolean))
  (is-true (cauldron.runtime:coerce-config-value "TRUE" :boolean))
  (is-true (cauldron.runtime:coerce-config-value "Yes" :boolean)))

(deftest test-coerce-boolean-false
  (is-false (cauldron.runtime:coerce-config-value "false" :boolean))
  (is-false (cauldron.runtime:coerce-config-value "0" :boolean))
  (is-false (cauldron.runtime:coerce-config-value "no" :boolean))
  (is-false (cauldron.runtime:coerce-config-value "" :boolean)))

(deftest test-coerce-float
  (let ((val (cauldron.runtime:coerce-config-value "3.14" :float)))
    (is-true (typep val 'double-float))
    (is-true (< (abs (- val 3.14d0)) 0.001d0))))

;;; --- get-env ---

(deftest test-get-env-default
  (is-equal "fallback" (cauldron.runtime:get-env "CAULDRON_TEST_NONEXISTENT_XYZ"
                                                  :default "fallback")))

(deftest test-get-env-required-missing
  (signals-condition error
    (cauldron.runtime:get-env "CAULDRON_TEST_NONEXISTENT_XYZ" :required t)))

(deftest test-get-env-type-default-values
  (is-equal 0 (cauldron.runtime:get-env "CAULDRON_TEST_NONEXISTENT_XYZ" :type :integer))
  (is-false (cauldron.runtime:get-env "CAULDRON_TEST_NONEXISTENT_XYZ" :type :boolean)))

;;; --- validate-config ---

(deftest test-validate-config-passes-when-no-required
  ;; Clear registry, add non-required entry, validate should pass
  (let ((cauldron.runtime::*config-entries* (make-hash-table :test 'eq)))
    (setf (gethash 'test-entry cauldron.runtime::*config-entries*)
          (list :env-var "CAULDRON_TEST_NONEXISTENT_XYZ" :type :string
                :default nil :required nil :description "test"))
    (is-true (cauldron.runtime:validate-config))))

(deftest test-validate-config-fails-for-missing-required
  (let ((cauldron.runtime::*config-entries* (make-hash-table :test 'eq)))
    (setf (gethash 'test-required cauldron.runtime::*config-entries*)
          (list :env-var "CAULDRON_TEST_NONEXISTENT_XYZ" :type :string
                :default nil :required t :description "test"))
    (signals-condition error (cauldron.runtime:validate-config))))

;;; --- config-summary masking ---

(deftest test-config-summary-masks-secrets
  (let ((cauldron.runtime::*config-entries* (make-hash-table :test 'eq)))
    ;; Set a real env var temporarily
    (%setenv "CAULDRON_TEST_SECRET_KEY" "supersecret")
    (unwind-protect
        (progn
          (setf (gethash 'test-secret cauldron.runtime::*config-entries*)
                (list :env-var "CAULDRON_TEST_SECRET_KEY" :type :string
                      :default nil :required nil :description "test"))
          (let ((summary (cauldron.runtime:config-summary)))
            (is-equal "********" (second (assoc 'test-secret summary)))))
      (%unsetenv "CAULDRON_TEST_SECRET_KEY"))))
