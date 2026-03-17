;;;; test/runner.lisp — Cauldron test framework
;;;; Minimal test runner: deftest, assertions, suites, pass/fail with exit codes.
;;;; ~200 lines, zero dependencies.

(defpackage :cauldron.test
  (:use :cl)
  (:export #:deftest
           #:defsuite
           #:run-suite
           #:run-all-tests
           #:is
           #:is-equal
           #:is-true
           #:is-false
           #:is-nil
           #:is-not-nil
           #:is-condition
           #:signals-condition
           #:test-count
           #:pass-count
           #:fail-count
           #:*verbose*
           ;; Phase 8: test orchestrator
           #:tag-suite
           #:suite-tags
           #:suites-with-tag
           #:suites-with-any-tag
           #:run-suites
           #:suite-setup
           #:suite-teardown
           #:check-postgres
           #:check-port-available))

(in-package :cauldron.test)

;;; --- Test registry ---

(defvar *suites* (make-hash-table :test 'eq)
  "Suite name → list of (test-name . test-fn).")

(defvar *current-suite* :default
  "Suite that deftest adds to unless overridden.")

(defvar *verbose* t
  "Print each test result when non-nil.")

(defvar *suite-setup* (make-hash-table :test 'eq)
  "Suite name → setup function (called before tests).")

(defvar *suite-teardown* (make-hash-table :test 'eq)
  "Suite name → teardown function (called after tests in unwind-protect).")

;;; --- Result tracking ---

(defstruct test-result
  (name nil :type symbol)
  (suite nil :type symbol)
  (passed nil :type boolean)
  (message "" :type string)
  (duration 0.0 :type single-float))

(defvar *results* '()
  "Accumulated results for current run.")

(defvar *current-test* nil
  "Name of currently executing test.")

(defvar *assertion-count* 0
  "Number of assertions in current test.")

(defvar *assertion-failures* '()
  "Failures in current test.")

;;; --- Assertion helpers ---

(defun assertion-pass ()
  (incf *assertion-count*))

(defun assertion-fail (message)
  (incf *assertion-count*)
  (push message *assertion-failures*))

(defmacro is (form &optional description)
  "Assert that FORM evaluates to a truthy value."
  (let ((desc (or description (format nil "~S" form))))
    `(if ,form
         (assertion-pass)
         (assertion-fail (format nil "FAIL: ~A~%  Expected truthy, got NIL" ,desc)))))

(defmacro is-equal (expected actual &optional description)
  "Assert that EXPECTED and ACTUAL are EQUAL."
  (let ((e (gensym "EXPECTED"))
        (a (gensym "ACTUAL"))
        (desc (gensym "DESC")))
    `(let ((,e ,expected)
           (,a ,actual)
           (,desc ,(or description `(format nil "(is-equal ~S ~S)" ',expected ',actual))))
       (if (equal ,e ,a)
           (assertion-pass)
           (assertion-fail (format nil "FAIL: ~A~%  Expected: ~S~%  Actual:   ~S" ,desc ,e ,a))))))

(defmacro is-equalp (expected actual &optional description)
  "Assert that EXPECTED and ACTUAL are EQUALP (case-insensitive, struct-equal)."
  (let ((e (gensym "EXPECTED"))
        (a (gensym "ACTUAL"))
        (desc (gensym "DESC")))
    `(let ((,e ,expected)
           (,a ,actual)
           (,desc ,(or description `(format nil "(is-equalp ~S ~S)" ',expected ',actual))))
       (if (equalp ,e ,a)
           (assertion-pass)
           (assertion-fail (format nil "FAIL: ~A~%  Expected: ~S~%  Actual:   ~S" ,desc ,e ,a))))))

(defmacro is-true (form &optional description)
  "Assert FORM is exactly T."
  `(is-equal t ,form ,(or description (format nil "~S is T" form))))

(defmacro is-false (form &optional description)
  "Assert FORM is exactly NIL."
  `(is-equal nil ,form ,(or description (format nil "~S is NIL" form))))

(defmacro is-nil (form &optional description)
  "Assert FORM is NIL."
  `(is-equal nil ,form ,description))

(defmacro is-not-nil (form &optional description)
  "Assert FORM is not NIL."
  `(is (not (null ,form)) ,(or description (format nil "~S is not NIL" form))))

(defmacro signals-condition (condition-type form &optional description)
  "Assert that evaluating FORM signals a condition of CONDITION-TYPE."
  (let ((desc (or description (format nil "~S signals ~S" form condition-type)))
        (block (gensym "SIGNALS-BLOCK")))
    `(block ,block
       (let ((signaled nil))
         (handler-case (progn ,form)
           (,condition-type () (setf signaled t))
           (condition (c)
             (assertion-fail (format nil "FAIL: ~A~%  Expected ~S but got ~S: ~A"
                                     ,desc ',condition-type (type-of c) c))
             (return-from ,block)))
         (if signaled
             (assertion-pass)
             (assertion-fail (format nil "FAIL: ~A~%  No condition signaled" ,desc)))))))

;;; --- Test definition ---

(defmacro defsuite (name)
  "Declare a test suite. Tests defined after this go into NAME."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *current-suite* ',name)
     (unless (gethash ',name *suites*)
       (setf (gethash ',name *suites*) '()))))

(defmacro deftest (name &body body)
  "Define a test. Adds to *current-suite*."
  `(progn
     (defun ,name ()
       (let ((*assertion-count* 0)
             (*assertion-failures* '()))
         ,@body
         (values *assertion-count* (reverse *assertion-failures*))))
     (let ((suite-tests (gethash *current-suite* *suites* '())))
       (unless (assoc ',name suite-tests)
         (setf (gethash *current-suite* *suites*)
               (append suite-tests (list (cons ',name #',name))))))))

;;; --- Test execution ---

(defun run-test (name fn)
  "Run a single test function, return a test-result."
  (let* ((*current-test* name)
         (start (get-internal-real-time))
         (passed t)
         (message ""))
    (handler-case
        (multiple-value-bind (count failures) (funcall fn)
          (when failures
            (setf passed nil
                  message (format nil "~{~A~^~%~}" failures)))
          (when (zerop count)
            (setf message "WARNING: no assertions")))
      (condition (c)
        (setf passed nil
              message (format nil "EXCEPTION: ~A" c))))
    (let* ((end (get-internal-real-time))
           (duration (/ (float (- end start))
                        (float internal-time-units-per-second))))
      (make-test-result :name name
                        :suite *current-suite*
                        :passed passed
                        :message message
                        :duration duration))))

(defun run-suite (suite-name &key (verbose *verbose*))
  "Run all tests in a suite. Returns (values total passed failed).
Calls suite setup before tests and teardown after (in unwind-protect)."
  (let ((tests (gethash suite-name *suites*)))
    (unless tests
      (format t "~%Suite ~A: no tests registered.~%" suite-name)
      (return-from run-suite (values 0 0 0)))
    (format t "~%=== Suite: ~A (~D tests) ===~%" suite-name (length tests))
    (let ((setup-fn (gethash suite-name *suite-setup*))
          (teardown-fn (gethash suite-name *suite-teardown*))
          (total 0) (passed 0) (failed 0))
      ;; Run setup if registered
      (when setup-fn
        (handler-case (funcall setup-fn)
          (error (c)
            (format t "  SETUP FAILED: ~A~%" c)
            (return-from run-suite (values 0 0 (length tests))))))
      (unwind-protect
          (dolist (entry tests)
            (let* ((name (car entry))
                   (fn (cdr entry))
                   (result (run-test name fn)))
              (incf total)
              (if (test-result-passed result)
                  (progn
                    (incf passed)
                    (when verbose
                      (format t "  PASS  ~A (~,3Fs)~%" name (test-result-duration result))))
                  (progn
                    (incf failed)
                    (format t "  FAIL  ~A (~,3Fs)~%" name (test-result-duration result))
                    (format t "        ~A~%" (test-result-message result))))))
        ;; Teardown always runs
        (when teardown-fn
          (handler-case (funcall teardown-fn)
            (error (c)
              (format t "  TEARDOWN ERROR: ~A~%" c)))))
      (format t "--- ~A: ~D total, ~D passed, ~D failed ---~%"
              suite-name total passed failed)
      (values total passed failed))))

;;; --- Suite tagging ---

(defvar *suite-tags* (make-hash-table :test 'eq)
  "Suite name → list of tags (keywords).")

(defun tag-suite (suite-name &rest tags)
  "Associate TAGS with SUITE-NAME. Tags are keywords like :unit, :integration, :http."
  (setf (gethash suite-name *suite-tags*) tags))

(defun suite-tags (suite-name)
  "Return the tags for SUITE-NAME, defaulting to (:unit) if none set."
  (or (gethash suite-name *suite-tags*) '(:unit)))

(defun suites-with-tag (tag)
  "Return all registered suite names that have TAG."
  (let ((result '()))
    (maphash (lambda (name _tests)
               (declare (ignore _tests))
               (when (member tag (suite-tags name))
                 (push name result)))
             *suites*)
    (sort result #'string< :key #'symbol-name)))

(defun suites-with-any-tag (&rest tags)
  "Return all registered suite names that have any of TAGS."
  (let ((result '()))
    (maphash (lambda (name _tests)
               (declare (ignore _tests))
               (when (intersection tags (suite-tags name))
                 (push name result)))
             *suites*)
    (sort result #'string< :key #'symbol-name)))

;;; --- Setup/teardown registration ---

(defun suite-setup (suite-name fn)
  "Register FN as the setup function for SUITE-NAME."
  (setf (gethash suite-name *suite-setup*) fn))

(defun suite-teardown (suite-name fn)
  "Register FN as the teardown function for SUITE-NAME."
  (setf (gethash suite-name *suite-teardown*) fn))

;;; --- Run multiple suites ---

(defun run-suites (suite-names &key (verbose *verbose*))
  "Run a list of suites by name. Same reporting as run-all-tests.
Returns (values total passed failed)."
  (format t "~%╔══════════════════════════════════╗~%")
  (format t "║     Cauldron Test Runner         ║~%")
  (format t "╚══════════════════════════════════╝~%")
  (let ((total 0) (passed 0) (failed 0))
    (dolist (suite suite-names)
      (multiple-value-bind (t-total t-passed t-failed)
          (run-suite suite :verbose verbose)
        (incf total t-total)
        (incf passed t-passed)
        (incf failed t-failed)))
    (format t "~%════════════════════════════════════~%")
    (format t "TOTAL: ~D tests, ~D passed, ~D failed~%" total passed failed)
    (format t "════════════════════════════════════~%")
    (values total passed failed)))

;;; --- Run all tests ---

(defun run-all-tests (&key (verbose *verbose*) (exit-on-failure t))
  "Run all registered suites. Return total/passed/failed. Optionally exit."
  (let ((suites (sort (loop for k being the hash-keys of *suites*
                            collect k)
                      #'string< :key #'symbol-name)))
    (multiple-value-bind (total passed failed)
        (run-suites suites :verbose verbose)
      (when (and exit-on-failure (plusp failed))
        (sb-ext:exit :code 1))
      (values total passed failed))))

;;; --- Pre-flight checks ---

(defun check-postgres (&key (host "127.0.0.1") (port 5432))
  "TCP-probe PostgreSQL at HOST:PORT.
Returns (values ok-p message)."
  (handler-case
      (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                    :type :stream :protocol :tcp)))
        (unwind-protect
            (progn
              (sb-bsd-sockets:socket-connect
               socket
               (sb-bsd-sockets:make-inet-address host)
               port)
              (values t (format nil "PostgreSQL reachable at ~A:~D" host port)))
          (ignore-errors (sb-bsd-sockets:socket-close socket))))
    (error (c)
      (values nil (format nil "PostgreSQL not reachable at ~A:~D — ~A" host port c)))))

(defun check-port-available (port)
  "Try to bind to PORT on 127.0.0.1 to check if it's available.
Returns (values ok-p message)."
  (handler-case
      (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                    :type :stream :protocol :tcp)))
        (unwind-protect
            (progn
              (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
              (sb-bsd-sockets:socket-bind
               socket
               (sb-bsd-sockets:make-inet-address "127.0.0.1")
               port)
              (values t (format nil "Port ~D is available" port)))
          (ignore-errors (sb-bsd-sockets:socket-close socket))))
    (error (c)
      (values nil (format nil "Port ~D is not available — ~A" port c)))))
