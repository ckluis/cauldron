;;;; test/cli/cli-test.lisp — Tests for CLI infrastructure
(in-package :cauldron.test)

(defsuite :cli)

;; Helper: muffle redefinition warnings when re-registering commands in tests
(defmacro with-muffled-redefinition (&body body)
  `(handler-bind ((sb-kernel:redefinition-warning #'muffle-warning))
     ,@body))

;;; ============================================================
;;; Argument parsing
;;; ============================================================

(deftest test-parse-args-empty
  (let ((result (cauldron.cli:parse-args '())))
    (is-equal "" (getf result :command))
    (is-nil (getf result :flags))
    (is-nil (getf result :options))))

(deftest test-parse-args-command-only
  (let ((result (cauldron.cli:parse-args '("company" "create"))))
    (is-equal "company create" (getf result :command))))

(deftest test-parse-args-with-options
  (let ((result (cauldron.cli:parse-args '("company" "create" "--name" "Acme" "--type" "crm"))))
    (is-equal "company create" (getf result :command))
    (is-equal "Acme" (cauldron.cli:get-arg result "--name"))
    (is-equal "crm" (cauldron.cli:get-arg result "--type"))))

(deftest test-parse-args-equals-syntax
  (let ((result (cauldron.cli:parse-args '("user" "create" "--email=admin@x.com"))))
    (is-equal "user create" (getf result :command))
    (is-equal "admin@x.com" (cauldron.cli:get-arg result "--email"))))

(deftest test-parse-args-flags
  (let ((result (cauldron.cli:parse-args '("list" "--verbose" "--json"))))
    (is-equal "list" (getf result :command))
    (is (cauldron.cli:get-flag result "--verbose"))
    (is (cauldron.cli:get-flag result "--json"))))

(deftest test-parse-args-mixed
  (let ((result (cauldron.cli:parse-args '("record" "list" "--company" "acme" "--limit" "10" "--verbose"))))
    (is-equal "record list" (getf result :command))
    (is-equal "acme" (cauldron.cli:get-arg result "--company"))
    (is-equal "10" (cauldron.cli:get-arg result "--limit"))
    (is (cauldron.cli:get-flag result "--verbose"))))

(deftest test-get-arg-default
  (let ((result (cauldron.cli:parse-args '("test"))))
    (is-nil (cauldron.cli:get-arg result "--missing"))
    (is-equal "default" (cauldron.cli:get-arg result "--missing" "default"))))

(deftest test-get-positional
  (let ((result (cauldron.cli:parse-args '("cmd" "--name" "val" "extra1" "extra2"))))
    ;; After --name val, remaining non-option args are positional
    ;; But "extra1" and "extra2" follow an option, so they're positional
    (is-equal "extra1" (cauldron.cli:get-positional result 0))
    (is-equal "extra2" (cauldron.cli:get-positional result 1))))

;;; ============================================================
;;; Command registration
;;; ============================================================

(deftest test-defcommand-and-find
  ;; Save and restore command registry
  (let ((saved (make-hash-table :test 'equal)))
    (maphash (lambda (k v) (setf (gethash k saved) v)) cauldron.cli:*commands*)
    (unwind-protect
        (with-muffled-redefinition
          (cauldron.cli:defcommand "test-cmd"
            (:description "A test command")
            (declare (ignore args))
            42)
          (let ((cmd (cauldron.cli:find-command "test-cmd")))
            (is-not-nil cmd)
            (is-equal "A test command"
                      (cauldron.cli::cli-command-description cmd))))
      ;; Restore
      (setf cauldron.cli:*commands* saved))))

;;; ============================================================
;;; Command dispatch
;;; ============================================================

(deftest test-dispatch-unknown-command
  (let ((saved (make-hash-table :test 'equal)))
    (maphash (lambda (k v) (setf (gethash k saved) v)) cauldron.cli:*commands*)
    (unwind-protect
        (let ((*error-output* (make-string-output-stream)))
          (is-equal 1 (cauldron.cli:dispatch-command '("nonexistent"))))
      (setf cauldron.cli:*commands* saved))))

(deftest test-dispatch-known-command
  (let ((saved (make-hash-table :test 'equal)))
    (maphash (lambda (k v) (setf (gethash k saved) v)) cauldron.cli:*commands*)
    (unwind-protect
        (with-muffled-redefinition
          (cauldron.cli:defcommand "greet"
            (:description "Say hello")
            (declare (ignore args))
            0)
          (let ((*standard-output* (make-string-output-stream)))
            (is-equal 0 (cauldron.cli:dispatch-command '("greet")))))
      (setf cauldron.cli:*commands* saved))))

(deftest test-dispatch-help
  (let ((saved (make-hash-table :test 'equal)))
    (maphash (lambda (k v) (setf (gethash k saved) v)) cauldron.cli:*commands*)
    (unwind-protect
        (let ((*standard-output* (make-string-output-stream)))
          (is-equal 0 (cauldron.cli:dispatch-command '("--help"))))
      (setf cauldron.cli:*commands* saved))))

;;; ============================================================
;;; Output formatting
;;; ============================================================

(deftest test-emit-json-hash-table
  (let ((ht (make-hash-table :test 'equal))
        (out (make-string-output-stream)))
    (setf (gethash "name" ht) "test")
    (cauldron.cli:emit-json ht :stream out)
    (let ((json (get-output-stream-string out)))
      (is (search "name" json))
      (is (search "test" json)))))

(deftest test-emit-table-basic
  (let* ((row1 (make-hash-table :test 'equal))
         (row2 (make-hash-table :test 'equal))
         (out (make-string-output-stream)))
    (setf (gethash "name" row1) "Alice")
    (setf (gethash "age" row1) "30")
    (setf (gethash "name" row2) "Bob")
    (setf (gethash "age" row2) "25")
    (cauldron.cli:emit-table (list row1 row2) :stream out)
    (let ((output (get-output-stream-string out)))
      (is (search "Alice" output))
      (is (search "Bob" output))
      (is (search "NAME" output)))))

(deftest test-emit-table-empty
  (let ((out (make-string-output-stream)))
    (cauldron.cli:emit-table '() :stream out)
    (let ((output (get-output-stream-string out)))
      (is (search "no results" output)))))

(deftest test-emit-json-envelope
  (let ((out (make-string-output-stream))
        (cauldron.cli:*output-format* :json))
    (cauldron.cli:emit :message "Created" :status "ok" :stream out)
    (let ((output (get-output-stream-string out)))
      (is (search "ok" output))
      (is (search "Created" output)))))

;;; ============================================================
;;; Fuzzy matching (suggest-command)
;;; ============================================================

(deftest test-suggest-command-close-match
  "suggest-command finds a close match within distance 3."
  (let ((saved (make-hash-table :test 'equal)))
    (maphash (lambda (k v) (setf (gethash k saved) v)) cauldron.cli:*commands*)
    (unwind-protect
        (with-muffled-redefinition
          (cauldron.cli:defcommand "company create"
            (:description "Create a new company")
            (declare (ignore args))
            0)
          (cauldron.cli:defcommand "company list"
            (:description "List companies")
            (declare (ignore args))
            0)
          ;; "compnay create" is distance 2 from "company create"
          (is-equal "company create" (cauldron.cli:suggest-command "compnay create")))
      (setf cauldron.cli:*commands* saved))))

(deftest test-suggest-command-no-match
  "suggest-command returns nil when no command is close enough."
  (let ((saved (make-hash-table :test 'equal)))
    (maphash (lambda (k v) (setf (gethash k saved) v)) cauldron.cli:*commands*)
    (unwind-protect
        (with-muffled-redefinition
          (cauldron.cli:defcommand "company create"
            (:description "Create a new company")
            (declare (ignore args))
            0)
          ;; "zzzzzzz" is too far from anything
          (is-nil (cauldron.cli:suggest-command "zzzzzzzzzzz")))
      (setf cauldron.cli:*commands* saved))))

(deftest test-suggest-command-in-dispatch
  "Unknown command dispatch includes 'Did you mean' when close match exists."
  (let ((saved (make-hash-table :test 'equal)))
    (maphash (lambda (k v) (setf (gethash k saved) v)) cauldron.cli:*commands*)
    (unwind-protect
        (with-muffled-redefinition
          (cauldron.cli:defcommand "company create"
            (:description "Create a company")
            (declare (ignore args))
            0)
          (let ((*error-output* (make-string-output-stream)))
            (cauldron.cli:dispatch-command '("compnay" "create"))
            (let ((err (get-output-stream-string *error-output*)))
              (is-not-nil (search "Did you mean" err)))))
      (setf cauldron.cli:*commands* saved))))

;;; ============================================================
;;; Progressive help grouping
;;; ============================================================

(deftest test-progressive-help-groups-by-category
  "print-help groups commands by their first word."
  (let ((saved (make-hash-table :test 'equal)))
    (maphash (lambda (k v) (setf (gethash k saved) v)) cauldron.cli:*commands*)
    (unwind-protect
        (with-muffled-redefinition
          ;; Clear commands and register test ones
          (clrhash cauldron.cli:*commands*)
          (cauldron.cli:defcommand "company create"
            (:description "Create a new company")
            (declare (ignore args)) 0)
          (cauldron.cli:defcommand "company list"
            (:description "List all companies")
            (declare (ignore args)) 0)
          (cauldron.cli:defcommand "user create"
            (:description "Create a user")
            (declare (ignore args)) 0)
          (let ((out (make-string-output-stream)))
            (cauldron.cli::print-help :stream out)
            (let ((output (get-output-stream-string out)))
              ;; Check category headers
              (is-not-nil (search "Company:" output))
              (is-not-nil (search "User:" output))
              ;; Check commands are listed under their categories
              (is-not-nil (search "company create" output))
              (is-not-nil (search "user create" output)))))
      (setf cauldron.cli:*commands* saved))))

;;; ============================================================
;;; Required option validation
;;; ============================================================

(deftest test-required-option-missing-returns-error
  "Dispatching a command with missing required options returns exit code 1."
  (let ((saved (make-hash-table :test 'equal)))
    (maphash (lambda (k v) (setf (gethash k saved) v)) cauldron.cli:*commands*)
    (unwind-protect
        (with-muffled-redefinition
          (cauldron.cli:defcommand "widget create"
            (:description "Create a widget"
             :options (("--name" :required t :description "Widget name")
                       ("--color" :default "blue" :description "Widget color")))
            (declare (ignore args))
            0)
          ;; Call without --name (required)
          (let ((*error-output* (make-string-output-stream)))
            (let ((code (cauldron.cli:dispatch-command '("widget" "create" "--color" "red"))))
              (is-equal 1 code)
              ;; Error output should mention the missing option
              (let ((err (get-output-stream-string *error-output*)))
                (is-not-nil (search "--name" err))))))
      (setf cauldron.cli:*commands* saved))))

(deftest test-required-option-present-succeeds
  "Dispatching a command with all required options present succeeds."
  (let ((saved (make-hash-table :test 'equal)))
    (maphash (lambda (k v) (setf (gethash k saved) v)) cauldron.cli:*commands*)
    (unwind-protect
        (with-muffled-redefinition
          (cauldron.cli:defcommand "widget create"
            (:description "Create a widget"
             :options (("--name" :required t :description "Widget name")))
            (declare (ignore args))
            0)
          (let ((*error-output* (make-string-output-stream))
                (*standard-output* (make-string-output-stream)))
            (is-equal 0 (cauldron.cli:dispatch-command '("widget" "create" "--name" "foo")))))
      (setf cauldron.cli:*commands* saved))))
