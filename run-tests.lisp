#!/usr/bin/env sbcl --script
;;;; run-tests.lisp — CLI test orchestrator
;;;; Parses command-line arguments to select which tests to run.
;;;;
;;;; Usage:
;;;;   sbcl --script run-tests.lisp              — unit tests + integration if deps available
;;;;   sbcl --script run-tests.lisp --unit        — unit tests only
;;;;   sbcl --script run-tests.lisp --integration — integration tests only (fail if deps missing)
;;;;   sbcl --script run-tests.lisp --suite NAME  — run specific suite(s) by name
;;;;   sbcl --script run-tests.lisp --tag TAG     — run all suites with tag
;;;;   sbcl --script run-tests.lisp --all         — force all (fail hard if deps missing)
;;;;   sbcl --script run-tests.lisp --quiet       — suppress per-test output

(require :asdf)

;; Add project to ASDF search path
(push (truename ".") asdf:*central-registry*)

;;; --- Argument parsing ---

(defun parse-args (argv)
  "Parse command-line arguments into a plist of options.
Returns (:mode MODE :suites LIST :tags LIST :quiet BOOL)
MODE is one of :default, :unit, :integration, :all, :suite, :tag."
  (let ((mode :default)
        (suites '())
        (tags '())
        (quiet nil)
        (i 0))
    (loop while (< i (length argv))
          for arg = (nth i argv)
          do (cond
               ((string= arg "--unit")
                (setf mode :unit))
               ((string= arg "--integration")
                (setf mode :integration))
               ((string= arg "--all")
                (setf mode :all))
               ((string= arg "--quiet")
                (setf quiet t))
               ((string= arg "--suite")
                (setf mode :suite)
                (incf i)
                (when (< i (length argv))
                  (push (intern (string-upcase (nth i argv)) :keyword) suites)))
               ((string= arg "--tag")
                (setf mode :tag)
                (incf i)
                (when (< i (length argv))
                  (push (intern (string-upcase (nth i argv)) :keyword) tags))))
             (incf i))
    (list :mode mode
          :suites (nreverse suites)
          :tags (nreverse tags)
          :quiet quiet)))

;;; --- Load system ---

;; Load the test system (cauldron/core + tests, no server yet)
(handler-case
    (asdf:load-system "cauldron/test")
  (error (c)
    (format *error-output* "~%LOAD FAILED: ~A~%" c)
    (sb-ext:exit :code 2)))

;;; --- Pre-flight helpers ---

(defun integration-deps-available-p ()
  "Check if integration test dependencies are available.
Returns (values ok-p messages) where messages is a list of status strings."
  (let ((messages '())
        (ok t))
    ;; Check HTTP port is available
    (multiple-value-bind (port-ok msg)
        (cauldron.test:check-port-available 4200)
      (push msg messages)
      (unless port-ok (setf ok nil)))
    ;; Check PostgreSQL is reachable
    (multiple-value-bind (pg-ok pg-msg)
        (cauldron.test:check-postgres :host "127.0.0.1" :port 5432)
      (push pg-msg messages)
      (unless pg-ok (setf ok nil)))
    (values ok (nreverse messages))))

(defun load-full-system ()
  "Load the full cauldron system (includes HTTP server)."
  (handler-case
      (progn
        (asdf:load-system "cauldron")
        t)
    (error (c)
      (format *error-output* "~%Failed to load full cauldron system: ~A~%" c)
      nil)))

;;; --- Main ---

(let* ((argv (rest sb-ext:*posix-argv*))  ; skip script name
       (opts (parse-args argv))
       (mode (getf opts :mode))
       (explicit-suites (getf opts :suites))
       (explicit-tags (getf opts :tags))
       (quiet (getf opts :quiet)))

  (when quiet
    (setf cauldron.test:*verbose* nil))

  (let ((total 0) (passed 0) (failed 0))
    (flet ((run-and-accumulate (suite-names)
             (multiple-value-bind (t-total t-passed t-failed)
                 (cauldron.test:run-suites suite-names :verbose (not quiet))
               (incf total t-total)
               (incf passed t-passed)
               (incf failed t-failed))))

      (case mode
        ;; Unit tests only
        (:unit
         (run-and-accumulate (cauldron.test:suites-with-tag :unit)))

        ;; Integration tests only — fail if deps missing
        (:integration
         (unless (load-full-system)
           (format *error-output* "~%Cannot run integration tests: full system failed to load~%")
           (sb-ext:exit :code 2))
         (multiple-value-bind (deps-ok messages)
             (integration-deps-available-p)
           (dolist (msg messages) (format t "  [preflight] ~A~%" msg))
           (unless deps-ok
             (format *error-output* "~%Integration dependencies not available~%")
             (sb-ext:exit :code 2)))
         (run-and-accumulate (cauldron.test:suites-with-tag :integration)))

        ;; Run specific suites by name
        (:suite
         ;; Load full system in case integration suites are requested
         (load-full-system)
         (run-and-accumulate explicit-suites))

        ;; Run all suites with specified tags
        (:tag
         (load-full-system)
         (run-and-accumulate (apply #'cauldron.test:suites-with-any-tag explicit-tags)))

        ;; Force all — fail hard if deps missing
        (:all
         (unless (load-full-system)
           (format *error-output* "~%Cannot run all tests: full system failed to load~%")
           (sb-ext:exit :code 2))
         (multiple-value-bind (deps-ok messages)
             (integration-deps-available-p)
           (dolist (msg messages) (format t "  [preflight] ~A~%" msg))
           (unless deps-ok
             (format *error-output* "~%Dependencies not available for --all mode~%")
             (sb-ext:exit :code 2)))
         (let ((all-suites (sort (loop for k being the hash-keys of cauldron.test::*suites*
                                       collect k)
                                 #'string< :key #'symbol-name)))
           (run-and-accumulate all-suites)))

        ;; Default: unit always, integration if deps available
        (:default
         ;; Run unit tests first
         (run-and-accumulate (cauldron.test:suites-with-tag :unit))
         ;; Attempt integration tests
         (when (load-full-system)
           (multiple-value-bind (deps-ok messages)
               (integration-deps-available-p)
             (dolist (msg messages) (format t "  [preflight] ~A~%" msg))
             (if deps-ok
                 (progn
                   (format t "~%--- Running integration tests ---~%")
                   (run-and-accumulate (cauldron.test:suites-with-tag :integration)))
                 (format t "~%--- Skipping integration tests (deps unavailable) ---~%")))))))

    ;; Final summary (already printed by run-suites, just check exit code)
    (when (plusp failed)
      (sb-ext:exit :code 1))
    (sb-ext:exit :code 0)))
