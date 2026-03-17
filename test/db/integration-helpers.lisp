;;;; test/db/integration-helpers.lisp — Shared utilities for PostgreSQL integration tests

(in-package :cauldron.test)

;;; --- Test database configuration ---

(defvar *test-db-name* "cauldron_test"
  "Database name for integration tests.")

(defvar *test-db-user* "ckluis"
  "User for integration test connections.")

(defvar *test-db-host* "127.0.0.1"
  "Host for integration test connections.")

(defvar *test-db-port* 5432
  "Port for integration test connections.")

;;; --- Database bootstrapping ---

(defun ensure-test-database ()
  "Create the test database if it doesn't exist.
Connects to the 'postgres' database to issue CREATE DATABASE."
  (handler-case
      (cauldron.db:with-connection (admin-conn
                                   :host *test-db-host*
                                   :port *test-db-port*
                                   :database "postgres"
                                   :user *test-db-user*)
        (handler-case
            (cauldron.db:execute-sql admin-conn
              (format nil "CREATE DATABASE ~A" *test-db-name*))
          (cauldron.db:pg-error (e)
            ;; 42P04 = duplicate_database — ignore
            (unless (string= (cauldron.db:pg-error-code e) "42P04")
              (error e)))))
    (error (e)
      (format *error-output* "~&Warning: Could not ensure test database: ~A~%" e))))

;;; --- Connection helpers ---

(defun test-connect ()
  "Connect to the test database with default parameters."
  (cauldron.db:connect :host *test-db-host*
                       :port *test-db-port*
                       :database *test-db-name*
                       :user *test-db-user*))

(defmacro with-test-connection ((var) &body body)
  "Execute BODY with VAR bound to a test database connection.
Ensures the connection is disconnected on exit."
  `(let ((,var (test-connect)))
     (unwind-protect (progn ,@body)
       (when ,var
         (ignore-errors (cauldron.db:disconnect ,var))))))

;;; --- Pool helpers ---

(defun make-test-pool (&key (min-size 2) (max-size 5))
  "Create a connection pool for the test database."
  (cauldron.db:make-pool :host *test-db-host*
                         :port *test-db-port*
                         :database *test-db-name*
                         :user *test-db-user*
                         :min-size min-size
                         :max-size max-size))

;;; --- Table management ---

(defun drop-test-tables (conn &rest names)
  "Drop tables by NAME from the test database (CASCADE)."
  (dolist (name names)
    (cauldron.db:execute-sql conn
      (format nil "DROP TABLE IF EXISTS ~A CASCADE" name))))
