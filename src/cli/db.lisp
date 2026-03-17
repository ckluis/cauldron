;;;; src/cli/db.lisp — CLI database pool management
;;;; Provides a dynamic pool variable and macro for creating DB connections
;;;; from environment variables or explicit keyword overrides.

(in-package :cauldron.cli)

(defvar *cli-pool* nil
  "Dynamic variable holding the CLI's database pool.")

(defmacro with-cli-pool ((&key (host nil) (port nil) (database nil)
                                (user nil) (password nil)) &body body)
  "Create a PG pool from env vars (CAULDRON_DB_*) or keyword overrides,
bind to *cli-pool*, execute BODY, shutdown pool on exit.

Environment variables:
  CAULDRON_DB_HOST     - Database host (default: 127.0.0.1)
  CAULDRON_DB_PORT     - Database port (default: 5432)
  CAULDRON_DB_DATABASE - Database name (required)
  CAULDRON_DB_USER     - Database user (required)
  CAULDRON_DB_PASSWORD - Database password (default: empty)"
  (let ((pool-var (gensym "POOL-")))
    `(let* ((,pool-var
              (cauldron.db:make-pool
               :host (or ,host
                         (sb-ext:posix-getenv "CAULDRON_DB_HOST")
                         "127.0.0.1")
               :port (or ,port
                         (let ((p (sb-ext:posix-getenv "CAULDRON_DB_PORT")))
                           (when p (parse-integer p)))
                         5432)
               :database (or ,database
                             (sb-ext:posix-getenv "CAULDRON_DB_DATABASE"))
               :user (or ,user
                         (sb-ext:posix-getenv "CAULDRON_DB_USER"))
               :password (or ,password
                             (sb-ext:posix-getenv "CAULDRON_DB_PASSWORD")
                             "")))
            (*cli-pool* ,pool-var))
       (unwind-protect
            (progn ,@body)
         (cauldron.db:shutdown-pool ,pool-var)))))
