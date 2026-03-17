;;;; src/grimoire/migration.lisp — Schema migration system
;;;; Timestamped migrations with up/down, tracking via schema_migrations table.

(in-package :cauldron.grimoire)

;;; --- Migration registry ---

(defstruct migration-def
  "A registered migration."
  (name nil :type symbol)
  (version nil :type string)       ; Timestamp string "20240101120000"
  (up-fn nil :type (or null function))
  (down-fn nil :type (or null function)))

(defvar *migrations* '()
  "List of registered migrations, sorted by version.")

(defmacro defmigration (name (&key version) &body body)
  "Define a migration with :up and :down forms.

Example:
  (defmigration create-users
    (:version \"20240101120000\")
    (:up
      \"CREATE TABLE users (
         id BIGSERIAL PRIMARY KEY,
         email VARCHAR(255) NOT NULL UNIQUE,
         name VARCHAR(255) NOT NULL,
         created_at TIMESTAMPTZ DEFAULT NOW())\")
    (:down
      \"DROP TABLE users\"))"
  (let ((up-form (find :up body :key (lambda (x) (when (consp x) (first x)))))
        (down-form (find :down body :key (lambda (x) (when (consp x) (first x))))))
    `(progn
       (let ((mig (make-migration-def
                   :name ',name
                   :version ,version
                   :up-fn (lambda (execute-fn)
                            ,@(when up-form
                                (mapcar (lambda (sql) `(funcall execute-fn ,sql))
                                        (rest up-form))))
                   :down-fn (lambda (execute-fn)
                              ,@(when down-form
                                  (mapcar (lambda (sql) `(funcall execute-fn ,sql))
                                          (rest down-form)))))))
         ;; Insert sorted by version
         (setf *migrations*
               (sort (cons mig (remove ',name *migrations* :key #'migration-def-name))
                     #'string< :key #'migration-def-version))
         mig))))

;;; --- Migration tracking ---

(defparameter *schema-migrations-ddl*
  "CREATE TABLE IF NOT EXISTS schema_migrations (
     version VARCHAR(14) PRIMARY KEY,
     migrated_at TIMESTAMPTZ DEFAULT NOW()
   )"
  "DDL for the migration tracking table.")

(defun ensure-migrations-table (execute-fn)
  "Create schema_migrations table if it doesn't exist."
  (funcall execute-fn *schema-migrations-ddl*))

(defun applied-versions (query-fn)
  "Return list of applied migration version strings."
  (mapcar (lambda (row) (gethash "version" row))
          (funcall query-fn "SELECT version FROM schema_migrations ORDER BY version")))

;;; --- Migration execution ---

(defun migrate (&key execute-fn query-fn (direction :up) to)
  "Run migrations.
EXECUTE-FN: (sql) → execute DDL/DML
QUERY-FN: (sql) → list of hash-table rows
DIRECTION: :up or :down
TO: optional target version (string)"
  (ensure-migrations-table execute-fn)
  (let ((applied (applied-versions query-fn)))
    (ecase direction
      (:up
       (let ((pending (remove-if (lambda (m)
                                   (member (migration-def-version m) applied
                                           :test #'string=))
                                 *migrations*)))
         (when to
           (setf pending (remove-if (lambda (m)
                                      (string> (migration-def-version m) to))
                                    pending)))
         (dolist (mig pending)
           (format t "~&Migrating UP: ~A (~A)~%" (migration-def-name mig) (migration-def-version mig))
           (handler-case
               (progn
                 (funcall (migration-def-up-fn mig) execute-fn)
                 (funcall execute-fn
                          (format nil "INSERT INTO schema_migrations (version) VALUES ('~A')"
                                  (migration-def-version mig))))
             (error (c)
               (error 'migration-error
                      :migration (migration-def-name mig)
                      :direction :up
                      :message (format nil "~A" c)))))
         (length pending)))

      (:down
       (let ((to-rollback (remove-if-not (lambda (m)
                                           (member (migration-def-version m) applied
                                                   :test #'string=))
                                         (reverse *migrations*))))
         (when to
           (setf to-rollback (remove-if (lambda (m)
                                          (string<= (migration-def-version m) to))
                                        to-rollback)))
         (dolist (mig to-rollback)
           (format t "~&Migrating DOWN: ~A (~A)~%" (migration-def-name mig) (migration-def-version mig))
           (handler-case
               (progn
                 (funcall (migration-def-down-fn mig) execute-fn)
                 (funcall execute-fn
                          (format nil "DELETE FROM schema_migrations WHERE version = '~A'"
                                  (migration-def-version mig))))
             (error (c)
               (error 'migration-error
                      :migration (migration-def-name mig)
                      :direction :down
                      :message (format nil "~A" c)))))
         (length to-rollback))))))

(defun rollback (&key execute-fn query-fn (step 1))
  "Roll back STEP migrations."
  (ensure-migrations-table execute-fn)
  (let* ((applied (applied-versions query-fn))
         (to-version (if (> (length applied) step)
                         (nth (- (length applied) step 1) applied)
                         nil)))
    (migrate :execute-fn execute-fn :query-fn query-fn
             :direction :down :to to-version)))

(defun migration-status (query-fn)
  "Return alist of (name version applied-p) for all migrations."
  (let ((applied (applied-versions query-fn)))
    (mapcar (lambda (m)
              (list (migration-def-name m)
                    (migration-def-version m)
                    (if (member (migration-def-version m) applied :test #'string=)
                        :applied
                        :pending)))
            *migrations*)))
