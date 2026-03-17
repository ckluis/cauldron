;;;; src/grimoire/conditions.lisp — Query and schema error conditions

(in-package :cauldron.grimoire)

(define-condition query-error (error)
  ((sql     :initarg :sql     :reader query-error-sql     :initform nil)
   (params  :initarg :params  :reader query-error-params  :initform nil)
   (message :initarg :message :reader query-error-message :initform "Query error"))
  (:report (lambda (c s)
             (format s "Query error: ~A~@[~%SQL: ~A~]"
                     (query-error-message c)
                     (query-error-sql c)))))

(define-condition migration-error (error)
  ((migration :initarg :migration :reader migration-error-migration :initform nil)
   (direction :initarg :direction :reader migration-error-direction :initform :up)
   (message   :initarg :message   :reader migration-error-message   :initform "Migration error"))
  (:report (lambda (c s)
             (format s "Migration error (~A ~A): ~A"
                     (migration-error-direction c)
                     (migration-error-migration c)
                     (migration-error-message c)))))
