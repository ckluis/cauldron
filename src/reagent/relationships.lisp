;;;; src/reagent/relationships.lisp — Resource relationship definitions
(in-package :cauldron.reagent)

;;; -------------------------------------------------------
;;; Relationship Definitions
;;;
;;; Declarative relationship system: belongs-to, has-many,
;;; many-to-many. Preload prevents N+1 queries by batching
;;; related record loads via WHERE id IN (...).
;;; -------------------------------------------------------

(defstruct relationship-def
  "Definition of a relationship between resources."
  (type nil :type keyword)          ; :belongs-to, :has-many, :many-to-many
  (source nil :type symbol)         ; owning resource
  (target nil :type symbol)         ; related resource
  (foreign-key nil :type (or null symbol string))
  (through nil :type (or null symbol string))  ; join table for many-to-many
  (inverse nil :type (or null symbol)))

;;; -------------------------------------------------------
;;; Relationship declaration functions
;;;
;;; These are called at class-definition time (from defresource)
;;; or standalone to add relationships to existing resources.
;;; -------------------------------------------------------

(defun default-foreign-key (target-name)
  "Derive a default foreign key name from TARGET-NAME.
E.g., user → user_id"
  (format nil "~A_id" (string-downcase (symbol-name target-name))))

(defun default-join-table (source target)
  "Derive a default join table name from SOURCE and TARGET.
Uses alphabetical ordering. E.g., (roles, users) → roles_users"
  (let ((s (string-downcase (symbol-name source)))
        (t-name (string-downcase (symbol-name target))))
    (if (string< s t-name)
        (format nil "~A_~A" (pluralize-name source) (pluralize-name target))
        (format nil "~A_~A" (pluralize-name target) (pluralize-name source)))))

(defun belongs-to (source target &key foreign-key)
  "Declare that SOURCE belongs to TARGET (SOURCE table holds the FK).
Returns a relationship-def and registers it on the source metaclass.

Example:
  (belongs-to 'post 'user)  ; post has user_id column"
  (let* ((fk (or foreign-key (default-foreign-key target)))
         (rel (make-relationship-def
               :type :belongs-to
               :source source
               :target target
               :foreign-key fk)))
    ;; Register on source metaclass if available
    (let ((class (find-class source nil)))
      (when (and class (typep class 'resource-metaclass))
        (push rel (resource-relationships class))))
    rel))

(defun has-many (source target &key foreign-key)
  "Declare that SOURCE has many TARGETs (TARGET table holds the FK back to SOURCE).

Example:
  (has-many 'user 'post)  ; post.user_id references user.id"
  (let* ((fk (or foreign-key (default-foreign-key source)))
         (rel (make-relationship-def
               :type :has-many
               :source source
               :target target
               :foreign-key fk)))
    (let ((class (find-class source nil)))
      (when (and class (typep class 'resource-metaclass))
        (push rel (resource-relationships class))))
    rel))

(defun many-to-many (source target &key through)
  "Declare a many-to-many relationship between SOURCE and TARGET via a join table.
THROUGH names the join table (defaults to alphabetical combination).

Example:
  (many-to-many 'user 'role :through \"users_roles\")"
  (let* ((join-table (or through (default-join-table source target)))
         (rel (make-relationship-def
               :type :many-to-many
               :source source
               :target target
               :through join-table)))
    (let ((class (find-class source nil)))
      (when (and class (typep class 'resource-metaclass))
        (push rel (resource-relationships class))))
    rel))

;;; -------------------------------------------------------
;;; Preload — batch-load related records to prevent N+1
;;; -------------------------------------------------------

(defun extract-ids (records key)
  "Extract a list of unique non-nil values for KEY from RECORDS (list of plists)."
  (let ((ids '()))
    (dolist (rec records)
      (let ((val (getf rec key)))
        (when (and val (not (member val ids :test #'equal)))
          (push val ids))))
    (nreverse ids)))

(defun build-in-clause (column ids)
  "Build a SQL 'WHERE column IN (...)' clause string.
IDS is a list of values (integers or strings)."
  (format nil "~A IN (~{~A~^, ~})"
          column
          (mapcar (lambda (id)
                    (if (stringp id)
                        (format nil "'~A'" id)  ; NOTE: real impl should use parameterized queries
                        (princ-to-string id)))
                  ids)))

(defun preload (records association &key query-fn foreign-key primary-key)
  "Batch-load ASSOCIATION for RECORDS to prevent N+1 queries.

RECORDS      — list of plists representing the parent records
ASSOCIATION  — symbol naming the relationship/target resource
QUERY-FN     — function (sql-string) → list of plists (executes the query)
FOREIGN-KEY  — the FK column (keyword) on the child table (for has-many)
                or on the parent table (for belongs-to)
PRIMARY-KEY  — the PK column (keyword), defaults to :id

Returns RECORDS with each record augmented with the association key
pointing to its related records.

Example:
  (preload users :posts
    :query-fn #'execute-query
    :foreign-key :user-id)
  ;; Each user plist now has :posts key with their posts"
  (let* ((pk (or primary-key :id))
         (fk (or foreign-key
                 (intern (format nil "~A-ID" (symbol-name association)) :keyword)))
         (ids (extract-ids records pk)))
    (when (and ids query-fn)
      (let* ((target-class (find-resource-class association))
             (table (if target-class
                        (resource-table-name target-class)
                        (string-downcase (symbol-name association))))
             (fk-column (substitute #\_ #\- (string-downcase (symbol-name fk))))
             (sql (format nil "SELECT * FROM ~A WHERE ~A"
                          table (build-in-clause fk-column ids)))
             (related (funcall query-fn sql))
             ;; Group related records by their FK value
             (grouped (make-hash-table :test 'equal)))
        (dolist (rec related)
          (let ((fk-val (getf rec fk)))
            (push rec (gethash fk-val grouped))))
        ;; Attach related records to each parent
        (let ((assoc-key (intern (symbol-name association) :keyword)))
          (mapcar (lambda (rec)
                    (let ((rec-id (getf rec pk)))
                      (append rec (list assoc-key
                                        (nreverse (gethash rec-id grouped))))))
                  records))))))
