;;;; src/grimoire/query.lisp — Composable, immutable query builder
;;;; All builder functions return new query structs (copy-on-modify).
;;;; to-sql generates parameterized SQL with $N placeholders.

(in-package :cauldron.grimoire)

;;; --- Query struct (immutable — all builders copy) ---

(defstruct (query (:constructor %make-query))
  "Immutable query representation."
  (source nil)                     ; Table name (string) or subquery
  (operation :select :type keyword) ; :select, :insert, :update, :delete
  (fields '() :type list)          ; SELECT columns
  (conditions '() :type list)      ; WHERE conditions
  (joins '() :type list)           ; JOIN clauses
  (order '() :type list)           ; ORDER BY clauses
  (group '() :type list)           ; GROUP BY columns
  (having-clause nil)              ; HAVING condition
  (limit-val nil)                  ; LIMIT
  (offset-val nil)                 ; OFFSET
  (distinct-p nil :type boolean)   ; DISTINCT
  (lock-mode nil)                  ; :for-update, :for-share
  ;; For INSERT/UPDATE
  (columns '() :type list)        ; Column names
  (values-list '() :type list)    ; Values (parallel to columns)
  (returning '() :type list))     ; RETURNING columns

(defun make-query (&rest args)
  (apply #'%make-query args))

(defun copy-query-with (q &rest overrides)
  "Copy Q with OVERRIDES applied."
  (let ((new (copy-query q)))
    (loop for (key val) on overrides by #'cddr
          do (case key
               (:source (setf (query-source new) val))
               (:operation (setf (query-operation new) val))
               (:fields (setf (query-fields new) val))
               (:conditions (setf (query-conditions new) val))
               (:joins (setf (query-joins new) val))
               (:order (setf (query-order new) val))
               (:group (setf (query-group new) val))
               (:having-clause (setf (query-having-clause new) val))
               (:limit-val (setf (query-limit-val new) val))
               (:offset-val (setf (query-offset-val new) val))
               (:distinct-p (setf (query-distinct-p new) val))
               (:lock-mode (setf (query-lock-mode new) val))
               (:columns (setf (query-columns new) val))
               (:values-list (setf (query-values-list new) val))
               (:returning (setf (query-returning new) val))))
    new))

;;; --- Builder functions (each returns new query) ---

(defun from (table)
  "Start a SELECT query from TABLE (string)."
  (%make-query :source table :operation :select))

(defun select-fields (query &rest fields)
  "Add fields to the SELECT clause."
  (copy-query-with query :fields (append (query-fields query) fields)))

(defun where-clause (query &rest conditions)
  "Add WHERE conditions. Each condition is (op field value)."
  (copy-query-with query :conditions (append (query-conditions query) conditions)))

(defun order-by (query &rest clauses)
  "Add ORDER BY clauses. Each clause is field or (field :desc)."
  (copy-query-with query :order (append (query-order query) clauses)))

(defun group-by (query &rest columns)
  "Add GROUP BY columns."
  (copy-query-with query :group (append (query-group query) columns)))

(defun having (query condition)
  "Add HAVING clause."
  (copy-query-with query :having-clause condition))

(defun limit-query (query n)
  "Set LIMIT."
  (copy-query-with query :limit-val n))

(defun offset-query (query n)
  "Set OFFSET."
  (copy-query-with query :offset-val n))

(defun join-query (query table &key on (type :inner))
  "Add a JOIN. ON is a condition. TYPE is :inner, :left, :right, :full."
  (copy-query-with query
    :joins (append (query-joins query)
                   (list (list type table on)))))

(defun distinct-query (query)
  "Add DISTINCT."
  (copy-query-with query :distinct-p t))

(defun lock-query (query &optional (mode :for-update))
  "Add row locking."
  (copy-query-with query :lock-mode mode))

;;; --- DML builders ---

(defun insert-into (table &rest column-value-pairs)
  "Create an INSERT query. Pairs are :column value :column value..."
  (let ((cols '()) (vals '()))
    (loop for (col val) on column-value-pairs by #'cddr
          do (push (string-downcase (symbol-name col)) cols)
             (push val vals))
    (%make-query :source table
                 :operation :insert
                 :columns (nreverse cols)
                 :values-list (nreverse vals))))

(defun update-set (table &rest column-value-pairs)
  "Create an UPDATE query. Pairs are :column value :column value..."
  (let ((cols '()) (vals '()))
    (loop for (col val) on column-value-pairs by #'cddr
          do (push (string-downcase (symbol-name col)) cols)
             (push val vals))
    (%make-query :source table
                 :operation :update
                 :columns (nreverse cols)
                 :values-list (nreverse vals))))

(defun delete-from (table)
  "Create a DELETE query."
  (%make-query :source table :operation :delete))

;;; --- Condition DSL operators ---

(defun qand (&rest conditions)
  "Combine conditions with AND."
  (cons :and conditions))

(defun qor (&rest conditions)
  "Combine conditions with OR."
  (cons :or conditions))

(defun qnot (condition)
  "Negate a condition."
  (list :not condition))

(defun validate-sql-identifier (name)
  "Ensure NAME is a safe SQL identifier (letters, digits, underscore, dot only).
Signals error on invalid names to prevent SQL injection."
  (unless (and (stringp name)
               (plusp (length name))
               (every (lambda (c) (or (alpha-char-p c) (digit-char-p c)
                                      (char= c #\_) (char= c #\.)))
                      name)
               (or (alpha-char-p (char name 0)) (char= (char name 0) #\_)))
    (error 'query-error :message (format nil "Invalid SQL identifier: ~A" name))))

;;; --- Aggregates ---

(defun count-rows (query &optional column)
  "Add COUNT to SELECT."
  (when column (validate-sql-identifier column))
  (select-fields query (if column
                           (format nil "COUNT(~A)" column)
                           "COUNT(*)")))

(defun sum-of (query column)
  "Add SUM to SELECT."
  (validate-sql-identifier column)
  (select-fields query (format nil "SUM(~A)" column)))

(defun avg-of (query column)
  "Add AVG to SELECT."
  (validate-sql-identifier column)
  (select-fields query (format nil "AVG(~A)" column)))

;;; --- SQL generation ---

(defun render-condition (condition params)
  "Render a condition expression to SQL.
PARAMS is a list (mutated) collecting parameter values.
Returns SQL string."
  (cond
    ;; Compound: (:and cond1 cond2 ...)
    ((and (consp condition) (eq (first condition) :and))
     (let ((parts (mapcar (lambda (c) (render-condition c params))
                          (rest condition))))
       (format nil "(~{~A~^ AND ~})" parts)))

    ((and (consp condition) (eq (first condition) :or))
     (let ((parts (mapcar (lambda (c) (render-condition c params))
                          (rest condition))))
       (format nil "(~{~A~^ OR ~})" parts)))

    ((and (consp condition) (eq (first condition) :not))
     (format nil "NOT (~A)" (render-condition (second condition) params)))

    ;; Simple: (op field value)
    ((and (consp condition) (= (length condition) 3))
     (destructuring-bind (op field value) condition
       (let ((col (if (stringp field) field (string-downcase (symbol-name field)))))
         (case op
           (:= (progn (nconc params (list value))
                       (format nil "~A = $~D" col (length (cdr params)))))
           (:/= (progn (nconc params (list value))
                        (format nil "~A != $~D" col (length (cdr params)))))
           (:> (progn (nconc params (list value))
                       (format nil "~A > $~D" col (length (cdr params)))))
           (:< (progn (nconc params (list value))
                       (format nil "~A < $~D" col (length (cdr params)))))
           (:>= (progn (nconc params (list value))
                        (format nil "~A >= $~D" col (length (cdr params)))))
           (:<= (progn (nconc params (list value))
                        (format nil "~A <= $~D" col (length (cdr params)))))
           (:like (progn (nconc params (list value))
                          (format nil "~A LIKE $~D" col (length (cdr params)))))
           (:ilike (progn (nconc params (list value))
                           (format nil "~A ILIKE $~D" col (length (cdr params)))))
           (:in
            (let ((placeholders
                    (loop for v in value
                          do (nconc params (list v))
                          collect (format nil "$~D" (length (cdr params))))))
              (format nil "~A IN (~{~A~^, ~})" col placeholders)))
           (:between
            (nconc params (list (first value)))
            (let ((p1 (length (cdr params))))
              (nconc params (list (second value)))
              (format nil "~A BETWEEN $~D AND $~D" col p1 (length (cdr params)))))
           (:is-null (format nil "~A IS NULL" col))
           (:is-not-null (format nil "~A IS NOT NULL" col))
           (otherwise (error 'query-error :message (format nil "Unknown operator: ~A" op)))))))

    ;; Unary: (op field) — for is-null etc
    ((and (consp condition) (= (length condition) 2))
     (destructuring-bind (op field) condition
       (let ((col (if (stringp field) field (string-downcase (symbol-name field)))))
         (case op
           (:is-null (format nil "~A IS NULL" col))
           (:is-not-null (format nil "~A IS NOT NULL" col))
           (otherwise (error 'query-error :message (format nil "Unknown unary operator: ~A" op)))))))

    (t (error 'query-error :message (format nil "Invalid condition: ~S" condition)))))

(defun render-order-clause (clause)
  "Render an ORDER BY clause."
  (cond
    ((stringp clause) clause)
    ((symbolp clause) (string-downcase (symbol-name clause)))
    ((and (consp clause) (= (length clause) 2))
     (format nil "~A ~A"
             (if (stringp (first clause))
                 (first clause)
                 (string-downcase (symbol-name (first clause))))
             (ecase (second clause)
               (:asc "ASC")
               (:desc "DESC"))))
    (t (error 'query-error :message (format nil "Invalid order clause: ~S" clause)))))

(defun render-join (join-spec params)
  "Render a JOIN clause."
  (destructuring-bind (type table on) join-spec
    (format nil "~A JOIN ~A ON ~A"
            (ecase type
              (:inner "INNER")
              (:left "LEFT")
              (:right "RIGHT")
              (:full "FULL OUTER")
              (:cross "CROSS"))
            table
            (if on
                (render-condition on params)
                "TRUE"))))

(defun to-sql (query)
  "Convert QUERY to parameterized SQL.
Returns (values sql-string params-list)."
  (let ((params (list nil)))  ; cons cell for nconc
    (multiple-value-bind (sql)
        (ecase (query-operation query)
          (:select (render-select query params))
          (:insert (render-insert query params))
          (:update (render-update query params))
          (:delete (render-delete query params)))
      (values sql (cdr params)))))

(defun render-select (query params)
  "Render SELECT SQL."
  (with-output-to-string (s)
    (write-string "SELECT " s)
    (when (query-distinct-p query)
      (write-string "DISTINCT " s))
    (if (query-fields query)
        (format s "~{~A~^, ~}" (query-fields query))
        (write-string "*" s))
    (format s " FROM ~A" (query-source query))
    ;; JOINs
    (dolist (j (query-joins query))
      (format s " ~A" (render-join j params)))
    ;; WHERE
    (when (query-conditions query)
      (let ((where-parts (mapcar (lambda (c) (render-condition c params))
                                 (query-conditions query))))
        (format s " WHERE ~{~A~^ AND ~}" where-parts)))
    ;; GROUP BY
    (when (query-group query)
      (format s " GROUP BY ~{~A~^, ~}" (query-group query)))
    ;; HAVING
    (when (query-having-clause query)
      (format s " HAVING ~A" (render-condition (query-having-clause query) params)))
    ;; ORDER BY
    (when (query-order query)
      (format s " ORDER BY ~{~A~^, ~}"
              (mapcar #'render-order-clause (query-order query))))
    ;; LIMIT / OFFSET
    (when (query-limit-val query)
      (format s " LIMIT ~D" (query-limit-val query)))
    (when (query-offset-val query)
      (format s " OFFSET ~D" (query-offset-val query)))
    ;; LOCK
    (when (query-lock-mode query)
      (format s " ~A" (ecase (query-lock-mode query)
                         (:for-update "FOR UPDATE")
                         (:for-share "FOR SHARE"))))))

(defun render-insert (query params)
  "Render INSERT SQL."
  (with-output-to-string (s)
    (format s "INSERT INTO ~A (~{~A~^, ~}) VALUES ("
            (query-source query) (query-columns query))
    (loop for val in (query-values-list query)
          for i from 1
          do (when (> i 1) (write-string ", " s))
             (nconc params (list val))
             (format s "$~D" (length (cdr params))))
    (write-string ")" s)
    (when (query-returning query)
      (format s " RETURNING ~{~A~^, ~}" (query-returning query)))))

(defun render-update (query params)
  "Render UPDATE SQL."
  (with-output-to-string (s)
    (format s "UPDATE ~A SET " (query-source query))
    (loop for col in (query-columns query)
          for val in (query-values-list query)
          for i from 1
          do (when (> i 1) (write-string ", " s))
             (nconc params (list val))
             (format s "~A = $~D" col (length (cdr params))))
    ;; WHERE
    (when (query-conditions query)
      (let ((where-parts (mapcar (lambda (c) (render-condition c params))
                                 (query-conditions query))))
        (format s " WHERE ~{~A~^ AND ~}" where-parts)))
    (when (query-returning query)
      (format s " RETURNING ~{~A~^, ~}" (query-returning query)))))

(defun render-delete (query params)
  "Render DELETE SQL."
  (with-output-to-string (s)
    (format s "DELETE FROM ~A" (query-source query))
    ;; WHERE
    (when (query-conditions query)
      (let ((where-parts (mapcar (lambda (c) (render-condition c params))
                                 (query-conditions query))))
        (format s " WHERE ~{~A~^ AND ~}" where-parts)))
    (when (query-returning query)
      (format s " RETURNING ~{~A~^, ~}" (query-returning query)))))

;;; --- Execution (requires cauldron.db at runtime) ---

(defun execute-query (connection query)
  "Execute QUERY on CONNECTION, returning all result rows.
CONNECTION should support the cauldron.db query protocol."
  (multiple-value-bind (sql params) (to-sql query)
    (apply #'cauldron.db:query connection sql params)))

(defun execute-one (connection query)
  "Execute QUERY on CONNECTION, returning the first result row."
  (let ((rows (execute-query connection (limit-query query 1))))
    (first rows)))
