;;;; test/grimoire/deftable-test.lisp — Tests for declarative table definitions
(in-package :cauldron.test)

(defsuite :grimoire-deftable)

;;; --- table-name-string ---

(deftest deftable-name-string-symbol
  "table-name-string converts symbols to lowercase underscore strings"
  (is-equal "contacts" (cauldron.grimoire:table-name-string 'contacts))
  (is-equal "pipeline_stages" (cauldron.grimoire:table-name-string 'pipeline-stages))
  (is-equal "crm_companies" (cauldron.grimoire:table-name-string 'crm-companies)))

(deftest deftable-name-string-keyword
  "table-name-string works with keywords"
  (is-equal "contacts" (cauldron.grimoire:table-name-string :contacts)))

;;; --- field-type-to-pg-type ---

(deftest deftable-field-type-text
  "field-type-to-pg-type maps text-like types to TEXT"
  (is-equal "TEXT" (cauldron.grimoire:field-type-to-pg-type "text"))
  (is-equal "TEXT" (cauldron.grimoire:field-type-to-pg-type "email"))
  (is-equal "TEXT" (cauldron.grimoire:field-type-to-pg-type "phone"))
  (is-equal "TEXT" (cauldron.grimoire:field-type-to-pg-type "url"))
  (is-equal "TEXT" (cauldron.grimoire:field-type-to-pg-type "textarea"))
  (is-equal "TEXT" (cauldron.grimoire:field-type-to-pg-type "enum")))

(deftest deftable-field-type-numeric
  "field-type-to-pg-type maps numeric types correctly"
  (is-equal "NUMERIC" (cauldron.grimoire:field-type-to-pg-type "number"))
  (is-equal "INTEGER" (cauldron.grimoire:field-type-to-pg-type "integer"))
  (is-equal "NUMERIC(12,2)" (cauldron.grimoire:field-type-to-pg-type "money")))

(deftest deftable-field-type-other
  "field-type-to-pg-type maps other types correctly"
  (is-equal "BOOLEAN" (cauldron.grimoire:field-type-to-pg-type "boolean"))
  (is-equal "DATE" (cauldron.grimoire:field-type-to-pg-type "date"))
  (is-equal "TIMESTAMPTZ" (cauldron.grimoire:field-type-to-pg-type "datetime"))
  (is-equal "JSONB" (cauldron.grimoire:field-type-to-pg-type "json")))

(deftest deftable-field-type-unknown
  "field-type-to-pg-type defaults to TEXT for unknown types"
  (is-equal "TEXT" (cauldron.grimoire:field-type-to-pg-type "unknown")))

;;; --- deftable macro & parse-table-clauses ---

(deftest deftable-basic-registration
  "deftable registers a table spec in the global registry"
  (cauldron.grimoire:deftable test-basic
    (:column name :type text :required t)
    (:column email :type email))
  (let ((spec (cauldron.grimoire:find-table-spec 'test-basic)))
    (is spec "table-spec should be registered")
    (is-equal 'test-basic (cauldron.grimoire:table-spec-name spec))
    (is-equal 2 (length (cauldron.grimoire:table-spec-columns spec)))))

(deftest deftable-column-properties
  "deftable columns have correct properties"
  (cauldron.grimoire:deftable test-cols
    (:column first_name :type text :required t)
    (:column value :type money :default 0)
    (:column active :type boolean :default t)
    (:column company_id :type integer :references companies :on-delete "CASCADE"))
  (let* ((spec (cauldron.grimoire:find-table-spec 'test-cols))
         (cols (cauldron.grimoire:table-spec-columns spec)))
    ;; first_name
    (let ((c (first cols)))
      (is-equal "text" (cauldron.grimoire:column-spec-field-type c))
      (is (cauldron.grimoire:column-spec-required-p c)))
    ;; value
    (let ((c (second cols)))
      (is-equal "money" (cauldron.grimoire:column-spec-field-type c))
      (is-equal 0 (cauldron.grimoire:column-spec-default c)))
    ;; company_id
    (let ((c (fourth cols)))
      (is-equal "integer" (cauldron.grimoire:column-spec-field-type c))
      (is (cauldron.grimoire:column-spec-references c)))))

(deftest deftable-indexes
  "deftable registers indexes correctly"
  (cauldron.grimoire:deftable test-idx
    (:column email :type email)
    (:column name :type text)
    (:index email)
    (:unique name)
    (:index email name))
  (let* ((spec (cauldron.grimoire:find-table-spec 'test-idx))
         (idxs (cauldron.grimoire:table-spec-indexes spec)))
    (is-equal 3 (length idxs))
    ;; First index: email, not unique
    (let ((i (first idxs)))
      (is-equal '("email") (getf i :columns))
      (is (not (getf i :unique-p))))
    ;; Second: name, unique
    (let ((i (second idxs)))
      (is-equal '("name") (getf i :columns))
      (is (getf i :unique-p)))
    ;; Third: composite
    (let ((i (third idxs)))
      (is-equal '("email" "name") (getf i :columns)))))

(deftest deftable-seed-data
  "deftable supports seed SQL"
  (cauldron.grimoire:deftable test-seed
    (:column name :type text)
    (:seed "INSERT INTO test_seed (name) VALUES ('default')"))
  (let* ((spec (cauldron.grimoire:find-table-spec 'test-seed))
         (seeds (cauldron.grimoire:table-spec-seed-sql spec)))
    (is-equal 1 (length seeds))
    (is (search "INSERT INTO" (first seeds)))))

;;; --- DDL generation ---

(deftest deftable-generate-create-table
  "generate-table-ddl produces correct CREATE TABLE SQL"
  (cauldron.grimoire:deftable test-ddl-gen
    (:column first_name :type text :required t)
    (:column email :type email)
    (:column phone :type phone))
  (let* ((spec (cauldron.grimoire:find-table-spec 'test-ddl-gen))
         (ddl (cauldron.grimoire:generate-table-ddl spec))
         (create-sql (first ddl)))
    ;; Should start with CREATE TABLE
    (is (search "CREATE TABLE test_ddl_gen" create-sql))
    ;; Should have id SERIAL PRIMARY KEY
    (is (search "id SERIAL PRIMARY KEY" create-sql))
    ;; Should have first_name TEXT NOT NULL
    (is (search "first_name TEXT NOT NULL" create-sql))
    ;; Should have email TEXT (no NOT NULL)
    (is (search "email TEXT" create-sql))
    ;; Should have created_at and updated_at
    (is (search "created_at TIMESTAMPTZ DEFAULT NOW()" create-sql))
    (is (search "updated_at TIMESTAMPTZ DEFAULT NOW()" create-sql))))

(deftest deftable-generate-with-defaults
  "generate-table-ddl handles default values correctly"
  (cauldron.grimoire:deftable test-defaults
    (:column status :type text :default "draft")
    (:column count :type integer :default 0)
    (:column active :type boolean :default t))
  (let* ((spec (cauldron.grimoire:find-table-spec 'test-defaults))
         (ddl (cauldron.grimoire:generate-table-ddl spec))
         (create-sql (first ddl)))
    (is (search "status TEXT DEFAULT 'draft'" create-sql))
    (is (search "count INTEGER DEFAULT 0" create-sql))
    (is (search "active BOOLEAN DEFAULT TRUE" create-sql))))

(deftest deftable-generate-with-references
  "generate-table-ddl handles foreign key references"
  (cauldron.grimoire:deftable test-fk
    (:column contact_id :type integer :references contacts)
    (:column stage_id :type integer :references pipeline-stages :on-delete "CASCADE"))
  (let* ((spec (cauldron.grimoire:find-table-spec 'test-fk))
         (ddl (cauldron.grimoire:generate-table-ddl spec))
         (create-sql (first ddl)))
    (is (search "contact_id INTEGER REFERENCES contacts(id)" create-sql))
    (is (search "stage_id INTEGER REFERENCES pipeline_stages(id) ON DELETE CASCADE" create-sql))))

(deftest deftable-generate-indexes
  "generate-table-ddl produces CREATE INDEX statements"
  (cauldron.grimoire:deftable test-idx-gen
    (:column email :type email)
    (:column name :type text)
    (:index email)
    (:unique name))
  (let* ((spec (cauldron.grimoire:find-table-spec 'test-idx-gen))
         (ddl (cauldron.grimoire:generate-table-ddl spec)))
    ;; Should have CREATE TABLE + 2 indexes = 3 statements
    (is (>= (length ddl) 3))
    ;; Find the index statements
    (let ((idx-stmts (remove-if-not (lambda (s) (search "CREATE" s))
                                     (rest ddl))))
      (is (some (lambda (s) (search "CREATE INDEX" s)) idx-stmts))
      (is (some (lambda (s) (search "CREATE UNIQUE INDEX" s)) idx-stmts)))))

(deftest deftable-generate-column-unique
  "generate-table-ddl creates unique index for column-level :unique"
  (cauldron.grimoire:deftable test-col-unique
    (:column slug :type text :required t :unique t))
  (let* ((spec (cauldron.grimoire:find-table-spec 'test-col-unique))
         (ddl (cauldron.grimoire:generate-table-ddl spec)))
    (is (some (lambda (s) (search "UNIQUE INDEX" s)) ddl))))

;;; --- Metadata SQL generation ---

(deftest deftable-generate-metadata
  "generate-table-metadata-sql produces correct INSERT statements"
  (cauldron.grimoire:deftable test-meta
    (:column first_name :type text :required t)
    (:column email :type email))
  (let* ((spec (cauldron.grimoire:find-table-spec 'test-meta))
         (meta-sql (cauldron.grimoire:generate-table-metadata-sql spec)))
    ;; 1 INSERT for _custom_objects + 2 for _custom_fields = 3
    (is-equal 3 (length meta-sql))
    ;; First should be _custom_objects INSERT
    (is (search "_custom_objects" (first meta-sql)))
    (is (search "test_meta" (first meta-sql)))
    ;; Rest should be _custom_fields INSERTs
    (is (search "_custom_fields" (second meta-sql)))
    (is (search "first_name" (second meta-sql)))
    (is (search "_custom_fields" (third meta-sql)))
    (is (search "email" (third meta-sql)))))

;;; --- generate-full-table-sql ---

(deftest deftable-generate-full-sql
  "generate-full-table-sql combines DDL and metadata"
  (cauldron.grimoire:deftable test-full
    (:column name :type text :required t)
    (:index name))
  (let* ((spec (cauldron.grimoire:find-table-spec 'test-full))
         (all-sql (cauldron.grimoire:generate-full-table-sql spec)))
    ;; CREATE TABLE + CREATE INDEX + _custom_objects INSERT + _custom_fields INSERT
    (is-equal 4 (length all-sql))
    (is (search "CREATE TABLE" (first all-sql)))
    (is (search "CREATE INDEX" (second all-sql)))
    (is (search "_custom_objects" (third all-sql)))
    (is (search "_custom_fields" (fourth all-sql)))))

;;; --- Money type ---

(deftest deftable-money-type
  "money type generates NUMERIC(12,2)"
  (cauldron.grimoire:deftable test-money
    (:column amount :type money)
    (:column price :type money :default 0))
  (let* ((spec (cauldron.grimoire:find-table-spec 'test-money))
         (ddl (cauldron.grimoire:generate-table-ddl spec))
         (create-sql (first ddl)))
    (is (search "NUMERIC(12,2)" create-sql))))
