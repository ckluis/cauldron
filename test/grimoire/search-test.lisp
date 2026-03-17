;;;; test/grimoire/search-test.lisp — Phase 43: Full-text search tests
(in-package :cauldron.test)

(defsuite :full-text-search)

;;; ============================================================
;;; Search DDL Generation
;;; ============================================================

(deftest test-add-search-column-ddl
  (let ((ddl (cauldron.grimoire:add-search-column-ddl "contacts" '("first_name" "last_name" "email"))))
    (is (listp ddl) "Returns list of SQL statements")
    (is-equal 4 (length ddl) "4 statements: ALTER, INDEX, FUNCTION, TRIGGER")
    (is (search "search_vector tsvector" (first ddl)) "ALTER adds tsvector column")
    (is (search "GIN" (second ddl)) "GIN index created")
    (is (search "to_tsvector" (third ddl)) "Trigger function uses to_tsvector")
    (is (search "BEFORE INSERT OR UPDATE" (fourth ddl)) "Trigger fires on insert/update")))

(deftest test-add-search-column-ddl-single-column
  (let ((ddl (cauldron.grimoire:add-search-column-ddl "posts" '("title"))))
    (is-equal 4 (length ddl))
    (is (search "coalesce(title, '')" (third ddl)) "Single column in tsvector")))

(deftest test-add-search-column-ddl-multiple-columns
  (let ((ddl (cauldron.grimoire:add-search-column-ddl "articles" '("title" "body" "summary"))))
    (let ((fn-sql (third ddl)))
      (is (search "title" fn-sql))
      (is (search "body" fn-sql))
      (is (search "summary" fn-sql)))))

;;; ============================================================
;;; Search Query Building
;;; ============================================================

(deftest test-build-search-query
  (multiple-value-bind (sql params)
      (cauldron.grimoire:build-search-query "contacts" "john smith")
    (is (search "plainto_tsquery" sql) "Uses plainto_tsquery")
    (is (search "ts_rank" sql) "Includes ranking")
    (is (search "ORDER BY rank DESC" sql) "Orders by rank")
    (is-equal 1 (length params))
    (is-equal "john smith" (first params))))

(deftest test-build-search-query-with-limit
  (multiple-value-bind (sql params)
      (cauldron.grimoire:build-search-query "contacts" "test" :limit 10 :offset 20)
    (is (search "LIMIT 10" sql))
    (is (search "OFFSET 20" sql))
    (is-equal 1 (length params))))

(deftest test-build-search-query-with-columns
  (multiple-value-bind (sql params)
      (cauldron.grimoire:build-search-query "contacts" "test" :columns '("id" "name" "email"))
    (declare (ignore params))
    (is (search "id, name, email" sql) "Specified columns in SELECT")))

(deftest test-search-records
  (multiple-value-bind (sql params)
      (cauldron.grimoire:search-records "deals" "enterprise")
    (is (search "deals" sql))
    (is-equal "enterprise" (first params))))

;;; ============================================================
;;; Highlight Match
;;; ============================================================

(deftest test-highlight-match
  (multiple-value-bind (sql params)
      (cauldron.grimoire:highlight-match "posts" "body" "quantum computing")
    (is (search "ts_headline" sql) "Uses ts_headline")
    (is (search "<mark>" sql) "Custom start selector")
    (is (search "</mark>" sql) "Custom stop selector")
    (is (search "body" sql) "Column name in headline")
    (is-equal 1 (length params))
    (is-equal "quantum computing" (first params))))

;;; ============================================================
;;; Reindex
;;; ============================================================

(deftest test-reindex-table-sql
  (multiple-value-bind (sql params)
      (cauldron.grimoire:reindex-table-sql "contacts" '("first_name" "email"))
    (is (search "UPDATE contacts" sql))
    (is (search "to_tsvector" sql))
    (is (search "first_name" sql))
    (is (search "email" sql))
    (is-nil params)))

;;; ============================================================
;;; Cross-Table Search
;;; ============================================================

(deftest test-cross-table-search-sql
  (multiple-value-bind (sql params)
      (cauldron.grimoire:cross-table-search-sql
       '((contacts "Contact") (deals "Deal") (companies "Company"))
       "acme")
    (is (search "UNION ALL" sql) "Uses UNION ALL")
    (is (search "Contact" sql) "Contact type label")
    (is (search "Deal" sql) "Deal type label")
    (is (search "Company" sql) "Company type label")
    (is-equal 1 (length params))
    (is-equal "acme" (first params))))

;;; ============================================================
;;; Generate Search DDL
;;; ============================================================

(deftest test-generate-search-ddl
  (let ((ddl (cauldron.grimoire:generate-search-ddl 'contacts '(first_name last_name))))
    (is (listp ddl))
    (is-equal 4 (length ddl))
    (is (search "contacts" (first ddl)))
    (is (search "first_name" (third ddl)))
    (is (search "last_name" (third ddl)))))
