;;;; src/grimoire/search.lisp — Full-text search via PostgreSQL tsvector + GIN indexes
(in-package :cauldron.grimoire)

;;; --- Search DDL Generation ---

(defun add-search-column-ddl (table-name columns)
  "Generate SQL to add a tsvector search column + GIN index + auto-update trigger.
TABLE-NAME is a string. COLUMNS is a list of column name strings.
Returns a list of SQL statements."
  (let* ((tbl (table-name-string table-name))
         (col-concat (format nil "~{coalesce(~A, '')~^ || ' ' || ~}" columns)))
    (list
     ;; Add tsvector column
     (format nil "ALTER TABLE ~A ADD COLUMN IF NOT EXISTS search_vector tsvector" tbl)
     ;; Create GIN index
     (format nil "CREATE INDEX IF NOT EXISTS idx_~A_search ON ~A USING GIN(search_vector)" tbl tbl)
     ;; Create trigger function
     (format nil "CREATE OR REPLACE FUNCTION ~A_search_update() RETURNS trigger AS $$ BEGIN NEW.search_vector := to_tsvector('english', ~A); RETURN NEW; END; $$ LANGUAGE plpgsql" tbl col-concat)
     ;; Create trigger
     (format nil "CREATE OR REPLACE TRIGGER trg_~A_search BEFORE INSERT OR UPDATE ON ~A FOR EACH ROW EXECUTE FUNCTION ~A_search_update()" tbl tbl tbl))))

;;; --- Search Query Building ---

(defun build-search-query (table-name search-term &key (limit 50) (offset 0) columns)
  "Build a parameterized full-text search query with ranking.
COLUMNS are the columns to return (default: all).
Returns (values sql params)."
  (let* ((tbl (table-name-string table-name))
         (cols (if columns
                   (format nil "~{~A~^, ~}" columns)
                   "*")))
    (values
     (format nil "SELECT ~A, ts_rank(search_vector, plainto_tsquery('english', $1)) AS rank FROM ~A WHERE search_vector @@ plainto_tsquery('english', $1) ORDER BY rank DESC LIMIT ~D OFFSET ~D"
             cols tbl limit offset)
     (list search-term))))

(defun search-records (table-name search-term &key (limit 50) (offset 0))
  "Build a search query for a single table.
Returns (values sql params)."
  (build-search-query table-name search-term :limit limit :offset offset))

(defun highlight-match (table-name column-name search-term)
  "Build SQL to get highlighted search snippets using ts_headline.
Returns (values sql params)."
  (let ((tbl (table-name-string table-name))
        (col (table-name-string column-name)))
    (values
     (format nil "SELECT id, ts_headline('english', ~A, plainto_tsquery('english', $1), 'StartSel=<mark>, StopSel=</mark>, MaxWords=50, MinWords=20') AS snippet FROM ~A WHERE search_vector @@ plainto_tsquery('english', $1) ORDER BY ts_rank(search_vector, plainto_tsquery('english', $1)) DESC"
             col tbl)
     (list search-term))))

(defun reindex-table-sql (table-name columns)
  "Build SQL to rebuild the search_vector for all rows in a table.
Returns (values sql params)."
  (let* ((tbl (table-name-string table-name))
         (col-concat (format nil "~{coalesce(~A, '')~^ || ' ' || ~}" columns)))
    (values
     (format nil "UPDATE ~A SET search_vector = to_tsvector('english', ~A)" tbl col-concat)
     nil)))

;;; --- Cross-Table Search ---

(defun cross-table-search-sql (tables search-term &key (limit 10))
  "Build UNION ALL query to search across multiple tables.
TABLES is a list of (table-name display-name) pairs.
Returns (values sql params) with results grouped by type."
  (let ((unions
          (mapcar (lambda (table-pair)
                    (let ((tbl (table-name-string (first table-pair)))
                          (display (second table-pair)))
                      (format nil "SELECT '~A' AS result_type, id, ts_rank(search_vector, plainto_tsquery('english', $1)) AS rank FROM ~A WHERE search_vector @@ plainto_tsquery('english', $1) ORDER BY rank DESC LIMIT ~D"
                              display tbl limit)))
                  tables)))
    (values
     (format nil "~{(~A)~^ UNION ALL ~} ORDER BY rank DESC" unions)
     (list search-term))))

;;; --- deftable :search clause extension ---

(defun generate-search-ddl (table-name search-columns)
  "Generate full search DDL from a :search clause.
TABLE-NAME is a symbol, SEARCH-COLUMNS is a list of column symbols.
Returns list of SQL strings."
  (add-search-column-ddl
   (table-name-string table-name)
   (mapcar #'table-name-string search-columns)))
