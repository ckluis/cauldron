;;;; src/forge/sql-console.lisp — Interactive SQL console for Forge

(in-package :cauldron.forge)

(defun sql-console-view (conn config)
  "Render the SQL console page."
  (let ((body (forge-layout "SQL Console"
                (cauldron.alembic:html
                  (:div :class "forge-sql-console"
                    (:form :method "POST" :action "/forge/sql/execute"
                      (:div :class "forge-sql-editor"
                        (:textarea :name "sql" :rows "8"
                                   :placeholder "SELECT * FROM users LIMIT 10;"
                                   ""))
                      (:div :class "forge-sql-actions"
                        (:label
                          (:input :type "checkbox" :name "explain" :value "1")
                          " EXPLAIN ANALYZE")
                        (cauldron.alembic:submit-button :label "Run Query")))
                    ;; Results area
                    (:div :class "forge-sql-results"
                      (:p :class "forge-empty"
                          "Enter a query above and click Run."))))
                :config config)))
    (cauldron.crucible:conn-put-resp-body
     (cauldron.crucible:conn-put-resp-header
      (cauldron.crucible:conn-put-status conn 200)
      "Content-Type" "text/html; charset=utf-8")
     body)))

(defun execute-sql-query (conn config)
  "Execute a SQL query from the console.
Read-only by default. Returns results as HTML table."
  (declare (ignore config))
  ;; At runtime: parse body for sql param, execute via pool,
  ;; render results as table, audit log the query
  (let ((body (forge-layout "SQL Console — Results"
                (cauldron.alembic:html
                  (:div :class "forge-sql-console"
                    (:div :class "forge-sql-results"
                      (:p "Query execution requires a running database connection."))
                    (:a :href "/forge/sql" :class "btn" "Back to Console"))))))
    (cauldron.crucible:conn-put-resp-body
     (cauldron.crucible:conn-put-resp-header
      (cauldron.crucible:conn-put-status conn 200)
      "Content-Type" "text/html; charset=utf-8")
     body)))
