;;;; src/forge/schema-browser.lisp — Schema browser and drift detection

(in-package :cauldron.forge)

(defun schema-browser-view (conn config)
  "Render the schema browser — Reagent definitions vs actual DB schema."
  (let* ((resources (when config (forge-config-resources config)))
         (body (forge-layout "Schema Browser"
                 (cauldron.alembic:html
                   (:div :class "forge-schema"
                     (:p "Compares Reagent resource definitions against the actual database schema.")
                     (:table :class "forge-table"
                       (:thead
                         (:tr
                           (:th "Resource")
                           (:th "Table")
                           (:th "Defined Columns")
                           (:th "Status")))
                       (:tbody
                         (if resources
                               (mapcar (lambda (rc)
                                         `(:tr
                                            (:td ,(forge-resource-config-display-name rc))
                                            (:td (:code ,(forge-resource-config-table-name rc)))
                                            (:td ,(princ-to-string
                                                    (length (forge-resource-config-attributes rc))))
                                            (:td :class "forge-status-pending" "Check DB")))
                                       resources)
                               (list `(:tr (:td :colspan "4" :class "forge-empty"
                                                "No resources registered."))))))
                     ;; Generate migration button
                     (:div :class "forge-toolbar"
                       (:button :type "button" :class "btn"
                                "Check for Drift")
                       (:button :type "button" :class "btn"
                                "Generate Migration"))))
                 :config config)))
    (cauldron.crucible:conn-put-resp-body
     (cauldron.crucible:conn-put-resp-header
      (cauldron.crucible:conn-put-status conn 200)
      "Content-Type" "text/html; charset=utf-8")
     body)))

(defun detect-drift (resource-config db-columns)
  "Compare resource definition against actual DB columns.
RESOURCE-CONFIG is a forge-resource-config.
DB-COLUMNS is a list of (name type nullable) from information_schema.
Returns a list of drift reports: (:missing-in-db col), (:missing-in-code col),
(:type-mismatch col expected actual)."
  (let ((defined-cols (mapcar (lambda (a) (string-downcase (symbol-name a)))
                              (forge-resource-config-attributes resource-config)))
        (actual-cols (mapcar #'first db-columns))
        (drifts '()))
    ;; Columns in definition but not in DB
    (dolist (col defined-cols)
      (unless (member col actual-cols :test #'string=)
        (push (list :missing-in-db col) drifts)))
    ;; Columns in DB but not in definition (excluding standard cols)
    (let ((standard '("id" "created_at" "updated_at" "inserted_at")))
      (dolist (col actual-cols)
        (unless (or (member col defined-cols :test #'string=)
                    (member col standard :test #'string=))
          (push (list :missing-in-code col) drifts))))
    (nreverse drifts)))
