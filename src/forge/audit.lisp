;;;; src/forge/audit.lisp — Audit logging system

(in-package :cauldron.forge)

;;; --- Audit table DDL ---

(defparameter *audit-table-ddl*
  "CREATE TABLE IF NOT EXISTS forge_audit_log (
     id BIGSERIAL PRIMARY KEY,
     resource_type VARCHAR(255) NOT NULL,
     resource_id VARCHAR(255),
     action VARCHAR(50) NOT NULL,
     actor VARCHAR(255),
     changes JSONB,
     metadata JSONB,
     created_at TIMESTAMPTZ DEFAULT NOW()
   )")

(defparameter *audit-index-ddl*
  '("CREATE INDEX IF NOT EXISTS idx_audit_resource ON forge_audit_log (resource_type, resource_id)"
    "CREATE INDEX IF NOT EXISTS idx_audit_actor ON forge_audit_log (actor)"
    "CREATE INDEX IF NOT EXISTS idx_audit_created ON forge_audit_log (created_at DESC)"))

(defun setup-audit-table (execute-fn)
  "Create the audit log table and indexes."
  (funcall execute-fn *audit-table-ddl*)
  (dolist (idx *audit-index-ddl*)
    (funcall execute-fn idx)))

;;; --- Audit recording ---

(defun record-audit-event (&key execute-fn resource-type resource-id action
                                actor changes metadata)
  "Record an audit event.
EXECUTE-FN is a function (sql &rest params) → execute parameterized SQL.
CHANGES and METADATA are plists or hash-tables (encoded as JSON)."
  (let ((changes-json (if changes
                          (cauldron.json:encode changes)
                          "null"))
        (metadata-json (if metadata
                           (cauldron.json:encode metadata)
                           "null")))
    (funcall execute-fn
             "INSERT INTO forge_audit_log (resource_type, resource_id, action, actor, changes, metadata) VALUES ($1, $2, $3, $4, $5::jsonb, $6::jsonb)"
             resource-type resource-id action actor changes-json metadata-json)))

;;; --- Audit view ---

(defun audit-log-view (conn config)
  "Render the audit log viewer."
  (let ((body (forge-layout "Audit Log"
                (cauldron.alembic:html
                  (:div :class "forge-audit"
                    ;; Filters
                    (:form :method "GET" :action "/forge/audit" :class "forge-filters"
                      (:input :type "text" :name "resource_type"
                              :placeholder "Resource type...")
                      (:input :type "text" :name "actor"
                              :placeholder "Actor...")
                      (:input :type "date" :name "from")
                      (:input :type "date" :name "to")
                      (cauldron.alembic:submit-button :label "Filter"))
                    ;; Audit table
                    (:table :class "forge-table"
                      (:thead
                        (:tr
                          (:th "Time")
                          (:th "Resource")
                          (:th "ID")
                          (:th "Action")
                          (:th "Actor")
                          (:th "Changes")))
                      (:tbody
                        (:tr (:td :colspan "6" :class "forge-empty"
                                  "Audit log loads from database at runtime."))))
                    ;; Retention info
                    (:p :class "forge-note"
                        "Retention: 90 days (configurable). Older entries are cleaned up automatically.")))
                :config config)))
    (cauldron.crucible:conn-put-resp-body
     (cauldron.crucible:conn-put-resp-header
      (cauldron.crucible:conn-put-status conn 200)
      "Content-Type" "text/html; charset=utf-8")
     body)))

;;; --- System info view ---

(defun system-info-view (conn config)
  "Render system information page."
  (let ((body (forge-layout "System Information"
                (cauldron.alembic:html
                  ;; SBCL info
                  (:div :class "forge-section"
                    (:h2 "Runtime")
                    (:table :class "forge-detail-table"
                      (:tr (:th "Implementation") (:td (lisp-implementation-type)))
                      (:tr (:th "Version") (:td (lisp-implementation-version)))
                      (:tr (:th "Machine Type") (:td (machine-type)))
                      (:tr (:th "Machine Instance") (:td (machine-instance)))
                      (:tr (:th "Features")
                           (:td (:code (format nil "~{~A~^, ~}"
                                               (subseq *features* 0 (min 10 (length *features*)))))))))
                  ;; Memory (GC info when available)
                  (:div :class "forge-section"
                    (:h2 "Memory")
                    (:table :class "forge-detail-table"
                      (:tr (:th "Dynamic Space Usage")
                           (:td (format nil "~,2F MB"
                                        (/ (sb-kernel:dynamic-usage) 1048576.0))))
                      (:tr (:th "GC Run Time")
                           (:td (format nil "~,3F s"
                                        (/ (float sb-ext:*gc-run-time*) 1000.0)))))))
                :config config)))
    (cauldron.crucible:conn-put-resp-body
     (cauldron.crucible:conn-put-resp-header
      (cauldron.crucible:conn-put-status conn 200)
      "Content-Type" "text/html; charset=utf-8")
     body)))
