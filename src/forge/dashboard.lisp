;;;; src/forge/dashboard.lisp — Forge admin dashboard
;;;; Shows resource counts, system info, recent activity.

(in-package :cauldron.forge)

(defun forge-layout (title content &key config)
  "Wrap CONTENT in the Forge admin layout."
  (let ((app-title (if config (forge-config-title config) "Forge Admin"))
        (resources (when config (forge-config-resources config))))
    (cauldron.alembic:html
      (:doctype)
      (:html :lang "en"
        (:head
          (:meta :charset "utf-8")
          (:meta :name "viewport" :content "width=device-width, initial-scale=1")
          (:title (format nil "~A — ~A" title app-title))
          (:link :rel "stylesheet" :href "/css/cauldron.css"))
        (:body :class "forge"
          (:div :class "forge-layout"
            ;; Sidebar
            (:nav :class "forge-sidebar"
              (:div :class "forge-logo"
                (:a :href "/forge" app-title))
              (:ul :class "forge-nav"
                (:li (:a :href "/forge" "Dashboard"))
                (when resources
                    (mapcar (lambda (rc)
                              `(:li (:a :href ,(format nil "/forge/~A"
                                                       (string-downcase
                                                        (symbol-name
                                                         (forge-resource-config-name rc))))
                                        ,(forge-resource-config-display-name rc))))
                            resources))
                (:li :class "forge-nav-divider")
                (:li (:a :href "/forge/agents" "Agents"))
                (:li (:a :href "/forge/integrations" "Integrations"))
                (:li (:a :href "/forge/sql" "SQL Console"))
                (:li (:a :href "/forge/schema" "Schema Browser"))
                (:li (:a :href "/forge/audit" "Audit Log"))
                (:li (:a :href "/forge/system" "System"))))
            ;; Main content
            (:main :class "forge-main"
              (:header :class "forge-header"
                (:h1 title))
              (:div :class "forge-content"
                content))))))))

(defun dashboard-view (conn config)
  "Render the Forge dashboard."
  (let* ((resources (when config (forge-config-resources config)))
         (body (forge-layout "Dashboard"
                 (cauldron.alembic:html
                   ;; Resource cards
                   (:div :class "forge-cards"
                     (mapcar (lambda (rc)
                                 `(:div :class "forge-card"
                                    (:h3 ,(forge-resource-config-display-name rc))
                                    (:p :class "forge-card-count" "—")
                                    (:a :href ,(format nil "/forge/~A"
                                                       (string-downcase
                                                        (symbol-name
                                                         (forge-resource-config-name rc))))
                                        "View all")))
                               (or resources '())))
                   ;; System info
                   (:div :class "forge-section"
                     (:h2 "System")
                     (:table :class "forge-table"
                       (:tr (:td "Lisp Implementation") (:td (lisp-implementation-type)))
                       (:tr (:td "Version") (:td (lisp-implementation-version)))
                       (:tr (:td "Machine") (:td (machine-type))))))
                 :config config)))
    (cauldron.crucible:conn-put-resp-body
     (cauldron.crucible:conn-put-resp-header
      (cauldron.crucible:conn-put-status conn 200)
      "Content-Type" "text/html; charset=utf-8")
     body)))
