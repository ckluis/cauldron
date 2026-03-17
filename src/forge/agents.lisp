;;;; src/forge/agents.lisp — Forge agent dashboard views
(in-package :cauldron.forge)

(defun agents-list-view (conn config)
  "Render the agent list dashboard.
Table: Name | Model | Status | Tools | Triggers | Max Rounds | Token Budget"
  (let* ((agent-names (cauldron.agent:list-agents))
         (body
           (forge-layout "Agents"
             (cauldron.alembic:html
               (:div :class "forge-section"
                 (:p (format nil "~D registered agent~:P" (length agent-names)))
                 (if agent-names
                     `(:table :class "forge-table"
                        (:thead
                          (:tr (:th "Name") (:th "Model") (:th "Status")
                               (:th "Tools") (:th "Triggers") (:th "Max Rounds")
                               (:th "Token Budget")))
                        (:tbody
                          ,@(mapcar
                              (lambda (name)
                                (let* ((spec (cauldron.agent:find-agent name))
                                       (name-str (string-downcase (symbol-name name)))
                                       (enabled (cauldron.agent:agent-spec-enabled-p spec))
                                       (model (string-downcase
                                               (symbol-name
                                                (cauldron.agent:agent-spec-model spec))))
                                       (tool-count (length (cauldron.agent:agent-spec-tool-specs spec)))
                                       (trigger-count (length (cauldron.agent:agent-spec-trigger-specs spec)))
                                       (max-rounds (cauldron.agent:agent-spec-max-rounds spec))
                                       (budget (cauldron.agent:agent-spec-token-budget spec)))
                                  `(:tr
                                     (:td (:a :href ,(format nil "/forge/agents/~A" name-str)
                                               ,name-str))
                                     (:td ,model)
                                     (:td (:span :class ,(if enabled
                                                             "forge-status-green"
                                                             "forge-status-red")
                                                 ,(if enabled "Enabled" "Disabled")))
                                     (:td ,(format nil "~D" tool-count))
                                     (:td ,(format nil "~D" trigger-count))
                                     (:td ,(format nil "~D" max-rounds))
                                     (:td ,(if budget
                                               (format nil "~:D" budget)
                                               "Unlimited")))))
                              agent-names)))
                     `(:p "No agents registered."))))
             :config config)))
    (cauldron.crucible:conn-put-resp-body
     (cauldron.crucible:conn-put-resp-header
      (cauldron.crucible:conn-put-status conn 200)
      "Content-Type" "text/html; charset=utf-8")
     body)))

(defun agent-detail-view (conn config)
  "Render detailed view of a single agent."
  (let* ((name-str (cdr (assoc "name" (cauldron.crucible:conn-params conn) :test #'string=)))
         (agent-name (when name-str
                       (find (string-upcase name-str) (cauldron.agent:list-agents)
                             :key #'symbol-name :test #'string=)))
         (spec (when agent-name (cauldron.agent:find-agent agent-name))))
    (if (null spec)
        (cauldron.crucible:json-error conn "Agent not found" :status 404)
        (let* ((model (string-downcase (symbol-name (cauldron.agent:agent-spec-model spec))))
               (enabled (cauldron.agent:agent-spec-enabled-p spec))
               (description (cauldron.agent:agent-spec-description spec))
               (role (cauldron.agent:agent-spec-role spec))
               (max-rounds (cauldron.agent:agent-spec-max-rounds spec))
               (budget (cauldron.agent:agent-spec-token-budget spec))
               (system-prompt (cauldron.agent:agent-spec-system-prompt spec))
               (tool-specs (cauldron.agent:agent-spec-tool-specs spec))
               (trigger-specs (cauldron.agent:agent-spec-trigger-specs spec))
               (memory-config (cauldron.agent:agent-spec-memory-config spec))
               (body
                 (forge-layout (format nil "Agent: ~A" name-str)
                   (cauldron.alembic:html
                     ;; Overview card
                     (:div :class "forge-section"
                       (:h2 "Overview")
                       (:table :class "forge-table"
                         (:tr (:td "Name") (:td name-str))
                         (:tr (:td "Description") (:td (if (string= description "") "—" description)))
                         (:tr (:td "Model") (:td model))
                         (:tr (:td "Status")
                              (:td (:span :class (if enabled "forge-status-green" "forge-status-red")
                                          (if enabled "Enabled" "Disabled"))))
                         (:tr (:td "Role") (:td role))
                         (:tr (:td "Max Rounds") (:td (format nil "~D" max-rounds)))
                         (:tr (:td "Token Budget") (:td (if budget (format nil "~:D" budget) "Unlimited")))
                         (:tr (:td "Memory")
                              (:td (if memory-config
                                       (format nil "Table: ~A, Scope: ~A"
                                               (getf memory-config :table)
                                               (getf memory-config :scope))
                                       "None")))))
                     ;; System prompt
                     (when (and system-prompt (> (length system-prompt) 0))
                       `(:div :class "forge-section"
                          (:h2 "System Prompt")
                          (:pre :class "forge-code" ,system-prompt)))
                     ;; Tools
                     (:div :class "forge-section"
                       (:h2 (format nil "Tool Sources (~D)" (length tool-specs)))
                       (if tool-specs
                           `(:table :class "forge-table"
                              (:thead (:tr (:th "Type") (:th "Details")))
                              (:tbody
                                ,@(mapcar
                                    (lambda (tspec)
                                      (let ((type (getf tspec :type)))
                                        `(:tr
                                           (:td ,(string-downcase (symbol-name type)))
                                           (:td ,(case type
                                                   (:from-resources
                                                    (format nil "Resources: ~{~A~^, ~}  Actions: ~{~A~^, ~}"
                                                            (mapcar (lambda (r) (string-downcase (symbol-name r)))
                                                                    (getf tspec :resources))
                                                            (mapcar (lambda (a) (string-downcase (symbol-name a)))
                                                                    (getf tspec :actions))))
                                                   (:from-integration
                                                    (format nil "Integration: ~A  Endpoints: ~{~A~^, ~}"
                                                            (string-downcase (symbol-name (getf tspec :integration)))
                                                            (mapcar (lambda (e) (string-downcase (symbol-name e)))
                                                                    (getf tspec :endpoints))))
                                                   (:from-agent
                                                    (format nil "Agents: ~{~A~^, ~}"
                                                            (mapcar (lambda (a) (string-downcase (symbol-name a)))
                                                                    (getf tspec :agents))))
                                                   (:custom
                                                    (format nil "~A: ~A"
                                                            (string-downcase (symbol-name (getf tspec :name)))
                                                            (or (getf tspec :description) "")))
                                                   (otherwise (format nil "~S" tspec)))))))
                                    tool-specs)))
                           `(:p "No tools configured.")))
                     ;; Triggers
                     (:div :class "forge-section"
                       (:h2 (format nil "Triggers (~D)" (length trigger-specs)))
                       (if trigger-specs
                           `(:table :class "forge-table"
                              (:thead (:tr (:th "Type") (:th "Configuration")))
                              (:tbody
                                ,@(mapcar
                                    (lambda (trigger)
                                      `(:tr
                                         (:td ,(string-downcase (symbol-name (first trigger))))
                                         (:td ,(format nil "~{~S~^ ~}" (rest trigger)))))
                                    trigger-specs)))
                           `(:p "No triggers configured.")))
                     ;; Back link
                     (:div :class "forge-toolbar"
                       (:a :href "/forge/agents" "← Back to agents")))
                   :config config)))
          (cauldron.crucible:conn-put-resp-body
           (cauldron.crucible:conn-put-resp-header
            (cauldron.crucible:conn-put-status conn 200)
            "Content-Type" "text/html; charset=utf-8")
           body)))))
