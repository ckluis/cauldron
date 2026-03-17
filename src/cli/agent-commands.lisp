;;;; src/cli/agent-commands.lisp — CLI commands for agent management
(in-package :cauldron.cli)

(defun %make-row (&rest pairs)
  "Build an alist row from alternating key-value pairs."
  (loop for (k v) on pairs by #'cddr
        collect (cons k v)))

;;; --- Agent List ---

(defcommand "agent list"
  (:description "List all registered agents"
   :options (("--format" :default "table" :description "Output format (table|json)")))
  (let ((agents (cauldron.agent:list-agents)))
    (if (null agents)
        (progn (emit :message "No agents registered." :status "ok") 0)
        (let ((rows (mapcar
                      (lambda (name)
                        (let ((spec (cauldron.agent:find-agent name)))
                          (%make-row
                           "NAME" (string-downcase (symbol-name name))
                           "MODEL" (string-downcase (symbol-name (cauldron.agent:agent-spec-model spec)))
                           "STATUS" (if (cauldron.agent:agent-spec-enabled-p spec) "enabled" "disabled")
                           "TOOLS" (format nil "~D" (length (cauldron.agent:agent-spec-tool-specs spec)))
                           "TRIGGERS" (format nil "~D" (length (cauldron.agent:agent-spec-trigger-specs spec)))
                           "MAX-ROUNDS" (format nil "~D" (cauldron.agent:agent-spec-max-rounds spec))
                           "BUDGET" (let ((b (cauldron.agent:agent-spec-token-budget spec)))
                                      (if b (format nil "~:D" b) "unlimited")))))
                      agents)))
          (emit-table rows :headers '("NAME" "MODEL" "STATUS" "TOOLS" "TRIGGERS" "MAX-ROUNDS" "BUDGET"))
          0))))

;;; --- Agent Invoke ---

(defcommand "agent invoke"
  (:description "Invoke an agent with a message"
   :options (("--name" :required t :description "Agent name")
             ("--message" :required t :description "Message to send")
             ("--scope" :default "_global" :description "Scope key")))
  (let* ((name-str (get-arg args "--name"))
         (message (get-arg args "--message"))
         (scope (get-arg args "--scope" "_global"))
         (agent-name (find (string-upcase name-str)
                           (cauldron.agent:list-agents)
                           :key #'symbol-name :test #'string=)))
    (if (null agent-name)
        (progn (emit-error (format nil "Agent '~A' not found" name-str)) 1)
        (handler-case
            (progn
              (let ((spec (cauldron.agent:find-agent agent-name)))
                (emit :data (cauldron.runtime:ht
                             "agent" name-str
                             "status" "invoked"
                             "model" (string-downcase
                                      (symbol-name (cauldron.agent:agent-spec-model spec)))
                             "message_length" (length message)
                             "scope" scope
                             "note" "Full execution requires a database connection (use with-cli-pool)")
                      :message (format nil "Agent ~A invoked (stub mode)" name-str))
                0))
          (error (e)
            (emit-error (format nil "~A" e))
            1)))))

;;; --- Agent Tools ---

(defcommand "agent tools"
  (:description "List assembled tools for an agent"
   :options (("--name" :required t :description "Agent name")))
  (let* ((name-str (get-arg args "--name"))
         (agent-name (find (string-upcase name-str)
                           (cauldron.agent:list-agents)
                           :key #'symbol-name :test #'string=)))
    (if (null agent-name)
        (progn (emit-error (format nil "Agent '~A' not found" name-str)) 1)
        (handler-case
            (let* ((spec (cauldron.agent:find-agent agent-name))
                   (tools (cauldron.agent:assemble-agent-tools spec))
                   (rows (mapcar
                           (lambda (tool)
                             (%make-row
                              "TOOL" (cauldron.llm:agent-tool-name tool)
                              "DESCRIPTION" (let ((desc (cauldron.llm:agent-tool-description tool)))
                                              (if (> (length desc) 60)
                                                  (concatenate 'string (subseq desc 0 57) "...")
                                                  desc))
                              "PARAMS" (format nil "~D" (length (cauldron.llm:agent-tool-parameters tool)))
                              "DESTRUCTIVE" (if (cauldron.llm:agent-tool-destructive-p tool) "yes" "no")))
                           tools)))
              (if rows
                  (emit-table rows :headers '("TOOL" "DESCRIPTION" "PARAMS" "DESTRUCTIVE"))
                  (format t "No tools assembled for agent ~A~%" name-str))
              0)
          (error (e)
            (emit-error (format nil "~A" e))
            1)))))

;;; --- Agent History ---

(defcommand "agent history"
  (:description "Show agent invocation history (requires database)"
   :options (("--name" :description "Filter by agent name")
             ("--limit" :default "20" :description "Number of entries")))
  (let* ((name-str (get-arg args "--name"))
         (agents (if name-str
                     (let ((agent-name (find (string-upcase name-str)
                                             (cauldron.agent:list-agents)
                                             :key #'symbol-name :test #'string=)))
                       (when agent-name (list agent-name)))
                     (cauldron.agent:list-agents))))
    (if (null agents)
        (progn
          (emit :message (if name-str
                             (format nil "Agent '~A' not found" name-str)
                             "No agents registered.")
                :status "ok")
          0)
        (let ((rows (mapcar
                      (lambda (name)
                        (let ((spec (cauldron.agent:find-agent name)))
                          (%make-row
                           "AGENT" (string-downcase (symbol-name name))
                           "DESCRIPTION" (cauldron.agent:agent-spec-description spec)
                           "STATUS" (if (cauldron.agent:agent-spec-enabled-p spec) "active" "inactive")
                           "NOTE" "Use with-cli-pool for full history")))
                      agents)))
          (emit-table rows :headers '("AGENT" "DESCRIPTION" "STATUS" "NOTE"))
          0))))
