;;;; src/agent/tools.lisp — Auto-tool derivation from resources + integrations
(in-package :cauldron.agent)

;;; --- Attribute → Tool Parameter Mapping ---

(defun attribute-type-to-tool-type (attr-type)
  "Map a Reagent attribute type keyword to a tool parameter type string."
  (case attr-type
    ((:string :text :keyword :uuid :timestamp :date) "string")
    (:integer "integer")
    (:float "number")
    (:boolean "boolean")
    (otherwise "string")))

(defun attribute-to-tool-parameter (attr-def)
  "Convert a cauldron.reagent:attribute-def to a cauldron.llm:tool-parameter."
  (let ((name (string-downcase (symbol-name (cauldron.reagent:attribute-def-name attr-def))))
        (type (attribute-type-to-tool-type (cauldron.reagent:attribute-def-type attr-def)))
        (one-of (cauldron.reagent:attribute-def-one-of attr-def)))
    (cauldron.llm:make-tool-parameter
     :name name
     :type type
     :description (format nil "~A (~A)" name type)
     :required-p (cauldron.reagent:attribute-def-required attr-def)
     :enum (when one-of
             (mapcar (lambda (k) (string-downcase (symbol-name k))) one-of)))))

;;; --- Resource Tool Generation ---

(defun %make-resource-action-handler (resource-name action-name role)
  "Create a handler function that delegates to run-action with the agent's role."
  (lambda (conn args)
    (declare (ignore conn))
    (let ((actor (list :role (intern (string-upcase role) :keyword)))
          (params (let ((plist nil))
                    (when (hash-table-p args)
                      (maphash (lambda (k v)
                                 (push (intern (string-upcase k) :keyword) plist)
                                 (push v plist))
                               args))
                    (nreverse plist))))
      (handler-case
          (let ((result (cauldron.reagent:run-action resource-name action-name actor params)))
            (if (stringp result)
                result
                (format nil "~S" result)))
        (cauldron.reagent:action-error (e)
          (format nil "Error: ~A" e))))))

(defun generate-resource-tools (resource-names actions role)
  "Generate agent-tools from Reagent resource metadata.
RESOURCE-NAMES is a list of resource name symbols.
ACTIONS is a list of action keywords like (:read :create :update :delete).
ROLE is the agent's role string for authorization.
Returns a list of cauldron.llm:agent-tool structs."
  (let ((tools nil))
    (dolist (rname resource-names)
      (let ((class (cauldron.reagent:find-resource-class rname)))
        (unless class
          (error "Resource ~S not found in registry" rname))
        (let* ((attrs (cauldron.reagent:resource-attributes class))
               (attr-params (mapcar #'attribute-to-tool-parameter attrs))
               (resource-str (string-downcase (symbol-name rname))))
          ;; :read → list + get tools
          (when (member :read actions)
            (push (cauldron.llm:make-agent-tool
                   :name (format nil "list_~A" resource-str)
                   :description (format nil "List ~A records with optional filters" resource-str)
                   :parameters (list
                                (cauldron.llm:make-tool-parameter
                                 :name "limit" :type "integer"
                                 :description "Maximum number of records to return")
                                (cauldron.llm:make-tool-parameter
                                 :name "offset" :type "integer"
                                 :description "Number of records to skip"))
                   :handler (%make-resource-action-handler rname :read role))
                  tools)
            (push (cauldron.llm:make-agent-tool
                   :name (format nil "get_~A" resource-str)
                   :description (format nil "Get a single ~A by ID" resource-str)
                   :parameters (list
                                (cauldron.llm:make-tool-parameter
                                 :name "id" :type "integer"
                                 :description (format nil "The ~A ID" resource-str)
                                 :required-p t))
                   :handler (%make-resource-action-handler rname :read role))
                  tools))
          ;; :create → create tool
          (when (member :create actions)
            (push (cauldron.llm:make-agent-tool
                   :name (format nil "create_~A" resource-str)
                   :description (format nil "Create a new ~A" resource-str)
                   :parameters attr-params
                   :handler (%make-resource-action-handler rname :create role))
                  tools))
          ;; :update → update tool
          (when (member :update actions)
            (push (cauldron.llm:make-agent-tool
                   :name (format nil "update_~A" resource-str)
                   :description (format nil "Update an existing ~A" resource-str)
                   :parameters (cons (cauldron.llm:make-tool-parameter
                                      :name "id" :type "integer"
                                      :description (format nil "The ~A ID to update" resource-str)
                                      :required-p t)
                                     attr-params)
                   :handler (%make-resource-action-handler rname :update role))
                  tools))
          ;; :delete → delete tool
          (when (member :delete actions)
            (push (cauldron.llm:make-agent-tool
                   :name (format nil "delete_~A" resource-str)
                   :description (format nil "Delete a ~A by ID" resource-str)
                   :parameters (list
                                (cauldron.llm:make-tool-parameter
                                 :name "id" :type "integer"
                                 :description (format nil "The ~A ID to delete" resource-str)
                                 :required-p t))
                   :handler (%make-resource-action-handler rname :delete role)
                   :destructive-p t)
                  tools)))))
    (nreverse tools)))

;;; --- Integration Tool Generation ---

(defun generate-integration-tools (integration-name endpoint-names)
  "Generate agent-tools from integration endpoint specs.
INTEGRATION-NAME is the integration symbol.
ENDPOINT-NAMES is a list of endpoint name keywords.
Returns a list of cauldron.llm:agent-tool structs."
  (let ((spec (cauldron.integration:find-integration integration-name)))
    (unless spec
      (error "Integration ~S not found in registry" integration-name))
    (let ((tools nil)
          (int-str (string-downcase (symbol-name integration-name))))
      (dolist (ep-name endpoint-names)
        (let ((ep (find ep-name (cauldron.integration:integration-spec-endpoints spec)
                        :key #'cauldron.integration:endpoint-spec-name)))
          (unless ep
            (error "Endpoint ~S not found in integration ~S" ep-name integration-name))
          (let* ((method (cauldron.integration:endpoint-spec-method ep))
                 (doc (cauldron.integration:endpoint-spec-doc ep))
                 (ep-str (string-downcase (symbol-name ep-name)))
                 (tool-name (format nil "~A_~A" int-str ep-str)))
            (push (cauldron.llm:make-agent-tool
                   :name tool-name
                   :description (or doc (format nil "Call ~A endpoint ~A (~A)"
                                                int-str ep-str method))
                   :parameters (list
                                (cauldron.llm:make-tool-parameter
                                 :name "params" :type "string"
                                 :description "JSON-encoded parameters for the API call"))
                   :handler (let ((int-s int-str) (ep-s ep-str))
                              (lambda (conn args)
                                (declare (ignore conn))
                                ;; In real execution, would call call-integration
                                (format nil "Would call ~A:~A with ~S" int-s ep-s args)))
                   :destructive-p (if (member method '(:post :put :patch :delete)) t nil))
                  tools))))
      (nreverse tools))))

;;; --- Agent-to-Agent Tool Generation ---

(defun generate-agent-tool (target-agent-name)
  "Generate a tool that invokes another agent by name.
The calling agent can send a message to TARGET-AGENT-NAME.
Cycle detection is handled by *agent-call-chain* in invoke-agent."
  (let ((name-str (string-downcase (symbol-name target-agent-name))))
    (cauldron.llm:make-agent-tool
     :name (format nil "ask_agent_~A" name-str)
     :description (format nil "Send a message to the ~A agent and get a response." name-str)
     :parameters (list
                  (cauldron.llm:make-tool-parameter
                   :name "message" :type "string"
                   :description (format nil "The message to send to ~A" name-str)
                   :required-p t))
     :handler (lambda (conn args)
                (let ((message (if (hash-table-p args)
                                   (gethash "message" args)
                                   "")))
                  (handler-case
                      (multiple-value-bind (response conv-id)
                          (invoke-agent target-agent-name message conn
                                        :trigger-type "agent"
                                        :trigger-source (format nil "agent-tool"))
                        (declare (ignore conv-id))
                        response)
                    (error (e)
                      (format nil "Error calling agent ~A: ~A" name-str e))))))))

;;; --- Tool Caching ---

(defvar *tool-cache* (make-hash-table :test 'eq)
  "Cache of assembled tools per agent: name → (timestamp . tools-list).")

(defvar *tool-cache-ttl* 300
  "Tool cache time-to-live in seconds. Default 300 (5 minutes).")

(defvar *tool-cache-lock* (cauldron.runtime:make-lock "tool-cache")
  "Lock for thread-safe tool cache access.")

(defun cached-assemble-agent-tools (agent-spec)
  "Assemble tools with caching. Returns cached tools if within TTL, else rebuilds."
  (let ((name (agent-spec-name agent-spec))
        (now (get-universal-time)))
    (cauldron.runtime:with-lock (*tool-cache-lock*)
      (let ((entry (gethash name *tool-cache*)))
        (if (and entry
                 (< (- now (car entry)) *tool-cache-ttl*))
            (cdr entry)
            (let ((tools (assemble-agent-tools agent-spec)))
              (setf (gethash name *tool-cache*) (cons now tools))
              tools))))))

(defun invalidate-agent-tools (&optional agent-name)
  "Clear the tool cache for AGENT-NAME, or all agents if nil."
  (cauldron.runtime:with-lock (*tool-cache-lock*)
    (if agent-name
        (remhash agent-name *tool-cache*)
        (clrhash *tool-cache*))))

;;; --- Tool Assembly ---

(defun assemble-agent-tools (agent-spec)
  "Assemble all tools for an agent from its spec.
Iterates tool-specs, dispatching on type to generate tools.
Appends memory tools if :memory is configured.
Returns a list of cauldron.llm:agent-tool structs."
  (let ((tools nil)
        (role (agent-spec-role agent-spec)))
    ;; Process each tool spec
    (dolist (tspec (agent-spec-tool-specs agent-spec))
      (let ((type (getf tspec :type)))
        (case type
          (:from-resources
           (setf tools (nconc tools
                              (generate-resource-tools
                               (getf tspec :resources)
                               (getf tspec :actions)
                               role))))
          (:from-integration
           (setf tools (nconc tools
                              (generate-integration-tools
                               (getf tspec :integration)
                               (getf tspec :endpoints)))))
          (:from-agent
           (dolist (agent-name (getf tspec :agents))
             (push (generate-agent-tool agent-name) tools)))
          (:custom
           (let ((name (getf tspec :name))
                 (desc (getf tspec :description))
                 (params-spec (getf tspec :params))
                 (handler (getf tspec :handler)))
             (push (cauldron.llm:make-agent-tool
                    :name (string-downcase (symbol-name name))
                    :description (or desc "")
                    :parameters (mapcar (lambda (p)
                                          (destructuring-bind (pname ptype &rest opts) p
                                            (cauldron.llm:make-tool-parameter
                                             :name (string-downcase (symbol-name pname))
                                             :type (string-downcase (symbol-name ptype))
                                             :required-p (member :required opts))))
                                        (or params-spec nil))
                    :handler (or handler
                                 (lambda (conn args)
                                   (declare (ignore conn args))
                                   "Tool not implemented")))
                   tools))))))
    ;; Add memory tools if configured
    (when (agent-spec-memory-config agent-spec)
      (setf tools (nconc tools (make-memory-tools (agent-spec-name agent-spec)))))
    tools))
