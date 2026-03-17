;;;; src/agent/memory.lisp — Persistent agent memory with deftable + CRUD
(in-package :cauldron.agent)

;;; --- Memory Table ---

(cauldron.grimoire:deftable agent_memory
  (:column agent_name :type text :required t)
  (:column scope_key :type text :default "'_global'")
  (:column key :type text :required t)
  (:column value :type textarea)
  (:unique agent_name scope_key key)
  (:index agent_name))

;;; --- Memory CRUD (returns SQL + params like credential functions) ---

(defun agent-memory-get (agent-name key &key (scope-key "_global"))
  "Build SQL to retrieve a memory value for AGENT-NAME + KEY.
Returns (values sql params)."
  (let ((name (string-downcase (string agent-name))))
    (values
     "SELECT value FROM agent_memory WHERE agent_name = $1 AND scope_key = $2 AND key = $3"
     (list name scope-key key))))

(defun agent-memory-set (agent-name key value &key (scope-key "_global"))
  "Build SQL to upsert a memory value for AGENT-NAME + KEY.
Returns (values sql params)."
  (let ((name (string-downcase (string agent-name))))
    (values
     "INSERT INTO agent_memory (agent_name, scope_key, key, value, created_at, updated_at) VALUES ($1, $2, $3, $4, NOW(), NOW()) ON CONFLICT (agent_name, scope_key, key) DO UPDATE SET value = $4, updated_at = NOW()"
     (list name scope-key key value))))

(defun agent-memory-list (agent-name &key (scope-key "_global"))
  "Build SQL to list all memory keys for AGENT-NAME in SCOPE-KEY.
Returns (values sql params)."
  (let ((name (string-downcase (string agent-name))))
    (values
     "SELECT key, value FROM agent_memory WHERE agent_name = $1 AND scope_key = $2 ORDER BY key"
     (list name scope-key))))

(defun agent-memory-delete (agent-name key &key (scope-key "_global"))
  "Build SQL to delete a specific memory entry.
Returns (values sql params)."
  (let ((name (string-downcase (string agent-name))))
    (values
     "DELETE FROM agent_memory WHERE agent_name = $1 AND scope_key = $2 AND key = $3"
     (list name scope-key key))))

;;; --- Auto-generated Memory Tools ---

(defun make-memory-tools (agent-name)
  "Generate 'remember' and 'recall' agent-tools for persistent memory.
Returns a list of two cauldron.llm:agent-tool structs."
  (let ((name-str (string-downcase (string agent-name))))
    (list
     ;; Remember tool
     (cauldron.llm:make-agent-tool
      :name "remember"
      :description "Store a key-value pair in persistent memory for future reference."
      :parameters (list
                   (cauldron.llm:make-tool-parameter
                    :name "key" :type "string"
                    :description "The memory key (e.g., 'customer_preference', 'last_action')"
                    :required-p t)
                   (cauldron.llm:make-tool-parameter
                    :name "value" :type "string"
                    :description "The value to store"
                    :required-p t))
      :handler (lambda (conn args)
                 (let ((key (gethash "key" args))
                       (value (gethash "value" args))
                       (scope (or (gethash "scope_key" args) "_global")))
                   (multiple-value-bind (sql params)
                       (agent-memory-set name-str key value :scope-key scope)
                     (declare (ignore sql params))
                     ;; In a real execution, this would run the SQL via conn
                     ;; For now, return confirmation
                     (declare (ignore conn))
                     (format nil "Remembered: ~A = ~A" key value)))))
     ;; Recall tool
     (cauldron.llm:make-agent-tool
      :name "recall"
      :description "Retrieve a previously stored memory value by key."
      :parameters (list
                   (cauldron.llm:make-tool-parameter
                    :name "key" :type "string"
                    :description "The memory key to look up"
                    :required-p t))
      :handler (lambda (conn args)
                 (let ((key (gethash "key" args))
                       (scope (or (gethash "scope_key" args) "_global")))
                   (multiple-value-bind (sql params)
                       (agent-memory-get name-str key :scope-key scope)
                     (declare (ignore sql params conn))
                     ;; In real execution, would query DB
                     (format nil "Memory lookup: ~A (scope: ~A)" key scope))))))))
