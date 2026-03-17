;;;; test/agent/memory-test.lisp — Agent memory CRUD SQL tests
(in-package :cauldron.test)

(defsuite :agent-memory)

;;; --- Memory Table Registration ---

(deftest test-agent-memory-table-registered
  (let ((spec (cauldron.grimoire:find-table-spec 'agent_memory)))
    (is-not-nil spec "agent_memory table registered")
    (is (>= (length (cauldron.grimoire:table-spec-columns spec)) 4)
        "Table has at least 4 columns")))

;;; --- Memory Get SQL ---

(deftest test-memory-get-sql
  (multiple-value-bind (sql params)
      (cauldron.agent:agent-memory-get 'support-bot "preference")
    (is (search "SELECT value FROM agent_memory" sql) "SQL contains SELECT")
    (is (search "agent_name = $1" sql) "Filters by agent name")
    (is (search "scope_key = $2" sql) "Filters by scope")
    (is (search "key = $3" sql) "Filters by key")
    (is-equal "support-bot" (first params) "Agent name downcased")
    (is-equal "_global" (second params) "Default scope")
    (is-equal "preference" (third params))))

(deftest test-memory-get-custom-scope
  (multiple-value-bind (sql params)
      (cauldron.agent:agent-memory-get 'bot "key1" :scope-key "company-42")
    (declare (ignore sql))
    (is-equal "company-42" (second params) "Custom scope key")))

;;; --- Memory Set SQL ---

(deftest test-memory-set-sql
  (multiple-value-bind (sql params)
      (cauldron.agent:agent-memory-set 'bot "color" "blue")
    (is (search "INSERT INTO agent_memory" sql) "SQL contains INSERT")
    (is (search "ON CONFLICT" sql) "SQL contains upsert")
    (is-equal "bot" (first params))
    (is-equal "_global" (second params))
    (is-equal "color" (third params))
    (is-equal "blue" (fourth params))))

;;; --- Memory List SQL ---

(deftest test-memory-list-sql
  (multiple-value-bind (sql params)
      (cauldron.agent:agent-memory-list 'helper)
    (is (search "SELECT key, value FROM agent_memory" sql))
    (is (search "ORDER BY key" sql))
    (is-equal "helper" (first params))
    (is-equal "_global" (second params))))

;;; --- Memory Delete SQL ---

(deftest test-memory-delete-sql
  (multiple-value-bind (sql params)
      (cauldron.agent:agent-memory-delete 'bot "old-key")
    (is (search "DELETE FROM agent_memory" sql))
    (is (search "agent_name = $1" sql))
    (is (search "key = $3" sql))
    (is-equal "bot" (first params))
    (is-equal "old-key" (third params))))

;;; --- Scope Isolation ---

(deftest test-memory-scope-isolation
  (multiple-value-bind (sql1 params1)
      (cauldron.agent:agent-memory-get 'bot "key" :scope-key "company-1")
    (multiple-value-bind (sql2 params2)
        (cauldron.agent:agent-memory-get 'bot "key" :scope-key "company-2")
      (is-equal sql1 sql2 "Same SQL structure")
      (is-equal "company-1" (second params1))
      (is-equal "company-2" (second params2))
      (is (not (equal (second params1) (second params2)))
          "Different scope keys produce different params"))))

;;; --- Memory Tools Generation ---

(deftest test-make-memory-tools
  (let ((tools (cauldron.agent::make-memory-tools 'test-agent)))
    (is-equal 2 (length tools) "Two memory tools generated")
    (let ((remember (first tools))
          (recall (second tools)))
      (is-equal "remember" (cauldron.llm:agent-tool-name remember))
      (is-equal "recall" (cauldron.llm:agent-tool-name recall))
      ;; Remember has key + value params
      (is-equal 2 (length (cauldron.llm:agent-tool-parameters remember)))
      ;; Recall has key param
      (is-equal 1 (length (cauldron.llm:agent-tool-parameters recall)))
      ;; Handlers are functions
      (is (functionp (cauldron.llm:agent-tool-handler remember)))
      (is (functionp (cauldron.llm:agent-tool-handler recall))))))
