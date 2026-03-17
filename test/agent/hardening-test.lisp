;;;; test/agent/hardening-test.lisp — Phase 39B: Agent hardening tests
;;;; Token budgets, agent-to-agent, tool caching, SSE streaming,
;;;; enhanced templates, CLI commands, Forge dashboard
(in-package :cauldron.test)

(defsuite :agent-hardening)

;;; ============================================================
;;; Token Budget
;;; ============================================================

(deftest test-agent-spec-token-budget-default-nil
  (let ((spec (cauldron.agent:make-agent-spec :name 'test-budget)))
    (is-nil (cauldron.agent:agent-spec-token-budget spec) "Default token budget is nil")))

(deftest test-agent-spec-token-budget-set
  (let ((spec (cauldron.agent:make-agent-spec :name 'test-budget :token-budget 10000)))
    (is-equal 10000 (cauldron.agent:agent-spec-token-budget spec))))

(deftest test-defagent-token-budget-clause
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq)))
    (cauldron.agent:defagent budget-agent
      (:description "Budget test")
      (:token-budget 50000))
    (let ((spec (cauldron.agent:find-agent 'budget-agent)))
      (is-not-nil spec)
      (is-equal 50000 (cauldron.agent:agent-spec-token-budget spec)))))

(deftest test-check-agent-budget-within-limit
  ;; Should not signal when within budget
  (handler-case
      (progn
        (cauldron.agent:check-agent-budget 'test 10000 5000)
        (assertion-pass))
    (cauldron.agent:agent-budget-exceeded ()
      (assertion-fail "Should not signal within budget"))))

(deftest test-check-agent-budget-exceeded
  (signals-condition cauldron.agent:agent-budget-exceeded
    (cauldron.agent:check-agent-budget 'test 10000 15000)
    "Should signal when over budget"))

(deftest test-check-agent-budget-nil-unlimited
  ;; nil budget means unlimited
  (handler-case
      (progn
        (cauldron.agent:check-agent-budget 'test nil 999999)
        (assertion-pass))
    (error ()
      (assertion-fail "nil budget should allow unlimited tokens"))))

(deftest test-budget-exceeded-condition-slots
  (handler-case
      (cauldron.agent:check-agent-budget 'my-agent 1000 2000)
    (cauldron.agent:agent-budget-exceeded (c)
      (is-equal 'my-agent (cauldron.agent::agent-budget-exceeded-agent c))
      (is-equal 1000 (cauldron.agent::agent-budget-exceeded-budget c))
      (is-equal 2000 (cauldron.agent::agent-budget-exceeded-used c)))))

(deftest test-make-budget-checked-provider-nil-budget
  (let* ((base (cauldron.llm:make-mock-provider '("test")))
         (wrapped (cauldron.agent:make-budget-checked-provider base 'test nil)))
    ;; nil budget returns original provider
    (is-equal (cauldron.llm:llm-provider-name base)
              (cauldron.llm:llm-provider-name wrapped)
              "nil budget returns same provider")))

(deftest test-make-budget-checked-provider-wraps
  (let* ((base (cauldron.llm:make-mock-provider '("response1")))
         (wrapped (cauldron.agent:make-budget-checked-provider base 'test 100000)))
    ;; Wrapped provider should have same name but different call-fn
    (is-equal (cauldron.llm:llm-provider-name base)
              (cauldron.llm:llm-provider-name wrapped)
              "Wrapped provider keeps name")))

;;; ============================================================
;;; Agent-to-Agent Communication
;;; ============================================================

(deftest test-defagent-from-agent-tool-clause
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq)))
    (cauldron.agent:defagent delegator-agent
      (:description "Delegates to other agents")
      (:tools
        (from-agent helper-agent worker-agent)))
    (let* ((spec (cauldron.agent:find-agent 'delegator-agent))
           (tspecs (cauldron.agent:agent-spec-tool-specs spec)))
      (is-equal 1 (length tspecs))
      (is-equal :from-agent (getf (first tspecs) :type))
      (is-equal '(helper-agent worker-agent) (getf (first tspecs) :agents)))))

(deftest test-generate-agent-tool
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq)))
    ;; Register a target agent so generate-agent-tool can reference it
    (cauldron.agent:defagent target-bot
      (:description "Target bot")
      (:model :mock))
    (let ((tool (cauldron.agent:generate-agent-tool 'target-bot)))
      (is-equal "ask_agent_target-bot" (cauldron.llm:agent-tool-name tool))
      (is (search "target-bot" (cauldron.llm:agent-tool-description tool)))
      (is-equal 1 (length (cauldron.llm:agent-tool-parameters tool)))
      (is-equal "message" (cauldron.llm:tool-parameter-name
                            (first (cauldron.llm:agent-tool-parameters tool)))))))

(deftest test-assemble-agent-tools-with-from-agent
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq)))
    (cauldron.agent:defagent helper-bot
      (:description "Helper")
      (:model :mock))
    (cauldron.agent:defagent main-bot
      (:description "Main bot with agent tools")
      (:tools
        (from-agent helper-bot)))
    (let* ((spec (cauldron.agent:find-agent 'main-bot))
           (tools (cauldron.agent:assemble-agent-tools spec)))
      (is-equal 1 (length tools))
      (is-equal "ask_agent_helper-bot"
                (cauldron.llm:agent-tool-name (first tools))))))

(deftest test-agent-call-chain-default
  (is-nil cauldron.agent:*agent-call-chain* "Default call chain is empty"))

(deftest test-agent-max-call-depth-default
  (is-equal 5 cauldron.agent:*agent-max-call-depth* "Default max depth is 5"))

(deftest test-invoke-agent-cycle-detection
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq))
        (cauldron.agent:*agent-call-chain* '(cyclic-agent)))
    (cauldron.agent:defagent cyclic-agent
      (:description "Would cause a cycle")
      (:model :mock))
    (signals-condition error
      (cauldron.agent:invoke-agent 'cyclic-agent "hello" nil)
      "Should detect cycle")))

(deftest test-invoke-agent-depth-limit
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq))
        (cauldron.agent:*agent-call-chain* '(a b c d e))
        (cauldron.agent:*agent-max-call-depth* 5))
    (cauldron.agent:defagent deep-agent
      (:description "Too deep")
      (:model :mock))
    (signals-condition error
      (cauldron.agent:invoke-agent 'deep-agent "hello" nil)
      "Should signal depth exceeded")))

;;; ============================================================
;;; Tool Caching
;;; ============================================================

(deftest test-tool-cache-defaults
  (is (hash-table-p cauldron.agent:*tool-cache*) "Tool cache is a hash table")
  (is-equal 300 cauldron.agent:*tool-cache-ttl* "Default TTL is 300 seconds"))

(deftest test-cached-assemble-agent-tools-returns-tools
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq))
        (cauldron.agent:*tool-cache* (make-hash-table :test 'eq)))
    (cauldron.agent:defagent cache-test-agent
      (:description "Cache test")
      (:tools (tool :ping :description "Ping")))
    (let* ((spec (cauldron.agent:find-agent 'cache-test-agent))
           (tools (cauldron.agent:cached-assemble-agent-tools spec)))
      (is-equal 1 (length tools))
      (is-equal "ping" (cauldron.llm:agent-tool-name (first tools))))))

(deftest test-cached-assemble-returns-same-list
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq))
        (cauldron.agent:*tool-cache* (make-hash-table :test 'eq)))
    (cauldron.agent:defagent cache-same-agent
      (:description "Cache same test")
      (:tools (tool :pong :description "Pong")))
    (let* ((spec (cauldron.agent:find-agent 'cache-same-agent))
           (tools1 (cauldron.agent:cached-assemble-agent-tools spec))
           (tools2 (cauldron.agent:cached-assemble-agent-tools spec)))
      (is (eq tools1 tools2) "Second call returns cached (eq) list"))))

(deftest test-invalidate-agent-tools-specific
  (let ((cauldron.agent:*tool-cache* (make-hash-table :test 'eq)))
    (setf (gethash 'foo cauldron.agent:*tool-cache*) (cons 0 '(:test)))
    (setf (gethash 'bar cauldron.agent:*tool-cache*) (cons 0 '(:test2)))
    (cauldron.agent:invalidate-agent-tools 'foo)
    (is-nil (gethash 'foo cauldron.agent:*tool-cache*) "foo removed")
    (is-not-nil (gethash 'bar cauldron.agent:*tool-cache*) "bar preserved")))

(deftest test-invalidate-agent-tools-all
  (let ((cauldron.agent:*tool-cache* (make-hash-table :test 'eq)))
    (setf (gethash 'foo cauldron.agent:*tool-cache*) (cons 0 '(:test)))
    (setf (gethash 'bar cauldron.agent:*tool-cache*) (cons 0 '(:test2)))
    (cauldron.agent:invalidate-agent-tools)
    (is-equal 0 (hash-table-count cauldron.agent:*tool-cache*) "All cleared")))

;;; ============================================================
;;; SSE Streaming
;;; ============================================================

(deftest test-agent-sse-handler-returns-function
  (let ((handler (cauldron.agent:agent-sse-handler 'some-agent)))
    (is (functionp handler) "SSE handler is a function")))

(deftest test-agent-sse-handler-not-found
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq)))
    (let* ((handler (cauldron.agent:agent-sse-handler 'missing-agent))
           (conn (cauldron.crucible:make-conn
                  :method :post :path "/agents/missing/sse"
                  :body "test")))
      ;; Should return a 404 conn
      (let ((result (funcall handler conn)))
        (is-equal 404 (cauldron.crucible:conn-status result))))))

(deftest test-agent-sse-handler-disabled
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq)))
    (cauldron.agent:defagent sse-disabled-agent
      (:description "SSE disabled test")
      (:enabled nil))
    (let* ((handler (cauldron.agent:agent-sse-handler 'sse-disabled-agent))
           (conn (cauldron.crucible:make-conn
                  :method :post :path "/agents/sse-disabled/sse"
                  :body "test")))
      (let ((result (funcall handler conn)))
        (is-equal 503 (cauldron.crucible:conn-status result))))))

(deftest test-agent-sse-handler-success
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq)))
    (cauldron.agent:defagent sse-ok-agent
      (:description "SSE success test")
      (:model :mock))
    (let* ((handler (cauldron.agent:agent-sse-handler 'sse-ok-agent))
           (conn (cauldron.crucible:make-conn
                  :method :post :path "/agents/sse-ok/sse"
                  :body "hello world")))
      (let ((result (funcall handler conn)))
        (is-equal 200 (cauldron.crucible:conn-status result))
        ;; Check content type is SSE
        (let ((ct (cdr (assoc "Content-Type" (cauldron.crucible:conn-resp-headers result) :test #'string=))))
          (is (search "text/event-stream" ct) "Content-Type is SSE"))
        ;; Check body contains SSE events
        (let ((body (cauldron.crucible:conn-resp-body result)))
          (is (search "event: start" body) "Contains start event")
          (is (search "event: message" body) "Contains message event")
          (is (search "event: done" body) "Contains done event"))))))

;;; ============================================================
;;; Enhanced Template Interpolation
;;; ============================================================

(deftest test-interpolate-date
  (let ((result (cauldron.agent::interpolate-system-prompt
                 "Today is {{date}}." 'test nil)))
    (is (search "-" result) "Date contains dash separator")
    (is (not (search "{{date}}" result)) "Date placeholder replaced")))

(deftest test-interpolate-time
  (let ((result (cauldron.agent::interpolate-system-prompt
                 "Time: {{time}}" 'test nil)))
    (is (search ":" result) "Time contains colon separator")
    (is (not (search "{{time}}" result)) "Time placeholder replaced")))

(deftest test-interpolate-config-key
  ;; Set a test env var
  (sb-posix:setenv "CAULDRON_TEST_PROMPT_VAR" "test-value" 1)
  (unwind-protect
      (let ((result (cauldron.agent::interpolate-system-prompt
                     "Value: {{config:CAULDRON_TEST_PROMPT_VAR}}" 'test nil)))
        (is (search "test-value" result) "Config var interpolated"))
    (sb-posix:unsetenv "CAULDRON_TEST_PROMPT_VAR")))

(deftest test-interpolate-config-key-missing
  (let ((result (cauldron.agent::interpolate-system-prompt
                 "Value: {{config:NONEXISTENT_CAULDRON_VAR_XYZ}}" 'test nil)))
    (is (search "Value: " result) "Missing config var resolves to empty string")
    (is (not (search "{{config:" result)) "Config placeholder replaced")))

(deftest test-interpolate-all-vars
  (sb-posix:setenv "CAULDRON_TEST_MODE" "production" 1)
  (unwind-protect
      (let ((result (cauldron.agent::interpolate-system-prompt
                     "I am {{agent_name}}, scope={{scope_key}}, date={{date}}, mode={{config:CAULDRON_TEST_MODE}}"
                     'my-bot "company-1")))
        (is (search "my-bot" result) "agent_name interpolated")
        (is (search "company-1" result) "scope_key interpolated")
        (is (not (search "{{date}}" result)) "date interpolated")
        (is (search "production" result) "config var interpolated"))
    (sb-posix:unsetenv "CAULDRON_TEST_MODE")))

;;; ============================================================
;;; CLI Agent Commands
;;; ============================================================

(deftest test-cli-agent-list-command-exists
  (is-not-nil (cauldron.cli:find-command "agent list") "agent list command registered"))

(deftest test-cli-agent-invoke-command-exists
  (is-not-nil (cauldron.cli:find-command "agent invoke") "agent invoke command registered"))

(deftest test-cli-agent-tools-command-exists
  (is-not-nil (cauldron.cli:find-command "agent tools") "agent tools command registered"))

(deftest test-cli-agent-history-command-exists
  (is-not-nil (cauldron.cli:find-command "agent history") "agent history command registered"))

(deftest test-cli-agent-list-empty
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq))
        (exit-code nil))
    (setf exit-code
          (cauldron.cli:dispatch-command '("agent" "list")))
    (is-equal 0 exit-code "agent list returns 0 with no agents")))

(deftest test-cli-agent-list-with-agents
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq)))
    (cauldron.agent:defagent cli-test-agent
      (:description "CLI test")
      (:model :mock)
      (:token-budget 5000))
    (let ((exit-code (cauldron.cli:dispatch-command '("agent" "list"))))
      (is-equal 0 exit-code "agent list returns 0 with agents"))))

(deftest test-cli-agent-invoke-missing-name
  (let ((exit-code (cauldron.cli:dispatch-command '("agent" "invoke" "--message" "hi"))))
    (is-equal 1 exit-code "Missing --name returns 1")))

(deftest test-cli-agent-tools-with-agent
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq)))
    (cauldron.agent:defagent cli-tools-agent
      (:description "Tools CLI test")
      (:tools (tool :echo :description "Echo back")))
    (let ((exit-code (cauldron.cli:dispatch-command
                       '("agent" "tools" "--name" "cli-tools-agent"))))
      (is-equal 0 exit-code "agent tools returns 0"))))

;;; ============================================================
;;; Forge Agent Dashboard
;;; ============================================================

(deftest test-forge-agents-list-view
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq)))
    (cauldron.agent:defagent forge-test-agent
      (:description "Forge dashboard test")
      (:model :claude-haiku)
      (:token-budget 10000))
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/forge/agents"))
           (result (cauldron.forge:agents-list-view conn nil)))
      (is-equal 200 (cauldron.crucible:conn-status result))
      (let ((body (cauldron.crucible:conn-resp-body result)))
        (is (search "forge-test-agent" body) "Agent name in output")
        (is (search "claude-haiku" body) "Model in output")
        (is (search "Enabled" body) "Status in output")))))

(deftest test-forge-agents-list-view-empty
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq)))
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/forge/agents"))
           (result (cauldron.forge:agents-list-view conn nil)))
      (is-equal 200 (cauldron.crucible:conn-status result))
      (let ((body (cauldron.crucible:conn-resp-body result)))
        (is (search "No agents" body) "Empty state message")))))

(deftest test-forge-agent-detail-view
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq)))
    (cauldron.agent:defagent detail-test-agent
      (:description "Detailed agent view test")
      (:model :claude-opus)
      (:system-prompt "You are {{agent_name}}.")
      (:tools (tool :ping :description "Ping test"))
      (:triggers (:pubsub "test.event"))
      (:memory :table agent_memory :scope :per-company)
      (:role "admin")
      (:max-rounds 15)
      (:token-budget 25000))
    (let* ((conn (cauldron.crucible:make-conn
                  :method :get :path "/forge/agents/detail-test-agent"
                  :params (list (cons "name" "detail-test-agent"))))
           (result (cauldron.forge:agent-detail-view conn nil)))
      (is-equal 200 (cauldron.crucible:conn-status result))
      (let ((body (cauldron.crucible:conn-resp-body result)))
        (is (search "detail-test-agent" body) "Agent name in detail")
        (is (search "claude-opus" body) "Model in detail")
        (is (search "admin" body) "Role in detail")
        (is (search "25,000" body) "Token budget in detail")
        (is (search "System Prompt" body) "System prompt section")
        (is (search "Tool Sources" body) "Tool sources section")
        (is (search "Triggers" body) "Triggers section")))))

(deftest test-forge-agent-detail-not-found
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq)))
    (let* ((conn (cauldron.crucible:make-conn
                  :method :get :path "/forge/agents/nonexistent"
                  :params (list (cons "name" "nonexistent"))))
           (result (cauldron.forge:agent-detail-view conn nil)))
      (is-equal 404 (cauldron.crucible:conn-status result)))))
