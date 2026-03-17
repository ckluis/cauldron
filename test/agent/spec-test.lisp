;;;; test/agent/spec-test.lisp — Agent spec, defagent macro, and registry tests
(in-package :cauldron.test)

(defsuite :agent-spec)

;;; --- Struct Creation ---

(deftest test-agent-spec-struct-defaults
  (let ((spec (cauldron.agent:make-agent-spec :name 'test-agent)))
    (is-equal 'test-agent (cauldron.agent:agent-spec-name spec))
    (is-equal "" (cauldron.agent:agent-spec-description spec))
    (is-equal :claude-sonnet (cauldron.agent:agent-spec-model spec))
    (is-equal "" (cauldron.agent:agent-spec-system-prompt spec))
    (is-nil (cauldron.agent:agent-spec-tool-specs spec))
    (is-nil (cauldron.agent:agent-spec-trigger-specs spec))
    (is-nil (cauldron.agent:agent-spec-memory-config spec))
    (is-equal "member" (cauldron.agent:agent-spec-role spec))
    (is-equal 10 (cauldron.agent:agent-spec-max-rounds spec))
    (is (cauldron.agent:agent-spec-enabled-p spec) "Enabled by default")))

(deftest test-agent-spec-struct-custom-values
  (let ((spec (cauldron.agent:make-agent-spec
               :name 'custom-bot
               :description "A custom bot"
               :model :claude-opus
               :system-prompt "You are helpful."
               :role "admin"
               :max-rounds 25
               :enabled-p nil)))
    (is-equal 'custom-bot (cauldron.agent:agent-spec-name spec))
    (is-equal "A custom bot" (cauldron.agent:agent-spec-description spec))
    (is-equal :claude-opus (cauldron.agent:agent-spec-model spec))
    (is-equal "You are helpful." (cauldron.agent:agent-spec-system-prompt spec))
    (is-equal "admin" (cauldron.agent:agent-spec-role spec))
    (is-equal 25 (cauldron.agent:agent-spec-max-rounds spec))
    (is-false (cauldron.agent:agent-spec-enabled-p spec))))

;;; --- Registry ---

(deftest test-agent-registry-find-and-list
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq)))
    ;; Empty registry
    (is-nil (cauldron.agent:find-agent 'nonexistent))
    (is-nil (cauldron.agent:list-agents))
    ;; Register manually
    (setf (gethash 'bot-a cauldron.agent:*agent-registry*)
          (cauldron.agent:make-agent-spec :name 'bot-a))
    (setf (gethash 'bot-b cauldron.agent:*agent-registry*)
          (cauldron.agent:make-agent-spec :name 'bot-b))
    ;; Find
    (is-not-nil (cauldron.agent:find-agent 'bot-a))
    (is-equal 'bot-a (cauldron.agent:agent-spec-name (cauldron.agent:find-agent 'bot-a)))
    ;; List (sorted)
    (let ((agents (cauldron.agent:list-agents)))
      (is-equal 2 (length agents))
      (is-equal 'bot-a (first agents))
      (is-equal 'bot-b (second agents)))))

;;; --- defagent Macro ---

(deftest test-defagent-basic
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq)))
    (cauldron.agent:defagent test-basic-agent
      (:description "A test agent")
      (:model :claude-haiku)
      (:system-prompt "Be concise.")
      (:role "viewer")
      (:max-rounds 5))
    (let ((spec (cauldron.agent:find-agent 'test-basic-agent)))
      (is-not-nil spec "Agent registered")
      (is-equal "A test agent" (cauldron.agent:agent-spec-description spec))
      (is-equal :claude-haiku (cauldron.agent:agent-spec-model spec))
      (is-equal "Be concise." (cauldron.agent:agent-spec-system-prompt spec))
      (is-equal "viewer" (cauldron.agent:agent-spec-role spec))
      (is-equal 5 (cauldron.agent:agent-spec-max-rounds spec)))))

(deftest test-defagent-with-tools
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq)))
    (cauldron.agent:defagent tools-agent
      (:description "Agent with tools")
      (:tools
        (tool :ping :description "Ping test" :params ((target :string :required)))))
    (let* ((spec (cauldron.agent:find-agent 'tools-agent))
           (tspecs (cauldron.agent:agent-spec-tool-specs spec)))
      (is-equal 1 (length tspecs))
      (is-equal :custom (getf (first tspecs) :type))
      (is-equal :ping (getf (first tspecs) :name))
      (is-equal "Ping test" (getf (first tspecs) :description)))))

(deftest test-defagent-with-triggers
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq)))
    (cauldron.agent:defagent triggered-agent
      (:description "Agent with triggers")
      (:triggers
        (:pubsub "events.new")
        (:http :post "/agents/test")
        (:schedule 3600 :task :hourly-check)))
    (let* ((spec (cauldron.agent:find-agent 'triggered-agent))
           (triggers (cauldron.agent:agent-spec-trigger-specs spec)))
      (is-equal 3 (length triggers))
      (is-equal :pubsub (first (first triggers)))
      (is-equal :http (first (second triggers)))
      (is-equal :schedule (first (third triggers))))))

(deftest test-defagent-with-memory
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq)))
    (cauldron.agent:defagent memory-agent
      (:description "Agent with memory")
      (:memory :table agent_memory :scope :per-company))
    (let* ((spec (cauldron.agent:find-agent 'memory-agent))
           (mem (cauldron.agent:agent-spec-memory-config spec)))
      (is-not-nil mem "Memory config present")
      (is-equal :table (first mem))
      (is-equal 'agent_memory (second mem)))))

(deftest test-defagent-with-resource-tools
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq)))
    (cauldron.agent:defagent resource-agent
      (:description "Agent with resource tools")
      (:tools
        (from-resources contact deal :actions (:read :create))))
    (let* ((spec (cauldron.agent:find-agent 'resource-agent))
           (tspecs (cauldron.agent:agent-spec-tool-specs spec)))
      (is-equal 1 (length tspecs))
      (is-equal :from-resources (getf (first tspecs) :type))
      (is-equal '(contact deal) (getf (first tspecs) :resources))
      (is-equal '(:read :create) (getf (first tspecs) :actions)))))

(deftest test-defagent-with-integration-tools
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq)))
    (cauldron.agent:defagent integration-agent
      (:description "Agent with integration tools")
      (:tools
        (from-integration stripe :endpoints (:list-charges :create-refund))))
    (let* ((spec (cauldron.agent:find-agent 'integration-agent))
           (tspecs (cauldron.agent:agent-spec-tool-specs spec)))
      (is-equal 1 (length tspecs))
      (is-equal :from-integration (getf (first tspecs) :type))
      (is-equal 'stripe (getf (first tspecs) :integration))
      (is-equal '(:list-charges :create-refund) (getf (first tspecs) :endpoints)))))

(deftest test-defagent-overwrites-existing
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq)))
    (cauldron.agent:defagent overwrite-test
      (:description "Version 1")
      (:max-rounds 5))
    (is-equal "Version 1"
              (cauldron.agent:agent-spec-description (cauldron.agent:find-agent 'overwrite-test)))
    ;; Redefine
    (cauldron.agent:defagent overwrite-test
      (:description "Version 2")
      (:max-rounds 10))
    (is-equal "Version 2"
              (cauldron.agent:agent-spec-description (cauldron.agent:find-agent 'overwrite-test)))
    (is-equal 10
              (cauldron.agent:agent-spec-max-rounds (cauldron.agent:find-agent 'overwrite-test)))))

(deftest test-defagent-unknown-clause-errors
  (signals-condition error
    (cauldron.agent::parse-agent-clauses 'bad '((:invalid-clause "oops")))
    "Unknown clause signals error"))

(deftest test-defagent-full-spec
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq)))
    (cauldron.agent:defagent full-agent
      (:description "Full featured agent")
      (:model :claude-opus)
      (:model-config :api-key "test-key" :max-tokens 4096)
      (:system-prompt "You are {{agent_name}}.")
      (:tools
        (tool :check-status :description "Check system status"))
      (:triggers
        (:pubsub "alerts.critical")
        (:http :post "/agents/full"))
      (:memory :table agent_memory :scope :per-company)
      (:role "admin")
      (:max-rounds 20)
      (:enabled t))
    (let ((spec (cauldron.agent:find-agent 'full-agent)))
      (is-not-nil spec)
      (is-equal "Full featured agent" (cauldron.agent:agent-spec-description spec))
      (is-equal :claude-opus (cauldron.agent:agent-spec-model spec))
      (is-not-nil (cauldron.agent:agent-spec-model-config spec))
      (is (search "{{agent_name}}" (cauldron.agent:agent-spec-system-prompt spec)))
      (is-equal 1 (length (cauldron.agent:agent-spec-tool-specs spec)))
      (is-equal 2 (length (cauldron.agent:agent-spec-trigger-specs spec)))
      (is-not-nil (cauldron.agent:agent-spec-memory-config spec))
      (is-equal "admin" (cauldron.agent:agent-spec-role spec))
      (is-equal 20 (cauldron.agent:agent-spec-max-rounds spec))
      (is (cauldron.agent:agent-spec-enabled-p spec)))))

(deftest test-defagent-disabled
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq)))
    (cauldron.agent:defagent disabled-agent
      (:description "Disabled agent")
      (:enabled nil))
    (let ((spec (cauldron.agent:find-agent 'disabled-agent)))
      (is-false (cauldron.agent:agent-spec-enabled-p spec)))))
