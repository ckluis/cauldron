;;;; test/agent/executor-test.lisp — Full invoke-agent flow with mock provider
(in-package :cauldron.test)

(defsuite :agent-executor)

;;; --- System Prompt Interpolation ---

(deftest test-interpolate-system-prompt-agent-name
  (let ((result (cauldron.agent::interpolate-system-prompt
                 "You are {{agent_name}}." 'support-bot nil)))
    (is-equal "You are support-bot." result)))

(deftest test-interpolate-system-prompt-scope-key
  (let ((result (cauldron.agent::interpolate-system-prompt
                 "Scope: {{scope_key}}" 'bot "company-42")))
    (is-equal "Scope: company-42" result)))

(deftest test-interpolate-system-prompt-no-vars
  (let ((result (cauldron.agent::interpolate-system-prompt
                 "No variables here." 'bot nil)))
    (is-equal "No variables here." result)))

(deftest test-interpolate-system-prompt-multiple-occurrences
  (let ((result (cauldron.agent::interpolate-system-prompt
                 "I am {{agent_name}}. Call me {{agent_name}}." 'helper nil)))
    (is-equal "I am helper. Call me helper." result)))

;;; --- Replace All ---

(deftest test-replace-all-basic
  (is-equal "hello world" (cauldron.agent::%replace-all "hello foo" "foo" "world"))
  (is-equal "aXbXc" (cauldron.agent::%replace-all "a.b.c" "." "X"))
  (is-equal "abc" (cauldron.agent::%replace-all "abc" "x" "y") "No match → unchanged"))

;;; --- Provider Resolution ---

(deftest test-resolve-provider-claude-sonnet
  (let ((spec (cauldron.agent:make-agent-spec :name 'test :model :claude-sonnet)))
    (let ((provider (cauldron.agent::resolve-agent-provider spec)))
      (is-not-nil provider "Provider resolved")
      (is (not (null (cauldron.llm:llm-provider-name provider))) "Is an llm-provider"))))

(deftest test-resolve-provider-mock
  (let ((spec (cauldron.agent:make-agent-spec
               :name 'test
               :model :mock
               :model-config (list :responses (list "Mock response 1")))))
    (let ((provider (cauldron.agent::resolve-agent-provider spec)))
      (is-not-nil provider)
      (is (not (null (cauldron.llm:llm-provider-name provider)))))))

(deftest test-resolve-provider-unknown-defaults-to-mock
  (let ((spec (cauldron.agent:make-agent-spec :name 'test :model :unknown-model)))
    (let ((provider (cauldron.agent::resolve-agent-provider spec)))
      (is-not-nil provider "Unknown model defaults to mock")
      (is (not (null (cauldron.llm:llm-provider-name provider)))))))

(deftest test-resolve-provider-ollama
  (let ((spec (cauldron.agent:make-agent-spec :name 'test :model :ollama)))
    (let ((provider (cauldron.agent::resolve-agent-provider spec)))
      (is-not-nil provider)
      (is (not (null (cauldron.llm:llm-provider-name provider)))))))

(deftest test-resolve-provider-claude-opus
  (let ((spec (cauldron.agent:make-agent-spec :name 'test :model :claude-opus)))
    (let ((provider (cauldron.agent::resolve-agent-provider spec)))
      (is-not-nil provider)
      (is (not (null (cauldron.llm:llm-provider-name provider)))))))

(deftest test-resolve-provider-claude-haiku
  (let ((spec (cauldron.agent:make-agent-spec :name 'test :model :claude-haiku)))
    (let ((provider (cauldron.agent::resolve-agent-provider spec)))
      (is-not-nil provider)
      (is (not (null (cauldron.llm:llm-provider-name provider)))))))

;;; --- Invoke Agent Validation ---

(deftest test-invoke-agent-not-found
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq)))
    (signals-condition error
      (cauldron.agent:invoke-agent 'nonexistent "hello" nil)
      "Not found signals error")))

(deftest test-invoke-agent-disabled
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq)))
    (cauldron.agent:defagent disabled-exec-agent
      (:description "Disabled")
      (:enabled nil))
    (signals-condition error
      (cauldron.agent:invoke-agent 'disabled-exec-agent "hello" nil)
      "Disabled agent signals error")))

;;; --- Full invoke flow requires DB connection (integration test)
;;; These are SQL-level unit tests that verify the spec→provider→tools path.

(deftest test-invoke-agent-assembles-tools
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq)))
    (cauldron.agent:defagent assembly-check-agent
      (:description "Assembly check")
      (:model :mock)
      (:tools
        (tool :ping :description "Ping"))
      (:memory :table agent_memory :scope :per-company))
    (let* ((spec (cauldron.agent:find-agent 'assembly-check-agent))
           (tools (cauldron.agent:assemble-agent-tools spec)))
      ;; 1 custom + 2 memory = 3
      (is-equal 3 (length tools))
      (is (find "ping" tools :key #'cauldron.llm:agent-tool-name :test #'string=))
      (is (find "remember" tools :key #'cauldron.llm:agent-tool-name :test #'string=))
      (is (find "recall" tools :key #'cauldron.llm:agent-tool-name :test #'string=)))))
