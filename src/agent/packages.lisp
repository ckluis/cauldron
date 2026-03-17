;;;; src/agent/packages.lisp — Agent module package definition
(defpackage :cauldron.agent
  (:use :cl)
  (:export
   ;; Spec + registry
   #:defagent
   #:agent-spec
   #:make-agent-spec
   #:agent-spec-p
   #:agent-spec-name
   #:agent-spec-description
   #:agent-spec-model
   #:agent-spec-model-config
   #:agent-spec-system-prompt
   #:agent-spec-tool-specs
   #:agent-spec-trigger-specs
   #:agent-spec-memory-config
   #:agent-spec-role
   #:agent-spec-max-rounds
   #:agent-spec-enabled-p
   #:agent-spec-token-budget
   #:find-agent
   #:list-agents
   #:*agent-registry*
   ;; Execution
   #:invoke-agent
   ;; Token budget
   #:agent-budget-exceeded
   #:check-agent-budget
   #:make-budget-checked-provider
   ;; Agent-to-agent
   #:generate-agent-tool
   #:*agent-call-chain*
   #:*agent-max-call-depth*
   ;; Tool caching
   #:*tool-cache*
   #:*tool-cache-ttl*
   #:cached-assemble-agent-tools
   #:invalidate-agent-tools
   ;; SSE streaming
   #:agent-sse-handler
   ;; Memory
   #:agent-memory-get
   #:agent-memory-set
   #:agent-memory-list
   #:agent-memory-delete
   ;; Tools
   #:generate-resource-tools
   #:generate-integration-tools
   #:assemble-agent-tools
   ;; Triggers
   #:wire-agent-triggers
   #:agent-http-handler))
