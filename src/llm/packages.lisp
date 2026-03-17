;;;; src/llm/packages.lisp — Package definitions for the LLM/Agent layer

(defpackage :cauldron.llm
  (:use :cl)
  (:export
   ;; Provider protocol
   #:llm-provider
   #:make-llm-provider
   #:llm-provider-name
   #:llm-provider-call-fn
   #:llm-response
   #:make-llm-response
   #:llm-response-content
   #:llm-response-tool-calls
   #:llm-response-stop-reason
   #:llm-response-input-tokens
   #:llm-response-output-tokens
   #:tool-call
   #:make-tool-call
   #:tool-call-id
   #:tool-call-name
   #:tool-call-arguments
   ;; Provider constructors
   #:make-claude-provider
   #:make-ollama-provider
   #:make-mock-provider
   #:parse-claude-response
   ;; Tool system
   #:agent-tool
   #:make-agent-tool
   #:agent-tool-name
   #:agent-tool-description
   #:agent-tool-parameters
   #:agent-tool-handler
   #:agent-tool-destructive-p
   #:copy-agent-tool
   #:tool-parameter
   #:make-tool-parameter
   #:tool-parameter-name
   #:tool-parameter-type
   #:tool-parameter-description
   #:tool-parameter-required-p
   #:tool-parameter-enum
   #:tools-to-claude-format
   #:execute-tool
   ;; Conversation
   #:create-conversation
   #:add-message
   #:get-conversation-messages
   #:list-conversations
   #:update-conversation-tokens
   ;; Agent loop
   #:messages-to-claude-format
   #:agent-turn))
