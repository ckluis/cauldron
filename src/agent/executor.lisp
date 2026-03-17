;;;; src/agent/executor.lisp — Unified agent execution flow
(in-package :cauldron.agent)

;;; --- Token Budget Condition ---

(define-condition agent-budget-exceeded (error)
  ((agent-name :initarg :agent-name :reader agent-budget-exceeded-agent)
   (budget :initarg :budget :reader agent-budget-exceeded-budget)
   (used :initarg :used :reader agent-budget-exceeded-used))
  (:report (lambda (c s)
             (format s "Agent ~A exceeded token budget: ~D used of ~D allowed"
                     (agent-budget-exceeded-agent c)
                     (agent-budget-exceeded-used c)
                     (agent-budget-exceeded-budget c)))))

(defun check-agent-budget (agent-name budget tokens-used)
  "Signal agent-budget-exceeded if TOKENS-USED exceeds BUDGET."
  (when (and budget (> tokens-used budget))
    (error 'agent-budget-exceeded
           :agent-name agent-name
           :budget budget
           :used tokens-used)))

(defun make-budget-checked-provider (provider agent-name budget)
  "Wrap PROVIDER's call function to track tokens and enforce BUDGET.
Returns a new provider that signals agent-budget-exceeded when exceeded."
  (if (null budget)
      provider
      (let ((total-tokens 0)
            (original-fn (cauldron.llm:llm-provider-call-fn provider)))
        (cauldron.llm:make-llm-provider
         :name (cauldron.llm:llm-provider-name provider)
         :call-fn (lambda (messages tools system-prompt)
                    (let ((response (funcall original-fn messages tools system-prompt)))
                      (incf total-tokens
                            (+ (cauldron.llm:llm-response-input-tokens response)
                               (cauldron.llm:llm-response-output-tokens response)))
                      (check-agent-budget agent-name budget total-tokens)
                      response))))))

;;; --- Agent-to-Agent Communication ---

(defvar *agent-call-chain* nil
  "Dynamic list of agent names currently in the call chain. Used for cycle detection.")

(defvar *agent-max-call-depth* 5
  "Maximum depth for nested agent-to-agent calls.")

;;; --- Invoke Agent ---

(defun invoke-agent (agent-name input conn
                     &key trigger-type trigger-source conversation-id scope-key
                          provider)
  "Invoke an agent by name with INPUT message and database CONN.

Execution flow:
  1. Look up agent-spec from registry
  2. Check for agent-to-agent cycles
  3. Assemble tools from spec (lazy, with caching)
  4. Build system prompt (template-interpolate {{vars}})
  5. Create/resolve LLM provider from :model config (budget-wrapped)
  6. Call agent-turn with tools + system prompt + max-rounds
  7. Emit canonical log line
  8. Return (values response conversation-id)

TRIGGER-TYPE and TRIGGER-SOURCE are for logging context.
SCOPE-KEY selects memory scope (e.g., company-id for :per-company).
PROVIDER overrides the default provider resolution."
  (let ((spec (find-agent agent-name)))
    (unless spec
      (error "Agent ~S not found in registry" agent-name))
    (unless (agent-spec-enabled-p spec)
      (error "Agent ~S is disabled" agent-name))
    ;; Cycle detection for agent-to-agent calls
    (when (member agent-name *agent-call-chain*)
      (error "Agent call cycle detected: ~{~A~^ → ~} → ~A"
             (mapcar (lambda (n) (string-downcase (symbol-name n)))
                     (reverse *agent-call-chain*))
             (string-downcase (symbol-name agent-name))))
    (when (>= (length *agent-call-chain*) *agent-max-call-depth*)
      (error "Agent call depth exceeded: max ~D, chain: ~{~A~^ → ~}"
             *agent-max-call-depth*
             (mapcar (lambda (n) (string-downcase (symbol-name n)))
                     (reverse *agent-call-chain*))))
    (let* ((*agent-call-chain* (cons agent-name *agent-call-chain*))
           (start-time (get-internal-real-time))
           (tools (cached-assemble-agent-tools spec))
           (system-prompt (interpolate-system-prompt
                           (agent-spec-system-prompt spec)
                           agent-name scope-key))
           (base-provider (or provider
                              (resolve-agent-provider spec)))
           (agent-provider (make-budget-checked-provider
                            base-provider agent-name
                            (agent-spec-token-budget spec)))
           (max-rounds (agent-spec-max-rounds spec)))
      (declare (ignorable trigger-type trigger-source))
      ;; Execute the agent turn
      (multiple-value-bind (response conv-id)
          (cauldron.llm:agent-turn conn agent-provider input
                                    :conversation-id conversation-id
                                    :tools tools
                                    :system-prompt system-prompt
                                    :max-rounds max-rounds)
        ;; Emit canonical log
        (let* ((end-time (get-internal-real-time))
               (duration-ms (round (* 1000.0
                                      (/ (- end-time start-time)
                                         internal-time-units-per-second)))))
          (format *error-output*
                  "~&[cauldron.agent] invoke agent=~A trigger=~A source=~A rounds=~D duration=~Dms conv=~A~%"
                  (string-downcase (symbol-name agent-name))
                  (or trigger-type "direct")
                  (or trigger-source "api")
                  max-rounds
                  duration-ms
                  conv-id))
        (values response conv-id)))))

;;; --- System Prompt Interpolation ---

(defun %replace-all (string old new)
  "Replace all occurrences of OLD in STRING with NEW."
  (let ((result string)
        (old-len (length old)))
    (loop
      (let ((pos (search old result)))
        (unless pos (return result))
        (setf result (concatenate 'string
                                  (subseq result 0 pos)
                                  new
                                  (subseq result (+ pos old-len))))))))

(defun %current-date-string ()
  "Return current date as YYYY-MM-DD."
  (multiple-value-bind (sec min hr day month year)
      (get-decoded-time)
    (declare (ignore sec min hr))
    (format nil "~4,'0D-~2,'0D-~2,'0D" year month day)))

(defun %current-time-string ()
  "Return current time as HH:MM:SS."
  (multiple-value-bind (sec min hr)
      (get-decoded-time)
    (format nil "~2,'0D:~2,'0D:~2,'0D" hr min sec)))

(defun %interpolate-config-vars (template)
  "Replace {{config:KEY}} placeholders with environment variable values."
  (let ((result template))
    (loop
      (let ((start (search "{{config:" result)))
        (unless start (return result))
        (let ((end (search "}}" result :start2 (+ start 9))))
          (unless end (return result))
          (let* ((key (subseq result (+ start 9) end))
                 (value (or (cauldron.runtime:get-env key) "")))
            (setf result (concatenate 'string
                                      (subseq result 0 start)
                                      value
                                      (subseq result (+ end 2))))))))))

(defun interpolate-system-prompt (template agent-name scope-key)
  "Replace {{variable}} placeholders in TEMPLATE with values.
Supports: {{agent_name}}, {{scope_key}}, {{date}}, {{time}}, {{config:KEY}}."
  (let ((result template))
    (setf result (%replace-all result "{{agent_name}}"
                               (string-downcase (symbol-name agent-name))))
    (when scope-key
      (setf result (%replace-all result "{{scope_key}}" scope-key)))
    (setf result (%replace-all result "{{date}}" (%current-date-string)))
    (setf result (%replace-all result "{{time}}" (%current-time-string)))
    (setf result (%interpolate-config-vars result))
    result))

;;; --- Provider Resolution ---

(defun resolve-agent-provider (spec)
  "Resolve an LLM provider from the agent spec's model configuration.
Returns a cauldron.llm:llm-provider struct."
  (let ((model (agent-spec-model spec))
        (config (agent-spec-model-config spec)))
    (case model
      (:claude-sonnet
       (cauldron.llm:make-claude-provider
        :api-key (or (getf config :api-key) "")
        :model "claude-sonnet-4-20250514"))
      (:claude-haiku
       (cauldron.llm:make-claude-provider
        :api-key (or (getf config :api-key) "")
        :model "claude-haiku-4-5-20251001"))
      (:claude-opus
       (cauldron.llm:make-claude-provider
        :api-key (or (getf config :api-key) "")
        :model "claude-opus-4-6"))
      (:ollama
       (cauldron.llm:make-ollama-provider
        :model (or (getf config :model) "llama3")))
      (:mock
       (cauldron.llm:make-mock-provider
        (or (getf config :responses)
            (list "I am a mock agent response."))))
      (otherwise
       ;; Default to mock for safety
       (cauldron.llm:make-mock-provider
        (list (format nil "Agent ~A: no provider configured for model ~S"
                      (agent-spec-name spec) model)))))))
