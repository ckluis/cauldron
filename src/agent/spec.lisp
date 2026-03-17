;;;; src/agent/spec.lisp — Agent specification struct, defagent macro, registry
(in-package :cauldron.agent)

;;; --- Agent Spec Struct ---

(defstruct agent-spec
  "Declarative specification for an autonomous agent."
  (name nil :type symbol)
  (description "" :type string)
  (model :claude-sonnet :type keyword)
  (model-config nil :type list)         ; (:api-key ... :max-tokens ...)
  (system-prompt "" :type string)
  (tool-specs nil :type list)           ; parsed tool source specs
  (trigger-specs nil :type list)        ; parsed trigger specs
  (memory-config nil :type list)        ; (:table name :scope :per-company)
  (role "member" :type string)
  (max-rounds 10 :type integer)
  (enabled-p t :type boolean)
  (token-budget nil :type (or null integer))) ; max tokens per invocation, nil = unlimited

;;; --- Registry ---

(defvar *agent-registry* (make-hash-table :test 'eq)
  "Global registry of agent specs: name (symbol) → agent-spec.")

(defvar *agent-registry-lock* (cauldron.runtime:make-lock "agent-registry")
  "Lock for thread-safe access to *agent-registry*.")

(defun find-agent (name)
  "Find an agent spec by NAME (symbol). Returns agent-spec or NIL."
  (cauldron.runtime:with-lock (*agent-registry-lock*)
    (gethash name *agent-registry*)))

(defun list-agents ()
  "Return a list of all registered agent names."
  (cauldron.runtime:with-lock (*agent-registry-lock*)
    (let ((result '()))
      (maphash (lambda (name spec)
                 (declare (ignore spec))
                 (push name result))
               *agent-registry*)
      (sort result #'string< :key #'symbol-name))))

;;; --- Clause Parsing ---

(defun parse-tool-clause (clause)
  "Parse a single tool clause from defagent :tools body.
Returns a plist describing the tool source.
Compares symbol names to handle cross-package macro expansion."
  (let ((kind-name (symbol-name (first clause))))
    (cond
      ((string= kind-name "FROM-RESOURCES")
       (let ((resource-names nil)
             (actions nil))
         ;; Find :actions keyword position in rest
         (let ((pos (position :actions (rest clause))))
           (if pos
               (progn
                 (setf resource-names (subseq (rest clause) 0 pos))
                 (setf actions (nth (1+ pos) (rest clause))))
               (setf resource-names (rest clause)
                     actions '(:read :create :update :delete))))
         (list :type :from-resources
               :resources resource-names
               :actions actions)))
      ((string= kind-name "FROM-INTEGRATION")
       (let ((integration-name (second clause))
             (endpoints (getf (cddr clause) :endpoints)))
         (list :type :from-integration
               :integration integration-name
               :endpoints endpoints)))
      ((string= kind-name "TOOL")
       (let ((name (second clause))
             (rest-args (cddr clause)))
         (list :type :custom
               :name name
               :description (getf rest-args :description)
               :params (getf rest-args :params)
               :handler (getf rest-args :handler))))
      ((string= kind-name "FROM-AGENT")
       (let ((agent-names (rest clause)))
         (list :type :from-agent
               :agents agent-names)))
      (t
       (error "Unknown tool clause type: ~S" (first clause))))))

(defun parse-agent-clauses (name clauses)
  "Parse defagent clauses into an agent-spec struct."
  (let ((description "")
        (model :claude-sonnet)
        (model-config nil)
        (system-prompt "")
        (tool-specs nil)
        (trigger-specs nil)
        (memory-config nil)
        (role "member")
        (max-rounds 10)
        (enabled-p t)
        (token-budget nil))
    (dolist (clause clauses)
      (let ((kind (first clause)))
        (case kind
          (:description
           (setf description (second clause)))
          (:model
           (setf model (second clause)))
          (:model-config
           (setf model-config (rest clause)))
          (:system-prompt
           (setf system-prompt (second clause)))
          (:tools
           (setf tool-specs (mapcar #'parse-tool-clause (rest clause))))
          (:triggers
           (setf trigger-specs (rest clause)))
          (:memory
           (setf memory-config (rest clause)))
          (:role
           (setf role (second clause)))
          (:max-rounds
           (setf max-rounds (second clause)))
          (:enabled
           (setf enabled-p (second clause)))
          (:token-budget
           (setf token-budget (second clause)))
          (otherwise
           (error "Unknown defagent clause: ~S" kind)))))
    (make-agent-spec
     :name name
     :description description
     :model model
     :model-config model-config
     :system-prompt system-prompt
     :tool-specs tool-specs
     :trigger-specs trigger-specs
     :memory-config memory-config
     :role role
     :max-rounds max-rounds
     :enabled-p enabled-p
     :token-budget token-budget)))

;;; --- defagent Macro ---

(defmacro defagent (name &body clauses)
  "Define and register a declarative autonomous agent.

Usage:
  (defagent support-bot
    (:description \"Customer support agent\")
    (:model :claude-sonnet)
    (:system-prompt \"You are a support agent.\")
    (:tools
      (from-resources contact deal :actions (:read :create))
      (from-integration stripe :endpoints (:list-charges))
      (tool :send-email :description \"Send email\" :params ((to :string :required)) :handler #'send-email))
    (:triggers
      (:pubsub \"support.ticket.created\")
      (:http :post \"/agents/support/ask\"))
    (:memory :table agent_memory :scope :per-company)
    (:role \"member\")
    (:max-rounds 15))"
  `(cauldron.runtime:with-lock (*agent-registry-lock*)
     (setf (gethash ',name *agent-registry*)
           (parse-agent-clauses ',name ',clauses))))
