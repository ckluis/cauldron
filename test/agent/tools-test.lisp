;;;; test/agent/tools-test.lisp — Tool generation from resources + integrations
(in-package :cauldron.test)

(defsuite :agent-tools)

;;; --- Helper: Define a test resource ---

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Register a test resource for tool generation tests
  (cauldron.reagent:defresource agent-test-contact
    :attributes ((name :type :string :required t :max-length 255)
                 (email :type :string :required t)
                 (age :type :integer)
                 (active :type :boolean)
                 (role :type :keyword :one-of (:user :admin :mod)))
    :actions ((create :accept (name email age active role))
              (update :accept (name email age active role))
              (read)
              (delete))
    :policies ((allow :read :always)
               (allow :create :always)
               (allow :update :always)
               (allow :delete :always))))

;;; --- Attribute → Tool Parameter Mapping ---

(deftest test-attribute-type-mapping
  (is-equal "string" (cauldron.agent::attribute-type-to-tool-type :string))
  (is-equal "string" (cauldron.agent::attribute-type-to-tool-type :text))
  (is-equal "integer" (cauldron.agent::attribute-type-to-tool-type :integer))
  (is-equal "number" (cauldron.agent::attribute-type-to-tool-type :float))
  (is-equal "boolean" (cauldron.agent::attribute-type-to-tool-type :boolean))
  (is-equal "string" (cauldron.agent::attribute-type-to-tool-type :keyword))
  (is-equal "string" (cauldron.agent::attribute-type-to-tool-type :uuid))
  (is-equal "string" (cauldron.agent::attribute-type-to-tool-type :timestamp))
  (is-equal "string" (cauldron.agent::attribute-type-to-tool-type :unknown-type)))

(deftest test-attribute-to-tool-parameter
  (let* ((attr (cauldron.reagent:make-attribute-def
                :name 'email
                :type :string
                :required t))
         (param (cauldron.agent::attribute-to-tool-parameter attr)))
    (is-equal "email" (cauldron.llm:tool-parameter-name param))
    (is-equal "string" (cauldron.llm:tool-parameter-type param))
    (is (cauldron.llm:tool-parameter-required-p param) "Required preserved")))

(deftest test-attribute-enum-mapping
  (let* ((attr (cauldron.reagent:make-attribute-def
                :name 'role
                :type :keyword
                :one-of '(:user :admin :mod)))
         (param (cauldron.agent::attribute-to-tool-parameter attr)))
    (is-not-nil (cauldron.llm:tool-parameter-enum param) "Enum set")
    (is-equal 3 (length (cauldron.llm:tool-parameter-enum param)))
    (is (member "admin" (cauldron.llm:tool-parameter-enum param) :test #'string=))))

;;; --- Resource Tool Generation ---

(deftest test-generate-resource-tools-read
  (let ((tools (cauldron.agent:generate-resource-tools
                '(agent-test-contact) '(:read) "member")))
    (is-equal 2 (length tools) "Read generates list + get")
    (is (find "list_agent-test-contact" tools
              :key #'cauldron.llm:agent-tool-name :test #'string=)
        "list tool generated")
    (is (find "get_agent-test-contact" tools
              :key #'cauldron.llm:agent-tool-name :test #'string=)
        "get tool generated")))

(deftest test-generate-resource-tools-create
  (let ((tools (cauldron.agent:generate-resource-tools
                '(agent-test-contact) '(:create) "member")))
    (is-equal 1 (length tools) "Create generates one tool")
    (let ((tool (first tools)))
      (is-equal "create_agent-test-contact" (cauldron.llm:agent-tool-name tool))
      ;; Should have params matching resource attributes
      (is (>= (length (cauldron.llm:agent-tool-parameters tool)) 5)
          "Has attribute-derived params"))))

(deftest test-generate-resource-tools-update
  (let ((tools (cauldron.agent:generate-resource-tools
                '(agent-test-contact) '(:update) "member")))
    (is-equal 1 (length tools))
    (let ((tool (first tools)))
      (is-equal "update_agent-test-contact" (cauldron.llm:agent-tool-name tool))
      ;; Update should have id param + attribute params
      (is (find "id" (cauldron.llm:agent-tool-parameters tool)
                :key #'cauldron.llm:tool-parameter-name :test #'string=)
          "Has id param"))))

(deftest test-generate-resource-tools-delete
  (let ((tools (cauldron.agent:generate-resource-tools
                '(agent-test-contact) '(:delete) "member")))
    (is-equal 1 (length tools))
    (let ((tool (first tools)))
      (is-equal "delete_agent-test-contact" (cauldron.llm:agent-tool-name tool))
      (is (cauldron.llm:agent-tool-destructive-p tool) "Delete is destructive"))))

(deftest test-generate-resource-tools-all-actions
  (let ((tools (cauldron.agent:generate-resource-tools
                '(agent-test-contact) '(:read :create :update :delete) "member")))
    ;; read=2 + create=1 + update=1 + delete=1 = 5
    (is-equal 5 (length tools))))

(deftest test-generate-resource-tools-handler-is-function
  (let ((tools (cauldron.agent:generate-resource-tools
                '(agent-test-contact) '(:read) "member")))
    (dolist (tool tools)
      (is (functionp (cauldron.llm:agent-tool-handler tool))
          "Handler is a function"))))

(deftest test-generate-resource-tools-missing-resource
  (signals-condition error
    (cauldron.agent:generate-resource-tools
     '(nonexistent-resource) '(:read) "member")
    "Missing resource signals error"))

;;; --- Integration Tool Generation ---

;; Register a test integration for tool generation
(eval-when (:compile-toplevel :load-toplevel :execute)
  (cauldron.integration:defintegration agent-test-stripe
    (:description "Test Stripe integration")
    (:base-url "https://api.stripe.com/v1")
    (:auth :bearer :credential-key :stripe-key)
    (:endpoint :list-charges :get "/charges"
      :doc "List all charges")
    (:endpoint :create-refund :post "/refunds"
      :doc "Create a refund")))

(deftest test-generate-integration-tools
  (let ((tools (cauldron.agent:generate-integration-tools
                'agent-test-stripe '(:list-charges :create-refund))))
    (is-equal 2 (length tools))
    (is (find "agent-test-stripe_list-charges" tools
              :key #'cauldron.llm:agent-tool-name :test #'string=)
        "list-charges tool generated")
    (is (find "agent-test-stripe_create-refund" tools
              :key #'cauldron.llm:agent-tool-name :test #'string=)
        "create-refund tool generated")))

(deftest test-integration-tool-destructive-flag
  (let ((tools (cauldron.agent:generate-integration-tools
                'agent-test-stripe '(:list-charges :create-refund))))
    (let ((list-tool (find "agent-test-stripe_list-charges" tools
                           :key #'cauldron.llm:agent-tool-name :test #'string=))
          (refund-tool (find "agent-test-stripe_create-refund" tools
                             :key #'cauldron.llm:agent-tool-name :test #'string=)))
      (is-false (cauldron.llm:agent-tool-destructive-p list-tool) "GET not destructive")
      (is (cauldron.llm:agent-tool-destructive-p refund-tool) "POST is destructive"))))

(deftest test-generate-integration-tools-missing-integration
  (signals-condition error
    (cauldron.agent:generate-integration-tools 'nonexistent '(:foo))
    "Missing integration signals error"))

(deftest test-generate-integration-tools-missing-endpoint
  (signals-condition error
    (cauldron.agent:generate-integration-tools 'agent-test-stripe '(:nonexistent))
    "Missing endpoint signals error"))

;;; --- Tool Assembly ---

(deftest test-assemble-agent-tools-empty
  (let ((spec (cauldron.agent:make-agent-spec :name 'empty-agent)))
    (let ((tools (cauldron.agent:assemble-agent-tools spec)))
      (is-nil tools "No tools for empty spec"))))

(deftest test-assemble-agent-tools-with-memory
  (let ((spec (cauldron.agent:make-agent-spec
               :name 'mem-agent
               :memory-config '(:table agent_memory :scope :per-company))))
    (let ((tools (cauldron.agent:assemble-agent-tools spec)))
      (is-equal 2 (length tools) "Memory tools added")
      (is (find "remember" tools :key #'cauldron.llm:agent-tool-name :test #'string=))
      (is (find "recall" tools :key #'cauldron.llm:agent-tool-name :test #'string=)))))

(deftest test-assemble-agent-tools-custom
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq)))
    (cauldron.agent:defagent assemble-test
      (:description "Test assembly")
      (:tools
        (tool :ping :description "Ping test")))
    (let* ((spec (cauldron.agent:find-agent 'assemble-test))
           (tools (cauldron.agent:assemble-agent-tools spec)))
      (is-equal 1 (length tools))
      (is-equal "ping" (cauldron.llm:agent-tool-name (first tools))))))

(deftest test-assemble-agent-tools-from-resources
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq)))
    (cauldron.agent:defagent resource-assemble-test
      (:description "Test resource assembly")
      (:tools
        (from-resources agent-test-contact :actions (:read))))
    (let* ((spec (cauldron.agent:find-agent 'resource-assemble-test))
           (tools (cauldron.agent:assemble-agent-tools spec)))
      ;; read = list + get = 2 tools
      (is-equal 2 (length tools)))))

(deftest test-assemble-agent-tools-from-integration
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq)))
    (cauldron.agent:defagent integration-assemble-test
      (:description "Test integration assembly")
      (:tools
        (from-integration agent-test-stripe :endpoints (:list-charges))))
    (let* ((spec (cauldron.agent:find-agent 'integration-assemble-test))
           (tools (cauldron.agent:assemble-agent-tools spec)))
      (is-equal 1 (length tools))
      (is-equal "agent-test-stripe_list-charges"
                (cauldron.llm:agent-tool-name (first tools))))))

(deftest test-assemble-mixed-tools-with-memory
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq)))
    (cauldron.agent:defagent mixed-tool-agent
      (:description "Mixed tools agent")
      (:tools
        (from-resources agent-test-contact :actions (:read))
        (from-integration agent-test-stripe :endpoints (:list-charges))
        (tool :custom-tool :description "Custom"))
      (:memory :table agent_memory :scope :per-company))
    (let* ((spec (cauldron.agent:find-agent 'mixed-tool-agent))
           (tools (cauldron.agent:assemble-agent-tools spec)))
      ;; 2 (read) + 1 (integration) + 1 (custom) + 2 (memory) = 6
      (is-equal 6 (length tools)))))
