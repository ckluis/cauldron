;;;; test/integration/spec-test.lisp — Integration spec and registry tests
(in-package :cauldron.test)

(defsuite :integration-spec)

;;; --- Setup: clean registry before tests ---
(suite-setup :integration-spec
  (lambda () (clrhash cauldron.integration:*integration-registry*)))

(suite-teardown :integration-spec
  (lambda () (clrhash cauldron.integration:*integration-registry*)))

;;; --- Spec struct creation ---

(deftest test-integration-spec-creation
  (let ((spec (cauldron.integration:make-integration-spec
               :name 'test-api
               :base-url "https://api.example.com"
               :auth-type :bearer
               :description "Test API")))
    (is-equal 'test-api (cauldron.integration:integration-spec-name spec))
    (is-equal "https://api.example.com" (cauldron.integration:integration-spec-base-url spec))
    (is-equal :bearer (cauldron.integration:integration-spec-auth-type spec))
    (is-equal "Test API" (cauldron.integration:integration-spec-description spec))))

;;; --- defintegration macro ---

(deftest test-defintegration-basic
  (eval '(cauldron.integration:defintegration test-stripe
    (:description "Stripe payment API")
    (:base-url "https://api.stripe.com/v1")
    (:scope :global :company)
    (:auth :bearer :credential-key :stripe-secret-key)
    (:rate-limit :max-requests 100 :window-seconds 1)
    (:retry :max-retries 3 :base-delay-ms 500 :max-delay-ms 30000)
    (:canonical-log t)
    (:endpoint :create-customer :post "/customers"
      :content-type "application/x-www-form-urlencoded"
      :doc "Create a new Stripe customer")
    (:endpoint :get-customer :get "/customers/:id"
      :doc "Get a Stripe customer by ID")
    (:webhook :payments :path "/webhooks/stripe"
      :signing-key-credential :stripe-webhook-secret
      :verify :hmac-sha256
      :topic "stripe.payments")))
  (let ((spec (cauldron.integration:find-integration 'test-stripe)))
    (is-not-nil spec "Integration registered in registry")
    (is-equal "https://api.stripe.com/v1"
              (cauldron.integration:integration-spec-base-url spec))
    (is-equal :bearer (cauldron.integration:integration-spec-auth-type spec))
    (is-equal '(:global :company) (cauldron.integration:integration-spec-scopes spec))
    ;; Endpoints
    (is-equal 2 (length (cauldron.integration:integration-spec-endpoints spec)))
    (let ((ep (first (cauldron.integration:integration-spec-endpoints spec))))
      (is-equal :create-customer (cauldron.integration:endpoint-spec-name ep))
      (is-equal :post (cauldron.integration:endpoint-spec-method ep))
      (is-equal "/customers" (cauldron.integration:endpoint-spec-path ep)))
    ;; Webhooks
    (is-equal 1 (length (cauldron.integration:integration-spec-webhooks spec)))
    (let ((wh (first (cauldron.integration:integration-spec-webhooks spec))))
      (is-equal :payments (cauldron.integration:webhook-spec-name wh))
      (is-equal "/webhooks/stripe" (cauldron.integration:webhook-spec-path wh))
      (is-equal "stripe.payments" (cauldron.integration:webhook-spec-topic wh)))))

;;; --- Registry operations ---

(deftest test-find-integration-nil
  (is-nil (cauldron.integration:find-integration 'nonexistent)
          "find-integration returns NIL for unknown"))

(deftest test-list-integrations
  (eval '(cauldron.integration:defintegration test-alpha
    (:base-url "https://alpha.com")))
  (eval '(cauldron.integration:defintegration test-beta
    (:base-url "https://beta.com")))
  (let ((names (cauldron.integration:list-integrations)))
    (is (member 'test-alpha names) "Alpha in list")
    (is (member 'test-beta names) "Beta in list")))

;;; --- Endpoint spec ---

(deftest test-endpoint-spec-defaults
  (let ((ep (cauldron.integration:make-endpoint-spec :name :test-ep)))
    (is-equal :get (cauldron.integration:endpoint-spec-method ep))
    (is-equal "application/json" (cauldron.integration:endpoint-spec-content-type ep))
    (is-equal "" (cauldron.integration:endpoint-spec-path ep))))

;;; --- Webhook spec ---

(deftest test-webhook-spec-defaults
  (let ((wh (cauldron.integration:make-webhook-spec :name :test-wh)))
    (is-equal :hmac-sha256 (cauldron.integration:webhook-spec-verify-method wh))
    (is-nil (cauldron.integration:webhook-spec-topic wh))))
