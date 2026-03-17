;;;; test/integration/client-test.lisp — Integration client tests
(in-package :cauldron.test)

(defsuite :integration-client)

;;; --- Setup: register test integrations ---

(suite-setup :integration-client
  (lambda ()
    (clrhash cauldron.integration:*integration-registry*)
    (eval '(cauldron.integration:defintegration test-api
      (:base-url "https://api.example.com/v1")
      (:auth :bearer :credential-key :test-api-key)
      (:rate-limit :max-requests 10 :window-seconds 60)
      (:retry :max-retries 2 :base-delay-ms 100 :max-delay-ms 1000)
      (:endpoint :list-items :get "/items"
        :doc "List all items")
      (:endpoint :get-item :get "/items/:id"
        :doc "Get item by ID")
      (:endpoint :create-item :post "/items"
        :doc "Create an item")
      (:webhook :events :path "/webhooks/test"
        :signing-key-credential :test-webhook-secret
        :verify :hmac-sha256
        :topic "test.events")))))

(suite-teardown :integration-client
  (lambda ()
    (clrhash cauldron.integration:*integration-registry*)
    (cauldron.integration:reset-health)))

;;; --- URL building ---

(deftest test-build-url-simple
  (let* ((spec (cauldron.integration:find-integration 'test-api))
         (endpoint (cauldron.integration::find-endpoint spec :list-items))
         (url (cauldron.integration::build-url spec endpoint)))
    (is-equal "https://api.example.com/v1/items" url)))

(deftest test-build-url-with-path-params
  (let* ((spec (cauldron.integration:find-integration 'test-api))
         (endpoint (cauldron.integration::find-endpoint spec :get-item))
         (url (cauldron.integration::build-url spec endpoint :path-params '(:id 42))))
    (is-equal "https://api.example.com/v1/items/42" url)))

(deftest test-build-url-with-query-params
  (let* ((spec (cauldron.integration:find-integration 'test-api))
         (endpoint (cauldron.integration::find-endpoint spec :list-items))
         (url (cauldron.integration::build-url spec endpoint
                                                :query-params '(:page 1 :limit 20))))
    (is-equal "https://api.example.com/v1/items?page=1&limit=20" url)))

;;; --- Auth injection ---

(deftest test-inject-auth-headers-bearer
  (let* ((spec (cauldron.integration:find-integration 'test-api))
         (headers (cauldron.integration::inject-auth-headers spec '() :token "sk_live_real_token")))
    (is-not-nil headers "Headers list is not empty")
    (let ((auth (cdr (assoc "Authorization" headers :test #'string=))))
      (is-not-nil auth "Authorization header present")
      (is-equal "Bearer sk_live_real_token" auth "Bearer token has actual value, not placeholder"))))

(deftest test-inject-auth-headers-none
  ;; Register a no-auth integration
  (eval '(cauldron.integration:defintegration test-noauth
    (:base-url "https://public.api.com")
    (:auth :none)))
  (let* ((spec (cauldron.integration:find-integration 'test-noauth))
         (headers (cauldron.integration::inject-auth-headers spec '(("Accept" . "application/json")))))
    ;; Should only have the original header
    (is-equal 1 (length headers) "No auth headers added")
    (is-equal "Accept" (car (first headers)))))

(deftest test-inject-auth-headers-bearer-requires-token
  (let* ((spec (cauldron.integration:find-integration 'test-api)))
    (signals-condition error
      (cauldron.integration::inject-auth-headers spec '())
      "Bearer auth without token signals error")))

(deftest test-inject-auth-headers-header-type
  ;; Register a header-auth integration
  (eval '(cauldron.integration:defintegration test-header-auth
    (:base-url "https://api.example.com")
    (:auth :header :header-name "X-API-Key" :credential-key :api-key)))
  (let* ((spec (cauldron.integration:find-integration 'test-header-auth))
         (headers (cauldron.integration::inject-auth-headers spec '() :token "my-api-key-123")))
    (let ((key-header (cdr (assoc "X-API-Key" headers :test #'string=))))
      (is-equal "my-api-key-123" key-header "Header auth injects actual token"))))
