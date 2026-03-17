;;;; src/integration/registry.lisp — Integration registry and defintegration macro
(in-package :cauldron.integration)

(defvar *integration-registry* (make-hash-table :test 'eq)
  "Global registry of integration specs: name (symbol) → integration-spec.")

(defvar *registry-lock* (cauldron.runtime:make-lock "integration-registry")
  "Lock for thread-safe access to *integration-registry*.")

(defmacro defintegration (name &body clauses)
  "Define and register an integration.

Usage:
  (defintegration stripe
    (:description \"Stripe payment API\")
    (:base-url \"https://api.stripe.com/v1\")
    (:scope :global :company)
    (:auth :bearer :credential-key :stripe-secret-key)
    (:rate-limit :max-requests 100 :window-seconds 1)
    (:retry :max-retries 3 :base-delay-ms 500 :max-delay-ms 30000)
    (:canonical-log t)
    (:endpoint :create-customer :post \"/customers\"
      :content-type \"application/x-www-form-urlencoded\"
      :doc \"Create a new Stripe customer\")
    (:webhook :payments :path \"/webhooks/stripe\"
      :signing-key-credential :stripe-webhook-secret
      :verify :hmac-sha256
      :topic \"stripe.payments\"))"
  `(cauldron.runtime:with-lock (*registry-lock*)
     (setf (gethash ',name *integration-registry*)
           (parse-integration-clauses ',name ',clauses))))

(defun parse-integration-clauses (name clauses)
  "Parse defintegration clauses into an integration-spec struct."
  (let ((base-url "")
        (scopes nil)
        (auth-type :none)
        (auth-config nil)
        (rate-limit nil)
        (retry-config nil)
        (canonical-log-p t)
        (endpoints nil)
        (webhooks nil)
        (description ""))
    (dolist (clause clauses)
      (let ((kind (first clause)))
        (case kind
          (:description
           (setf description (second clause)))
          (:base-url
           (setf base-url (second clause)))
          (:scope
           (setf scopes (rest clause)))
          (:auth
           (setf auth-type (second clause))
           (setf auth-config (cddr clause)))
          (:rate-limit
           (setf rate-limit (rest clause)))
          (:retry
           (setf retry-config (rest clause)))
          (:canonical-log
           (setf canonical-log-p (second clause)))
          (:endpoint
           (push (parse-endpoint-clause (rest clause)) endpoints))
          (:webhook
           (push (parse-webhook-clause (rest clause)) webhooks))
          (otherwise
           (error "Unknown defintegration clause: ~S" clause)))))
    (make-integration-spec
     :name name
     :base-url base-url
     :scopes scopes
     :auth-type auth-type
     :auth-config auth-config
     :rate-limit rate-limit
     :retry-config retry-config
     :canonical-log-p canonical-log-p
     :endpoints (nreverse endpoints)
     :webhooks (nreverse webhooks)
     :description description)))

(defun parse-endpoint-clause (args)
  "Parse (:endpoint name method path &key content-type params doc)."
  (let ((name (first args))
        (method (second args))
        (path (third args))
        (rest-args (cdddr args)))
    (make-endpoint-spec
     :name name
     :method (or method :get)
     :path (or path "")
     :content-type (or (getf rest-args :content-type) "application/json")
     :params (getf rest-args :params)
     :doc (or (getf rest-args :doc) ""))))

(defun parse-webhook-clause (args)
  "Parse (:webhook name :path path :signing-key-credential key :verify method :topic topic)."
  (let ((name (first args))
        (rest-args (rest args)))
    (make-webhook-spec
     :name name
     :path (or (getf rest-args :path) "")
     :signing-key-credential (getf rest-args :signing-key-credential)
     :verify-method (or (getf rest-args :verify) :hmac-sha256)
     :topic (getf rest-args :topic))))

(defun find-integration (name)
  "Find an integration spec by NAME (symbol). Returns integration-spec or NIL."
  (cauldron.runtime:with-lock (*registry-lock*)
    (gethash name *integration-registry*)))

(defun list-integrations ()
  "Return a list of all registered integration names."
  (cauldron.runtime:with-lock (*registry-lock*)
    (let ((result '()))
      (maphash (lambda (name spec)
                 (declare (ignore spec))
                 (push name result))
               *integration-registry*)
      (sort result #'string< :key #'symbol-name))))
