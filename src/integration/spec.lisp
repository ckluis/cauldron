;;;; src/integration/spec.lisp — Integration specification structs
(in-package :cauldron.integration)

(defstruct integration-spec
  "Declarative specification for an external integration."
  (name nil :type symbol)
  (base-url "" :type string)
  (scopes nil :type list)              ; (:global), (:global :company), etc.
  (auth-type :none :type keyword)      ; :none, :bearer, :basic, :header
  (auth-config nil :type list)         ; plist e.g. (:credential-key :stripe-secret-key)
  (rate-limit nil :type list)          ; (:max-requests N :window-seconds N) or nil
  (retry-config nil :type list)        ; (:max-retries 3 :base-delay-ms 500 :max-delay-ms 30000) or nil
  (canonical-log-p t :type boolean)
  (endpoints nil :type list)           ; list of endpoint-spec
  (webhooks nil :type list)            ; list of webhook-spec
  (description "" :type string))

(defstruct endpoint-spec
  "Specification for an integration endpoint."
  (name nil :type keyword)
  (method :get :type keyword)
  (path "" :type string)
  (content-type "application/json" :type string)
  (params nil :type list)
  (doc "" :type string))

(defstruct webhook-spec
  "Specification for an integration webhook."
  (name nil :type keyword)
  (path "" :type string)
  (signing-key-credential nil)         ; credential key for HMAC secret
  (verify-method :hmac-sha256 :type keyword)  ; :hmac-sha256 | :hmac-sha1
  (topic nil :type (or null string)))  ; ether PubSub topic
