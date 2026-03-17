;;;; src/integration/webhook.lisp — Webhook verification and handler generation
(in-package :cauldron.integration)

(defun verify-webhook-hmac (payload secret signature &key (algorithm :sha256))
  "Verify HMAC signature of PAYLOAD using SECRET.
ALGORITHM is :sha256 (default) or :sha1.
Returns T if signature matches."
  (let* ((computed (case algorithm
                     (:sha256 (cauldron.crypto:hmac-sha256 secret payload))
                     (:sha1 (error "SHA-1 webhook verification not yet implemented"))
                     (otherwise (error "Unknown webhook algorithm: ~A" algorithm))))
         (computed-hex (with-output-to-string (s)
                         (loop for byte across computed
                               do (format s "~(~2,'0x~)" byte)))))
    ;; Handle signatures with algorithm prefix like "sha256=..."
    (let ((clean-sig (let ((eq-pos (position #\= signature)))
                       (if (and eq-pos (< eq-pos 10))
                           (subseq signature (1+ eq-pos))
                           signature))))
      (cauldron.crypto:secure-equal computed-hex clean-sig))))

(defun find-webhook (spec webhook-name)
  "Find a webhook-spec by name within an integration-spec."
  (find webhook-name (integration-spec-webhooks spec)
        :key #'webhook-spec-name))

(defun verify-webhook (integration-name webhook-name payload signature secret)
  "Verify a webhook signature for the given integration and webhook.
PAYLOAD is the raw request body string.
SIGNATURE is the signature header value.
SECRET is the signing secret string.
Returns T if valid."
  (let* ((spec (find-integration integration-name))
         (_ (unless spec (error "Unknown integration: ~A" integration-name)))
         (webhook (find-webhook spec webhook-name))
         (_2 (unless webhook (error "Unknown webhook ~A for integration ~A"
                                     webhook-name integration-name)))
         (algorithm (case (webhook-spec-verify-method webhook)
                      (:hmac-sha256 :sha256)
                      (:hmac-sha1 :sha1)
                      (otherwise :sha256))))
    (declare (ignore _ _2))
    (verify-webhook-hmac payload secret signature :algorithm algorithm)))

(defun make-webhook-handler (integration-name webhook-name
                             &key (get-signature-fn nil) (get-secret-fn nil))
  "Create a webhook handler function for the given integration and webhook.

GET-SIGNATURE-FN: (lambda (conn) ...) → signature string from request headers.
  Default: reads X-Signature-256 header.
GET-SECRET-FN: (lambda () ...) → signing secret string.
  Must be provided for actual use.

Returns (lambda (conn) ...) that:
  1. Extracts the signature header
  2. Reads the request body
  3. Verifies the HMAC signature
  4. Parses JSON body
  5. Broadcasts to the ether topic (if configured)
  6. Returns 200 on success, 401 on verification failure."
  (let* ((spec (find-integration integration-name))
         (_ (unless spec (error "Unknown integration: ~A" integration-name)))
         (webhook (find-webhook spec webhook-name))
         (_2 (unless webhook (error "Unknown webhook ~A for integration ~A"
                                     webhook-name integration-name)))
         (topic (webhook-spec-topic webhook)))
    (declare (ignore _ _2))
    (lambda (conn)
      (let* ((body (or (cauldron.crucible:conn-get-assign conn :raw-body)
                       (cauldron.crucible:conn-body conn)))
             (signature (if get-signature-fn
                            (funcall get-signature-fn conn)
                            (cdr (assoc "x-signature-256"
                                        (cauldron.crucible:conn-headers conn)
                                        :test #'string-equal))))
             (secret (when get-secret-fn (funcall get-secret-fn))))
        (cond
          ((or (null signature) (null secret))
           (cauldron.crucible:conn-put-resp-body
            (cauldron.crucible:conn-put-status conn 401)
            "Unauthorized: Missing signature or secret"))
          ((verify-webhook-hmac body secret signature
                                :algorithm (case (webhook-spec-verify-method webhook)
                                             (:hmac-sha256 :sha256)
                                             (:hmac-sha1 :sha1)
                                             (otherwise :sha256)))
           ;; Success: parse body and broadcast
           (let ((parsed (handler-case (cauldron.json:decode body)
                           (error () body))))
             (when topic
               (cauldron.ether:broadcast cauldron.ether:*pubsub* topic parsed))
             (cauldron.crucible:conn-put-resp-body
              (cauldron.crucible:conn-put-status conn 200)
              "OK")))
          (t
           (cauldron.crucible:conn-put-resp-body
            (cauldron.crucible:conn-put-status conn 401)
            "Unauthorized: Invalid signature")))))))
