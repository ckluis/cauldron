;;;; src/integration/delivery.lisp — Webhook delivery with HMAC signing and retries
;;;; Manages webhook endpoint registration, payload delivery with exponential backoff.
(in-package :cauldron.integration)

;;; --- Webhook Endpoint Table ---

(cauldron.grimoire:deftable webhook_endpoints
  (:column company_id :type integer :required t)
  (:column url :type text :required t)
  (:column secret :type text :required t)            ; HMAC signing secret
  (:column events :type text :default "'[]'")        ; JSON array of event types
  (:column enabled_p :type boolean :default "true")
  (:index company_id))

;;; --- Webhook Delivery Table ---

(cauldron.grimoire:deftable webhook_deliveries
  (:column webhook_id :type integer :required t :references webhook_endpoints :on-delete "CASCADE")
  (:column event_type :type text :required t)
  (:column payload :type text :required t)           ; JSON payload
  (:column url :type text :required t)
  (:column status :type text :default "'pending'")   ; pending, delivered, failed, exhausted
  (:column attempts :type integer :default "0")
  (:column max_attempts :type integer :default "5")
  (:column next_retry_at :type timestamp)
  (:column last_error :type text)
  (:column delivered_at :type timestamp)
  (:index webhook_id)
  (:index status)
  (:index next_retry_at))

;;; --- Retry Backoff ---

(defvar *delivery-backoff-seconds* '(10 60 300 1800 7200)
  "Exponential backoff schedule in seconds: 10s, 1m, 5m, 30m, 2h.")

(defun compute-delivery-backoff (attempt)
  "Return backoff seconds for ATTEMPT (0-indexed).
Returns nil if no more retries."
  (let ((schedule *delivery-backoff-seconds*))
    (when (< attempt (length schedule))
      (nth attempt schedule))))

;;; --- Hex Helper ---

(defun %octets-to-hex (octets)
  "Convert an octet vector to a lowercase hex string."
  (let ((hex (make-string (* (length octets) 2))))
    (loop for i from 0 below (length octets)
          for byte = (aref octets i)
          for hi = (ash byte -4)
          for lo = (logand byte #x0F)
          for pos = (* i 2)
          do (setf (char hex pos)       (char "0123456789abcdef" hi))
             (setf (char hex (+ pos 1)) (char "0123456789abcdef" lo)))
    hex))

;;; --- Payload Signing ---

(defun sign-webhook-payload (payload secret)
  "Sign PAYLOAD with SECRET using HMAC-SHA256.
Returns the hex signature string prefixed with sha256=."
  (let ((signature (%octets-to-hex (cauldron.crypto:hmac-sha256 secret payload))))
    (concatenate 'string "sha256=" signature)))

;;; --- Delivery SQL ---

(defun enqueue-delivery-sql (webhook-id event-type payload url)
  "Build SQL to insert a delivery record.
Returns (values sql params)."
  (let ((payload-json (if (stringp payload)
                          payload
                          (cauldron.json:encode payload))))
    (values
     "INSERT INTO webhook_deliveries (webhook_id, event_type, payload, url, status, attempts, max_attempts, created_at, updated_at) VALUES ($1, $2, $3, $4, 'pending', 0, 5, NOW(), NOW()) RETURNING id"
     (list webhook-id event-type payload-json url))))

(defun mark-delivery-success-sql (delivery-id)
  "Build SQL to mark a delivery as successful.
Returns (values sql params)."
  (values
   "UPDATE webhook_deliveries SET status = 'delivered', delivered_at = NOW(), updated_at = NOW() WHERE id = $1"
   (list delivery-id)))

(defun mark-delivery-retry-sql (delivery-id error-message next-retry-seconds)
  "Build SQL to mark a delivery for retry with the next backoff.
Returns (values sql params)."
  (values
   (format nil "UPDATE webhook_deliveries SET status = 'pending', attempts = attempts + 1, last_error = $2, next_retry_at = NOW() + INTERVAL '~D seconds', updated_at = NOW() WHERE id = $1" next-retry-seconds)
   (list delivery-id error-message)))

(defun mark-delivery-exhausted-sql (delivery-id error-message)
  "Build SQL to mark a delivery as exhausted (no more retries).
Returns (values sql params)."
  (values
   "UPDATE webhook_deliveries SET status = 'exhausted', last_error = $2, updated_at = NOW() WHERE id = $1"
   (list delivery-id error-message)))

(defun pending-deliveries-sql (&key (limit 50))
  "Build SQL to fetch pending deliveries ready for retry.
Returns (values sql params)."
  (values
   (format nil "SELECT id, webhook_id, event_type, payload, url, attempts, max_attempts FROM webhook_deliveries WHERE status = 'pending' AND (next_retry_at IS NULL OR next_retry_at <= NOW()) ORDER BY created_at ASC LIMIT ~D" limit)
   nil))

(defun list-deliveries-sql (&key webhook-id (limit 50))
  "Build SQL to list deliveries, optionally filtered by webhook.
Returns (values sql params)."
  (if webhook-id
      (values
       (format nil "SELECT id, event_type, status, attempts, last_error, delivered_at, created_at FROM webhook_deliveries WHERE webhook_id = $1 ORDER BY created_at DESC LIMIT ~D" limit)
       (list webhook-id))
      (values
       (format nil "SELECT id, webhook_id, event_type, status, attempts, last_error, delivered_at, created_at FROM webhook_deliveries ORDER BY created_at DESC LIMIT ~D" limit)
       nil)))

;;; --- Webhook Endpoint SQL ---

(defun create-webhook-endpoint-sql (company-id url secret events)
  "Build SQL to register a webhook endpoint.
Returns (values sql params)."
  (let ((events-json (if events
                         (cauldron.json:encode (coerce events 'vector))
                         "[]")))
    (values
     "INSERT INTO webhook_endpoints (company_id, url, secret, events, enabled_p, created_at, updated_at) VALUES ($1, $2, $3, $4, true, NOW(), NOW()) RETURNING id"
     (list company-id url secret events-json))))

(defun list-webhook-endpoints-sql (company-id)
  "Build SQL to list webhook endpoints for a company.
Returns (values sql params)."
  (values
   "SELECT id, url, events, enabled_p, created_at FROM webhook_endpoints WHERE company_id = $1 ORDER BY created_at DESC"
   (list company-id)))

(defun disable-webhook-endpoint-sql (endpoint-id)
  "Build SQL to disable a webhook endpoint.
Returns (values sql params)."
  (values
   "UPDATE webhook_endpoints SET enabled_p = false, updated_at = NOW() WHERE id = $1"
   (list endpoint-id)))

;;; --- Delivery Execution ---

(defun deliver-webhook (url payload secret)
  "Deliver a webhook payload to URL with HMAC-SHA256 signature.
Returns (values success-p status-code error-message).
Does not retry — caller handles retry logic."
  (let ((signature (sign-webhook-payload payload secret)))
    (handler-case
        (multiple-value-bind (body status)
            (cauldron.http-client:http-post url
              :body payload
              :headers (list
                        (cons "Content-Type" "application/json")
                        (cons "X-Webhook-Signature" signature)
                        (cons "X-Webhook-Timestamp" (format nil "~D" (get-universal-time)))))
          (declare (ignore body))
          (if (and status (>= status 200) (< status 300))
              (values t status nil)
              (values nil status (format nil "HTTP ~D" status))))
      (error (e)
        (values nil 0 (format nil "~A" e))))))

(defun process-delivery (delivery-id url payload secret attempts max-attempts)
  "Process a single webhook delivery attempt.
Returns (values outcome sql-fn) where outcome is :delivered, :retry, or :exhausted.
SQL-FN is a function that returns (values sql params) for the appropriate status update."
  (multiple-value-bind (success-p status error-msg)
      (deliver-webhook url payload secret)
    (declare (ignore status))
    (cond
      (success-p
       (values :delivered
               (lambda () (mark-delivery-success-sql delivery-id))))
      ((>= (1+ attempts) max-attempts)
       (values :exhausted
               (lambda () (mark-delivery-exhausted-sql delivery-id (or error-msg "unknown")))))
      (t
       (let ((backoff (or (compute-delivery-backoff attempts) 7200)))
         (values :retry
                 (lambda ()
                   (mark-delivery-retry-sql delivery-id
                                            (or error-msg "unknown")
                                            backoff))))))))
