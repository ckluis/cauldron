;;;; src/integration/client.lisp — Integration client: call-integration
(in-package :cauldron.integration)

;;; --- In-memory rate limiting (per-integration) ---

(defvar *integration-rate-limits* (make-hash-table :test 'eq)
  "Integration name → (count . window-start) for rate limiting.")

(defvar *rate-limit-lock* (cauldron.runtime:make-lock "integration-rate-limit")
  "Lock for thread-safe rate limit access.")

(defun check-rate-limit (spec)
  "Check if the integration is within its rate limit. Returns T if allowed.
Updates the rate limit counter."
  (let ((config (integration-spec-rate-limit spec)))
    (unless config
      (return-from check-rate-limit t))
    (let ((max-requests (getf config :max-requests 100))
          (window-seconds (getf config :window-seconds 60))
          (name (integration-spec-name spec))
          (now (get-universal-time)))
      (cauldron.runtime:with-lock (*rate-limit-lock*)
        (let ((entry (gethash name *integration-rate-limits*)))
          (cond
            ;; No entry or window expired → reset
            ((or (null entry)
                 (>= (- now (cdr entry)) window-seconds))
             (setf (gethash name *integration-rate-limits*) (cons 1 now))
             t)
            ;; Under limit → increment
            ((< (car entry) max-requests)
             (incf (car entry))
             t)
            ;; Over limit
            (t nil)))))))

;;; --- URL building ---

(defun build-url (spec endpoint-spec &key path-params query-params)
  "Build full URL from base-url + endpoint path, substituting :param placeholders."
  (let* ((base (integration-spec-base-url spec))
         (path (endpoint-spec-path endpoint-spec))
         ;; Substitute :param placeholders
         (resolved-path
           (if path-params
               (let ((result path))
                 (loop for (key value) on path-params by #'cddr
                       for placeholder = (format nil ":~A"
                                                 (string-downcase (string key)))
                       do (setf result (substitute-param result placeholder
                                                         (format nil "~A" value))))
                 result)
               path))
         ;; Build query string
         (query-string
           (when query-params
             (format nil "?~{~A=~A~^&~}"
                     (loop for (k v) on query-params by #'cddr
                           collect (string-downcase (string k))
                           collect (format nil "~A" v))))))
    (concatenate 'string base resolved-path (or query-string ""))))

(defun substitute-param (path placeholder value)
  "Replace PLACEHOLDER in PATH with VALUE."
  (let ((pos (search placeholder path)))
    (if pos
        (concatenate 'string
                     (subseq path 0 pos)
                     value
                     (subseq path (+ pos (length placeholder))))
        path)))

;;; --- Auth injection ---

(defun inject-auth-headers (spec headers &key token)
  "Inject authentication headers based on the integration's auth config.
TOKEN is the resolved credential value. Returns augmented headers alist."
  (case (integration-spec-auth-type spec)
    (:bearer
     (if token
         (cons (cons "Authorization" (format nil "Bearer ~A" token))
               headers)
         (error "Bearer auth requires a credential token for integration ~A"
                (integration-spec-name spec))))
    (:basic
     (if token
         (cons (cons "Authorization" (format nil "Basic ~A" token))
               headers)
         (error "Basic auth requires a credential token for integration ~A"
                (integration-spec-name spec))))
    (:header
     (let ((header-name (getf (integration-spec-auth-config spec) :header-name)))
       (if token
           (cons (cons (or header-name "X-API-Key") token) headers)
           (error "Header auth requires a credential token for integration ~A"
                  (integration-spec-name spec)))))
    (otherwise headers)))

;;; --- Main entry point ---

(defun find-endpoint (spec endpoint-name)
  "Find an endpoint-spec by name within an integration-spec."
  (find endpoint-name (integration-spec-endpoints spec)
        :key #'endpoint-spec-name))

(defun call-integration (integration-name endpoint-name
                         &key body query-params path-params headers
                              company-id user-id credential-value)
  "Call an external integration endpoint.

1. Lookup spec + endpoint from registry
2. Inject auth headers
3. Build URL (base-url + path with :param substitution + query string)
4. Check rate limit (in-memory, per-integration)
5. Execute via http-request with retry
6. Record health stats
7. Emit canonical log fields

Returns (values body status response-headers)."
  (let* ((spec (find-integration integration-name))
         (_ (unless spec (error "Unknown integration: ~A" integration-name)))
         (endpoint (find-endpoint spec endpoint-name))
         (_2 (unless endpoint (error "Unknown endpoint ~A for integration ~A"
                                     endpoint-name integration-name)))
         (url (build-url spec endpoint :path-params path-params :query-params query-params))
         (method (endpoint-spec-method endpoint))
         (content-type (endpoint-spec-content-type endpoint))
         ;; Merge content-type into headers
         (all-headers (cons (cons "Content-Type" content-type)
                            (inject-auth-headers spec headers
                                                 :token credential-value)))
         ;; Encode body if hash-table/list
         (body-string (cond
                        ((null body) nil)
                        ((stringp body) body)
                        (t (cauldron.json:encode body))))
         ;; Retry config
         (retry (integration-spec-retry-config spec))
         (max-retries (or (getf retry :max-retries) 0))
         (base-delay (or (getf retry :base-delay-ms) 500))
         (max-delay (or (getf retry :max-delay-ms) 30000)))
    (declare (ignore _ _2))
    ;; Rate limit check
    (unless (check-rate-limit spec)
      (record-health integration-name 429 0)
      (return-from call-integration
        (values nil 429 nil)))
    ;; Execute with retry
    (let ((start-time (get-internal-real-time)))
      (multiple-value-bind (resp-body status resp-headers)
          (if (plusp max-retries)
              (with-retry (:max-retries max-retries
                           :base-delay-ms base-delay
                           :max-delay-ms max-delay)
                (cauldron.http-client:http-request url
                  :method method :headers all-headers :body body-string))
              (cauldron.http-client:http-request url
                :method method :headers all-headers :body body-string))
        (let* ((end-time (get-internal-real-time))
               (duration-ms (round (* 1000 (/ (- end-time start-time)
                                              internal-time-units-per-second)))))
          ;; Record health
          (record-health integration-name status duration-ms
                         :error (when (>= status 400) resp-body))
          ;; Canonical log
          (when (integration-spec-canonical-log-p spec)
            (cauldron.logging:log-set :integration (string-downcase (string integration-name)))
            (cauldron.logging:log-set :integration-endpoint (string-downcase (string endpoint-name)))
            (cauldron.logging:log-set :integration-status status)
            (cauldron.logging:log-timing :integration-duration-ms duration-ms))
          (values resp-body status resp-headers))))))
