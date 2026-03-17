;;;; src/crucible/bridge.lisp — Bridge between HTTP server and Crucible router
;;;; Converts HTTP requests to conn structs, dispatches through router,
;;;; and converts final conn back to HTTP responses.

(in-package :cauldron.crucible)

;;; --- Request to Conn ---

(defun request-to-conn (request)
  "Convert an HTTP request struct to a Crucible conn struct.
Parses query string and copies all relevant fields."
  (make-conn
   :method (cauldron.http:request-method request)
   :path (cauldron.http:request-path request)
   :query-params (cauldron.http:parse-query-string
                  (cauldron.http:request-query-string request))
   :headers (cauldron.http:request-headers request)
   :body (cauldron.http:request-body request)))

;;; --- Conn to Response ---

(defun conn-to-response (conn)
  "Convert a final conn struct to an HTTP response struct."
  (cauldron.http:make-response
   :status (conn-status conn)
   :headers (conn-resp-headers conn)
   :body (conn-resp-body conn)))

;;; --- App Handler ---

(defun make-app-handler (router &key pipelines)
  "Create an HTTP handler function from a Crucible router.
Returns (lambda (request) response) matching the server handler contract.

PIPELINES is an optional alist of (name . plug-list) to register before
the handler starts serving.

Usage:
  (start-server :handler (make-app-handler *my-router*))"
  ;; Register pipelines if provided
  (when pipelines
    (loop for (name . plugs) in pipelines
          do (make-pipeline name plugs)))
  (lambda (request)
    (let* ((ctx (cauldron.logging:make-context))
           (cauldron.logging:*request-context* ctx)
           ;; Wire HTTP client logging into canonical context
           (cauldron.http-client:*log-external-call-fn*
            (lambda (plist)
              (cauldron.logging:log-inc :external-call-count)
              (cauldron.logging:log-timing :external-call-duration-ms
                                           (getf plist :duration-ms))))
           (request-id (format nil "req_~A"
                               (let ((bytes (cauldron.crypto:secure-random-bytes 8)))
                                 (with-output-to-string (s)
                                   (loop for b across bytes
                                         do (format s "~(~2,'0x~)" b))))))
           (conn (request-to-conn request))
           ;; Store original request and remote addr in assigns
           (conn (conn-put-assign conn :request request))
           (conn (conn-put-assign conn :remote-addr
                    (cauldron.http:request-remote-addr request)))
           (conn (conn-put-assign conn :request-id request-id)))
      ;; Populate canonical log context
      (cauldron.logging:log-set :request-id request-id)
      (cauldron.logging:log-set :method (string (conn-method conn)))
      (cauldron.logging:log-set :path (conn-path conn))
      (let ((result (dispatch router conn)))
        ;; Post-dispatch context fields
        (cauldron.logging:log-set :status (or (conn-status result) 200))
        (let* ((user-id (conn-get-assign result :current-user-id))
               (company-id (conn-get-assign result :company-id))
               (company-slug (conn-get-assign result :company-slug))
               (pipeline (conn-pipeline result))
               (auth-type (conn-get-assign result :auth-type)))
          (when user-id (cauldron.logging:log-set :user-id user-id))
          (when company-id (cauldron.logging:log-set :company-id company-id))
          (when company-slug (cauldron.logging:log-set :company-slug company-slug))
          (when pipeline (cauldron.logging:log-set :pipeline (string pipeline)))
          (when auth-type (cauldron.logging:log-set :auth-type (string auth-type))))
        (compress-response result)
        (log-request-completion result)
        ;; Emit canonical log line
        (cauldron.logging:log-emit)
        (conn-to-response result)))))
