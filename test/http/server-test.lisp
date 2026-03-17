;;;; test/http/server-test.lisp — HTTP server integration tests
;;;; Starts a real server, sends real TCP requests, asserts on responses.

(in-package :cauldron.test)

(defsuite :http-server-integration)

;;; --- Test configuration ---

(defvar *test-server* nil)
(defvar *test-port* 4200)

;;; --- Test handler ---

(defun make-test-handler ()
  "Create a handler for integration tests.
Routes:
  GET  /              → 200 'OK'
  GET  /users/:id     → 200 'user:<id>'
  POST /echo          → 200 with echoed body
  GET  /error         → raises an error (tests 500 handling)
  GET  /headers       → 200 with Content-Type: application/json
  GET  /mounted/hello → 200 'mounted' (via sub-router)"
  (let ((router (cauldron.crucible:make-router))
        (sub-router (cauldron.crucible:make-router)))
    ;; Main routes
    (cauldron.crucible:add-route router :get "/"
      (lambda (conn)
        (setf (cauldron.crucible:conn-status conn) 200)
        (setf (cauldron.crucible:conn-resp-body conn) "OK")
        conn))
    (cauldron.crucible:add-route router :get "/users/:id"
      (lambda (conn)
        (let ((id (cdr (assoc "id" (cauldron.crucible:conn-params conn) :test #'string=))))
          (setf (cauldron.crucible:conn-status conn) 200)
          (setf (cauldron.crucible:conn-resp-body conn) (format nil "user:~A" id))
          conn)))
    (cauldron.crucible:add-route router :post "/echo"
      (lambda (conn)
        (setf (cauldron.crucible:conn-status conn) 200)
        (setf (cauldron.crucible:conn-resp-body conn)
              (let ((body (cauldron.crucible:conn-body conn)))
                (if (typep body '(vector (unsigned-byte 8)))
                    (sb-ext:octets-to-string body :external-format :utf-8)
                    (or body ""))))
        conn))
    (cauldron.crucible:add-route router :get "/error"
      (lambda (conn)
        (declare (ignore conn))
        (error "Intentional test error")))
    (cauldron.crucible:add-route router :get "/headers"
      (lambda (conn)
        (setf (cauldron.crucible:conn-status conn) 200)
        (setf (cauldron.crucible:conn-resp-headers conn)
              (list (cons "Content-Type" "application/json")))
        (setf (cauldron.crucible:conn-resp-body conn) "{}")
        conn))
    ;; Sub-router for mount testing
    (cauldron.crucible:add-route sub-router :get "/hello"
      (lambda (conn)
        (setf (cauldron.crucible:conn-status conn) 200)
        (setf (cauldron.crucible:conn-resp-body conn) "mounted")
        conn))
    (cauldron.crucible:mount router "/mounted" sub-router)
    ;; Bridge: router dispatch → HTTP handler
    (lambda (request)
      (let* ((conn (cauldron.crucible:make-conn
                    :method (cauldron.http:request-method request)
                    :path (cauldron.http:request-path request)
                    :headers (cauldron.http:request-headers request)
                    :body (cauldron.http:request-body request)))
             (result (cauldron.crucible:dispatch router conn)))
        (cauldron.http:make-response
         :status (or (cauldron.crucible:conn-status result) 200)
         :headers (cauldron.crucible:conn-resp-headers result)
         :body (cauldron.crucible:conn-resp-body result))))))

;;; --- Setup/teardown ---

(suite-setup :http-server-integration
  (lambda ()
    (format t "  [setup] Starting test HTTP server on port ~D...~%" *test-port*)
    (setf *test-server*
          (cauldron.http:start-server
           :host "127.0.0.1"
           :port *test-port*
           :handler (make-test-handler)
           :workers 4
           :dev-mode t))
    ;; Give server a moment to bind
    (sleep 0.2)
    (format t "  [setup] Server started.~%")))

(suite-teardown :http-server-integration
  (lambda ()
    (format t "  [teardown] Stopping test HTTP server...~%")
    (when *test-server*
      (cauldron.http:stop-server *test-server*)
      (setf *test-server* nil))
    (format t "  [teardown] Server stopped.~%")))

;;; --- HTTP client helpers ---

(defun send-raw-request (method path &key headers body (connection nil))
  "Send a raw HTTP/1.1 request to the test server.
Returns (values status-code response-headers response-body).
If CONNECTION is provided, reuse that socket/stream pair."
  (let* ((reuse (not (null connection)))
         (socket (if reuse (car connection) nil))
         (stream (if reuse (cdr connection) nil)))
    (unless reuse
      (multiple-value-setq (socket stream)
        (cauldron.runtime:make-tcp-connection "127.0.0.1" *test-port*)))
    (unwind-protect
        (progn
          ;; Write request line
          (write-request-line stream method path)
          ;; Write headers
          (write-header stream "Host" "localhost")
          ;; Add Connection: close by default for clean disconnects
          (unless (assoc "Connection" headers :test #'string-equal)
            (write-header stream "Connection" "close"))
          (dolist (h headers)
            (write-header stream (car h) (cdr h)))
          ;; Write body if present
          (when body
            (let ((octets (if (stringp body)
                              (sb-ext:string-to-octets body :external-format :utf-8)
                              body)))
              (write-header stream "Content-Length" (princ-to-string (length octets)))
              (write-crlf-bytes stream)
              (write-sequence octets stream)))
          (unless body
            (write-crlf-bytes stream))
          (force-output stream)
          ;; Read response
          (read-http-response stream :head-request-p (eq method :head)))
      ;; Cleanup unless reusing
      (unless reuse
        (ignore-errors (close stream))
        (ignore-errors (cauldron.runtime:socket-close socket))))))

(defun send-raw-request-keepalive (method path &key headers body connection)
  "Like send-raw-request but returns (values status headers body socket stream)
for connection reuse."
  (let* ((reuse (not (null connection)))
         (socket (if reuse (car connection) nil))
         (stream (if reuse (cdr connection) nil)))
    (unless reuse
      (multiple-value-setq (socket stream)
        (cauldron.runtime:make-tcp-connection "127.0.0.1" *test-port*)))
    ;; Write request
    (write-request-line stream method path)
    (write-header stream "Host" "localhost")
    (dolist (h headers)
      (write-header stream (car h) (cdr h)))
    (when body
      (let ((octets (if (stringp body)
                        (sb-ext:string-to-octets body :external-format :utf-8)
                        body)))
        (write-header stream "Content-Length" (princ-to-string (length octets)))
        (write-crlf-bytes stream)
        (write-sequence octets stream)))
    (unless body
      (write-crlf-bytes stream))
    (force-output stream)
    ;; Read response
    (multiple-value-bind (status resp-headers resp-body)
        (read-http-response stream :head-request-p (eq method :head))
      (values status resp-headers resp-body socket stream))))

(defun write-request-line (stream method path)
  "Write 'METHOD path HTTP/1.1\\r\\n' to stream."
  (let ((line (format nil "~A ~A HTTP/1.1" (string-upcase (string method)) path)))
    (write-sequence (sb-ext:string-to-octets line :external-format :ascii) stream)
    (write-crlf-bytes stream)))

(defun write-header (stream name value)
  "Write 'Name: value\\r\\n' to stream."
  (let ((line (format nil "~A: ~A" name value)))
    (write-sequence (sb-ext:string-to-octets line :external-format :ascii) stream)
    (write-crlf-bytes stream)))

(defun write-crlf-bytes (stream)
  "Write CRLF to a binary stream."
  (write-byte 13 stream)
  (write-byte 10 stream))

(defun read-http-response (stream &key head-request-p)
  "Read an HTTP response from stream.
Returns (values status-code headers body-string).
If HEAD-REQUEST-P, skip body reading (HEAD responses have no body)."
  ;; Read status line
  (let* ((status-line (read-line-from-stream stream))
         (status-code (parse-status-code status-line))
         (headers '())
         (body ""))
    ;; Read headers
    (loop for line = (read-line-from-stream stream)
          while (and line (> (length line) 0))
          do (let ((colon (position #\: line)))
               (when colon
                 (push (cons (subseq line 0 colon)
                             (string-trim '(#\Space) (subseq line (1+ colon))))
                       headers))))
    (setf headers (nreverse headers))
    ;; Read body based on Content-Length (skip for HEAD requests)
    (unless head-request-p
      (let ((cl-header (cdr (assoc "Content-Length" headers :test #'string-equal))))
        (when (and cl-header (> (parse-integer cl-header :junk-allowed t) 0))
          (let* ((len (parse-integer cl-header))
                 (buf (make-array len :element-type '(unsigned-byte 8))))
            (read-sequence buf stream)
            (setf body (sb-ext:octets-to-string buf :external-format :utf-8))))))
    (values status-code headers body)))

(defun read-line-from-stream (stream)
  "Read a CRLF-terminated line from a binary stream. Returns string or NIL."
  (let ((bytes (make-array 0 :element-type '(unsigned-byte 8)
                             :adjustable t :fill-pointer 0)))
    (handler-case
        (loop
          (let ((byte (read-byte stream)))
            (cond
              ((= byte 13) ; CR
               (let ((next (read-byte stream nil nil)))
                 (when (and next (= next 10)) ; LF
                   (return (sb-ext:octets-to-string bytes :external-format :utf-8)))
                 (vector-push-extend byte bytes)
                 (when next (vector-push-extend next bytes))))
              (t (vector-push-extend byte bytes)))))
      (end-of-file ()
        (if (> (length bytes) 0)
            (sb-ext:octets-to-string bytes :external-format :utf-8)
            nil)))))

(defun parse-status-code (status-line)
  "Extract the numeric status code from 'HTTP/1.1 200 OK'."
  (when status-line
    (let ((sp1 (position #\Space status-line)))
      (when sp1
        (parse-integer status-line :start (1+ sp1) :junk-allowed t)))))

;;; =====================
;;; Tests
;;; =====================

;;; --- Basic request/response ---

(deftest test-http-get-root
  (multiple-value-bind (status headers body)
      (send-raw-request :get "/")
    (declare (ignore headers))
    (is-equal 200 status "GET / returns 200")
    (is-equal "OK" body "GET / body is 'OK'")))

(deftest test-http-get-not-found
  (multiple-value-bind (status headers body)
      (send-raw-request :get "/not-found")
    (declare (ignore headers))
    (is-equal 404 status "GET /not-found returns 404")
    (is-equal "Not Found" body "404 body")))

(deftest test-http-post-echo
  (multiple-value-bind (status headers body)
      (send-raw-request :post "/echo" :body "hello world")
    (declare (ignore headers))
    (is-equal 200 status "POST /echo returns 200")
    (is-equal "hello world" body "POST /echo echoes body")))

(deftest test-http-head-no-body
  (multiple-value-bind (status headers body)
      (send-raw-request :head "/")
    (declare (ignore headers))
    (is-equal 200 status "HEAD / returns 200")
    (is-equal "" body "HEAD response has no body")))

(deftest test-http-response-content-type
  (multiple-value-bind (status headers body)
      (send-raw-request :get "/headers")
    (declare (ignore status body))
    (let ((ct (cdr (assoc "Content-Type" headers :test #'string-equal))))
      (is-not-nil ct "Response includes Content-Type")
      (is-equal "application/json" ct "Content-Type is application/json"))))

(deftest test-http-response-content-length
  (multiple-value-bind (status headers body)
      (send-raw-request :get "/")
    (declare (ignore status body))
    (let ((cl (cdr (assoc "Content-Length" headers :test #'string-equal))))
      (is-not-nil cl "Response includes Content-Length")
      (is-equal "2" cl "Content-Length for 'OK' is 2"))))

;;; --- Keep-alive ---

(deftest test-http-keepalive-two-requests
  "Two requests on the same connection both succeed."
  (multiple-value-bind (status1 headers1 body1 socket stream)
      (send-raw-request-keepalive :get "/"
        :headers (list (cons "Connection" "keep-alive")))
    (declare (ignore headers1))
    (is-equal 200 status1 "First request returns 200")
    (is-equal "OK" body1 "First request body")
    ;; Second request on same connection
    (multiple-value-bind (status2 headers2 body2)
        (send-raw-request-keepalive :get "/users/42"
          :headers (list (cons "Connection" "keep-alive"))
          :connection (cons socket stream))
      (declare (ignore headers2))
      (is-equal 200 status2 "Second request returns 200")
      (is-equal "user:42" body2 "Second request body"))
    ;; Cleanup
    (ignore-errors (close stream))
    (ignore-errors (cauldron.runtime:socket-close socket))))

(deftest test-http-connection-close
  "Connection: close header causes server to close after response."
  (multiple-value-bind (status headers body)
      (send-raw-request :get "/"
        :headers (list (cons "Connection" "close")))
    (declare (ignore body))
    (is-equal 200 status)
    (let ((conn-header (cdr (assoc "Connection" headers :test #'string-equal))))
      (is-not-nil conn-header "Response has Connection header")
      (is (search "close" (or conn-header "") :test #'char-equal)
          "Connection header contains 'close'"))))

(deftest test-http-connection-keepalive-header
  "Connection: keep-alive header is echoed back."
  (multiple-value-bind (status headers body)
      (send-raw-request :get "/"
        :headers (list (cons "Connection" "keep-alive")))
    (declare (ignore status body))
    (let ((conn-header (cdr (assoc "Connection" headers :test #'string-equal))))
      (is-not-nil conn-header "Response has Connection header")
      (is (search "keep-alive" (or conn-header "") :test #'char-equal)
          "Connection header contains 'keep-alive'"))))

;;; --- Error handling ---

(deftest test-http-handler-error-500
  "Handler exception returns 500."
  (multiple-value-bind (status headers body)
      (send-raw-request :get "/error")
    (declare (ignore headers))
    (is-equal 500 status "Error route returns 500")
    (is-not-nil body "500 has a body")))

(deftest test-http-dev-mode-stack-trace
  "Dev-mode 500 includes error details."
  (multiple-value-bind (status headers body)
      (send-raw-request :get "/error")
    (declare (ignore headers))
    (is-equal 500 status)
    ;; Server is started in dev-mode, so body should include the error message
    (is (search "Intentional test error" body :test #'char-equal)
        "Dev-mode 500 includes error text")))

;;; --- Concurrency ---

(deftest test-http-concurrent-requests
  "10 concurrent requests all return correct responses."
  (let ((results (make-array 10 :initial-element nil))
        (threads '()))
    ;; Spawn 10 threads, each sends a request
    (dotimes (i 10)
      (let ((idx i))
        (push (cauldron.runtime:spawn
               (format nil "test-concurrent-~D" idx)
               (lambda ()
                 (handler-case
                     (multiple-value-bind (status headers body)
                         (send-raw-request :get (format nil "/users/~D" idx))
                       (declare (ignore headers))
                       (setf (aref results idx)
                             (list status body)))
                   (error (c)
                     (setf (aref results idx) (list :error (format nil "~A" c)))))))
              threads)))
    ;; Wait for all threads
    (dolist (thread threads)
      (ignore-errors (cauldron.runtime:join-thread thread)))
    ;; Check results
    (dotimes (i 10)
      (let ((result (aref results i)))
        (is-not-nil result (format nil "Thread ~D got a result" i))
        (when result
          (is-equal 200 (first result) (format nil "Thread ~D status 200" i))
          (is-equal (format nil "user:~D" i) (second result)
                    (format nil "Thread ~D body correct" i)))))))

;;; --- Server lifecycle ---

(deftest test-http-server-running-p
  "server-running-p returns T while server is running."
  (is-true (cauldron.http:server-running-p *test-server*)
           "Server reports running"))

(deftest test-http-server-lifecycle
  "start-server returns server struct, stop makes it not running."
  (let ((server (cauldron.http:start-server
                 :host "127.0.0.1"
                 :port 4201
                 :handler (lambda (req)
                            (declare (ignore req))
                            (cauldron.http:make-response :status 200 :body "temp"))
                 :workers 2)))
    (sleep 0.1)
    (is-true (cauldron.http:server-running-p server) "Temp server is running")
    (cauldron.http:stop-server server)
    (is-false (cauldron.http:server-running-p server) "Temp server stopped")))

;;; --- Router integration ---

(deftest test-http-path-params
  "Route with path params captures param correctly."
  (multiple-value-bind (status headers body)
      (send-raw-request :get "/users/99")
    (declare (ignore headers))
    (is-equal 200 status)
    (is-equal "user:99" body "Path param :id captured as 99")))

(deftest test-http-mounted-subrouter
  "Mounted sub-router dispatches to correct handler."
  (multiple-value-bind (status headers body)
      (send-raw-request :get "/mounted/hello")
    (declare (ignore headers))
    (is-equal 200 status "Mounted route returns 200")
    (is-equal "mounted" body "Mounted route body correct")))
