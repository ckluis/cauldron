;;;; src/http/server.lisp — HTTP/1.1 server
;;;; Accept loop with thread pool, keep-alive, graceful shutdown.

(in-package :cauldron.http)

;;; --- Server struct ---

(defstruct server
  "A running HTTP server."
  (host "0.0.0.0" :type string)
  (port 4000 :type integer)
  (handler nil :type (or null function))  ; (request) → response
  (listener nil)                           ; Socket
  (thread nil)                             ; Accept loop thread
  (workers nil)                            ; Thread pool
  (running nil :type boolean)
  (connections 0 :type integer)            ; Active connection count
  ;; Config
  (worker-count 16 :type integer)
  (keep-alive-timeout 30 :type integer)    ; Seconds
  (max-requests-per-conn 1000 :type integer)
  (read-timeout 30 :type integer)          ; Seconds
  (dev-mode nil :type boolean))            ; Show stack traces in errors

;;; --- Connection handling ---

(defun handle-connection (server client-stream remote-addr)
  "Handle an HTTP connection. Supports keep-alive."
  (let ((request-count 0))
    (unwind-protect
        (loop
          (incf request-count)
          (when (> request-count (server-max-requests-per-conn server))
            (return))
          ;; Parse request (with timeout)
          (let* ((timeout (if (= request-count 1)
                              (server-read-timeout server)
                              (server-keep-alive-timeout server)))
                 (request (handler-case
                              (sb-ext:with-timeout timeout
                                (parse-request client-stream :remote-addr remote-addr))
                            (sb-ext:timeout () nil)
                            (error (c)
                              (declare (ignore c))
                              nil))))
            (unless request
              (return))
            ;; Check for keep-alive
            (let* ((connection-header (request-header request "Connection"))
                   (keep-alive (and connection-header
                                    (search "keep-alive" connection-header :test #'char-equal)))
                   (close-requested (and connection-header
                                         (search "close" connection-header :test #'char-equal))))
              ;; Dispatch to handler
              (let ((response (handler-case
                                  (funcall (server-handler server) request)
                                (error (c)
                                  (make-response
                                   :status 500
                                   :headers (list (cons "Content-Type" "text/plain"))
                                   :body (if (server-dev-mode server)
                                             (format nil "Internal Server Error~%~A" c)
                                             "Internal Server Error"))))))
                ;; Add Connection header
                (cond
                  (close-requested
                   (push (cons "Connection" "close") (response-headers response)))
                  (keep-alive
                   (push (cons "Connection" "keep-alive") (response-headers response))))
                ;; Write response
                (write-response response client-stream
                                :head-only (eq (request-method request) :head))
                ;; Close if requested
                (when (or close-requested (not keep-alive))
                  (return))))))
      ;; Cleanup
      (ignore-errors (close client-stream)))))

;;; --- Server lifecycle ---

(defun start-server (&key (host "0.0.0.0") (port 4000) handler
                          (workers 16) (dev-mode nil))
  "Start an HTTP server.
HANDLER is a function (request) → response.
Returns a server struct."
  (unless handler
    (error "HTTP server requires a :handler function"))
  (let ((server (make-server :host host
                              :port port
                              :handler handler
                              :worker-count workers
                              :running t
                              :dev-mode dev-mode)))
    ;; Create listener socket
    (let ((listener (make-instance 'sb-bsd-sockets:inet-socket
                                   :type :stream :protocol :tcp)))
      (setf (sb-bsd-sockets:sockopt-reuse-address listener) t)
      (sb-bsd-sockets:socket-bind listener
                                   (sb-bsd-sockets:make-inet-address host)
                                   port)
      (sb-bsd-sockets:socket-listen listener 128)
      (setf (server-listener server) listener))
    ;; Create worker thread pool
    (let ((pool (cauldron.runtime:make-thread-pool :size workers)))
      (setf (server-workers server) pool))
    ;; Start accept loop
    (setf (server-thread server)
          (cauldron.runtime:spawn "cauldron-http-accept"
            (lambda ()
              (accept-loop server))))
    (format t "~&Cauldron HTTP server listening on ~A:~D (~D workers)~%"
            host port workers)
    server))

(defun accept-loop (server)
  "Accept loop: accept connections and dispatch to worker pool."
  (loop while (server-running server)
        do (handler-case
               (let ((client-socket (sb-bsd-sockets:socket-accept
                                      (server-listener server))))
                 (when client-socket
                   (let* ((peer (ignore-errors
                                  (multiple-value-list
                                   (sb-bsd-sockets:socket-peername client-socket))))
                          (remote-addr (when peer
                                         (format nil "~{~D~^.~}" (coerce (first peer) 'list))))
                          (stream (sb-bsd-sockets:socket-make-stream
                                   client-socket
                                   :input t :output t
                                   :element-type '(unsigned-byte 8)
                                   :buffering :full)))
                     (cauldron.runtime:submit-task
                      (server-workers server)
                      (lambda ()
                        (handle-connection server stream remote-addr))))))
             (error (c)
               (when (server-running server)
                 (format *error-output* "~&Accept error: ~A~%" c))))))

(defun stop-server (server)
  "Stop a running HTTP server gracefully."
  (setf (server-running server) nil)
  ;; Close listener to unblock accept
  (ignore-errors
    (sb-bsd-sockets:socket-close (server-listener server)))
  ;; Wait for accept thread
  (when (server-thread server)
    (ignore-errors
      (cauldron.runtime:join-thread (server-thread server))))
  ;; Shutdown worker pool
  (when (server-workers server)
    (cauldron.runtime:shutdown-pool (server-workers server)))
  (format t "~&Cauldron HTTP server stopped.~%")
  t)

(defun server-running-p (server)
  "Return T if server is running."
  (server-running server))
