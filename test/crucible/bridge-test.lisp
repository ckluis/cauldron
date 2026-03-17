;;;; test/crucible/bridge-test.lisp — Tests for the request-to-conn bridge
(in-package :cauldron.test)

(defsuite :crucible-bridge)

;;; --- request-to-conn ---

(deftest test-request-to-conn-basic
  (let* ((request (cauldron.http:make-request
                   :method :get
                   :path "/hello"
                   :headers '(("Host" . "localhost"))))
         (conn (cauldron.crucible:request-to-conn request)))
    (is-equal :get (cauldron.crucible:conn-method conn))
    (is-equal "/hello" (cauldron.crucible:conn-path conn))
    (is-not-nil (cauldron.crucible:conn-headers conn))
    (is-equal '() (cauldron.crucible:conn-query-params conn))))

(deftest test-request-to-conn-post
  (let* ((request (cauldron.http:make-request
                   :method :post
                   :path "/users"
                   :body "name=Alice"))
         (conn (cauldron.crucible:request-to-conn request)))
    (is-equal :post (cauldron.crucible:conn-method conn))
    (is-equal "name=Alice" (cauldron.crucible:conn-body conn))))

(deftest test-request-to-conn-query-params
  (let* ((request (cauldron.http:make-request
                   :method :get
                   :path "/search"
                   :query-string "q=hello&page=1"))
         (conn (cauldron.crucible:request-to-conn request)))
    (is-equal "hello" (cdr (assoc "q" (cauldron.crucible:conn-query-params conn)
                                  :test #'string=)))
    (is-equal "1" (cdr (assoc "page" (cauldron.crucible:conn-query-params conn)
                              :test #'string=)))))

(deftest test-request-to-conn-no-query-string
  (let* ((request (cauldron.http:make-request :method :get :path "/"))
         (conn (cauldron.crucible:request-to-conn request)))
    (is-nil (cauldron.crucible:conn-query-params conn))))

(deftest test-request-to-conn-headers-copied
  (let* ((headers '(("Content-Type" . "text/html") ("Accept" . "*/*")))
         (request (cauldron.http:make-request :method :get :path "/" :headers headers))
         (conn (cauldron.crucible:request-to-conn request)))
    (is-equal 2 (length (cauldron.crucible:conn-headers conn)))))

;;; --- conn-to-response ---

(deftest test-conn-to-response-basic
  (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
         (_ (cauldron.crucible:conn-put-status conn 200))
         (_ (cauldron.crucible:conn-put-resp-body conn "OK"))
         (response (cauldron.crucible:conn-to-response conn)))
    (declare (ignore _ _))
    (is-equal 200 (cauldron.http:response-status response))
    (is-equal "OK" (cauldron.http:response-body response))))

(deftest test-conn-to-response-status
  (let* ((conn (cauldron.crucible:make-conn)))
    (cauldron.crucible:conn-put-status conn 404)
    (cauldron.crucible:conn-put-resp-body conn "Not Found")
    (let ((response (cauldron.crucible:conn-to-response conn)))
      (is-equal 404 (cauldron.http:response-status response)))))

(deftest test-conn-to-response-headers
  (let* ((conn (cauldron.crucible:make-conn)))
    (cauldron.crucible:conn-put-resp-header conn "Content-Type" "text/html")
    (let* ((response (cauldron.crucible:conn-to-response conn))
           (ct (cdr (assoc "Content-Type" (cauldron.http:response-headers response)
                           :test #'string-equal))))
      (is-equal "text/html" ct))))

;;; --- make-app-handler ---

(deftest test-make-app-handler-basic
  (let* ((router (cauldron.crucible:make-router))
         (handler-fn (lambda (conn)
                       (cauldron.crucible:conn-put-resp-body conn "Hello!")
                       conn)))
    (cauldron.crucible:add-route router :get "/" handler-fn)
    (let* ((app (cauldron.crucible:make-app-handler router))
           (request (cauldron.http:make-request :method :get :path "/"))
           (response (funcall app request)))
      (is-equal 200 (cauldron.http:response-status response))
      (is-equal "Hello!" (cauldron.http:response-body response)))))

(deftest test-make-app-handler-404
  (let* ((router (cauldron.crucible:make-router))
         (app (cauldron.crucible:make-app-handler router))
         (request (cauldron.http:make-request :method :get :path "/missing"))
         (response (funcall app request)))
    (is-equal 404 (cauldron.http:response-status response))))

(deftest test-make-app-handler-stores-request
  (let* ((router (cauldron.crucible:make-router))
         (captured-request nil)
         (handler-fn (lambda (conn)
                       (setf captured-request
                             (cauldron.crucible:conn-get-assign conn :request))
                       conn)))
    (cauldron.crucible:add-route router :get "/" handler-fn)
    (let* ((app (cauldron.crucible:make-app-handler router))
           (request (cauldron.http:make-request :method :get :path "/")))
      (funcall app request)
      (is-not-nil captured-request)
      (is (eq request captured-request)))))

(deftest test-make-app-handler-with-pipelines
  (let* ((router (cauldron.crucible:make-router))
         (plug-ran nil)
         (handler-fn (lambda (conn)
                       (cauldron.crucible:conn-put-resp-body conn "OK")
                       conn)))
    (cauldron.crucible:add-route router :get "/" handler-fn :pipeline :test-pipe)
    (let* ((app (cauldron.crucible:make-app-handler
                 router
                 :pipelines `((:test-pipe .
                               ,(list (lambda (conn)
                                        (setf plug-ran t)
                                        conn))))))
           (request (cauldron.http:make-request :method :get :path "/"))
           (response (funcall app request)))
      (is-true plug-ran)
      (is-equal 200 (cauldron.http:response-status response)))))

(deftest test-make-app-handler-pipeline-halts
  (let* ((router (cauldron.crucible:make-router))
         (handler-fn (lambda (conn)
                       (cauldron.crucible:conn-put-resp-body conn "Should not run")
                       conn)))
    (cauldron.crucible:add-route router :get "/" handler-fn :pipeline :halt-pipe)
    (let* ((app (cauldron.crucible:make-app-handler
                 router
                 :pipelines `((:halt-pipe .
                               ,(list (lambda (conn)
                                        (cauldron.crucible:halt-conn conn
                                          :status 401 :body "Unauthorized")))))))
           (request (cauldron.http:make-request :method :get :path "/"))
           (response (funcall app request)))
      (is-equal 401 (cauldron.http:response-status response))
      (is-equal "Unauthorized" (cauldron.http:response-body response)))))

(deftest test-make-app-handler-query-params-forwarded
  (let* ((router (cauldron.crucible:make-router))
         (captured-q nil)
         (handler-fn (lambda (conn)
                       (setf captured-q
                             (cdr (assoc "q" (cauldron.crucible:conn-query-params conn)
                                         :test #'string=)))
                       conn)))
    (cauldron.crucible:add-route router :get "/search" handler-fn)
    (let* ((app (cauldron.crucible:make-app-handler router))
           (request (cauldron.http:make-request :method :get :path "/search"
                                                :query-string "q=test")))
      (funcall app request)
      (is-equal "test" captured-q))))

;;; --- Request ID format ---

(deftest test-request-id-format
  (let* ((router (cauldron.crucible:make-router))
         (captured-id nil)
         (handler-fn (lambda (conn)
                       (setf captured-id
                             (cauldron.crucible:conn-get-assign conn :request-id))
                       conn)))
    (cauldron.crucible:add-route router :get "/" handler-fn)
    (let* ((app (cauldron.crucible:make-app-handler router))
           (request (cauldron.http:make-request :method :get :path "/")))
      (funcall app request)
      (is-not-nil captured-id "Request ID is set")
      (is (stringp captured-id) "Request ID is a string")
      ;; req_ prefix + 16 hex chars = 20 chars total
      (is-equal 20 (length captured-id)
                (format nil "Request ID has correct length: ~A" captured-id))
      (is (string= "req_" (subseq captured-id 0 4)) "Request ID has req_ prefix"))))
