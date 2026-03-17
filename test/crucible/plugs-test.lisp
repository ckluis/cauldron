;;;; test/crucible/plugs-test.lisp — Tests for standard plugs (middleware)
(in-package :cauldron.test)

(defsuite :crucible-plugs)

;;; ============================================================
;;; plug-parse-body
;;; ============================================================

(deftest test-parse-body-form-urlencoded
  (let* ((conn (cauldron.crucible:make-conn
                :method :post
                :path "/submit"
                :headers '(("Content-Type" . "application/x-www-form-urlencoded"))
                :body "name=Alice&age=30"))
         (result (cauldron.crucible:plug-parse-body conn)))
    (is (listp (cauldron.crucible:conn-body result)))
    (is-equal "Alice" (cdr (assoc "name" (cauldron.crucible:conn-body result) :test #'string=)))
    (is-equal "30" (cdr (assoc "age" (cauldron.crucible:conn-body result) :test #'string=)))))

(deftest test-parse-body-json
  (let* ((conn (cauldron.crucible:make-conn
                :method :post
                :path "/api/data"
                :headers '(("Content-Type" . "application/json"))
                :body "{\"name\":\"Bob\"}"))
         (result (cauldron.crucible:plug-parse-body conn))
         (body (cauldron.crucible:conn-body result)))
    (is-not-nil body)
    ;; JSON decoder returns hash-table
    (is-equal "Bob" (gethash "name" body))))

(deftest test-parse-body-json-charset
  (let* ((conn (cauldron.crucible:make-conn
                :method :post
                :path "/api"
                :headers '(("Content-Type" . "application/json; charset=utf-8"))
                :body "{\"x\":1}"))
         (result (cauldron.crucible:plug-parse-body conn))
         (body (cauldron.crucible:conn-body result)))
    (is-not-nil body)))

(deftest test-parse-body-no-content-type
  (let* ((conn (cauldron.crucible:make-conn :method :post :path "/" :body "raw data"))
         (result (cauldron.crucible:plug-parse-body conn)))
    (is-equal "raw data" (cauldron.crucible:conn-body result))))

(deftest test-parse-body-nil-body
  (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
         (result (cauldron.crucible:plug-parse-body conn)))
    (is-nil (cauldron.crucible:conn-body result))))

(deftest test-parse-body-stores-raw-body
  (let* ((conn (cauldron.crucible:make-conn
                :method :post
                :path "/"
                :headers '(("Content-Type" . "application/x-www-form-urlencoded"))
                :body "k=v"))
         (result (cauldron.crucible:plug-parse-body conn)))
    (is-equal "k=v" (cauldron.crucible:conn-get-assign result :raw-body))))

(deftest test-parse-body-octet-body-urlencoded
  (let* ((body-octets (let* ((s "foo=bar")
                             (arr (make-array (length s) :element-type '(unsigned-byte 8))))
                        (dotimes (i (length s) arr)
                          (setf (aref arr i) (char-code (char s i))))))
         (conn (cauldron.crucible:make-conn
                :method :post
                :path "/"
                :headers '(("Content-Type" . "application/x-www-form-urlencoded"))
                :body body-octets))
         (result (cauldron.crucible:plug-parse-body conn)))
    (is-equal "bar" (cdr (assoc "foo" (cauldron.crucible:conn-body result) :test #'string=)))))

;;; ============================================================
;;; plug-parse-cookies
;;; ============================================================

(deftest test-parse-cookies-basic
  (let* ((conn (cauldron.crucible:make-conn
                :headers '(("Cookie" . "session=abc123; theme=dark"))))
         (result (cauldron.crucible:plug-parse-cookies conn))
         (cookies (cauldron.crucible:conn-get-assign result :cookies)))
    (is-equal "abc123" (cdr (assoc "session" cookies :test #'string=)))
    (is-equal "dark" (cdr (assoc "theme" cookies :test #'string=)))))

(deftest test-parse-cookies-single
  (let* ((conn (cauldron.crucible:make-conn
                :headers '(("Cookie" . "token=xyz"))))
         (result (cauldron.crucible:plug-parse-cookies conn))
         (cookies (cauldron.crucible:conn-get-assign result :cookies)))
    (is-equal 1 (length cookies))
    (is-equal "xyz" (cdr (assoc "token" cookies :test #'string=)))))

(deftest test-parse-cookies-none
  (let* ((conn (cauldron.crucible:make-conn :headers '()))
         (result (cauldron.crucible:plug-parse-cookies conn))
         (cookies (cauldron.crucible:conn-get-assign result :cookies)))
    (is-nil cookies)))

(deftest test-parse-cookies-empty-value
  (let* ((conn (cauldron.crucible:make-conn
                :headers '(("Cookie" . "name="))))
         (result (cauldron.crucible:plug-parse-cookies conn))
         (cookies (cauldron.crucible:conn-get-assign result :cookies)))
    (is-equal "" (cdr (assoc "name" cookies :test #'string=)))))

;;; ============================================================
;;; plug-set-cookie
;;; ============================================================

(deftest test-set-cookie-basic
  (let* ((conn (cauldron.crucible:make-conn))
         (result (cauldron.crucible:plug-set-cookie conn "sid" "abc123")))
    (let ((cookie-header (cdr (assoc "Set-Cookie" (cauldron.crucible:conn-resp-headers result)
                                     :test #'string-equal))))
      (is-not-nil cookie-header)
      (is (search "sid=abc123" cookie-header)))))

(deftest test-set-cookie-with-options
  (let* ((conn (cauldron.crucible:make-conn))
         (result (cauldron.crucible:plug-set-cookie conn "sid" "xyz"
                   :path "/" :http-only t :secure t
                   :same-site "Strict" :max-age 3600)))
    (let ((cookie-header (cdr (assoc "Set-Cookie" (cauldron.crucible:conn-resp-headers result)
                                     :test #'string-equal))))
      (is (search "sid=xyz" cookie-header))
      (is (search "Path=/" cookie-header))
      (is (search "HttpOnly" cookie-header))
      (is (search "Secure" cookie-header))
      (is (search "SameSite=Strict" cookie-header))
      (is (search "Max-Age=3600" cookie-header)))))

;;; ============================================================
;;; plug-session
;;; ============================================================

(deftest test-session-creates-new
  (let ((cauldron.crucible:*session-secret* "test-secret-key-for-session-tests")
        (cauldron.crucible:*session-store* (make-hash-table :test 'equal)))
    (let* ((conn (cauldron.crucible:make-conn))
           ;; Need cookies parsed first
           (conn (cauldron.crucible:plug-parse-cookies conn))
           (result (cauldron.crucible:plug-session conn)))
      (is-not-nil (cauldron.crucible:conn-get-assign result :session-id))
      (is-not-nil (cauldron.crucible:conn-get-assign result :session))
      ;; Should set a session cookie
      (let ((set-cookie (cdr (assoc "Set-Cookie" (cauldron.crucible:conn-resp-headers result)
                                    :test #'string-equal))))
        (is-not-nil set-cookie)
        (is (search "_cauldron_session=" set-cookie))))))

(deftest test-session-loads-existing
  (let* ((cauldron.crucible:*session-secret* "test-secret-key-for-session-tests")
         (cauldron.crucible:*session-store* (make-hash-table :test 'equal))
         ;; Create a session first
         (session-id (cauldron.crypto:generate-token :length 32))
         (session-data (make-hash-table :test 'equal)))
    (setf (gethash "user" session-data) "Alice")
    (setf (gethash session-id cauldron.crucible:*session-store*) session-data)
    ;; Build signed cookie
    (let* ((signed-cookie (cauldron.crucible::sign-session-id session-id))
           (cookie-header (format nil "_cauldron_session=~A" signed-cookie))
           (conn (cauldron.crucible:make-conn
                  :headers (list (cons "Cookie" cookie-header))))
           (conn (cauldron.crucible:plug-parse-cookies conn))
           (result (cauldron.crucible:plug-session conn)))
      (is-equal session-id (cauldron.crucible:conn-get-assign result :session-id))
      (is-equal "Alice" (gethash "user" (cauldron.crucible:conn-get-assign result :session))))))

(deftest test-session-rejects-tampered-cookie
  (let ((cauldron.crucible:*session-secret* "test-secret-key-for-session-tests")
        (cauldron.crucible:*session-store* (make-hash-table :test 'equal)))
    (let* ((cookie-header "_cauldron_session=fakeid.badsignature")
           (conn (cauldron.crucible:make-conn
                  :headers (list (cons "Cookie" cookie-header))))
           (conn (cauldron.crucible:plug-parse-cookies conn))
           (result (cauldron.crucible:plug-session conn)))
      ;; Should create a new session, not load the tampered one
      (is-not-nil (cauldron.crucible:conn-get-assign result :session-id))
      (is (not (string= "fakeid" (cauldron.crucible:conn-get-assign result :session-id)))))))

;;; --- Session helpers ---

(deftest test-session-put-get
  (let ((cauldron.crucible:*session-secret* "test-secret")
        (cauldron.crucible:*session-store* (make-hash-table :test 'equal)))
    (let* ((conn (cauldron.crucible:make-conn))
           (conn (cauldron.crucible:plug-parse-cookies conn))
           (conn (cauldron.crucible:plug-session conn)))
      (cauldron.crucible:session-put conn "user" "Bob")
      (is-equal "Bob" (cauldron.crucible:session-get conn "user")))))

(deftest test-session-delete
  (let ((cauldron.crucible:*session-secret* "test-secret")
        (cauldron.crucible:*session-store* (make-hash-table :test 'equal)))
    (let* ((conn (cauldron.crucible:make-conn))
           (conn (cauldron.crucible:plug-parse-cookies conn))
           (conn (cauldron.crucible:plug-session conn)))
      (cauldron.crucible:session-put conn "key" "val")
      (cauldron.crucible:session-delete conn "key")
      (is-nil (cauldron.crucible:session-get conn "key")))))

(deftest test-session-clear
  (let ((cauldron.crucible:*session-secret* "test-secret")
        (cauldron.crucible:*session-store* (make-hash-table :test 'equal)))
    (let* ((conn (cauldron.crucible:make-conn))
           (conn (cauldron.crucible:plug-parse-cookies conn))
           (conn (cauldron.crucible:plug-session conn)))
      (cauldron.crucible:session-put conn "a" "1")
      (cauldron.crucible:session-put conn "b" "2")
      (cauldron.crucible:session-clear conn)
      (is-nil (cauldron.crucible:session-get conn "a"))
      (is-nil (cauldron.crucible:session-get conn "b")))))

;;; ============================================================
;;; plug-csrf
;;; ============================================================

(deftest test-csrf-generates-token-on-get
  (let ((cauldron.crucible:*session-secret* "test-secret")
        (cauldron.crucible:*session-store* (make-hash-table :test 'equal)))
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
           (conn (cauldron.crucible:plug-parse-cookies conn))
           (conn (cauldron.crucible:plug-session conn))
           (result (cauldron.crucible:plug-csrf conn)))
      (is-not-nil (cauldron.crucible:conn-get-assign result :csrf-token))
      (is-false (cauldron.crucible:conn-halted-p result)))))

(deftest test-csrf-validates-post
  (let ((cauldron.crucible:*session-secret* "test-secret")
        (cauldron.crucible:*session-store* (make-hash-table :test 'equal)))
    ;; Step 1: GET to generate token
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
           (conn (cauldron.crucible:plug-parse-cookies conn))
           (conn (cauldron.crucible:plug-session conn))
           (conn (cauldron.crucible:plug-csrf conn))
           (token (cauldron.crucible:conn-get-assign conn :csrf-token))
           (session-id (cauldron.crucible:conn-get-assign conn :session-id))
           ;; Step 2: POST with valid token
           (signed (cauldron.crucible::sign-session-id session-id))
           (post-conn (cauldron.crucible:make-conn
                       :method :post
                       :path "/submit"
                       :headers (list (cons "Cookie"
                                            (format nil "_cauldron_session=~A" signed)))
                       :body (list (cons "_csrf_token" token))))
           (post-conn (cauldron.crucible:plug-parse-cookies post-conn))
           (post-conn (cauldron.crucible:plug-session post-conn))
           (result (cauldron.crucible:plug-csrf post-conn)))
      (is-false (cauldron.crucible:conn-halted-p result)))))

(deftest test-csrf-rejects-missing-token
  (let ((cauldron.crucible:*session-secret* "test-secret")
        (cauldron.crucible:*session-store* (make-hash-table :test 'equal)))
    ;; GET to create session with token
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
           (conn (cauldron.crucible:plug-parse-cookies conn))
           (conn (cauldron.crucible:plug-session conn))
           (conn (cauldron.crucible:plug-csrf conn))
           (session-id (cauldron.crucible:conn-get-assign conn :session-id))
           ;; POST without token
           (signed (cauldron.crucible::sign-session-id session-id))
           (post-conn (cauldron.crucible:make-conn
                       :method :post
                       :path "/submit"
                       :headers (list (cons "Cookie"
                                            (format nil "_cauldron_session=~A" signed)))))
           (post-conn (cauldron.crucible:plug-parse-cookies post-conn))
           (post-conn (cauldron.crucible:plug-session post-conn))
           (result (cauldron.crucible:plug-csrf post-conn)))
      (is-true (cauldron.crucible:conn-halted-p result))
      (is-equal 403 (cauldron.crucible:conn-status result)))))

(deftest test-csrf-rejects-wrong-token
  (let ((cauldron.crucible:*session-secret* "test-secret")
        (cauldron.crucible:*session-store* (make-hash-table :test 'equal)))
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
           (conn (cauldron.crucible:plug-parse-cookies conn))
           (conn (cauldron.crucible:plug-session conn))
           (conn (cauldron.crucible:plug-csrf conn))
           (session-id (cauldron.crucible:conn-get-assign conn :session-id))
           (signed (cauldron.crucible::sign-session-id session-id))
           ;; POST with wrong token
           (post-conn (cauldron.crucible:make-conn
                       :method :post
                       :path "/submit"
                       :headers (list (cons "Cookie"
                                            (format nil "_cauldron_session=~A" signed)))
                       :body (list (cons "_csrf_token" "wrong-token"))))
           (post-conn (cauldron.crucible:plug-parse-cookies post-conn))
           (post-conn (cauldron.crucible:plug-session post-conn))
           (result (cauldron.crucible:plug-csrf post-conn)))
      (is-true (cauldron.crucible:conn-halted-p result))
      (is-equal 403 (cauldron.crucible:conn-status result)))))

(deftest test-csrf-accepts-header-token
  (let ((cauldron.crucible:*session-secret* "test-secret")
        (cauldron.crucible:*session-store* (make-hash-table :test 'equal)))
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
           (conn (cauldron.crucible:plug-parse-cookies conn))
           (conn (cauldron.crucible:plug-session conn))
           (conn (cauldron.crucible:plug-csrf conn))
           (token (cauldron.crucible:conn-get-assign conn :csrf-token))
           (session-id (cauldron.crucible:conn-get-assign conn :session-id))
           (signed (cauldron.crucible::sign-session-id session-id))
           ;; POST with token in X-CSRF-Token header
           (post-conn (cauldron.crucible:make-conn
                       :method :post
                       :path "/api"
                       :headers (list (cons "Cookie"
                                            (format nil "_cauldron_session=~A" signed))
                                      (cons "X-CSRF-Token" token))))
           (post-conn (cauldron.crucible:plug-parse-cookies post-conn))
           (post-conn (cauldron.crucible:plug-session post-conn))
           (result (cauldron.crucible:plug-csrf post-conn)))
      (is-false (cauldron.crucible:conn-halted-p result)))))

;;; ============================================================
;;; plug-flash
;;; ============================================================

(deftest test-flash-loads-and-clears
  (let ((cauldron.crucible:*session-secret* "test-secret")
        (cauldron.crucible:*session-store* (make-hash-table :test 'equal)))
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
           (conn (cauldron.crucible:plug-parse-cookies conn))
           (conn (cauldron.crucible:plug-session conn)))
      ;; Set a flash message
      (cauldron.crucible:put-flash conn :info "Saved!")
      ;; Simulate next request — flash should be available
      (let ((result (cauldron.crucible:plug-flash conn)))
        (let ((flash (cauldron.crucible:conn-get-assign result :flash)))
          (is-not-nil flash)
          (is-equal "Saved!" (cdr (assoc :info flash)))))
      ;; After reading, flash should be cleared from session
      (is-nil (cauldron.crucible:session-get conn "_flash")))))

(deftest test-flash-nil-when-no-flash
  (let ((cauldron.crucible:*session-secret* "test-secret")
        (cauldron.crucible:*session-store* (make-hash-table :test 'equal)))
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
           (conn (cauldron.crucible:plug-parse-cookies conn))
           (conn (cauldron.crucible:plug-session conn))
           (result (cauldron.crucible:plug-flash conn)))
      (is-nil (cauldron.crucible:conn-get-assign result :flash)))))

;;; ============================================================
;;; plug-security-headers
;;; ============================================================

(deftest test-security-headers-set
  (let* ((conn (cauldron.crucible:make-conn))
         (result (cauldron.crucible:plug-security-headers conn))
         (headers (cauldron.crucible:conn-resp-headers result)))
    (is-not-nil (assoc "X-Content-Type-Options" headers :test #'string-equal))
    (is-not-nil (assoc "X-Frame-Options" headers :test #'string-equal))
    (is-not-nil (assoc "Referrer-Policy" headers :test #'string-equal))
    (is-equal "nosniff" (cdr (assoc "X-Content-Type-Options" headers :test #'string-equal)))
    (is-equal "DENY" (cdr (assoc "X-Frame-Options" headers :test #'string-equal)))))

;;; ============================================================
;;; make-plug-static
;;; ============================================================

(deftest test-static-path-traversal-prevention
  (let* ((plug (cauldron.crucible:make-plug-static "/static" :root "/tmp"))
         (conn (cauldron.crucible:make-conn :method :get :path "/static/../etc/passwd"))
         (result (funcall plug conn)))
    ;; Should not halt (let request fall through to router)
    (is-false (cauldron.crucible:conn-halted-p result))))

(deftest test-static-non-matching-path
  (let* ((plug (cauldron.crucible:make-plug-static "/static" :root "/tmp"))
         (conn (cauldron.crucible:make-conn :method :get :path "/api/data"))
         (result (funcall plug conn)))
    (is-false (cauldron.crucible:conn-halted-p result))))

(deftest test-static-post-ignored
  (let* ((plug (cauldron.crucible:make-plug-static "/static" :root "/tmp"))
         (conn (cauldron.crucible:make-conn :method :post :path "/static/test.css"))
         (result (funcall plug conn)))
    (is-false (cauldron.crucible:conn-halted-p result))))

;;; ============================================================
;;; MIME type guessing
;;; ============================================================

(deftest test-mime-type-css
  (is-equal "text/css; charset=utf-8"
            (cauldron.crucible:guess-mime-type "style.css")))

(deftest test-mime-type-js
  (is-equal "application/javascript; charset=utf-8"
            (cauldron.crucible:guess-mime-type "app.js")))

(deftest test-mime-type-png
  (is-equal "image/png"
            (cauldron.crucible:guess-mime-type "logo.png")))

(deftest test-mime-type-unknown
  (is-equal "application/octet-stream"
            (cauldron.crucible:guess-mime-type "data.xyz")))

(deftest test-mime-type-json
  (is-equal "application/json"
            (cauldron.crucible:guess-mime-type "config.json")))

(deftest test-mime-type-html
  (is-equal "text/html; charset=utf-8"
            (cauldron.crucible:guess-mime-type "index.html")))

(deftest test-mime-type-svg
  (is-equal "image/svg+xml"
            (cauldron.crucible:guess-mime-type "icon.svg")))

;;; ============================================================
;;; Session secret enforcement
;;; ============================================================

(deftest test-verify-session-cookie-nil-secret-error
  (let ((cauldron.crucible:*session-secret* nil))
    (signals-condition error
      (cauldron.crucible::verify-session-cookie "someid.somesig")
      "NIL session secret signals error")))
