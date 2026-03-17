;;;; test/crucible/security-test.lisp — Phase 45: Security hardening tests
(in-package :cauldron.test)

(defsuite :security-hardening)

;;; ============================================================
;;; CSP
;;; ============================================================

(deftest test-csp-plug-default
  (let ((plug (cauldron.crucible:make-plug-content-security-policy)))
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
           (result (funcall plug conn)))
      (let ((csp (cdr (assoc "Content-Security-Policy"
                              (cauldron.crucible:conn-resp-headers result) :test #'string=))))
        (is-not-nil csp "CSP header set")
        (is (search "default-src 'self'" csp) "default-src present")
        (is (search "frame-ancestors 'none'" csp) "frame-ancestors present")))))

(deftest test-csp-plug-custom
  (let ((plug (cauldron.crucible:make-plug-content-security-policy
               :script-src "'self' https://cdn.example.com"
               :img-src "'self' https://images.example.com")))
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
           (result (funcall plug conn)))
      (let ((csp (cdr (assoc "Content-Security-Policy"
                              (cauldron.crucible:conn-resp-headers result) :test #'string=))))
        (is (search "cdn.example.com" csp) "Custom script-src")
        (is (search "images.example.com" csp) "Custom img-src")))))

;;; ============================================================
;;; HSTS
;;; ============================================================

(deftest test-hsts-plug-default
  (let ((plug (cauldron.crucible:make-plug-hsts)))
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
           (result (funcall plug conn)))
      (let ((hsts (cdr (assoc "Strict-Transport-Security"
                               (cauldron.crucible:conn-resp-headers result) :test #'string=))))
        (is-not-nil hsts)
        (is (search "max-age=31536000" hsts) "Default max-age 1 year")))))

(deftest test-hsts-plug-with-subdomains
  (let ((plug (cauldron.crucible:make-plug-hsts :include-subdomains t)))
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
           (result (funcall plug conn)))
      (let ((hsts (cdr (assoc "Strict-Transport-Security"
                               (cauldron.crucible:conn-resp-headers result) :test #'string=))))
        (is (search "includeSubDomains" hsts))))))

(deftest test-hsts-plug-with-preload
  (let ((plug (cauldron.crucible:make-plug-hsts :preload t :include-subdomains t)))
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
           (result (funcall plug conn)))
      (let ((hsts (cdr (assoc "Strict-Transport-Security"
                               (cauldron.crucible:conn-resp-headers result) :test #'string=))))
        (is (search "preload" hsts))))))

(deftest test-hsts-plug-custom-max-age
  (let ((plug (cauldron.crucible:make-plug-hsts :max-age 86400)))
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
           (result (funcall plug conn)))
      (let ((hsts (cdr (assoc "Strict-Transport-Security"
                               (cauldron.crucible:conn-resp-headers result) :test #'string=))))
        (is (search "max-age=86400" hsts))))))

;;; ============================================================
;;; Enhanced Security Headers
;;; ============================================================

(deftest test-enhanced-security-headers
  (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
         (result (cauldron.crucible:plug-secure-headers-enhanced conn))
         (headers (cauldron.crucible:conn-resp-headers result)))
    (is-not-nil (assoc "X-Content-Type-Options" headers :test #'string=))
    (is-not-nil (assoc "X-Frame-Options" headers :test #'string=))
    (is-not-nil (assoc "Referrer-Policy" headers :test #'string=))
    (is-not-nil (assoc "Permissions-Policy" headers :test #'string=))
    (is-not-nil (assoc "Cross-Origin-Opener-Policy" headers :test #'string=))
    (is-not-nil (assoc "Cross-Origin-Resource-Policy" headers :test #'string=))))

(deftest test-enhanced-headers-payment-policy
  (let* ((conn (cauldron.crucible:make-conn :method :get :path "/"))
         (result (cauldron.crucible:plug-secure-headers-enhanced conn))
         (pp (cdr (assoc "Permissions-Policy" (cauldron.crucible:conn-resp-headers result) :test #'string=))))
    (is (search "payment=()" pp) "Payment policy set")))

;;; ============================================================
;;; IP Allowlist
;;; ============================================================

(deftest test-ip-allowlist-allowed
  (let ((plug (cauldron.crucible:make-plug-ip-allowlist '("10.0.0.1" "10.0.0.2"))))
    (let* ((conn (cauldron.crucible:make-conn
                  :method :get :path "/admin"
                  :headers '(("X-Forwarded-For" . "10.0.0.1"))))
           (result (funcall plug conn)))
      (is (not (cauldron.crucible:conn-halted-p result)) "Allowed IP passes"))))

(deftest test-ip-allowlist-blocked
  (let ((plug (cauldron.crucible:make-plug-ip-allowlist '("10.0.0.1"))))
    (let* ((conn (cauldron.crucible:make-conn
                  :method :get :path "/admin"
                  :headers '(("X-Forwarded-For" . "192.168.1.1"))))
           (result (funcall plug conn)))
      (is-equal 403 (cauldron.crucible:conn-status result)))))

(deftest test-ip-allowlist-x-real-ip
  (let ((plug (cauldron.crucible:make-plug-ip-allowlist '("172.16.0.1"))))
    (let* ((conn (cauldron.crucible:make-conn
                  :method :get :path "/admin"
                  :headers '(("X-Real-IP" . "172.16.0.1"))))
           (result (funcall plug conn)))
      (is (not (cauldron.crucible:conn-halted-p result)) "X-Real-IP fallback works"))))

;;; ============================================================
;;; Request Size Limit
;;; ============================================================

(deftest test-request-size-limit-under
  (let ((plug (cauldron.crucible:make-plug-request-size-limit :max-json-bytes 1000)))
    (let* ((conn (cauldron.crucible:make-conn
                  :method :post :path "/api/data"
                  :headers '(("Content-Type" . "application/json")
                             ("Content-Length" . "500"))))
           (result (funcall plug conn)))
      (is (not (cauldron.crucible:conn-halted-p result)) "Under limit passes"))))

(deftest test-request-size-limit-over
  (let ((plug (cauldron.crucible:make-plug-request-size-limit :max-json-bytes 1000)))
    (let* ((conn (cauldron.crucible:make-conn
                  :method :post :path "/api/data"
                  :headers '(("Content-Type" . "application/json")
                             ("Content-Length" . "5000"))))
           (result (funcall plug conn)))
      (is-equal 413 (cauldron.crucible:conn-status result)))))

(deftest test-request-size-limit-multipart
  (let ((plug (cauldron.crucible:make-plug-request-size-limit
               :max-json-bytes 1000 :max-multipart-bytes 5000)))
    (let* ((conn (cauldron.crucible:make-conn
                  :method :post :path "/api/upload"
                  :headers '(("Content-Type" . "multipart/form-data; boundary=abc")
                             ("Content-Length" . "3000"))))
           (result (funcall plug conn)))
      (is (not (cauldron.crucible:conn-halted-p result)) "Multipart under limit passes"))))

(deftest test-request-size-limit-multipart-over
  (let ((plug (cauldron.crucible:make-plug-request-size-limit
               :max-json-bytes 1000 :max-multipart-bytes 5000)))
    (let* ((conn (cauldron.crucible:make-conn
                  :method :post :path "/api/upload"
                  :headers '(("Content-Type" . "multipart/form-data; boundary=abc")
                             ("Content-Length" . "10000"))))
           (result (funcall plug conn)))
      (is-equal 413 (cauldron.crucible:conn-status result)))))

(deftest test-request-size-limit-no-content-length
  (let ((plug (cauldron.crucible:make-plug-request-size-limit :max-json-bytes 1000)))
    (let* ((conn (cauldron.crucible:make-conn
                  :method :post :path "/api/data"
                  :headers '(("Content-Type" . "application/json"))))
           (result (funcall plug conn)))
      (is (not (cauldron.crucible:conn-halted-p result)) "No Content-Length passes"))))

;;; ============================================================
;;; Secure Random String
;;; ============================================================

(deftest test-secure-random-string-length
  (let ((s (cauldron.crucible:secure-random-string 32)))
    (is-equal 32 (length s))))

(deftest test-secure-random-string-uniqueness
  (let ((s1 (cauldron.crucible:secure-random-string 32))
        (s2 (cauldron.crucible:secure-random-string 32)))
    (is (not (string= s1 s2)) "Random strings are unique")))

(deftest test-secure-random-string-custom-alphabet
  (let ((s (cauldron.crucible:secure-random-string 20 :alphabet "01")))
    (is (every (lambda (c) (or (char= c #\0) (char= c #\1))) s)
        "Only uses alphabet chars")))

;;; ============================================================
;;; Session Fixation Protection
;;; ============================================================

(deftest test-regenerate-session-id
  (let ((id1 (cauldron.crucible:regenerate-session-id))
        (id2 (cauldron.crucible:regenerate-session-id)))
    (is (stringp id1) "Returns string")
    (is (> (length id1) 0) "Non-empty")
    (is (not (string= id1 id2)) "Unique each time")))

;;; ============================================================
;;; SQL Identifier Validation
;;; ============================================================

(deftest test-validate-sql-identifier-valid
  (is-equal "users" (cauldron.crucible:validate-sql-identifier "users"))
  (is-equal "user_name" (cauldron.crucible:validate-sql-identifier "user_name"))
  (is-equal "table123" (cauldron.crucible:validate-sql-identifier "table123")))

(deftest test-validate-sql-identifier-invalid-chars
  (signals-condition error
    (cauldron.crucible:validate-sql-identifier "users; DROP TABLE")
    "SQL injection attempt rejected"))

(deftest test-validate-sql-identifier-empty
  (signals-condition error
    (cauldron.crucible:validate-sql-identifier "")
    "Empty identifier rejected"))

(deftest test-validate-sql-identifier-special-chars
  (signals-condition error
    (cauldron.crucible:validate-sql-identifier "table-name")
    "Hyphen rejected")
  (signals-condition error
    (cauldron.crucible:validate-sql-identifier "table.name")
    "Dot rejected"))

;;; ============================================================
;;; IP Parsing
;;; ============================================================

(deftest test-parse-ip-xff-with-chain
  (let* ((conn (cauldron.crucible:make-conn
                :method :get :path "/"
                :headers '(("X-Forwarded-For" . "1.2.3.4, 5.6.7.8"))))
         (ip (cauldron.crucible::%parse-ip-from-conn conn)))
    (is-equal "1.2.3.4" ip "Takes first IP from X-Forwarded-For chain")))
