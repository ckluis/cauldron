;;;; test/crucible/api-keys-test.lisp — Phase 40: API key tests
(in-package :cauldron.test)

(defsuite :api-keys)

;;; ============================================================
;;; Key Generation
;;; ============================================================

(deftest test-generate-api-key-format
  (multiple-value-bind (full-key prefix hash)
      (cauldron.crucible:generate-api-key)
    ;; Full key starts with cld_
    (is (and (stringp full-key)
             (string= "cld_" (subseq full-key 0 4)))
        "Key starts with cld_")
    ;; Full key is 44 chars (cld_ + 40 hex)
    (is-equal 44 (length full-key))
    ;; Prefix is first 12 chars
    (is-equal 12 (length prefix))
    (is-equal prefix (subseq full-key 0 12))
    ;; Hash is SHA-256 (64 hex chars)
    (is-equal 64 (length hash))))

(deftest test-generate-api-key-uniqueness
  (multiple-value-bind (k1) (cauldron.crucible:generate-api-key)
    (multiple-value-bind (k2) (cauldron.crucible:generate-api-key)
      (is (not (string= k1 k2)) "Generated keys are unique"))))

(deftest test-generate-api-key-hash-matches
  (multiple-value-bind (full-key prefix hash)
      (cauldron.crucible:generate-api-key)
    (declare (ignore prefix))
    ;; Re-hash the full key should match
    (is-equal hash (cauldron.crucible::%octets-to-hex (cauldron.crypto:sha256 full-key))
              "Hash matches SHA-256 of full key")))

;;; ============================================================
;;; Key Storage SQL
;;; ============================================================

(deftest test-create-api-key-sql
  (multiple-value-bind (sql params)
      (cauldron.crucible:create-api-key-sql "cld_abc12345" "deadbeef" "Test Key"
                                             :user-id 1 :company-id 2
                                             :scopes '("read" "write"))
    (is (search "INSERT INTO api_keys" sql) "INSERT statement")
    (is (search "RETURNING" sql) "Has RETURNING clause")
    (is-equal 7 (length params))
    (is-equal "cld_abc12345" (first params))
    (is-equal "deadbeef" (second params))
    (is-equal 1 (third params))
    (is-equal 2 (fourth params))
    (is-equal "Test Key" (fifth params))))

(deftest test-create-api-key-sql-no-scopes
  (multiple-value-bind (sql params)
      (cauldron.crucible:create-api-key-sql "prefix" "hash" "Name")
    (is (search "INSERT" sql))
    ;; scopes param should be "[]"
    (is-equal "[]" (sixth params))))

(deftest test-revoke-api-key-sql
  (multiple-value-bind (sql params)
      (cauldron.crucible:revoke-api-key-sql 42)
    (is (search "UPDATE api_keys" sql))
    (is (search "revoked_at" sql))
    (is-equal 1 (length params))
    (is-equal 42 (first params))))

(deftest test-list-api-keys-sql-by-company
  (multiple-value-bind (sql params)
      (cauldron.crucible:list-api-keys-sql :company-id 5)
    (is (search "SELECT" sql))
    (is (not (search "key_hash" sql)) "Hash excluded from SELECT")
    (is-equal 1 (length params))
    (is-equal 5 (first params))))

(deftest test-list-api-keys-sql-by-user
  (multiple-value-bind (sql params)
      (cauldron.crucible:list-api-keys-sql :user-id 3)
    (is (search "user_id" sql))
    (is-equal 1 (length params))
    (is-equal 3 (first params))))

(deftest test-list-api-keys-sql-all
  (multiple-value-bind (sql params)
      (cauldron.crucible:list-api-keys-sql)
    (is (search "SELECT" sql))
    (is-nil params)))

(deftest test-find-api-key-by-hash-sql
  (multiple-value-bind (sql params)
      (cauldron.crucible:find-api-key-by-hash-sql "abc123")
    (is (search "key_hash" sql))
    (is-equal 1 (length params))
    (is-equal "abc123" (first params))))

(deftest test-touch-api-key-sql
  (multiple-value-bind (sql params)
      (cauldron.crucible:touch-api-key-sql 10)
    (is (search "last_used_at" sql))
    (is-equal 1 (length params))))

(deftest test-rotate-api-key-sql
  (let ((result (cauldron.crucible:rotate-api-key-sql 1 "new-prefix" "new-hash")))
    (is-equal 2 (length result) "Two SQL operations")
    ;; First: revoke old
    (is (search "revoked_at" (car (first result))) "First revokes")
    ;; Second: insert new from old
    (is (search "INSERT" (car (second result))) "Second inserts")))

;;; ============================================================
;;; Auth Plug
;;; ============================================================

(deftest test-plug-api-key-auth-no-header
  (let ((plug (cauldron.crucible:make-plug-api-key-auth)))
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/api/test"))
           (result (funcall plug conn)))
      (is-equal 401 (cauldron.crucible:conn-status result)))))

(deftest test-plug-api-key-auth-invalid-scheme
  (let ((plug (cauldron.crucible:make-plug-api-key-auth)))
    (let* ((conn (cauldron.crucible:make-conn
                  :method :get :path "/api/test"
                  :headers '(("Authorization" . "Basic abc123"))))
           (result (funcall plug conn)))
      (is-equal 401 (cauldron.crucible:conn-status result)))))

(deftest test-plug-api-key-auth-valid-bearer
  (let ((plug (cauldron.crucible:make-plug-api-key-auth)))
    (let* ((conn (cauldron.crucible:make-conn
                  :method :get :path "/api/test"
                  :headers '(("Authorization" . "Bearer cld_test1234567890abcdef"))))
           (result (funcall plug conn)))
      ;; Should set hash assign and not halt
      (is (not (cauldron.crucible:conn-halted-p result)) "Not halted")
      (is-not-nil (cauldron.crucible:conn-get-assign result :api-key-hash)
                  "Hash assign set"))))

(deftest test-plug-api-key-auth-sets-correct-hash
  (let ((plug (cauldron.crucible:make-plug-api-key-auth))
        (token "cld_test1234567890abcdef1234567890abcdef12"))
    (let* ((conn (cauldron.crucible:make-conn
                  :method :get :path "/api/test"
                  :headers (list (cons "Authorization"
                                       (concatenate 'string "Bearer " token)))))
           (result (funcall plug conn)))
      (is-equal (cauldron.crucible::%octets-to-hex (cauldron.crypto:sha256 token))
                (cauldron.crucible:conn-get-assign result :api-key-hash)
                "Hash matches SHA-256 of token"))))

;;; ============================================================
;;; Scope Plug
;;; ============================================================

(deftest test-plug-require-scope-has-scope
  (let ((plug (cauldron.crucible:make-plug-require-scope "read")))
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/api/test"))
           (conn (cauldron.crucible:conn-put-assign conn :api-key-scopes '("read" "write")))
           (result (funcall plug conn)))
      (is (not (cauldron.crucible:conn-halted-p result)) "Has scope, not halted"))))

(deftest test-plug-require-scope-missing-scope
  (let ((plug (cauldron.crucible:make-plug-require-scope "admin")))
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/api/test"))
           (conn (cauldron.crucible:conn-put-assign conn :api-key-scopes '("read")))
           (result (funcall plug conn)))
      (is-equal 403 (cauldron.crucible:conn-status result)))))

(deftest test-plug-require-scope-no-scopes
  (let ((plug (cauldron.crucible:make-plug-require-scope "read")))
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/api/test"))
           (result (funcall plug conn)))
      (is-equal 403 (cauldron.crucible:conn-status result)))))
