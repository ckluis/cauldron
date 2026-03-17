;;;; test/integration/credential-test.lisp — Credential encryption tests
(in-package :cauldron.test)

(defsuite :integration-credentials)

;;; --- Helper to set env vars for testing ---
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

(defun %set-env (name value)
  "Set environment variable NAME to VALUE."
  (sb-posix:setenv name value 1))

(defun %unset-env (name)
  "Unset environment variable NAME."
  (sb-posix:unsetenv name))

(defmacro with-test-encryption-key ((key-value) &body body)
  "Execute BODY with CAULDRON_ENCRYPTION_KEY set to KEY-VALUE, restoring after."
  (let ((original (gensym "ORIG")))
    `(let ((,original (sb-ext:posix-getenv "CAULDRON_ENCRYPTION_KEY")))
       (%set-env "CAULDRON_ENCRYPTION_KEY" ,key-value)
       (unwind-protect (progn ,@body)
         (if ,original
             (%set-env "CAULDRON_ENCRYPTION_KEY" ,original)
             (%unset-env "CAULDRON_ENCRYPTION_KEY"))))))

;;; --- Encrypt/decrypt roundtrip ---

(deftest test-credential-encrypt-decrypt-roundtrip
  (with-test-encryption-key ("test-master-key-for-unit-tests")
    (let* ((secret "sk_live_abc123xyz789")
           (encrypted (cauldron.integration::encrypt-credential secret))
           (decrypted (cauldron.integration::decrypt-credential encrypted)))
      ;; Encrypted should be base64 (only valid base64 chars)
      (is (stringp encrypted) "Encrypted value is a string")
      (is (> (length encrypted) 0) "Encrypted value is non-empty")
      ;; Should not contain the original secret
      (is (not (search secret encrypted)) "Encrypted doesn't contain plaintext")
      ;; Roundtrip
      (is-equal secret decrypted "Decrypt restores original"))))

(deftest test-credential-different-encryptions-differ
  (with-test-encryption-key ("test-master-key-2")
    (let* ((secret "my-api-key")
           (enc1 (cauldron.integration::encrypt-credential secret))
           (enc2 (cauldron.integration::encrypt-credential secret)))
      (is (not (equal enc1 enc2)) "Two encryptions of same value differ (random IV)")
      ;; Both should decrypt to same value
      (is-equal secret (cauldron.integration::decrypt-credential enc1))
      (is-equal secret (cauldron.integration::decrypt-credential enc2)))))

;;; --- Key derivation ---

(deftest test-key-derivation-deterministic
  (with-test-encryption-key ("deterministic-test-key")
    (let ((key1 (cauldron.integration::derive-encryption-key))
          (key2 (cauldron.integration::derive-encryption-key)))
      (is-equalp key1 key2 "Same master key derives same encryption key")
      (is-equal 32 (length key1) "Derived key is 32 bytes"))))

;;; --- DDL generation ---

(deftest test-credential-table-ddl
  (let ((spec (cauldron.grimoire:find-table-spec 'integration_credentials)))
    (is-not-nil spec "integration_credentials table registered")
    (is (>= (length (cauldron.grimoire:table-spec-columns spec)) 5)
        "Table has at least 5 columns")))

;;; --- Store/resolve SQL generation ---

(deftest test-store-credential-sql
  (with-test-encryption-key ("test-key-for-sql")
    (multiple-value-bind (sql params)
        (cauldron.integration:store-credential 'stripe :api-key "sk_test_123")
      (is (search "INSERT INTO integration_credentials" sql) "SQL contains INSERT")
      (is (search "ON CONFLICT" sql) "SQL contains upsert")
      (is-equal "stripe" (first params) "Integration name normalized")
      (is-equal :api-key (second params) "Credential key preserved"))))

(deftest test-resolve-credential-sql
  (multiple-value-bind (sql params)
      (cauldron.integration:resolve-credential 'stripe :api-key :company-id 1 :user-id 2)
    (is (search "SELECT encrypted_value" sql) "SQL selects encrypted value")
    (is (search "ORDER BY" sql) "SQL orders by specificity")
    (is-equal "stripe" (first params))
    (is-equal :api-key (second params))))

(deftest test-credential-roundtrip-non-ascii
  (with-test-encryption-key ("test-key-for-unicode")
    (let* ((secret "key-with-ñ-and-ü-and-€")
           (encrypted (cauldron.integration::encrypt-credential secret))
           (decrypted (cauldron.integration::decrypt-credential encrypted)))
      (is-equal secret decrypted "Non-ASCII credential roundtrip works"))))

;;; --- SQL param alignment ---

(deftest test-delete-credential-both-nil
  (multiple-value-bind (sql params)
      (cauldron.integration:delete-credential 'stripe :api-key)
    (is (not (search "company_id" sql)) "No company_id clause when nil")
    (is (not (search "user_id" sql)) "No user_id clause when nil")
    (is-equal 3 (length params) "3 params: name, key, scope")))

(deftest test-delete-credential-company-only
  (multiple-value-bind (sql params)
      (cauldron.integration:delete-credential 'stripe :api-key :company-id 42)
    (is (search "company_id = $4" sql) "company_id uses $4")
    (is (not (search "user_id" sql)) "No user_id clause")
    (is-equal 4 (length params))
    (is-equal 42 (fourth params))))

(deftest test-delete-credential-both-provided
  (multiple-value-bind (sql params)
      (cauldron.integration:delete-credential 'stripe :api-key :company-id 42 :user-id 7)
    (is (search "company_id = $4" sql) "company_id uses $4")
    (is (search "user_id = $5" sql) "user_id uses $5")
    (is-equal 5 (length params))
    (is-equal 42 (fourth params))
    (is-equal 7 (fifth params))))

(deftest test-delete-credential-user-only
  (multiple-value-bind (sql params)
      (cauldron.integration:delete-credential 'stripe :api-key :user-id 7)
    (is (not (search "company_id" sql)) "No company_id clause")
    (is (search "user_id = $4" sql) "user_id uses $4")
    (is-equal 4 (length params))
    (is-equal 7 (fourth params))))

(deftest test-list-credentials-no-filters
  (multiple-value-bind (sql params)
      (cauldron.integration:list-credentials 'stripe)
    (is (not (search "scope = $" sql)) "No scope filter")
    (is (not (search "company_id = $" sql)) "No company_id filter")
    (is-equal 1 (length params))))

(deftest test-list-credentials-with-scope-and-company
  (multiple-value-bind (sql params)
      (cauldron.integration:list-credentials 'stripe :scope "company" :company-id 42)
    (is (search "scope = $2" sql) "scope uses $2")
    (is (search "company_id = $3" sql) "company_id uses $3")
    (is-equal 3 (length params))
    (is-equal "company" (second params))
    (is-equal 42 (third params))))
