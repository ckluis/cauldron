;;;; src/integration/credentials.lisp — Encrypted credential storage
(in-package :cauldron.integration)

;;; --- Credential table (deftable for DDL generation) ---

(cauldron.grimoire:deftable integration_credentials
  (:column integration_name :type text :required t)
  (:column credential_key :type text :required t)
  (:column encrypted_value :type text :required t)
  (:column scope :type text :default "'global'")
  (:column company_id :type integer)
  (:column user_id :type integer)
  (:unique integration_name credential_key scope company_id user_id))

;;; --- Config ---

(cauldron.runtime:defconfig encryption-master-key "CAULDRON_ENCRYPTION_KEY"
  :type :string
  :description "Master encryption key for credential storage")

;;; --- Internal encryption helpers ---

(defvar *credential-pbkdf2-salt*
  (cauldron.crypto:ensure-octets "cauldron-credential-store-v1")
  "Static salt for PBKDF2 key derivation from master key.")

(defun derive-encryption-key ()
  "Derive a 32-byte AES-256 key from the master encryption key via PBKDF2."
  (let ((master (encryption-master-key)))
    (unless master
      (error "CAULDRON_ENCRYPTION_KEY environment variable is not set"))
    (cauldron.crypto:pbkdf2-sha256 master *credential-pbkdf2-salt*
                                    :iterations 100000 :key-length 32)))

(defun encrypt-credential (value)
  "Encrypt VALUE string and return base64(IV+ciphertext)."
  (let ((key (derive-encryption-key)))
    (multiple-value-bind (ciphertext iv)
        (cauldron.crypto:aes-256-cbc-encrypt value key)
      (declare (ignore iv))
      ;; ciphertext already has IV prepended
      (cauldron.crypto:base64-encode ciphertext))))

(defun decrypt-credential (stored)
  "Decrypt a base64-encoded credential back to plaintext string."
  (let* ((key (derive-encryption-key))
         (ciphertext (cauldron.crypto:base64-decode stored))
         (plaintext-octets (cauldron.crypto:aes-256-cbc-decrypt ciphertext key)))
    ;; Convert octets back to string
    (sb-ext:octets-to-string plaintext-octets :external-format :utf-8)))

;;; --- Public API ---
;;; Note: These functions generate SQL via the grimoire query DSL.
;;; They require a database connection to execute. For unit testing,
;;; we test the encrypt/decrypt roundtrip and DDL generation independently.

(defun store-credential (integration-name key value &key (scope "global") company-id user-id)
  "Encrypt VALUE and store/upsert the credential.
Returns the SQL and parameters for execution."
  (let ((encrypted (encrypt-credential value))
        (int-name (string-downcase (string integration-name))))
    ;; Return an INSERT ... ON CONFLICT UPDATE SQL and params
    (values
     (format nil "INSERT INTO integration_credentials (integration_name, credential_key, encrypted_value, scope, company_id, user_id, created_at, updated_at) VALUES ($1, $2, $3, $4, $5, $6, NOW(), NOW()) ON CONFLICT (integration_name, credential_key, scope, company_id, user_id) DO UPDATE SET encrypted_value = $3, updated_at = NOW()")
     (list int-name key encrypted scope company-id user-id))))

(defun resolve-credential (integration-name key &key company-id user-id)
  "Build cascading credential resolution SQL: user→company→global.
Returns SQL that tries most-specific scope first."
  (let ((int-name (string-downcase (string integration-name))))
    ;; Build a COALESCE-style query for cascading resolution
    (values
     (format nil "SELECT encrypted_value, scope FROM integration_credentials WHERE integration_name = $1 AND credential_key = $2 AND ((scope = 'user' AND user_id = $3 AND company_id = $4) OR (scope = 'company' AND company_id = $4 AND user_id IS NULL) OR (scope = 'global' AND company_id IS NULL AND user_id IS NULL)) ORDER BY CASE scope WHEN 'user' THEN 1 WHEN 'company' THEN 2 WHEN 'global' THEN 3 END LIMIT 1")
     (list int-name key user-id company-id))))

(defun delete-credential (integration-name key &key (scope "global") company-id user-id)
  "Build SQL to delete a specific credential."
  (let* ((int-name (string-downcase (string integration-name)))
         (sql "DELETE FROM integration_credentials WHERE integration_name = $1 AND credential_key = $2 AND scope = $3")
         (params (list int-name key scope))
         (idx 3))
    (when company-id
      (incf idx)
      (setf sql (concatenate 'string sql (format nil " AND company_id = $~D" idx)))
      (setf params (append params (list company-id))))
    (when user-id
      (incf idx)
      (setf sql (concatenate 'string sql (format nil " AND user_id = $~D" idx)))
      (setf params (append params (list user-id))))
    (values sql params)))

(defun list-credentials (integration-name &key scope company-id)
  "Build SQL to list credential keys (not values) for an integration."
  (let* ((int-name (string-downcase (string integration-name)))
         (sql "SELECT credential_key, scope, company_id, user_id FROM integration_credentials WHERE integration_name = $1")
         (params (list int-name))
         (idx 1))
    (when scope
      (incf idx)
      (setf sql (concatenate 'string sql (format nil " AND scope = $~D" idx)))
      (setf params (append params (list scope))))
    (when company-id
      (incf idx)
      (setf sql (concatenate 'string sql (format nil " AND company_id = $~D" idx)))
      (setf params (append params (list company-id))))
    (values sql params)))
