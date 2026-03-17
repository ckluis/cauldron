;;;; src/crucible/api-keys.lisp — API key generation, authentication, and management
;;;; Framework-level API key support with SHA-256 hashing and scope-based access control.
(in-package :cauldron.crucible)

;;; --- API Key Table DDL ---
;;; Table definition as DDL string (crucible loads before grimoire in core subsystem)

(defvar *api-keys-ddl*
  "CREATE TABLE IF NOT EXISTS api_keys (
  id SERIAL PRIMARY KEY,
  key_prefix TEXT NOT NULL,
  key_hash TEXT NOT NULL,
  user_id INTEGER,
  company_id INTEGER,
  name TEXT NOT NULL,
  scopes TEXT DEFAULT '[]',
  last_used_at TIMESTAMP,
  revoked_at TIMESTAMP,
  expires_at TIMESTAMP,
  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW()
);
CREATE INDEX IF NOT EXISTS idx_api_keys_key_prefix ON api_keys(key_prefix);
CREATE INDEX IF NOT EXISTS idx_api_keys_key_hash ON api_keys(key_hash);
CREATE INDEX IF NOT EXISTS idx_api_keys_user_id ON api_keys(user_id);
CREATE INDEX IF NOT EXISTS idx_api_keys_company_id ON api_keys(company_id);"
  "DDL for the api_keys table.")

;;; --- Hex Encoding ---

(defun %octets-to-hex (octets)
  "Convert an octet vector to a lowercase hex string."
  (let ((hex (make-string (* (length octets) 2))))
    (loop for i from 0 below (length octets)
          for byte = (aref octets i)
          for hi = (ash byte -4)
          for lo = (logand byte #x0F)
          for pos = (* i 2)
          do (setf (char hex pos)       (char "0123456789abcdef" hi))
             (setf (char hex (+ pos 1)) (char "0123456789abcdef" lo)))
    hex))

;;; --- Key Generation ---

(defun generate-api-key ()
  "Generate a new API key with cld_ prefix.
Returns (values full-key prefix hash) where:
  full-key: cld_<40-char-hex> (44 chars total)
  prefix: first 12 chars (cld_ + 8 hex)
  hash: SHA-256 hex digest of full-key"
  (let* ((token (cauldron.crypto:generate-token :length 20))
         (full-key (concatenate 'string "cld_" token))
         (prefix (subseq full-key 0 12))
         (hash (%octets-to-hex (cauldron.crypto:sha256 full-key))))
    (values full-key prefix hash)))

;;; --- Key Storage SQL ---

(defun create-api-key-sql (prefix hash name &key user-id company-id scopes expires-at)
  "Build SQL to insert a new API key.
Returns (values sql params)."
  (let ((scope-json (if scopes
                        (cauldron.json:encode (coerce scopes 'vector))
                        "[]")))
    (values
     "INSERT INTO api_keys (key_prefix, key_hash, user_id, company_id, name, scopes, expires_at, created_at, updated_at) VALUES ($1, $2, $3, $4, $5, $6, $7, NOW(), NOW()) RETURNING id, key_prefix, name"
     (list prefix hash user-id company-id name scope-json expires-at))))

(defun revoke-api-key-sql (key-id)
  "Build SQL to revoke an API key by setting revoked_at.
Returns (values sql params)."
  (values
   "UPDATE api_keys SET revoked_at = NOW(), updated_at = NOW() WHERE id = $1 AND revoked_at IS NULL"
   (list key-id)))

(defun list-api-keys-sql (&key user-id company-id)
  "Build SQL to list API keys. Excludes the hash for security.
Returns (values sql params)."
  (cond
    ((and user-id company-id)
     (values
      "SELECT id, key_prefix, name, scopes, last_used_at, revoked_at, expires_at, created_at FROM api_keys WHERE user_id = $1 AND company_id = $2 ORDER BY created_at DESC"
      (list user-id company-id)))
    (company-id
     (values
      "SELECT id, key_prefix, name, scopes, last_used_at, revoked_at, expires_at, created_at FROM api_keys WHERE company_id = $1 ORDER BY created_at DESC"
      (list company-id)))
    (user-id
     (values
      "SELECT id, key_prefix, name, scopes, last_used_at, revoked_at, expires_at, created_at FROM api_keys WHERE user_id = $1 ORDER BY created_at DESC"
      (list user-id)))
    (t
     (values
      "SELECT id, key_prefix, name, scopes, last_used_at, revoked_at, expires_at, created_at FROM api_keys ORDER BY created_at DESC"
      nil))))

(defun find-api-key-by-hash-sql (key-hash)
  "Build SQL to look up an API key by its hash.
Returns (values sql params)."
  (values
   "SELECT id, key_prefix, key_hash, user_id, company_id, name, scopes, last_used_at, revoked_at, expires_at FROM api_keys WHERE key_hash = $1"
   (list key-hash)))

(defun touch-api-key-sql (key-id)
  "Build SQL to update last_used_at for an API key.
Returns (values sql params)."
  (values
   "UPDATE api_keys SET last_used_at = NOW() WHERE id = $1"
   (list key-id)))

(defun rotate-api-key-sql (old-key-id new-prefix new-hash)
  "Build SQL to revoke old key and insert a replacement in one step.
Returns two (sql params) pairs as a list of conses."
  (list
   (cons "UPDATE api_keys SET revoked_at = NOW(), updated_at = NOW() WHERE id = $1"
         (list old-key-id))
   (cons "INSERT INTO api_keys (key_prefix, key_hash, user_id, company_id, name, scopes, created_at, updated_at) SELECT $1, $2, user_id, company_id, name, scopes, NOW(), NOW() FROM api_keys WHERE id = $3 RETURNING id, key_prefix, name"
         (list new-prefix new-hash old-key-id))))

;;; --- Authentication Plug ---

(defun make-plug-api-key-auth (&key (header "Authorization") (scheme "Bearer"))
  "Create a plug that authenticates requests via API key in Bearer token.
Extracts key from Authorization header, hashes it, looks up in DB.
Sets :current-user and :api-key-scopes assigns on success.
Halts with 401 on failure.

The returned plug is a function (conn) → conn. In real execution,
it expects a :db-execute assign on conn for running SQL."
  (lambda (conn)
    (let* ((auth-header (cdr (assoc header (conn-headers conn) :test #'string-equal)))
           (token (when (and auth-header
                             (> (length auth-header) (1+ (length scheme)))
                             (string-equal (subseq auth-header 0 (length scheme)) scheme))
                    (string-trim '(#\Space)
                                 (subseq auth-header (1+ (length scheme)))))))
      (if (null token)
          (progn
            (json-error conn "Missing or invalid API key" :status 401)
            (halt-conn conn :status 401))
          ;; Hash the token and look up
          (let ((key-hash (%octets-to-hex (cauldron.crypto:sha256 token))))
            ;; Store lookup info for the application layer to execute
            (conn-put-assign conn :api-key-hash key-hash)
            (conn-put-assign conn :api-key-token token)
            conn)))))

(defun make-plug-require-scope (required-scope)
  "Create a plug that checks if the current API key has REQUIRED-SCOPE.
Expects :api-key-scopes to be set by make-plug-api-key-auth."
  (lambda (conn)
    (let ((scopes (conn-get-assign conn :api-key-scopes)))
      (if (and scopes (member required-scope scopes :test #'string=))
          conn
          (progn
            (json-error conn
                        (format nil "Insufficient scope: requires ~A" required-scope)
                        :status 403)
            (halt-conn conn :status 403))))))
