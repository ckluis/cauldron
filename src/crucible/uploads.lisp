;;;; src/crucible/uploads.lisp — File upload management, storage, and attachments
;;;; Framework-level file upload support with configurable storage, type validation,
;;;; and polymorphic attachments.
(in-package :cauldron.crucible)

;;; ============================================================
;;; Configuration
;;; ============================================================

(defvar *upload-dir*
  (cauldron.runtime:get-env "CAULDRON_UPLOAD_DIR" :type :string :default "data/uploads")
  "Directory where uploaded files are stored on disk.")

(defvar *max-upload-bytes*
  (cauldron.runtime:get-env "CAULDRON_MAX_UPLOAD_BYTES" :type :integer :default 10485760)
  "Maximum upload size in bytes (default: 10MB).")

;;; ============================================================
;;; Uploads DDL
;;; ============================================================
;;; Table definition as DDL string (crucible loads before grimoire in core subsystem)

(defvar *uploads-ddl*
  "CREATE TABLE IF NOT EXISTS uploads (
  id SERIAL PRIMARY KEY,
  company_id INTEGER NOT NULL,
  user_id INTEGER NOT NULL,
  filename TEXT NOT NULL,
  storage_key TEXT NOT NULL,
  content_type TEXT NOT NULL,
  size_bytes BIGINT NOT NULL,
  metadata TEXT DEFAULT '{}',
  public_p BOOLEAN DEFAULT FALSE,
  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW()
);
CREATE INDEX IF NOT EXISTS idx_uploads_company_id ON uploads(company_id);
CREATE INDEX IF NOT EXISTS idx_uploads_user_id ON uploads(user_id);
CREATE INDEX IF NOT EXISTS idx_uploads_storage_key ON uploads(storage_key);"
  "DDL for the uploads table.")

;;; ============================================================
;;; Attachments DDL (polymorphic)
;;; ============================================================

(defvar *attachments-ddl*
  "CREATE TABLE IF NOT EXISTS attachments (
  id SERIAL PRIMARY KEY,
  attachable_type TEXT NOT NULL,
  attachable_id INTEGER NOT NULL,
  upload_id INTEGER NOT NULL,
  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW()
);
CREATE INDEX IF NOT EXISTS idx_attachments_polymorphic ON attachments(attachable_type, attachable_id);
CREATE INDEX IF NOT EXISTS idx_attachments_upload_id ON attachments(upload_id);"
  "DDL for the polymorphic attachments table.")

;;; ============================================================
;;; Storage Key Generation
;;; ============================================================

(defun generate-storage-key (filename)
  "Generate a storage key in the format {year}/{month}/{uuid}.{ext}.
Uses current time for year/month and a crypto token for the UUID portion."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore sec min hour day))
    (let* ((token (cauldron.crypto:generate-token :length 16))
           (dot-pos (position #\. filename :from-end t))
           (ext (if dot-pos
                    (string-downcase (subseq filename (1+ dot-pos)))
                    "bin")))
      (format nil "~4,'0D/~2,'0D/~A.~A" year month token ext))))

;;; ============================================================
;;; Upload Type Validation
;;; ============================================================

(defparameter *allowed-upload-types*
  '("image/png" "image/jpg" "image/jpeg" "image/gif" "image/webp" "image/svg+xml"
    "application/pdf" "text/csv" "text/plain" "application/zip")
  "Allowlist of permitted upload content types.")

(defun validate-upload-type (content-type)
  "Check if CONTENT-TYPE is in the allowlist.
Returns T if allowed, NIL otherwise."
  (if (member content-type *allowed-upload-types* :test #'string-equal)
      t
      nil))

;;; ============================================================
;;; Upload SQL Functions
;;; ============================================================

(defun save-upload-sql (company-id user-id filename storage-key content-type size-bytes
                        &key metadata public-p)
  "Build SQL to insert a new upload record.
Returns (values sql params)."
  (let ((meta-json (or metadata "{}"))
        (pub (if public-p t nil)))
    (values
     "INSERT INTO uploads (company_id, user_id, filename, storage_key, content_type, size_bytes, metadata, public_p, created_at, updated_at) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, NOW(), NOW()) RETURNING id, filename, storage_key"
     (list company-id user-id filename storage-key content-type size-bytes meta-json pub))))

(defun delete-upload-sql (upload-id)
  "Build SQL to delete an upload record by ID.
Returns (values sql params)."
  (values
   "DELETE FROM uploads WHERE id = $1"
   (list upload-id)))

(defun find-upload-sql (upload-id)
  "Build SQL to find an upload record by ID.
Returns (values sql params)."
  (values
   "SELECT id, company_id, user_id, filename, storage_key, content_type, size_bytes, metadata, public_p, created_at, updated_at FROM uploads WHERE id = $1"
   (list upload-id)))

(defun list-uploads-sql (company-id &key limit)
  "Build SQL to list uploads for a company.
Returns (values sql params)."
  (if limit
      (values
       "SELECT id, company_id, user_id, filename, storage_key, content_type, size_bytes, metadata, public_p, created_at FROM uploads WHERE company_id = $1 ORDER BY created_at DESC LIMIT $2"
       (list company-id limit))
      (values
       "SELECT id, company_id, user_id, filename, storage_key, content_type, size_bytes, metadata, public_p, created_at FROM uploads WHERE company_id = $1 ORDER BY created_at DESC"
       (list company-id))))

;;; ============================================================
;;; Serve Upload Headers
;;; ============================================================

(defun serve-upload-headers (content-type &key public-p)
  "Build response headers alist for serving an uploaded file.
Returns an alist of header name/value pairs."
  (let ((headers (list (cons "Content-Type" content-type)
                       (cons "X-Content-Type-Options" "nosniff"))))
    (if public-p
        (push (cons "Cache-Control" "public, max-age=31536000") headers)
        (push (cons "Cache-Control" "private, no-cache") headers))
    headers))

;;; ============================================================
;;; Upload Limit Plug
;;; ============================================================

(defun make-plug-upload-limit ()
  "Create a plug that checks Content-Length against *max-upload-bytes*.
Halts with 413 Payload Too Large if the upload exceeds the limit.
Returns a plug function (conn) -> conn."
  (lambda (conn)
    (let* ((content-length-header (cdr (assoc "Content-Length" (conn-headers conn)
                                              :test #'string-equal)))
           (content-length (when content-length-header
                             (parse-integer-safe content-length-header))))
      (if (and content-length (> content-length *max-upload-bytes*))
          (progn
            (conn-put-resp-header conn "Content-Type" "text/plain")
            (halt-conn conn :status 413
                            :body (format nil "Payload Too Large: ~D bytes exceeds limit of ~D"
                                          content-length *max-upload-bytes*)))
          conn))))

(defun parse-integer-safe (string)
  "Parse STRING as a non-negative integer. Returns NIL if not a valid integer."
  (when (and string (> (length string) 0))
    (let ((result 0)
          (valid t))
      (loop for i from 0 below (length string)
            for ch = (char string i)
            do (if (digit-char-p ch)
                   (setf result (+ (* result 10) (digit-char-p ch)))
                   (progn (setf valid nil) (return))))
      (when valid result))))

;;; ============================================================
;;; Attachment SQL Functions
;;; ============================================================

(defun attach-upload-sql (attachable-type attachable-id upload-id)
  "Build SQL to create an attachment linking an upload to an entity.
Returns (values sql params)."
  (values
   "INSERT INTO attachments (attachable_type, attachable_id, upload_id, created_at, updated_at) VALUES ($1, $2, $3, NOW(), NOW()) RETURNING id"
   (list attachable-type attachable-id upload-id)))

(defun list-attachments-sql (attachable-type attachable-id)
  "Build SQL to list all attachments for a polymorphic entity.
Returns (values sql params)."
  (values
   "SELECT a.id, a.upload_id, a.created_at, u.filename, u.content_type, u.size_bytes FROM attachments a JOIN uploads u ON u.id = a.upload_id WHERE a.attachable_type = $1 AND a.attachable_id = $2 ORDER BY a.created_at DESC"
   (list attachable-type attachable-id)))

(defun detach-upload-sql (attachable-type attachable-id upload-id)
  "Build SQL to remove an attachment.
Returns (values sql params)."
  (values
   "DELETE FROM attachments WHERE attachable_type = $1 AND attachable_id = $2 AND upload_id = $3"
   (list attachable-type attachable-id upload-id)))
