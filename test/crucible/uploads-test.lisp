;;;; test/crucible/uploads-test.lisp — Phase 42: File upload tests
(in-package :cauldron.test)

(defsuite :uploads)

;;; ============================================================
;;; Configuration Defaults
;;; ============================================================

(deftest test-upload-dir-default
  (is (stringp cauldron.crucible:*upload-dir*) "Upload dir is a string")
  (is-equal "data/uploads" cauldron.crucible:*upload-dir*))

(deftest test-max-upload-bytes-default
  (is (integerp cauldron.crucible:*max-upload-bytes*) "Max upload bytes is an integer")
  (is-equal 10485760 cauldron.crucible:*max-upload-bytes*))

;;; ============================================================
;;; Storage Key Generation
;;; ============================================================

(deftest test-storage-key-format
  (let ((key (cauldron.crucible:generate-storage-key "photo.png")))
    (is (stringp key) "Storage key is a string")
    ;; Format: YYYY/MM/uuid.ext
    (is (position #\/ key) "Contains path separator")
    (is (search ".png" key) "Preserves file extension")))

(deftest test-storage-key-year-month-prefix
  (let ((key (cauldron.crucible:generate-storage-key "doc.pdf")))
    (multiple-value-bind (sec min hour day month year)
        (decode-universal-time (get-universal-time))
      (declare (ignore sec min hour day))
      (let ((expected-prefix (format nil "~4,'0D/~2,'0D/" year month)))
        (is (string= expected-prefix (subseq key 0 8))
            "Key starts with YYYY/MM/")))))

(deftest test-storage-key-uuid-portion
  (let ((key (cauldron.crucible:generate-storage-key "test.txt")))
    ;; After YYYY/MM/ there should be a hex token + .ext
    (let* ((last-slash (position #\/ key :from-end t))
           (filename-part (subseq key (1+ last-slash)))
           (dot-pos (position #\. filename-part)))
      (is-not-nil dot-pos "UUID part has extension separator")
      (is (> dot-pos 0) "UUID part is non-empty"))))

(deftest test-storage-key-uniqueness
  (let ((k1 (cauldron.crucible:generate-storage-key "a.png"))
        (k2 (cauldron.crucible:generate-storage-key "a.png")))
    (is (not (string= k1 k2)) "Storage keys are unique")))

(deftest test-storage-key-no-extension
  (let ((key (cauldron.crucible:generate-storage-key "noext")))
    (is (search ".bin" key) "Falls back to .bin extension")))

(deftest test-storage-key-uppercase-extension
  (let ((key (cauldron.crucible:generate-storage-key "photo.PNG")))
    (is (search ".png" key) "Extension is lowercased")))

;;; ============================================================
;;; Upload Type Validation
;;; ============================================================

(deftest test-validate-upload-type-png
  (is-true (cauldron.crucible:validate-upload-type "image/png")))

(deftest test-validate-upload-type-jpg
  (is-true (cauldron.crucible:validate-upload-type "image/jpg")))

(deftest test-validate-upload-type-jpeg
  (is-true (cauldron.crucible:validate-upload-type "image/jpeg")))

(deftest test-validate-upload-type-gif
  (is-true (cauldron.crucible:validate-upload-type "image/gif")))

(deftest test-validate-upload-type-webp
  (is-true (cauldron.crucible:validate-upload-type "image/webp")))

(deftest test-validate-upload-type-svg
  (is-true (cauldron.crucible:validate-upload-type "image/svg+xml")))

(deftest test-validate-upload-type-pdf
  (is-true (cauldron.crucible:validate-upload-type "application/pdf")))

(deftest test-validate-upload-type-csv
  (is-true (cauldron.crucible:validate-upload-type "text/csv")))

(deftest test-validate-upload-type-txt
  (is-true (cauldron.crucible:validate-upload-type "text/plain")))

(deftest test-validate-upload-type-zip
  (is-true (cauldron.crucible:validate-upload-type "application/zip")))

(deftest test-validate-upload-type-reject-exe
  (is-nil (cauldron.crucible:validate-upload-type "application/x-msdownload")))

(deftest test-validate-upload-type-reject-js
  (is-nil (cauldron.crucible:validate-upload-type "application/javascript")))

(deftest test-validate-upload-type-reject-html
  (is-nil (cauldron.crucible:validate-upload-type "text/html")))

;;; ============================================================
;;; Upload SQL Functions
;;; ============================================================

(deftest test-save-upload-sql
  (multiple-value-bind (sql params)
      (cauldron.crucible:save-upload-sql 1 2 "photo.png" "2026/03/abc.png"
                                          "image/png" 12345)
    (is (search "INSERT INTO uploads" sql) "INSERT statement")
    (is (search "RETURNING" sql) "Has RETURNING clause")
    (is-equal 8 (length params))
    (is-equal 1 (first params))
    (is-equal 2 (second params))
    (is-equal "photo.png" (third params))
    (is-equal "2026/03/abc.png" (fourth params))
    (is-equal "image/png" (fifth params))
    (is-equal 12345 (sixth params))))

(deftest test-save-upload-sql-with-metadata
  (multiple-value-bind (sql params)
      (cauldron.crucible:save-upload-sql 1 2 "doc.pdf" "2026/03/def.pdf"
                                          "application/pdf" 5000
                                          :metadata "{\"alt\":\"test\"}")
    (is (search "INSERT" sql))
    (is-equal "{\"alt\":\"test\"}" (seventh params))))

(deftest test-save-upload-sql-public
  (multiple-value-bind (sql params)
      (cauldron.crucible:save-upload-sql 1 2 "logo.png" "2026/03/ghi.png"
                                          "image/png" 2000
                                          :public-p t)
    (is (search "INSERT" sql))
    (is-equal t (eighth params))))

(deftest test-delete-upload-sql
  (multiple-value-bind (sql params)
      (cauldron.crucible:delete-upload-sql 42)
    (is (search "DELETE FROM uploads" sql))
    (is-equal 1 (length params))
    (is-equal 42 (first params))))

(deftest test-find-upload-sql
  (multiple-value-bind (sql params)
      (cauldron.crucible:find-upload-sql 7)
    (is (search "SELECT" sql))
    (is (search "FROM uploads" sql))
    (is (search "WHERE id" sql))
    (is-equal 1 (length params))
    (is-equal 7 (first params))))

(deftest test-list-uploads-sql
  (multiple-value-bind (sql params)
      (cauldron.crucible:list-uploads-sql 5)
    (is (search "SELECT" sql))
    (is (search "company_id" sql))
    (is (search "ORDER BY" sql))
    (is-equal 1 (length params))
    (is-equal 5 (first params))))

(deftest test-list-uploads-sql-with-limit
  (multiple-value-bind (sql params)
      (cauldron.crucible:list-uploads-sql 5 :limit 10)
    (is (search "LIMIT" sql))
    (is-equal 2 (length params))
    (is-equal 5 (first params))
    (is-equal 10 (second params))))

;;; ============================================================
;;; Serve Upload Headers
;;; ============================================================

(deftest test-serve-headers-content-type
  (let ((headers (cauldron.crucible:serve-upload-headers "image/png")))
    (is-equal "image/png" (cdr (assoc "Content-Type" headers :test #'string=))
              "Content-Type header set")))

(deftest test-serve-headers-nosniff
  (let ((headers (cauldron.crucible:serve-upload-headers "image/png")))
    (is-equal "nosniff" (cdr (assoc "X-Content-Type-Options" headers :test #'string=))
              "X-Content-Type-Options set to nosniff")))

(deftest test-serve-headers-private-cache
  (let ((headers (cauldron.crucible:serve-upload-headers "image/png")))
    (is-equal "private, no-cache"
              (cdr (assoc "Cache-Control" headers :test #'string=))
              "Private cache by default")))

(deftest test-serve-headers-public-cache
  (let ((headers (cauldron.crucible:serve-upload-headers "image/png" :public-p t)))
    (is (search "public" (cdr (assoc "Cache-Control" headers :test #'string=)))
        "Public cache when public-p")))

;;; ============================================================
;;; Upload Limit Plug
;;; ============================================================

(deftest test-upload-limit-under-limit-passes
  (let* ((cauldron.crucible:*max-upload-bytes* 10485760)
         (plug (cauldron.crucible:make-plug-upload-limit))
         (conn (cauldron.crucible:make-conn
                :method :post :path "/upload"
                :headers '(("Content-Length" . "1024"))))
         (result (funcall plug conn)))
    (is-false (cauldron.crucible:conn-halted-p result)
              "Under limit passes through")))

(deftest test-upload-limit-over-limit-413
  (let* ((cauldron.crucible:*max-upload-bytes* 1000)
         (plug (cauldron.crucible:make-plug-upload-limit))
         (conn (cauldron.crucible:make-conn
                :method :post :path "/upload"
                :headers '(("Content-Length" . "5000"))))
         (result (funcall plug conn)))
    (is-equal 413 (cauldron.crucible:conn-status result)
              "Over limit returns 413")
    (is-true (cauldron.crucible:conn-halted-p result)
             "Over limit halts connection")))

(deftest test-upload-limit-no-content-length-passes
  (let* ((plug (cauldron.crucible:make-plug-upload-limit))
         (conn (cauldron.crucible:make-conn :method :post :path "/upload"))
         (result (funcall plug conn)))
    (is-false (cauldron.crucible:conn-halted-p result)
              "No Content-Length passes through")))

(deftest test-upload-limit-exact-limit-passes
  (let* ((cauldron.crucible:*max-upload-bytes* 1000)
         (plug (cauldron.crucible:make-plug-upload-limit))
         (conn (cauldron.crucible:make-conn
                :method :post :path "/upload"
                :headers '(("Content-Length" . "1000"))))
         (result (funcall plug conn)))
    (is-false (cauldron.crucible:conn-halted-p result)
              "Exactly at limit passes through")))

(deftest test-upload-limit-413-body-message
  (let* ((cauldron.crucible:*max-upload-bytes* 500)
         (plug (cauldron.crucible:make-plug-upload-limit))
         (conn (cauldron.crucible:make-conn
                :method :post :path "/upload"
                :headers '(("Content-Length" . "1000"))))
         (result (funcall plug conn)))
    (is (search "Payload Too Large" (cauldron.crucible:conn-resp-body result))
        "413 body contains descriptive message")))

;;; ============================================================
;;; Attachment SQL Functions
;;; ============================================================

(deftest test-attach-upload-sql
  (multiple-value-bind (sql params)
      (cauldron.crucible:attach-upload-sql "contact" 5 10)
    (is (search "INSERT INTO attachments" sql) "INSERT statement")
    (is (search "RETURNING" sql) "Has RETURNING clause")
    (is-equal 3 (length params))
    (is-equal "contact" (first params))
    (is-equal 5 (second params))
    (is-equal 10 (third params))))

(deftest test-list-attachments-sql
  (multiple-value-bind (sql params)
      (cauldron.crucible:list-attachments-sql "deal" 3)
    (is (search "SELECT" sql))
    (is (search "attachable_type" sql))
    (is (search "JOIN uploads" sql) "Joins with uploads table")
    (is-equal 2 (length params))
    (is-equal "deal" (first params))
    (is-equal 3 (second params))))

(deftest test-detach-upload-sql
  (multiple-value-bind (sql params)
      (cauldron.crucible:detach-upload-sql "contact" 5 10)
    (is (search "DELETE FROM attachments" sql))
    (is (search "attachable_type" sql))
    (is-equal 3 (length params))
    (is-equal "contact" (first params))
    (is-equal 5 (second params))
    (is-equal 10 (third params))))

;;; ============================================================
;;; DDL Strings
;;; ============================================================

(deftest test-uploads-ddl-defined
  (is (stringp cauldron.crucible:*uploads-ddl*) "Uploads DDL is a string")
  (is (search "CREATE TABLE" cauldron.crucible:*uploads-ddl*) "Contains CREATE TABLE")
  (is (search "uploads" cauldron.crucible:*uploads-ddl*) "References uploads table")
  (is (search "storage_key" cauldron.crucible:*uploads-ddl*) "Has storage_key column")
  (is (search "content_type" cauldron.crucible:*uploads-ddl*) "Has content_type column"))

(deftest test-attachments-ddl-defined
  (is (stringp cauldron.crucible:*attachments-ddl*) "Attachments DDL is a string")
  (is (search "CREATE TABLE" cauldron.crucible:*attachments-ddl*) "Contains CREATE TABLE")
  (is (search "attachable_type" cauldron.crucible:*attachments-ddl*) "Has attachable_type column")
  (is (search "attachable_id" cauldron.crucible:*attachments-ddl*) "Has attachable_id column")
  (is (search "upload_id" cauldron.crucible:*attachments-ddl*) "Has upload_id column"))
