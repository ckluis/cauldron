;;;; test/db/connection-test.lisp — DB connection parsing tests (no real DB)
(in-package :cauldron.test)

(defsuite :db-connection)

;;; ============================================================
;;; Helpers — build binary payloads for testing parse functions
;;; ============================================================

(defun %make-cstring-bytes (string)
  "Build a null-terminated UTF-8 byte vector from STRING."
  (concatenate '(vector (unsigned-byte 8))
    (sb-ext:string-to-octets string :external-format :utf-8)
    #(0)))

(defun %make-int16-bytes (val)
  "Build a 2-byte big-endian vector for an int16."
  (vector (logand (ash val -8) #xFF) (logand val #xFF)))

(defun %make-int32-bytes (val)
  "Build a 4-byte big-endian vector for an int32."
  (vector (logand (ash val -24) #xFF) (logand (ash val -16) #xFF)
          (logand (ash val -8) #xFF) (logand val #xFF)))

(defun %make-int32-signed-bytes (val)
  "Build a 4-byte big-endian vector for a signed int32."
  (let ((u (if (< val 0) (+ val (expt 2 32)) val)))
    (%make-int32-bytes u)))

;;; ============================================================
;;; pg-connection struct
;;; ============================================================

(deftest test-pg-connection-defaults
  (let ((conn (cauldron.db::make-pg-connection)))
    (is-not-nil conn)
    (is-equal "127.0.0.1" (cauldron.db::pg-connection-host conn))
    (is-equal 5432 (cauldron.db::pg-connection-port conn))
    (is-equal "" (cauldron.db::pg-connection-user conn))
    (is-equal "" (cauldron.db::pg-connection-database conn))
    (is-true (cauldron.db::pg-connection-alive-p conn))
    (is-equal #\I (cauldron.db::pg-connection-transaction-status conn))
    (is-false (cauldron.db::pg-connection-in-transaction conn))
    (is-equal 0 (cauldron.db::pg-connection-backend-pid conn))
    (is-equal 0 (cauldron.db::pg-connection-backend-key conn))))

(deftest test-pg-connection-custom-fields
  (let ((conn (cauldron.db::make-pg-connection
               :host "10.0.0.1" :port 5433 :user "admin" :database "mydb")))
    (is-equal "10.0.0.1" (cauldron.db::pg-connection-host conn))
    (is-equal 5433 (cauldron.db::pg-connection-port conn))
    (is-equal "admin" (cauldron.db::pg-connection-user conn))
    (is-equal "mydb" (cauldron.db::pg-connection-database conn))))

(deftest test-pg-connection-parameters-hash-table
  (let ((conn (cauldron.db::make-pg-connection)))
    (is (hash-table-p (cauldron.db::pg-connection-parameters conn)))
    (is-equal 0 (hash-table-count (cauldron.db::pg-connection-parameters conn)))))

(deftest test-pg-connection-prepared-statements-hash-table
  (let ((conn (cauldron.db::make-pg-connection)))
    (is (hash-table-p (cauldron.db::pg-connection-prepared-statements conn)))
    (is-equal 0 (hash-table-count (cauldron.db::pg-connection-prepared-statements conn)))))

;;; ============================================================
;;; pg-error condition
;;; ============================================================

(deftest test-pg-error-condition-creation
  (let ((err (make-condition 'cauldron.db:pg-error
                             :severity "FATAL"
                             :code "28P01"
                             :message "password authentication failed"
                             :detail "User \"foo\" does not exist."
                             :hint "Check pg_hba.conf")))
    (is-equal "FATAL" (cauldron.db:pg-error-severity err))
    (is-equal "28P01" (cauldron.db:pg-error-code err))
    (is-equal "password authentication failed" (cauldron.db:pg-error-message err))
    (is-equal "User \"foo\" does not exist." (cauldron.db:pg-error-detail err))))

(deftest test-pg-error-report-format
  (let* ((err (make-condition 'cauldron.db:pg-error
                              :severity "ERROR"
                              :code "42P01"
                              :message "relation does not exist"
                              :detail nil
                              :hint "Create the table first"))
         (report (format nil "~A" err)))
    (is (search "ERROR" report))
    (is (search "42P01" report))
    (is (search "relation does not exist" report))
    (is (search "Create the table first" report))))

(deftest test-pg-error-defaults
  (let ((err (make-condition 'cauldron.db:pg-error)))
    (is-equal "ERROR" (cauldron.db:pg-error-severity err))
    (is-equal "" (cauldron.db:pg-error-code err))
    (is-equal "" (cauldron.db:pg-error-message err))))

;;; ============================================================
;;; parse-error-fields
;;; ============================================================

(defun %build-error-payload (&rest field-pairs)
  "Build an error/notice payload. FIELD-PAIRS are (char string) pairs."
  (let ((parts '()))
    (dolist (pair field-pairs)
      (push (vector (char-code (first pair))) parts)
      (push (%make-cstring-bytes (second pair)) parts))
    ;; Null terminator
    (push #(0) parts)
    (apply #'concatenate '(vector (unsigned-byte 8)) (nreverse parts))))

(deftest test-parse-error-fields-standard
  (let* ((payload (%build-error-payload '(#\S "ERROR") '(#\C "42P01")
                                        '(#\M "table not found")))
         (fields (cauldron.db::parse-error-fields payload)))
    (is-equal 3 (length fields))
    (is-equal "ERROR" (cdr (assoc #\S fields)))
    (is-equal "42P01" (cdr (assoc #\C fields)))
    (is-equal "table not found" (cdr (assoc #\M fields)))))

(deftest test-parse-error-fields-with-detail-and-hint
  (let* ((payload (%build-error-payload '(#\S "FATAL") '(#\C "28000")
                                        '(#\M "auth failed")
                                        '(#\D "no such user")
                                        '(#\H "check credentials")))
         (fields (cauldron.db::parse-error-fields payload)))
    (is-equal 5 (length fields))
    (is-equal "no such user" (cdr (assoc #\D fields)))
    (is-equal "check credentials" (cdr (assoc #\H fields)))))

(deftest test-parse-error-fields-empty-payload
  (let* ((payload (make-array 1 :element-type '(unsigned-byte 8)
                                :initial-contents '(0)))
         (fields (cauldron.db::parse-error-fields payload)))
    (is-nil fields)))

(deftest test-parse-error-fields-preserves-order
  (let* ((payload (%build-error-payload '(#\S "ERROR") '(#\M "msg") '(#\C "code")))
         (fields (cauldron.db::parse-error-fields payload)))
    (is-equal #\S (car (first fields)))
    (is-equal #\M (car (second fields)))
    (is-equal #\C (car (third fields)))))

;;; ============================================================
;;; signal-pg-error
;;; ============================================================

(deftest test-signal-pg-error-signals-condition
  (let ((payload (%build-error-payload '(#\S "ERROR") '(#\C "42601")
                                       '(#\M "syntax error"))))
    (signals-condition cauldron.db:pg-error
      (cauldron.db::signal-pg-error payload))))

(deftest test-signal-pg-error-fields-extracted
  (let ((payload (%build-error-payload '(#\S "FATAL") '(#\C "3D000")
                                       '(#\M "database does not exist")
                                       '(#\D "extra detail"))))
    (handler-case
        (cauldron.db::signal-pg-error payload)
      (cauldron.db:pg-error (e)
        (is-equal "FATAL" (cauldron.db:pg-error-severity e))
        (is-equal "3D000" (cauldron.db:pg-error-code e))
        (is-equal "database does not exist" (cauldron.db:pg-error-message e))
        (is-equal "extra detail" (cauldron.db:pg-error-detail e))))))

;;; ============================================================
;;; parse-row-description
;;; ============================================================

(defun %build-row-description-payload (columns)
  "Build a RowDescription payload. COLUMNS is a list of
(name table-oid col-num type-oid type-size type-mod format-code)."
  (let ((parts (list (%make-int16-bytes (length columns)))))
    (dolist (col columns)
      (destructuring-bind (name table-oid col-num type-oid type-size type-mod fmt) col
        (push (%make-cstring-bytes name) parts)
        (push (%make-int32-bytes table-oid) parts)
        (push (%make-int16-bytes col-num) parts)
        (push (%make-int32-bytes type-oid) parts)
        (push (%make-int16-bytes type-size) parts)
        (push (%make-int32-bytes type-mod) parts)
        (push (%make-int16-bytes fmt) parts)))
    (apply #'concatenate '(vector (unsigned-byte 8)) (nreverse parts))))

(deftest test-parse-row-description-single-column
  (let* ((payload (%build-row-description-payload
                   '(("id" 16384 1 23 4 -1 0))))
         (rd (cauldron.db::parse-row-description payload)))
    (is-not-nil rd)
    (is-equal 1 (length (cauldron.db::row-description-columns rd)))
    (let ((col (aref (cauldron.db::row-description-columns rd) 0)))
      (is-equal "id" (cauldron.db::column-description-name col))
      (is-equal 23 (cauldron.db::column-description-type-oid col))
      (is-equal 0 (cauldron.db::column-description-format-code col)))))

(deftest test-parse-row-description-multiple-columns
  (let* ((payload (%build-row-description-payload
                   '(("id" 16384 1 23 4 -1 0)
                     ("name" 16384 2 25 -1 -1 0)
                     ("active" 16384 3 16 1 -1 0))))
         (rd (cauldron.db::parse-row-description payload))
         (cols (cauldron.db::row-description-columns rd)))
    (is-equal 3 (length cols))
    (is-equal "id" (cauldron.db::column-description-name (aref cols 0)))
    (is-equal "name" (cauldron.db::column-description-name (aref cols 1)))
    (is-equal "active" (cauldron.db::column-description-name (aref cols 2)))
    (is-equal 25 (cauldron.db::column-description-type-oid (aref cols 1)))
    (is-equal 16 (cauldron.db::column-description-type-oid (aref cols 2)))))

;;; ============================================================
;;; parse-data-row
;;; ============================================================

(defun %build-data-row-payload (values)
  "Build a DataRow payload. VALUES is a list of strings or NIL (for NULL)."
  (let ((parts (list (%make-int16-bytes (length values)))))
    (dolist (val values)
      (if (null val)
          (push (%make-int32-signed-bytes -1) parts)
          (let ((bytes (sb-ext:string-to-octets val :external-format :utf-8)))
            (push (%make-int32-bytes (length bytes)) parts)
            (push bytes parts))))
    (apply #'concatenate '(vector (unsigned-byte 8)) (nreverse parts))))

(deftest test-parse-data-row-single-text
  (let* ((rd-payload (%build-row-description-payload '(("name" 0 1 25 -1 -1 0))))
         (row-desc (cauldron.db::parse-row-description rd-payload))
         (dr-payload (%build-data-row-payload '("Alice")))
         (row (cauldron.db::parse-data-row dr-payload row-desc)))
    (is-equal "Alice" (gethash "name" row))))

(deftest test-parse-data-row-null-value
  (let* ((rd-payload (%build-row-description-payload '(("name" 0 1 25 -1 -1 0))))
         (row-desc (cauldron.db::parse-row-description rd-payload))
         (dr-payload (%build-data-row-payload '(nil)))
         (row (cauldron.db::parse-data-row dr-payload row-desc)))
    (is-nil (gethash "name" row))
    ;; Verify key exists (gethash returns two values)
    (multiple-value-bind (val present) (gethash "name" row)
      (declare (ignore val))
      (is-true present))))

(deftest test-parse-data-row-multiple-columns
  (let* ((rd-payload (%build-row-description-payload
                      '(("id" 0 1 23 4 -1 0)
                        ("name" 0 2 25 -1 -1 0)
                        ("active" 0 3 16 1 -1 0))))
         (row-desc (cauldron.db::parse-row-description rd-payload))
         (dr-payload (%build-data-row-payload '("42" "Bob" "t")))
         (row (cauldron.db::parse-data-row dr-payload row-desc)))
    (is-equal 42 (gethash "id" row))
    (is-equal "Bob" (gethash "name" row))
    (is-true (gethash "active" row))))

(deftest test-parse-data-row-mixed-null-and-value
  (let* ((rd-payload (%build-row-description-payload
                      '(("a" 0 1 25 -1 -1 0) ("b" 0 2 25 -1 -1 0))))
         (row-desc (cauldron.db::parse-row-description rd-payload))
         (dr-payload (%build-data-row-payload '("hello" nil)))
         (row (cauldron.db::parse-data-row dr-payload row-desc)))
    (is-equal "hello" (gethash "a" row))
    (is-nil (gethash "b" row))))

;;; ============================================================
;;; process-backend-message
;;; ============================================================

(deftest test-process-backend-message-parameter-status
  (let ((conn (cauldron.db::make-pg-connection))
        (payload (concatenate '(vector (unsigned-byte 8))
                   (%make-cstring-bytes "server_version")
                   (%make-cstring-bytes "15.2"))))
    (let ((result (cauldron.db::process-backend-message conn #\S payload)))
      (is-equal :parameter-status result)
      (is-equal "15.2" (gethash "server_version"
                                (cauldron.db::pg-connection-parameters conn))))))

(deftest test-process-backend-message-backend-key-data
  (let ((conn (cauldron.db::make-pg-connection))
        (payload (concatenate '(vector (unsigned-byte 8))
                   (%make-int32-bytes 12345)
                   (%make-int32-bytes 67890))))
    (let ((result (cauldron.db::process-backend-message conn #\K payload)))
      (is-equal :backend-key-data result)
      (is-equal 12345 (cauldron.db::pg-connection-backend-pid conn))
      (is-equal 67890 (cauldron.db::pg-connection-backend-key conn)))))

(deftest test-process-backend-message-ready-idle
  (let ((conn (cauldron.db::make-pg-connection))
        (payload (make-array 1 :element-type '(unsigned-byte 8)
                               :initial-contents (list (char-code #\I)))))
    (let ((result (cauldron.db::process-backend-message conn #\Z payload)))
      (is-equal :ready result)
      (is-equal #\I (cauldron.db::pg-connection-transaction-status conn))
      (is-false (cauldron.db::pg-connection-in-transaction conn)))))

(deftest test-process-backend-message-ready-in-transaction
  (let ((conn (cauldron.db::make-pg-connection))
        (payload (make-array 1 :element-type '(unsigned-byte 8)
                               :initial-contents (list (char-code #\T)))))
    (let ((result (cauldron.db::process-backend-message conn #\Z payload)))
      (is-equal :ready result)
      (is-equal #\T (cauldron.db::pg-connection-transaction-status conn))
      (is-true (cauldron.db::pg-connection-in-transaction conn)))))

(deftest test-process-backend-message-ready-error-state
  (let ((conn (cauldron.db::make-pg-connection))
        (payload (make-array 1 :element-type '(unsigned-byte 8)
                               :initial-contents (list (char-code #\E)))))
    (let ((result (cauldron.db::process-backend-message conn #\Z payload)))
      (is-equal :ready result)
      (is-equal #\E (cauldron.db::pg-connection-transaction-status conn))
      (is-false (cauldron.db::pg-connection-in-transaction conn)))))

(deftest test-process-backend-message-auth
  (let ((conn (cauldron.db::make-pg-connection))
        (payload (make-array 0 :element-type '(unsigned-byte 8))))
    (is-equal :auth (cauldron.db::process-backend-message conn #\R payload))))

(deftest test-process-backend-message-row-description
  (let ((conn (cauldron.db::make-pg-connection))
        (payload (make-array 0 :element-type '(unsigned-byte 8))))
    (is-equal :row-description (cauldron.db::process-backend-message conn #\T payload))))

(deftest test-process-backend-message-data-row
  (let ((conn (cauldron.db::make-pg-connection))
        (payload (make-array 0 :element-type '(unsigned-byte 8))))
    (is-equal :data-row (cauldron.db::process-backend-message conn #\D payload))))

(deftest test-process-backend-message-command-complete
  (let ((conn (cauldron.db::make-pg-connection))
        (payload (make-array 0 :element-type '(unsigned-byte 8))))
    (is-equal :command-complete (cauldron.db::process-backend-message conn #\C payload))))

(deftest test-process-backend-message-parse-complete
  (let ((conn (cauldron.db::make-pg-connection))
        (payload (make-array 0 :element-type '(unsigned-byte 8))))
    (is-equal :parse-complete (cauldron.db::process-backend-message conn #\1 payload))))

(deftest test-process-backend-message-bind-complete
  (let ((conn (cauldron.db::make-pg-connection))
        (payload (make-array 0 :element-type '(unsigned-byte 8))))
    (is-equal :bind-complete (cauldron.db::process-backend-message conn #\2 payload))))

(deftest test-process-backend-message-close-complete
  (let ((conn (cauldron.db::make-pg-connection))
        (payload (make-array 0 :element-type '(unsigned-byte 8))))
    (is-equal :close-complete (cauldron.db::process-backend-message conn #\3 payload))))

(deftest test-process-backend-message-no-data
  (let ((conn (cauldron.db::make-pg-connection))
        (payload (make-array 0 :element-type '(unsigned-byte 8))))
    (is-equal :no-data (cauldron.db::process-backend-message conn #\n payload))))

(deftest test-process-backend-message-parameter-description
  (let ((conn (cauldron.db::make-pg-connection))
        (payload (make-array 0 :element-type '(unsigned-byte 8))))
    (is-equal :parameter-description (cauldron.db::process-backend-message conn #\t payload))))

(deftest test-process-backend-message-notification
  (let ((conn (cauldron.db::make-pg-connection))
        (payload (make-array 0 :element-type '(unsigned-byte 8))))
    (is-equal :notification (cauldron.db::process-backend-message conn #\A payload))))
