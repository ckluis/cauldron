;;;; src/db/connection.lisp — PostgreSQL connection management
;;;;
;;;; Implements connect/disconnect, Simple Query, Extended Query (prepare/execute),
;;;; transaction management, and LISTEN/NOTIFY.
(in-package :cauldron.db)

;;; ============================================================
;;; Condition (Error) Types
;;; ============================================================

(define-condition pg-error (error)
  ((severity :initarg :severity :reader pg-error-severity :initform "ERROR")
   (code     :initarg :code     :reader pg-error-code     :initform "")
   (message  :initarg :message  :reader pg-error-message  :initform "")
   (detail   :initarg :detail   :reader pg-error-detail   :initform nil)
   (hint     :initarg :hint     :reader pg-error-hint     :initform nil))
  (:report (lambda (c stream)
             (format stream "PostgreSQL ~A [~A]: ~A~@[ — ~A~]~@[ (hint: ~A)~]"
                     (pg-error-severity c)
                     (pg-error-code c)
                     (pg-error-message c)
                     (pg-error-detail c)
                     (pg-error-hint c)))))

(define-condition pg-notice (condition)
  ((severity :initarg :severity :reader pg-notice-severity :initform "NOTICE")
   (message  :initarg :message  :reader pg-notice-message  :initform ""))
  (:report (lambda (c stream)
             (format stream "PG NOTICE [~A]: ~A"
                     (pg-notice-severity c)
                     (pg-notice-message c)))))

;;; ============================================================
;;; Connection Struct
;;; ============================================================

(defstruct pg-connection
  "A connection to a PostgreSQL server."
  (socket     nil)
  (stream     nil)
  (parameters (make-hash-table :test #'equal) :type hash-table)
  (backend-pid    0 :type integer)
  (backend-key    0 :type integer)
  (in-transaction nil :type boolean)
  (transaction-status #\I :type character) ; I=idle, T=in-transaction, E=error
  (prepared-statements (make-hash-table :test #'equal) :type hash-table)
  (host     "127.0.0.1" :type string)
  (port     5432 :type integer)
  (user     "" :type string)
  (database "" :type string)
  ;; For pool management
  (checkout-time 0 :type integer)
  (alive-p  t :type boolean))

;;; ============================================================
;;; Error/Notice Parsing
;;; ============================================================

(defun parse-error-fields (payload)
  "Parse an ErrorResponse or NoticeResponse payload into a plist.
Format: repeated (byte-field-type, cstring-value), terminated by null byte."
  (let ((fields '())
        (offset 0))
    (loop while (< offset (length payload))
          for field-byte = (aref payload offset)
          do (if (zerop field-byte)
                 (return)
                 (progn
                   (incf offset)
                   (multiple-value-bind (value new-offset)
                       (payload-read-cstring payload offset)
                     (let ((key (code-char field-byte)))
                       (push (cons key value) fields))
                     (setf offset new-offset)))))
    (nreverse fields)))

(defun signal-pg-error (payload)
  "Parse ErrorResponse payload and signal a pg-error condition."
  (let ((fields (parse-error-fields payload)))
    (error 'pg-error
           :severity (or (cdr (assoc #\S fields)) "ERROR")
           :code     (or (cdr (assoc #\C fields)) "")
           :message  (or (cdr (assoc #\M fields)) "Unknown error")
           :detail   (cdr (assoc #\D fields))
           :hint     (cdr (assoc #\H fields)))))

(defun handle-notice (payload)
  "Parse NoticeResponse payload and log it."
  (let ((fields (parse-error-fields payload)))
    (format *error-output* "~&[pg-notice] ~A: ~A~%"
            (or (cdr (assoc #\S fields)) "NOTICE")
            (or (cdr (assoc #\M fields)) ""))))

;;; ============================================================
;;; Row Parsing
;;; ============================================================

(defstruct row-description
  "Describes the columns returned by a query."
  (columns #() :type vector))

(defstruct column-description
  "Describes a single column in a result set."
  (name   "" :type string)
  (table-oid 0 :type integer)
  (column-number 0 :type integer)
  (type-oid 0 :type integer)
  (type-size 0 :type integer)
  (type-modifier 0 :type integer)
  (format-code 0 :type integer))

(defun parse-row-description (payload)
  "Parse a RowDescription ('T') payload into a row-description struct."
  (multiple-value-bind (num-fields offset) (payload-read-int16 payload 0)
    (let ((columns (make-array num-fields)))
      (loop for i from 0 below num-fields
            do (multiple-value-bind (name new-off) (payload-read-cstring payload offset)
                 (setf offset new-off)
                 (multiple-value-bind (table-oid off2) (payload-read-int32 payload offset)
                   (setf offset off2)
                   (multiple-value-bind (col-num off3) (payload-read-int16 payload offset)
                     (setf offset off3)
                     (multiple-value-bind (type-oid off4) (payload-read-int32 payload offset)
                       (setf offset off4)
                       (multiple-value-bind (type-size off5) (payload-read-int16 payload offset)
                         (setf offset off5)
                         (multiple-value-bind (type-mod off6) (payload-read-int32 payload offset)
                           (setf offset off6)
                           (multiple-value-bind (fmt-code off7) (payload-read-int16 payload offset)
                             (setf offset off7)
                             (setf (aref columns i)
                                   (make-column-description
                                    :name name
                                    :table-oid table-oid
                                    :column-number col-num
                                    :type-oid type-oid
                                    :type-size type-size
                                    :type-modifier type-mod
                                    :format-code fmt-code))))))))))
      (make-row-description :columns columns))))

(defun parse-data-row (payload row-desc)
  "Parse a DataRow ('D') payload into a hash-table using ROW-DESC for column names/types."
  (multiple-value-bind (num-columns offset) (payload-read-int16 payload 0)
    (let ((row (make-hash-table :test #'equal))
          (columns (row-description-columns row-desc)))
      (loop for i from 0 below num-columns
            do (multiple-value-bind (col-length new-off)
                   (payload-read-int32-signed payload offset)
                 (setf offset new-off)
                 (let* ((col-desc (when (< i (length columns)) (aref columns i)))
                        (col-name (if col-desc (column-description-name col-desc) (format nil "column~D" i)))
                        (type-oid (if col-desc (column-description-type-oid col-desc) 25)))
                   (if (= col-length -1)
                       ;; NULL value
                       (setf (gethash col-name row) nil)
                       ;; Read col-length bytes and decode
                       (let ((value-bytes (subseq payload offset (+ offset col-length))))
                         (setf offset (+ offset col-length))
                         (let ((text-value (sb-ext:octets-to-string
                                            value-bytes :external-format :utf-8)))
                           (setf (gethash col-name row)
                                 (decode-pg-value type-oid text-value))))))))
      row)))

;;; ============================================================
;;; Backend Message Processing
;;; ============================================================

(defun process-backend-message (conn type payload)
  "Process a single backend message. Returns a keyword indicating what was received:
:auth, :ready, :row-description, :data-row, :command-complete, :error,
:notice, :parameter-status, :backend-key-data, :notification,
:parse-complete, :bind-complete, :no-data, :parameter-description, :close-complete."
  (let ((stream (pg-connection-stream conn)))
    (declare (ignore stream))
    (case type
      ;; ParameterStatus
      (#\S
       (multiple-value-bind (name offset) (payload-read-cstring payload 0)
         (multiple-value-bind (value _off) (payload-read-cstring payload offset)
           (declare (ignore _off))
           (setf (gethash name (pg-connection-parameters conn)) value)))
       :parameter-status)

      ;; BackendKeyData
      (#\K
       (multiple-value-bind (pid offset) (payload-read-int32 payload 0)
         (multiple-value-bind (key _off) (payload-read-int32 payload offset)
           (declare (ignore _off))
           (setf (pg-connection-backend-pid conn) pid)
           (setf (pg-connection-backend-key conn) key)))
       :backend-key-data)

      ;; ReadyForQuery
      (#\Z
       (let ((status (code-char (aref payload 0))))
         (setf (pg-connection-transaction-status conn) status)
         (setf (pg-connection-in-transaction conn)
               (char= status #\T)))
       :ready)

      ;; Authentication
      (#\R :auth)

      ;; RowDescription
      (#\T :row-description)

      ;; DataRow
      (#\D :data-row)

      ;; CommandComplete
      (#\C :command-complete)

      ;; ErrorResponse
      (#\E
       (signal-pg-error payload)
       :error)

      ;; NoticeResponse
      (#\N
       (handle-notice payload)
       :notice)

      ;; NotificationResponse
      (#\A :notification)

      ;; ParseComplete
      (#\1 :parse-complete)

      ;; BindComplete
      (#\2 :bind-complete)

      ;; CloseComplete
      (#\3 :close-complete)

      ;; NoData
      (#\n :no-data)

      ;; ParameterDescription
      (#\t :parameter-description)

      (otherwise
       (format *error-output* "~&[pg] Unknown backend message type: ~A (~D)~%"
               type (char-code type))
       :unknown))))

;;; ============================================================
;;; Connect / Disconnect
;;; ============================================================

(defun connect (&key (host "127.0.0.1") (port 5432) database user (password ""))
  "Connect to a PostgreSQL server. Returns a pg-connection struct.
HOST defaults to 127.0.0.1, PORT to 5432."
  (unless database (error "connect: :database is required"))
  (unless user (error "connect: :user is required"))
  (multiple-value-bind (socket stream)
      (cauldron.runtime:make-tcp-connection host port)
    (let ((conn (make-pg-connection
                 :socket socket
                 :stream stream
                 :host host
                 :port port
                 :user user
                 :database database)))
      (handler-bind ((error (lambda (e)
                              (declare (ignore e))
                              ;; Clean up on failure
                              (ignore-errors
                                (cauldron.runtime:socket-close socket)))))
        ;; Send startup message
        (write-startup-message stream user database)
        ;; Read messages until ReadyForQuery
        (loop
          (multiple-value-bind (type payload) (read-message stream)
            (let ((msg-type (process-backend-message conn type payload)))
              (case msg-type
                (:auth
                 (let ((done (handle-auth-response stream payload user password)))
                   ;; If auth returned T (AuthOk), we expect more messages
                   ;; (ParameterStatus, BackendKeyData, ReadyForQuery)
                   (declare (ignore done))))
                (:ready (return conn))
                (:error nil) ; signal-pg-error already called, will unwind
                (otherwise nil)))))))))

(defun disconnect (conn)
  "Disconnect from the PostgreSQL server."
  (when (and conn (pg-connection-alive-p conn))
    (ignore-errors
      (write-terminate (pg-connection-stream conn)))
    (ignore-errors
      (cauldron.runtime:socket-close (pg-connection-socket conn)))
    (setf (pg-connection-alive-p conn) nil))
  (values))

;;; ============================================================
;;; Simple Query Protocol
;;; ============================================================

(defun query (conn sql &rest params)
  "Execute SQL via the Simple Query protocol. Returns a list of hash-tables (rows).
If PARAMS are provided, uses the Extended Query protocol with parameter binding."
  (when params
    (return-from query (apply #'execute-parameterized conn sql params)))
  (let ((sql-start (get-internal-real-time))
        (stream (pg-connection-stream conn))
        (rows '())
        (row-desc nil)
        (command-tag nil))
    (write-message stream #\Q (build-query-payload sql))
    ;; Read responses until ReadyForQuery
    (loop
      (multiple-value-bind (type payload) (read-message stream)
        (let ((msg-type (process-backend-message conn type payload)))
          (case msg-type
            (:row-description
             (setf row-desc (parse-row-description payload)))
            (:data-row
             (when row-desc
               (push (parse-data-row payload row-desc) rows)))
            (:command-complete
             (setf command-tag
                   (sb-ext:octets-to-string payload :external-format :utf-8
                                                     :end (1- (length payload)))))
            (:ready
             (let ((elapsed (round (* 1000 (/ (- (get-internal-real-time) sql-start)
                                               internal-time-units-per-second)))))
               (cauldron.logging:log-inc :sql-count)
               (cauldron.logging:log-timing :sql-duration-ms elapsed))
             (return (values (nreverse rows) command-tag)))
            (:error nil)  ; already signaled
            (otherwise nil)))))))

(defun execute-sql (conn sql &rest params)
  "Execute SQL and return just the command tag string (e.g., \"INSERT 0 1\").
Uses Simple Query if no params, Extended Query otherwise."
  (when params
    (multiple-value-bind (_rows tag)
        (apply #'execute-parameterized conn sql params)
      (declare (ignore _rows))
      (return-from execute-sql tag)))
  (let ((sql-start (get-internal-real-time))
        (stream (pg-connection-stream conn))
        (command-tag nil))
    (write-message stream #\Q (build-query-payload sql))
    (loop
      (multiple-value-bind (type payload) (read-message stream)
        (let ((msg-type (process-backend-message conn type payload)))
          (case msg-type
            (:command-complete
             (setf command-tag
                   (sb-ext:octets-to-string payload :external-format :utf-8
                                                     :end (1- (length payload)))))
            (:ready
             (let ((elapsed (round (* 1000 (/ (- (get-internal-real-time) sql-start)
                                               internal-time-units-per-second)))))
               (cauldron.logging:log-inc :sql-count)
               (cauldron.logging:log-timing :sql-duration-ms elapsed))
             (return command-tag))
            (:error nil)
            (otherwise nil)))))))

;;; ============================================================
;;; Extended Query Protocol
;;; ============================================================

(defun prepare (conn name sql &optional (param-oids '()))
  "Send a Parse message to prepare a statement. NAME is the statement name (string).
Returns T on success."
  (let ((stream (pg-connection-stream conn)))
    ;; Send Parse
    (write-message stream #\P (build-parse-payload name sql param-oids))
    ;; Send Sync to get response
    (write-sync stream)
    ;; Read until ReadyForQuery
    (loop
      (multiple-value-bind (type payload) (read-message stream)
        (let ((msg-type (process-backend-message conn type payload)))
          (case msg-type
            (:parse-complete
             (setf (gethash name (pg-connection-prepared-statements conn)) sql))
            (:ready (return t))
            (:error nil)
            (otherwise nil)))))))

(defun execute-prepared (conn stmt-name params)
  "Execute a prepared statement with PARAMS (list of Lisp values).
Returns (values rows command-tag)."
  (let ((stream (pg-connection-stream conn))
        (encoded-params (mapcar #'encode-pg-value params))
        (rows '())
        (row-desc nil)
        (command-tag nil))
    ;; Bind
    (write-message stream #\B (build-bind-payload "" stmt-name encoded-params))
    ;; Describe portal
    (write-message stream #\D (build-describe-payload #\P ""))
    ;; Execute
    (write-message stream #\E (build-execute-payload "" 0))
    ;; Sync
    (write-sync stream)
    ;; Read responses
    (loop
      (multiple-value-bind (type payload) (read-message stream)
        (let ((msg-type (process-backend-message conn type payload)))
          (case msg-type
            (:bind-complete nil)
            (:row-description
             (setf row-desc (parse-row-description payload)))
            (:no-data nil)
            (:data-row
             (when row-desc
               (push (parse-data-row payload row-desc) rows)))
            (:command-complete
             (setf command-tag
                   (sb-ext:octets-to-string payload :external-format :utf-8
                                                     :end (1- (length payload)))))
            (:ready
             (return (values (nreverse rows) command-tag)))
            (:error nil)
            (otherwise nil)))))))

(defun execute-parameterized (conn sql &rest params)
  "Execute SQL with positional parameters ($1, $2, ...) via Extended Query.
Uses an unnamed prepared statement. Returns (values rows command-tag)."
  (let ((sql-start (get-internal-real-time))
        (stream (pg-connection-stream conn))
        (encoded-params (mapcar #'encode-pg-value params))
        (rows '())
        (row-desc nil)
        (command-tag nil))
    ;; Parse (unnamed statement)
    (write-message stream #\P (build-parse-payload "" sql '()))
    ;; Bind (unnamed portal, unnamed statement)
    (write-message stream #\B (build-bind-payload "" "" encoded-params))
    ;; Describe portal
    (write-message stream #\D (build-describe-payload #\P ""))
    ;; Execute
    (write-message stream #\E (build-execute-payload "" 0))
    ;; Sync
    (write-sync stream)
    ;; Read responses
    (loop
      (multiple-value-bind (type payload) (read-message stream)
        (let ((msg-type (process-backend-message conn type payload)))
          (case msg-type
            (:parse-complete nil)
            (:bind-complete nil)
            (:row-description
             (setf row-desc (parse-row-description payload)))
            (:no-data nil)
            (:parameter-description nil)
            (:data-row
             (when row-desc
               (push (parse-data-row payload row-desc) rows)))
            (:command-complete
             (setf command-tag
                   (sb-ext:octets-to-string payload :external-format :utf-8
                                                     :end (1- (length payload)))))
            (:ready
             (let ((elapsed (round (* 1000 (/ (- (get-internal-real-time) sql-start)
                                               internal-time-units-per-second)))))
               (cauldron.logging:log-inc :sql-count)
               (cauldron.logging:log-timing :sql-duration-ms elapsed))
             (return (values (nreverse rows) command-tag)))
            (:error nil)
            (otherwise nil)))))))

;;; ============================================================
;;; Macros
;;; ============================================================

(defmacro with-connection ((var &rest connect-args) &body body)
  "Execute BODY with VAR bound to a database connection. Ensures disconnect."
  `(let ((,var (connect ,@connect-args)))
     (unwind-protect (progn ,@body)
       (when ,var (disconnect ,var)))))

(defmacro with-transaction ((conn &key (isolation nil isolation-p)) &body body)
  "Execute BODY within a database transaction on CONN.
On success, commits. On error, rolls back.
Optional ISOLATION level: :serializable, :repeatable-read, :read-committed."
  (let ((begin-sql (if isolation-p
                       `(format nil "BEGIN ISOLATION LEVEL ~A"
                                (ecase ,isolation
                                  (:serializable "SERIALIZABLE")
                                  (:repeatable-read "REPEATABLE READ")
                                  (:read-committed "READ COMMITTED")))
                       `"BEGIN")))
    `(progn
       (execute-sql ,conn ,begin-sql)
       (handler-case
           (prog1 (progn ,@body)
             (execute-sql ,conn "COMMIT"))
         (error (e)
           (ignore-errors (execute-sql ,conn "ROLLBACK"))
           (error e))))))

;;; ============================================================
;;; LISTEN / NOTIFY
;;; ============================================================

(defun listen (conn channel)
  "Send LISTEN for CHANNEL. Notifications will arrive asynchronously."
  (execute-sql conn (format nil "LISTEN ~A" channel)))

(defun notify (conn channel &optional payload)
  "Send NOTIFY on CHANNEL with optional PAYLOAD string."
  (if payload
      (execute-sql conn (format nil "NOTIFY ~A, '~A'" channel payload))
      (execute-sql conn (format nil "NOTIFY ~A" channel))))

(defun read-notification (conn &key timeout)
  "Read the next NotificationResponse from CONN.
Returns (values channel payload pid) or NIL on timeout.
TIMEOUT is in seconds (nil = block forever).
Note: this consumes any other messages that arrive first."
  (let ((stream (pg-connection-stream conn)))
    (handler-case
        (flet ((read-loop ()
                 (loop
                   (multiple-value-bind (type payload) (read-message stream)
                     (case type
                       (#\A
                        ;; NotificationResponse: int32 pid, cstring channel, cstring payload
                        (multiple-value-bind (pid offset) (payload-read-int32 payload 0)
                          (multiple-value-bind (channel offset2) (payload-read-cstring payload offset)
                            (multiple-value-bind (notify-payload _off) (payload-read-cstring payload offset2)
                              (declare (ignore _off))
                              (return (values channel notify-payload pid))))))
                       (otherwise
                        ;; Process other messages normally
                        (process-backend-message conn type payload)))))))
          (if timeout
              (sb-ext:with-timeout timeout (read-loop))
              (read-loop)))
      (sb-ext:timeout () nil))))

;;; ============================================================
;;; Connection Health Check
;;; ============================================================

(defun connection-alive-p (conn)
  "Check if CONN is still usable by sending a simple query."
  (and (pg-connection-alive-p conn)
       (handler-case
           (progn
             (execute-sql conn "SELECT 1")
             t)
         (error () nil))))
