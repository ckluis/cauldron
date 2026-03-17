;;;; src/db/protocol.lisp — PostgreSQL v3.0 wire protocol: message framing
;;;;
;;;; Backend (server→client): 1-byte type + 4-byte BE length (includes self) + payload
;;;; Frontend (client→server): same, except startup message has no type byte
(in-package :cauldron.db)

;;; ---------- Integer I/O (big-endian) ----------

(defun write-int32 (stream value)
  "Write a 32-bit big-endian integer to STREAM."
  (write-byte (logand (ash value -24) #xFF) stream)
  (write-byte (logand (ash value -16) #xFF) stream)
  (write-byte (logand (ash value -8)  #xFF) stream)
  (write-byte (logand value #xFF) stream)
  (values))

(defun write-int16 (stream value)
  "Write a 16-bit big-endian integer to STREAM."
  (write-byte (logand (ash value -8) #xFF) stream)
  (write-byte (logand value #xFF) stream)
  (values))

(defun read-int32 (stream)
  "Read a 32-bit big-endian integer from STREAM."
  (let ((b3 (read-byte stream))
        (b2 (read-byte stream))
        (b1 (read-byte stream))
        (b0 (read-byte stream)))
    ;; Handle signed interpretation: PG uses signed int32 for lengths
    ;; but we keep as unsigned for most uses
    (logior (ash b3 24) (ash b2 16) (ash b1 8) b0)))

(defun read-int32-signed (stream)
  "Read a 32-bit big-endian SIGNED integer from STREAM.
Used for DataRow column lengths where -1 means NULL."
  (let ((val (read-int32 stream)))
    (if (>= val #x80000000)
        (- val #x100000000)
        val)))

(defun read-int16 (stream)
  "Read a 16-bit big-endian integer from STREAM."
  (let ((hi (read-byte stream))
        (lo (read-byte stream)))
    (logior (ash hi 8) lo)))

(defun read-byte-from-stream (stream)
  "Read a single byte from STREAM."
  (read-byte stream))

;;; ---------- String I/O ----------

(defun write-cstring (stream string)
  "Write STRING as a null-terminated UTF-8 C-string to STREAM."
  (let ((octets (sb-ext:string-to-octets string :external-format :utf-8)))
    (write-sequence octets stream)
    (write-byte 0 stream))
  (values))

(defun read-cstring (stream)
  "Read a null-terminated UTF-8 C-string from STREAM. Returns a Lisp string."
  (let ((bytes (make-array 0 :element-type '(unsigned-byte 8)
                             :adjustable t :fill-pointer 0)))
    (loop for byte = (read-byte stream)
          until (zerop byte)
          do (vector-push-extend byte bytes))
    (sb-ext:octets-to-string bytes :external-format :utf-8)))

;;; ---------- Message Framing ----------

(defun write-message (stream type-char payload-bytes)
  "Write a typed PG protocol message: TYPE-CHAR (character), then 4-byte length
\(includes self but not type byte), then PAYLOAD-BYTES (octet vector)."
  (write-byte (char-code type-char) stream)
  (write-int32 stream (+ 4 (length payload-bytes)))
  (when (plusp (length payload-bytes))
    (write-sequence payload-bytes stream))
  (force-output stream)
  (values))

(defun read-message (stream)
  "Read a backend message from STREAM.
Returns (VALUES type-char payload) where type-char is a character and
payload is an octet vector."
  (let* ((type-byte (read-byte stream))
         (type-char (code-char type-byte))
         (length (read-int32 stream))
         (payload-length (- length 4))
         (payload (if (plusp payload-length)
                      (let ((buf (make-array payload-length
                                             :element-type '(unsigned-byte 8))))
                        (let ((n (read-sequence buf stream)))
                          (unless (= n payload-length)
                            (error "Short read in PG message: expected ~D, got ~D"
                                   payload-length n)))
                        buf)
                      (make-array 0 :element-type '(unsigned-byte 8)))))
    (values type-char payload)))

(defun write-startup-message (stream user database)
  "Write the startup message (no type byte): length + protocol 3.0 + params + null.
USER and DATABASE are strings."
  (let ((payload (make-array 0 :element-type '(unsigned-byte 8)
                               :adjustable t :fill-pointer 0)))
    ;; Protocol version 3.0 = 196608
    (vector-push-extend (logand (ash 196608 -24) #xFF) payload)
    (vector-push-extend (logand (ash 196608 -16) #xFF) payload)
    (vector-push-extend (logand (ash 196608 -8)  #xFF) payload)
    (vector-push-extend (logand 196608 #xFF) payload)
    ;; "user" parameter
    (loop for byte across (sb-ext:string-to-octets "user" :external-format :utf-8)
          do (vector-push-extend byte payload))
    (vector-push-extend 0 payload)
    (loop for byte across (sb-ext:string-to-octets user :external-format :utf-8)
          do (vector-push-extend byte payload))
    (vector-push-extend 0 payload)
    ;; "database" parameter
    (loop for byte across (sb-ext:string-to-octets "database" :external-format :utf-8)
          do (vector-push-extend byte payload))
    (vector-push-extend 0 payload)
    (loop for byte across (sb-ext:string-to-octets database :external-format :utf-8)
          do (vector-push-extend byte payload))
    (vector-push-extend 0 payload)
    ;; Terminating null
    (vector-push-extend 0 payload)
    ;; Write length (4 bytes for the length field itself + payload)
    (let ((total-length (+ 4 (length payload))))
      (write-int32 stream total-length))
    (write-sequence payload stream)
    (force-output stream)
    (values)))

;;; ---------- Payload Helpers ----------
;;; These help parse fields out of a raw payload octet vector.

(defun payload-read-int32 (payload offset)
  "Read a 32-bit big-endian integer from PAYLOAD at OFFSET. Returns (values int new-offset)."
  (values (logior (ash (aref payload offset) 24)
                  (ash (aref payload (+ offset 1)) 16)
                  (ash (aref payload (+ offset 2)) 8)
                  (aref payload (+ offset 3)))
          (+ offset 4)))

(defun payload-read-int32-signed (payload offset)
  "Read a signed 32-bit big-endian integer from PAYLOAD at OFFSET."
  (multiple-value-bind (val new-offset) (payload-read-int32 payload offset)
    (values (if (>= val #x80000000) (- val #x100000000) val)
            new-offset)))

(defun payload-read-int16 (payload offset)
  "Read a 16-bit big-endian integer from PAYLOAD at OFFSET. Returns (values int new-offset)."
  (values (logior (ash (aref payload offset) 8)
                  (aref payload (+ offset 1)))
          (+ offset 2)))

(defun payload-read-cstring (payload offset)
  "Read a null-terminated string from PAYLOAD at OFFSET. Returns (values string new-offset)."
  (let ((end (position 0 payload :start offset)))
    (unless end
      (error "No null terminator found in payload at offset ~D" offset))
    (values (sb-ext:octets-to-string payload :external-format :utf-8
                                              :start offset :end end)
            (1+ end))))

(defun payload-read-bytes (payload offset length)
  "Read LENGTH bytes from PAYLOAD at OFFSET. Returns (values octet-vector new-offset)."
  (values (subseq payload offset (+ offset length))
          (+ offset length)))

;;; ---------- Frontend Message Builders ----------
;;; These build payload byte vectors for specific message types.

(defun build-password-payload (password)
  "Build the payload for a PasswordMessage: null-terminated password string."
  (let ((octets (sb-ext:string-to-octets password :external-format :utf-8)))
    (let ((payload (make-array (1+ (length octets))
                               :element-type '(unsigned-byte 8))))
      (replace payload octets)
      (setf (aref payload (length octets)) 0)
      payload)))

(defun build-query-payload (sql)
  "Build the payload for a Query message: null-terminated SQL string."
  (build-password-payload sql))

(defun build-parse-payload (stmt-name sql param-oids)
  "Build the payload for a Parse message."
  (let ((buf (make-array 0 :element-type '(unsigned-byte 8)
                           :adjustable t :fill-pointer 0)))
    ;; Statement name (empty string = unnamed)
    (loop for byte across (sb-ext:string-to-octets stmt-name :external-format :utf-8)
          do (vector-push-extend byte buf))
    (vector-push-extend 0 buf)
    ;; Query string
    (loop for byte across (sb-ext:string-to-octets sql :external-format :utf-8)
          do (vector-push-extend byte buf))
    (vector-push-extend 0 buf)
    ;; Number of parameter type OIDs
    (let ((n (length param-oids)))
      (vector-push-extend (logand (ash n -8) #xFF) buf)
      (vector-push-extend (logand n #xFF) buf)
      ;; Each OID as int32
      (dolist (oid param-oids)
        (vector-push-extend (logand (ash oid -24) #xFF) buf)
        (vector-push-extend (logand (ash oid -16) #xFF) buf)
        (vector-push-extend (logand (ash oid -8)  #xFF) buf)
        (vector-push-extend (logand oid #xFF) buf)))
    (coerce buf '(simple-array (unsigned-byte 8) (*)))))

(defun build-bind-payload (portal-name stmt-name param-values)
  "Build the payload for a Bind message. PARAM-VALUES is a list of strings (or NIL for NULL)."
  (let ((buf (make-array 0 :element-type '(unsigned-byte 8)
                           :adjustable t :fill-pointer 0)))
    ;; Portal name
    (loop for byte across (sb-ext:string-to-octets portal-name :external-format :utf-8)
          do (vector-push-extend byte buf))
    (vector-push-extend 0 buf)
    ;; Statement name
    (loop for byte across (sb-ext:string-to-octets stmt-name :external-format :utf-8)
          do (vector-push-extend byte buf))
    (vector-push-extend 0 buf)
    ;; Number of parameter format codes: 1 (all text)
    (vector-push-extend 0 buf)
    (vector-push-extend 1 buf)
    ;; Format code: 0 = text
    (vector-push-extend 0 buf)
    (vector-push-extend 0 buf)
    ;; Number of parameters
    (let ((n (length param-values)))
      (vector-push-extend (logand (ash n -8) #xFF) buf)
      (vector-push-extend (logand n #xFF) buf))
    ;; Each parameter value
    (dolist (val param-values)
      (if (null val)
          ;; NULL: length = -1
          (progn
            (vector-push-extend #xFF buf)
            (vector-push-extend #xFF buf)
            (vector-push-extend #xFF buf)
            (vector-push-extend #xFF buf))
          ;; Text value: length + bytes
          (let ((octets (sb-ext:string-to-octets val :external-format :utf-8)))
            (let ((len (length octets)))
              (vector-push-extend (logand (ash len -24) #xFF) buf)
              (vector-push-extend (logand (ash len -16) #xFF) buf)
              (vector-push-extend (logand (ash len -8)  #xFF) buf)
              (vector-push-extend (logand len #xFF) buf))
            (loop for byte across octets
                  do (vector-push-extend byte buf)))))
    ;; Number of result format codes: 1 (all text)
    (vector-push-extend 0 buf)
    (vector-push-extend 1 buf)
    ;; Result format: 0 = text
    (vector-push-extend 0 buf)
    (vector-push-extend 0 buf)
    (coerce buf '(simple-array (unsigned-byte 8) (*)))))

(defun build-describe-payload (type name)
  "Build Describe payload. TYPE is #\\S (statement) or #\\P (portal)."
  (let ((octets (sb-ext:string-to-octets name :external-format :utf-8)))
    (let ((buf (make-array (+ 1 (length octets) 1)
                           :element-type '(unsigned-byte 8))))
      (setf (aref buf 0) (char-code type))
      (replace buf octets :start1 1)
      (setf (aref buf (+ 1 (length octets))) 0)
      buf)))

(defun build-execute-payload (portal-name max-rows)
  "Build Execute payload. MAX-ROWS 0 means unlimited."
  (let ((octets (sb-ext:string-to-octets portal-name :external-format :utf-8)))
    (let ((buf (make-array (+ (length octets) 1 4)
                           :element-type '(unsigned-byte 8)
                           :initial-element 0)))
      (replace buf octets)
      (setf (aref buf (length octets)) 0)
      ;; max-rows as int32
      (let ((off (1+ (length octets))))
        (setf (aref buf off)       (logand (ash max-rows -24) #xFF))
        (setf (aref buf (+ off 1)) (logand (ash max-rows -16) #xFF))
        (setf (aref buf (+ off 2)) (logand (ash max-rows -8)  #xFF))
        (setf (aref buf (+ off 3)) (logand max-rows #xFF)))
      buf)))

(defun write-sync (stream)
  "Send a Sync message (empty payload)."
  (write-message stream #\S (make-array 0 :element-type '(unsigned-byte 8))))

(defun write-flush (stream)
  "Send a Flush message (empty payload)."
  (write-message stream #\H (make-array 0 :element-type '(unsigned-byte 8))))

(defun write-terminate (stream)
  "Send a Terminate message."
  (write-message stream #\X (make-array 0 :element-type '(unsigned-byte 8))))
