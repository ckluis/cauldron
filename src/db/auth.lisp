;;;; src/db/auth.lisp — PostgreSQL authentication (MD5, SCRAM-SHA-256)
;;;;
;;;; MD5 is implemented here directly (legacy PG auth only).
;;;; SCRAM-SHA-256 uses cauldron.crypto primitives.
(in-package :cauldron.db)

;;; ============================================================
;;; MD5 Implementation (RFC 1321) — minimal, for PG auth only
;;; ============================================================

(defvar +md5-s+
  #(7 12 17 22  7 12 17 22  7 12 17 22  7 12 17 22
    5  9 14 20  5  9 14 20  5  9 14 20  5  9 14 20
    4 11 16 23  4 11 16 23  4 11 16 23  4 11 16 23
    6 10 15 21  6 10 15 21  6 10 15 21  6 10 15 21)
  "Per-round shift amounts for MD5.")

(defvar +md5-k+
  #(#xd76aa478 #xe8c7b756 #x242070db #xc1bdceee
    #xf57c0faf #x4787c62a #xa8304613 #xfd469501
    #x698098d8 #x8b44f7af #xffff5bb1 #x895cd7be
    #x6b901122 #xfd987193 #xa679438e #x49b40821
    #xf61e2562 #xc040b340 #x265e5a51 #xe9b6c7aa
    #xd62f105d #x02441453 #xd8a1e681 #xe7d3fbc8
    #x21e1cde6 #xc33707d6 #xf4d50d87 #x455a14ed
    #xa9e3e905 #xfcefa3f8 #x676f02d9 #x8d2a4c8a
    #xfffa3942 #x8771f681 #x6d9d6122 #xfde5380c
    #xa4beea44 #x4bdecfa9 #xf6bb4b60 #xbebfbc70
    #x289b7ec6 #xeaa127fa #xd4ef3085 #x04881d05
    #xd9d4d039 #xe6db99e5 #x1fa27cf8 #xc4ac5665
    #xf4292244 #x432aff97 #xab9423a7 #xfc93a039
    #x655b59c3 #x8f0ccc92 #xffeff47d #x85845dd1
    #x6fa87e4f #xfe2ce6e0 #xa3014314 #x4e0811a1
    #xf7537e82 #xbd3af235 #x2ad7d2bb #xeb86d391)
  "Precomputed constants for MD5.")

(declaim (inline md5-mod32+ md5-left-rotate))

(defun md5-mod32+ (&rest args)
  "Add modulo 2^32."
  (logand #xFFFFFFFF (apply #'+ args)))

(defun md5-left-rotate (x n)
  "Left-rotate 32-bit value X by N bits."
  (logand #xFFFFFFFF
          (logior (ash x n)
                  (ash x (- n 32)))))

(defun md5-pad-message (message)
  "Pad MESSAGE per MD5 spec."
  (let* ((msg-len (length message))
         (bit-len (* msg-len 8))
         (r (mod (+ msg-len 1) 64))
         (pad-zeros (if (<= r 56) (- 56 r) (- 120 r)))
         (total-len (+ msg-len 1 pad-zeros 8))
         (padded (make-array total-len :element-type '(unsigned-byte 8) :initial-element 0)))
    (replace padded message)
    (setf (aref padded msg-len) #x80)
    ;; 64-bit LITTLE-endian bit-length at end
    (loop for i from 0 below 8
          do (setf (aref padded (+ msg-len 1 pad-zeros i))
                   (logand (ash bit-len (* -8 i)) #xFF)))
    padded))

(defun md5-get-le32 (buf offset)
  "Read 32-bit little-endian integer from BUF at OFFSET."
  (logior (aref buf offset)
          (ash (aref buf (+ offset 1)) 8)
          (ash (aref buf (+ offset 2)) 16)
          (ash (aref buf (+ offset 3)) 24)))

(defun md5 (data)
  "Compute MD5 hash of DATA (octet vector or string). Returns 16-byte octet vector."
  (let* ((message (etypecase data
                    ((vector (unsigned-byte 8)) data)
                    (string (sb-ext:string-to-octets data :external-format :utf-8))))
         (padded (md5-pad-message message))
         (a0 #x67452301)
         (b0 #xefcdab89)
         (c0 #x98badcfe)
         (d0 #x10325476))
    (loop for block-start from 0 below (length padded) by 64
          do (let ((m (make-array 16 :element-type '(unsigned-byte 32))))
               ;; Read 16 little-endian 32-bit words
               (loop for j from 0 below 16
                     do (setf (aref m j) (md5-get-le32 padded (+ block-start (* j 4)))))
               (let ((a a0) (b b0) (c c0) (d d0))
                 (loop for i from 0 below 64
                       for f = (cond ((< i 16)
                                      (logior (logand b c)
                                              (logand (logand #xFFFFFFFF (lognot b)) d)))
                                     ((< i 32)
                                      (logior (logand d b)
                                              (logand (logand #xFFFFFFFF (lognot d)) c)))
                                     ((< i 48)
                                      (logxor b c d))
                                     (t
                                      (logxor c (logior b
                                                        (logand #xFFFFFFFF (lognot d))))))
                       for g = (cond ((< i 16) i)
                                     ((< i 32) (mod (+ (* 5 i) 1) 16))
                                     ((< i 48) (mod (+ (* 3 i) 5) 16))
                                     (t (mod (* 7 i) 16)))
                       do (let ((temp d))
                            (setf d c)
                            (setf c b)
                            (setf b (md5-mod32+ b
                                                (md5-left-rotate
                                                 (md5-mod32+ a f (aref +md5-k+ i) (aref m g))
                                                 (aref +md5-s+ i))))
                            (setf a temp)))
                 (setf a0 (md5-mod32+ a0 a))
                 (setf b0 (md5-mod32+ b0 b))
                 (setf c0 (md5-mod32+ c0 c))
                 (setf d0 (md5-mod32+ d0 d)))))
    ;; Produce 16-byte digest (little-endian)
    (let ((digest (make-array 16 :element-type '(unsigned-byte 8))))
      (loop for word in (list a0 b0 c0 d0)
            for offset from 0 by 4
            do (setf (aref digest offset)       (logand word #xFF))
               (setf (aref digest (+ offset 1)) (logand (ash word -8)  #xFF))
               (setf (aref digest (+ offset 2)) (logand (ash word -16) #xFF))
               (setf (aref digest (+ offset 3)) (logand (ash word -24) #xFF)))
      digest)))

(defun md5-hex (data)
  "Compute MD5 of DATA and return as lowercase hex string."
  (let* ((digest (md5 data))
         (hex (make-string 32)))
    (loop for i from 0 below 16
          for byte = (aref digest i)
          for pos = (* i 2)
          do (setf (char hex pos)       (char "0123456789abcdef" (ash byte -4)))
             (setf (char hex (1+ pos))  (char "0123456789abcdef" (logand byte #x0F))))
    hex))

;;; ============================================================
;;; MD5 Password Authentication
;;; ============================================================

(defun md5-auth (stream user password salt)
  "Perform MD5 password authentication.
inner = md5(password || username) as hex
outer = \"md5\" || md5(inner || salt) as hex
SALT is a 4-byte octet vector."
  (let* ((inner-input (concatenate 'string password user))
         (inner-hex (md5-hex inner-input))
         ;; outer input = inner_hex bytes + salt bytes
         (inner-octets (sb-ext:string-to-octets inner-hex :external-format :utf-8))
         (outer-input (make-array (+ (length inner-octets) (length salt))
                                  :element-type '(unsigned-byte 8)))
         (outer-hex nil))
    (replace outer-input inner-octets)
    (replace outer-input salt :start1 (length inner-octets))
    (setf outer-hex (concatenate 'string "md5" (md5-hex outer-input)))
    ;; Send PasswordMessage
    (write-message stream #\p (build-password-payload outer-hex))))

;;; ============================================================
;;; SCRAM-SHA-256 Authentication (RFC 5802 / 7677)
;;; ============================================================

(defun make-client-nonce ()
  "Generate a 24-byte random client nonce, base64 encoded."
  (cauldron.crypto:base64-encode (cauldron.crypto:secure-random-bytes 24)))

(defun parse-scram-attributes (message)
  "Parse a SCRAM message into an alist of (character . value) pairs.
Message format: key=value,key=value,..."
  (let ((result '()))
    (loop for part in (split-scram-string message #\,)
          when (>= (length part) 2)
          do (push (cons (char part 0) (subseq part 2)) result))
    (nreverse result)))

(defun split-scram-string (string delimiter)
  "Split STRING by single-character DELIMITER."
  (let ((result '())
        (start 0))
    (loop for i from 0 below (length string)
          when (char= (char string i) delimiter)
          do (push (subseq string start i) result)
             (setf start (1+ i)))
    (push (subseq string start) result)
    (nreverse result)))

(defun scram-sha256-auth (stream user password)
  "Perform full SCRAM-SHA-256 authentication flow.
Sends SASLInitialResponse, processes SASLContinue, sends SASLResponse,
verifies SASLFinal."
  (let* ((client-nonce (make-client-nonce))
         (client-first-bare (format nil "n=~A,r=~A" user client-nonce))
         (client-first-message (format nil "n,,~A" client-first-bare)))
    ;; Step 1: Send SASLInitialResponse with mechanism "SCRAM-SHA-256"
    (let ((mechanism "SCRAM-SHA-256"))
      (let ((mech-octets (sb-ext:string-to-octets mechanism :external-format :utf-8))
            (msg-octets (sb-ext:string-to-octets client-first-message :external-format :utf-8)))
        (let* ((payload-len (+ (length mech-octets) 1  ; mechanism + null
                               4                        ; int32 length of client-first
                               (length msg-octets)))
               (payload (make-array payload-len :element-type '(unsigned-byte 8)
                                                :initial-element 0))
               (offset 0))
          ;; Mechanism name + null
          (replace payload mech-octets :start1 offset)
          (incf offset (length mech-octets))
          (setf (aref payload offset) 0)
          (incf offset)
          ;; Length of client-first-message as int32
          (let ((len (length msg-octets)))
            (setf (aref payload offset)       (logand (ash len -24) #xFF))
            (setf (aref payload (+ offset 1)) (logand (ash len -16) #xFF))
            (setf (aref payload (+ offset 2)) (logand (ash len -8)  #xFF))
            (setf (aref payload (+ offset 3)) (logand len #xFF)))
          (incf offset 4)
          ;; client-first-message bytes
          (replace payload msg-octets :start1 offset)
          (write-message stream #\p payload))))

    ;; Step 2: Read server response — should be AuthenticationSASLContinue (type 11)
    (multiple-value-bind (type payload) (read-message stream)
      (unless (char= type #\R)
        (error "Expected Authentication response, got '~A'" type))
      (let ((auth-type (payload-read-int32 payload 0)))
        (multiple-value-bind (auth-code) auth-type
          (unless (= auth-code 11)
            (error "Expected SASLContinue (11), got ~D" auth-code))))
      ;; Parse server-first-message from payload (after 4-byte auth type)
      (let* ((server-first (sb-ext:octets-to-string payload :external-format :utf-8
                                                             :start 4))
             (attrs (parse-scram-attributes server-first))
             (server-nonce (cdr (assoc #\r attrs)))
             (salt-b64 (cdr (assoc #\s attrs)))
             (iterations (parse-integer (cdr (assoc #\i attrs))))
             (salt (cauldron.crypto:base64-decode salt-b64)))
        ;; Verify server nonce starts with client nonce
        (unless (and server-nonce
                     (>= (length server-nonce) (length client-nonce))
                     (string= server-nonce client-nonce
                              :end1 (length client-nonce)))
          (error "SCRAM: server nonce does not begin with client nonce"))

        ;; Step 3: Compute client proof
        (let* ((salted-password (cauldron.crypto:pbkdf2-sha256
                                 password salt
                                 :iterations iterations :key-length 32))
               (client-key (cauldron.crypto:hmac-sha256 salted-password "Client Key"))
               (stored-key (cauldron.crypto:sha256 client-key))
               (client-final-without-proof
                 (format nil "c=biws,r=~A" server-nonce))
               (auth-message
                 (format nil "~A,~A,~A"
                         client-first-bare server-first client-final-without-proof))
               (client-signature (cauldron.crypto:hmac-sha256 stored-key auth-message))
               (client-proof (make-array 32 :element-type '(unsigned-byte 8))))
          ;; ClientProof = ClientKey XOR ClientSignature
          (loop for i from 0 below 32
                do (setf (aref client-proof i)
                         (logxor (aref client-key i) (aref client-signature i))))
          ;; Build client-final-message
          (let* ((proof-b64 (cauldron.crypto:base64-encode client-proof))
                 (client-final (format nil "~A,p=~A"
                                       client-final-without-proof proof-b64))
                 (final-octets (sb-ext:string-to-octets client-final
                                                        :external-format :utf-8)))
            ;; Send SASLResponse
            (write-message stream #\p final-octets))

          ;; Step 4: Read SASLFinal
          (multiple-value-bind (type2 payload2) (read-message stream)
            (unless (char= type2 #\R)
              (error "Expected Authentication response for SASLFinal, got '~A'" type2))
            (let ((auth-type2 (payload-read-int32 payload2 0)))
              (multiple-value-bind (auth-code2) auth-type2
                (unless (= auth-code2 12)
                  (error "Expected SASLFinal (12), got ~D" auth-code2))))
            ;; Parse server-final-message and verify server signature
            (let* ((server-final (sb-ext:octets-to-string payload2 :external-format :utf-8
                                                                    :start 4))
                   (sf-attrs (parse-scram-attributes server-final))
                   (server-sig-b64 (cdr (assoc #\v sf-attrs))))
              (when server-sig-b64
                (let* ((server-key (cauldron.crypto:hmac-sha256
                                    salted-password "Server Key"))
                       (expected-sig (cauldron.crypto:hmac-sha256
                                      server-key auth-message))
                       (received-sig (cauldron.crypto:base64-decode server-sig-b64)))
                  (unless (cauldron.crypto:secure-equal expected-sig received-sig)
                    (error "SCRAM: server signature verification failed")))))))))))

;;; ============================================================
;;; Authentication Dispatch
;;; ============================================================

(defun handle-auth-response (stream payload user password)
  "Handle an Authentication ('R') message PAYLOAD.
Dispatches on auth type code. Returns T when authentication is complete."
  (let ((auth-type (payload-read-int32 payload 0)))
    (multiple-value-bind (auth-code) auth-type
      (cond
        ;; AuthenticationOk
        ((= auth-code 0) t)

        ;; CleartextPassword
        ((= auth-code 3)
         (write-message stream #\p (build-password-payload password))
         nil)

        ;; MD5Password — 4-byte salt follows auth type
        ((= auth-code 5)
         (let ((salt (subseq payload 4 8)))
           (md5-auth stream user password salt))
         nil)

        ;; SASL — list of mechanisms
        ((= auth-code 10)
         ;; Parse mechanism names from payload (after 4 bytes auth type)
         ;; Each mechanism is a null-terminated string, list ends with extra null
         (let ((mechanisms '())
               (offset 4))
           (loop while (< offset (length payload))
                 do (if (zerop (aref payload offset))
                        (return) ; end of list
                        (multiple-value-bind (mech new-offset)
                            (payload-read-cstring payload offset)
                          (push mech mechanisms)
                          (setf offset new-offset))))
           (setf mechanisms (nreverse mechanisms))
           ;; We only support SCRAM-SHA-256
           (unless (member "SCRAM-SHA-256" mechanisms :test #'string=)
             (error "Server requires unsupported SASL mechanism(s): ~A" mechanisms))
           (scram-sha256-auth stream user password))
         ;; After SCRAM completes, we expect AuthenticationOk next
         nil)

        ;; SASLContinue / SASLFinal should not arrive here
        ;; (they are handled inside scram-sha256-auth)
        ((= auth-code 11)
         (error "Unexpected SASLContinue outside SCRAM flow"))
        ((= auth-code 12)
         (error "Unexpected SASLFinal outside SCRAM flow"))

        (t
         (error "Unsupported authentication type: ~D" auth-code))))))
