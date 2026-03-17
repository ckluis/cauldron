;;;; src/ws/connection.lisp — WebSocket connection management
;;;; Upgrade handshake, ping/pong, close handshake.

(in-package :cauldron.ws)

;;; --- WebSocket magic string (RFC 6455 §4.2.2) ---

(defparameter +ws-magic-string+ "258EAFA5-E914-47DA-95CA-5AB5A43ADE65")

;;; --- Connection struct ---

(defstruct ws-connection
  "A WebSocket connection."
  (stream nil)                       ; Binary I/O stream
  (state :open :type keyword)        ; :open, :closing, :closed
  (on-message nil)                   ; Callback (connection payload opcode)
  (on-close nil)                     ; Callback (connection code reason)
  (last-ping-time 0 :type integer)
  (missed-pongs 0 :type integer)
  (max-missed-pongs 3 :type integer))

;;; --- Upgrade handshake ---

(defun ws-upgrade-p (request)
  "Return T if REQUEST is a valid WebSocket upgrade request."
  (and (eq (cauldron.http:request-method request) :get)
       (let ((upgrade (cauldron.http:request-header request "Upgrade"))
             (connection (cauldron.http:request-header request "Connection"))
             (key (cauldron.http:request-header request "Sec-WebSocket-Key"))
             (version (cauldron.http:request-header request "Sec-WebSocket-Version")))
         (and upgrade (string-equal upgrade "websocket")
              connection (search "upgrade" (string-downcase connection))
              key (= (length key) 24)  ; Base64 of 16 bytes
              version (string= version "13")))))

(defun ws-accept-key (client-key)
  "Compute the Sec-WebSocket-Accept value from CLIENT-KEY.
accept = base64(sha1(key + magic-string))"
  (let* ((concat (concatenate 'string client-key +ws-magic-string+))
         (input (map '(vector (unsigned-byte 8)) #'char-code concat))
         (hash (cauldron.crypto:sha1 input)))
    (cauldron.crypto:base64-encode hash)))

(defun ws-upgrade-response (request stream)
  "Send the 101 Switching Protocols response for a WebSocket upgrade.
Returns a WS-CONNECTION on success."
  (let* ((key (cauldron.http:request-header request "Sec-WebSocket-Key"))
         (accept (ws-accept-key key)))
    ;; Write 101 response directly
    (let ((response-text (format nil "HTTP/1.1 101 Switching Protocols~C~CUpgrade: websocket~C~CConnection: Upgrade~C~CSec-WebSocket-Accept: ~A~C~C~C~C"
                                 #\Return #\Linefeed
                                 #\Return #\Linefeed
                                 #\Return #\Linefeed
                                 accept
                                 #\Return #\Linefeed
                                 #\Return #\Linefeed)))
      (loop for char across response-text
            do (write-byte (char-code char) stream))
      (force-output stream))
    ;; Return connection
    (make-ws-connection :stream stream :state :open)))

;;; --- Control frames ---

(defun ws-ping (conn &optional payload)
  "Send a ping frame."
  (let ((data (or payload (make-array 0 :element-type '(unsigned-byte 8)))))
    (ws-write-frame (ws-connection-stream conn)
                    :opcode +opcode-ping+
                    :payload data)))

(defun ws-pong (conn payload)
  "Send a pong frame with the same payload as the ping."
  (ws-write-frame (ws-connection-stream conn)
                  :opcode +opcode-pong+
                  :payload payload))

(defun ws-handle-control-frame (conn payload opcode)
  "Handle a control frame. Returns T if connection should continue, NIL if closing."
  (cond
    ((= opcode +opcode-ping+)
     (ws-pong conn payload)
     t)
    ((= opcode +opcode-pong+)
     (setf (ws-connection-missed-pongs conn) 0)
     t)
    ((= opcode +opcode-close+)
     ;; Parse close code and reason
     (let ((code (if (>= (length payload) 2)
                     (+ (ash (aref payload 0) 8) (aref payload 1))
                     1000))
           (reason (if (> (length payload) 2)
                       ;; Simple ASCII extraction
                       (map 'string #'code-char (subseq payload 2))
                       "")))
       ;; Send close frame back if we haven't already
       (when (eq (ws-connection-state conn) :open)
         (setf (ws-connection-state conn) :closing)
         (ws-send-close conn code reason))
       (setf (ws-connection-state conn) :closed)
       ;; Invoke close callback
       (when (ws-connection-on-close conn)
         (funcall (ws-connection-on-close conn) conn code reason))
       nil))
    (t t)))

;;; --- Sending ---

(defun ws-send (conn text)
  "Send a text message over the WebSocket."
  (ws-write-message (ws-connection-stream conn) text :opcode +opcode-text+))

(defun ws-send-binary (conn data)
  "Send a binary message over the WebSocket."
  (ws-write-message (ws-connection-stream conn) data :opcode +opcode-binary+))

(defun ws-send-close (conn code reason)
  "Send a close frame with CODE and REASON."
  (let* ((reason-bytes (map '(vector (unsigned-byte 8)) #'char-code reason))
         (payload (make-array (+ 2 (length reason-bytes))
                              :element-type '(unsigned-byte 8))))
    (setf (aref payload 0) (ash code -8))
    (setf (aref payload 1) (logand code #xFF))
    (replace payload reason-bytes :start1 2)
    (ws-write-frame (ws-connection-stream conn)
                    :opcode +opcode-close+
                    :payload payload)))

;;; --- Close ---

(defun ws-close (conn &key (code 1000) (reason ""))
  "Initiate a clean WebSocket close handshake."
  (when (eq (ws-connection-state conn) :open)
    (setf (ws-connection-state conn) :closing)
    (ws-send-close conn code reason)))
