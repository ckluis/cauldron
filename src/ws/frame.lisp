;;;; src/ws/frame.lisp — WebSocket frame encoding/decoding (RFC 6455)

(in-package :cauldron.ws)

;;; --- Constants ---

(defconstant +opcode-continuation+ #x0)
(defconstant +opcode-text+         #x1)
(defconstant +opcode-binary+       #x2)
(defconstant +opcode-close+        #x8)
(defconstant +opcode-ping+         #x9)
(defconstant +opcode-pong+         #xA)

(defconstant +max-frame-payload+ (* 16 1024 1024)
  "Maximum frame payload: 16MB.")

;;; --- Frame struct ---

(defstruct ws-frame
  "A WebSocket frame."
  (fin t :type boolean)
  (opcode +opcode-text+ :type (unsigned-byte 4))
  (mask nil)                         ; 4-byte mask key or nil
  (payload #() :type (vector (unsigned-byte 8))))

(defun control-frame-p (opcode)
  "Return T if OPCODE is a control frame."
  (>= opcode #x8))

;;; --- Frame reading ---

(defun read-bytes (stream n)
  "Read exactly N bytes from STREAM."
  (let ((buf (make-array n :element-type '(unsigned-byte 8))))
    (let ((read (read-sequence buf stream)))
      (unless (= read n)
        (error "WebSocket: unexpected end of stream (wanted ~D, got ~D)" n read)))
    buf))

(defun ws-read-frame (stream)
  "Read and decode a WebSocket frame from STREAM.
Returns a WS-FRAME struct."
  ;; Byte 0: FIN + RSV + opcode
  (let* ((byte0 (read-byte stream))
         (fin (logbitp 7 byte0))
         (opcode (logand byte0 #x0F))
         ;; Byte 1: MASK + payload length
         (byte1 (read-byte stream))
         (masked (logbitp 7 byte1))
         (len7 (logand byte1 #x7F)))
    ;; Extended payload length
    (let ((payload-length
            (cond
              ((< len7 126) len7)
              ((= len7 126)
               (let ((buf (read-bytes stream 2)))
                 (+ (ash (aref buf 0) 8) (aref buf 1))))
              (t ; len7 = 127
               (let ((buf (read-bytes stream 8)))
                 (loop for i from 0 below 8
                       sum (ash (aref buf i) (* 8 (- 7 i)))))))))
      ;; Validate
      (when (> payload-length +max-frame-payload+)
        (error "WebSocket: frame too large (~D bytes)" payload-length))
      (when (and (control-frame-p opcode) (> payload-length 125))
        (error "WebSocket: control frame payload too large"))
      ;; Mask key
      (let ((mask-key (when masked (read-bytes stream 4))))
        ;; Payload
        (let ((payload (if (> payload-length 0)
                           (read-bytes stream payload-length)
                           (make-array 0 :element-type '(unsigned-byte 8)))))
          ;; Unmask
          (when mask-key
            (dotimes (i (length payload))
              (setf (aref payload i)
                    (logxor (aref payload i)
                            (aref mask-key (mod i 4))))))
          (make-ws-frame :fin fin
                         :opcode opcode
                         :mask mask-key
                         :payload payload))))))

;;; --- Frame writing ---

(defun ws-write-frame (stream &key (fin t) (opcode +opcode-text+) payload)
  "Write a WebSocket frame to STREAM.
Server frames are NOT masked (per RFC 6455)."
  (let* ((data (or payload (make-array 0 :element-type '(unsigned-byte 8))))
         (len (length data)))
    ;; Byte 0: FIN + opcode
    (write-byte (logior (if fin #x80 0) opcode) stream)
    ;; Byte 1: no mask + length
    (cond
      ((< len 126)
       (write-byte len stream))
      ((< len 65536)
       (write-byte 126 stream)
       (write-byte (ash len -8) stream)
       (write-byte (logand len #xFF) stream))
      (t
       (write-byte 127 stream)
       (loop for shift from 56 downto 0 by 8
             do (write-byte (logand (ash len (- shift)) #xFF) stream))))
    ;; Payload
    (when (> len 0)
      (write-sequence data stream))
    (force-output stream)))

;;; --- Message assembly (handles fragmentation) ---

(defun ws-read-message (stream)
  "Read a complete WebSocket message (may span multiple frames).
Returns (values payload opcode).
Handles interleaved control frames."
  (let ((fragments '())
        (message-opcode nil))
    (loop
      (let ((frame (ws-read-frame stream)))
        (cond
          ;; Control frame — can appear between fragments
          ((control-frame-p (ws-frame-opcode frame))
           ;; Return control frame immediately for handling
           (return-from ws-read-message
             (values (ws-frame-payload frame) (ws-frame-opcode frame))))
          ;; First frame of message
          ((null message-opcode)
           (setf message-opcode (ws-frame-opcode frame))
           (push (ws-frame-payload frame) fragments)
           (when (ws-frame-fin frame)
             (return)))
          ;; Continuation frame
          ((= (ws-frame-opcode frame) +opcode-continuation+)
           (push (ws-frame-payload frame) fragments)
           (when (ws-frame-fin frame)
             (return)))
          (t
           (error "WebSocket: expected continuation frame, got opcode ~D"
                  (ws-frame-opcode frame))))))
    ;; Assemble fragments
    (let* ((total-len (reduce #'+ fragments :key #'length))
           (result (make-array total-len :element-type '(unsigned-byte 8)))
           (pos 0))
      (dolist (frag (nreverse fragments))
        (replace result frag :start1 pos)
        (incf pos (length frag)))
      (values result message-opcode))))

(defun ws-write-message (stream data &key (opcode +opcode-text+) (max-frame-size 65536))
  "Write DATA as a WebSocket message, fragmenting if needed."
  (let ((payload (if (stringp data)
                     ;; Convert string to UTF-8 bytes
                     (let* ((len (length data))
                            (octets (make-array len :element-type '(unsigned-byte 8))))
                       (dotimes (i len octets)
                         (setf (aref octets i) (char-code (char data i)))))
                     data)))
    (if (<= (length payload) max-frame-size)
        ;; Single frame
        (ws-write-frame stream :fin t :opcode opcode :payload payload)
        ;; Fragmented
        (let ((remaining (length payload))
              (offset 0)
              (first t))
          (loop while (> remaining 0)
                do (let* ((chunk-size (min remaining max-frame-size))
                          (chunk (subseq payload offset (+ offset chunk-size)))
                          (last-frame (<= remaining max-frame-size)))
                     (ws-write-frame stream
                                     :fin last-frame
                                     :opcode (if first opcode +opcode-continuation+)
                                     :payload chunk)
                     (incf offset chunk-size)
                     (decf remaining chunk-size)
                     (setf first nil)))))))
