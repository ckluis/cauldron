;;;; test/ws/frame-test.lisp — WebSocket frame encoding/decoding tests (RFC 6455)
(in-package :cauldron.test)

(defsuite :ws-frame)

;;; --- control-frame-p ---

(deftest test-control-frame-p-close
  (is-true (cauldron.ws::control-frame-p #x8)))

(deftest test-control-frame-p-ping
  (is-true (cauldron.ws::control-frame-p #x9)))

(deftest test-control-frame-p-pong
  (is-true (cauldron.ws::control-frame-p #xA)))

(deftest test-control-frame-p-text-is-not
  (is-false (cauldron.ws::control-frame-p #x1)))

(deftest test-control-frame-p-binary-is-not
  (is-false (cauldron.ws::control-frame-p #x2)))

(deftest test-control-frame-p-continuation-is-not
  (is-false (cauldron.ws::control-frame-p #x0)))

;;; --- Helper: encode frame bytes manually ---

(defun write-frame-to-octets (opcode payload &key (fin t))
  "Encode a WS frame as server (no mask) and return byte vector."
  (let ((data (or payload (make-array 0 :element-type '(unsigned-byte 8))))
        (result '()))
    (let ((len (length data)))
      ;; Byte 0: FIN + opcode
      (push (logior (if fin #x80 0) opcode) result)
      ;; Byte 1: no mask + length
      (cond
        ((< len 126)
         (push len result))
        ((< len 65536)
         (push 126 result)
         (push (ash len -8) result)
         (push (logand len #xFF) result))
        (t
         (push 127 result)
         (loop for shift from 56 downto 0 by 8
               do (push (logand (ash len (- shift)) #xFF) result))))
      ;; Payload
      (let* ((header (nreverse result))
             (out (make-array (+ (length header) len)
                              :element-type '(unsigned-byte 8))))
        (loop for i from 0
              for b in header
              do (setf (aref out i) b))
        (replace out data :start1 (length header))
        out))))

;;; --- Byte layout tests ---

(deftest test-frame-text-small-payload
  "Text frame with small payload (<126 bytes) has correct byte layout."
  (let* ((payload (make-array 5 :element-type '(unsigned-byte 8)
                                :initial-contents '(72 101 108 108 111))) ; "Hello"
         (bytes (write-frame-to-octets cauldron.ws:+opcode-text+ payload)))
    ;; Byte 0: FIN(1) + RSV(000) + opcode(0001) = #x81
    (is-equal #x81 (aref bytes 0))
    ;; Byte 1: MASK(0) + length(5) = #x05
    (is-equal #x05 (aref bytes 1))
    ;; Bytes 2-6: payload
    (is-equal 72 (aref bytes 2))   ; H
    (is-equal 111 (aref bytes 6))  ; o
    (is-equal 7 (length bytes))))

(deftest test-frame-binary-opcode
  "Binary frame has opcode 2."
  (let* ((payload (make-array 3 :element-type '(unsigned-byte 8)
                                :initial-contents '(1 2 3)))
         (bytes (write-frame-to-octets cauldron.ws:+opcode-binary+ payload)))
    ;; Byte 0: FIN + opcode 2 = #x82
    (is-equal #x82 (aref bytes 0))))

(deftest test-frame-close-opcode
  "Close frame has opcode 8."
  (let* ((status-code #x03E8)  ; 1000 = normal close
         (payload (make-array 2 :element-type '(unsigned-byte 8)
                                :initial-contents (list (ash status-code -8)
                                                        (logand status-code #xFF))))
         (bytes (write-frame-to-octets cauldron.ws:+opcode-close+ payload)))
    ;; Byte 0: FIN + opcode 8 = #x88
    (is-equal #x88 (aref bytes 0))
    ;; Byte 1: length 2
    (is-equal 2 (aref bytes 1))
    ;; Status code in payload
    (is-equal #x03 (aref bytes 2))
    (is-equal #xE8 (aref bytes 3))))

(deftest test-frame-no-fin-bit
  "Non-final frame has FIN=0."
  (let* ((payload (make-array 1 :element-type '(unsigned-byte 8)
                                :initial-contents '(65)))
         (bytes (write-frame-to-octets cauldron.ws:+opcode-text+ payload :fin nil)))
    ;; Byte 0: FIN(0) + opcode(1) = #x01
    (is-equal #x01 (aref bytes 0))))

(deftest test-frame-empty-payload
  "Frame with empty payload."
  (let ((bytes (write-frame-to-octets cauldron.ws:+opcode-text+ nil)))
    (is-equal #x81 (aref bytes 0))
    (is-equal 0 (aref bytes 1))
    (is-equal 2 (length bytes))))

(deftest test-frame-medium-payload-length-encoding
  "Payload 126-65535 bytes uses 2-byte extended length."
  (let* ((payload (make-array 200 :element-type '(unsigned-byte 8) :initial-element 42))
         (bytes (write-frame-to-octets cauldron.ws:+opcode-binary+ payload)))
    ;; Byte 1: 126 indicates 2-byte length follows
    (is-equal 126 (aref bytes 1))
    ;; Bytes 2-3: big-endian 200
    (is-equal 0 (aref bytes 2))
    (is-equal 200 (aref bytes 3))
    ;; Total: 2 (header) + 2 (extended length) + 200 (payload) = 204
    (is-equal 204 (length bytes))))

(deftest test-frame-exactly-125-bytes
  "125-byte payload uses single-byte length."
  (let* ((payload (make-array 125 :element-type '(unsigned-byte 8) :initial-element 0))
         (bytes (write-frame-to-octets cauldron.ws:+opcode-text+ payload)))
    (is-equal 125 (aref bytes 1))
    (is-equal 127 (length bytes)))) ; 2 header + 125 payload

(deftest test-frame-exactly-126-bytes
  "126-byte payload triggers extended length encoding."
  (let* ((payload (make-array 126 :element-type '(unsigned-byte 8) :initial-element 0))
         (bytes (write-frame-to-octets cauldron.ws:+opcode-text+ payload)))
    (is-equal 126 (aref bytes 1))
    (is-equal 0 (aref bytes 2))
    (is-equal 126 (aref bytes 3))
    (is-equal 130 (length bytes)))) ; 2 + 2 + 126

(deftest test-frame-ping-opcode
  "Ping frame opcode."
  (let ((bytes (write-frame-to-octets cauldron.ws:+opcode-ping+ nil)))
    (is-equal #x89 (aref bytes 0))))

(deftest test-frame-pong-opcode
  "Pong frame opcode."
  (let ((bytes (write-frame-to-octets cauldron.ws:+opcode-pong+ nil)))
    (is-equal #x8A (aref bytes 0))))

(deftest test-frame-continuation-opcode
  (let* ((payload (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(0)))
         (bytes (write-frame-to-octets cauldron.ws:+opcode-continuation+ payload :fin nil)))
    ;; FIN=0 + opcode 0 = #x00
    (is-equal #x00 (aref bytes 0))))

;;; --- Opcode constants ---

(deftest test-opcode-constants
  (is-equal #x0 cauldron.ws:+opcode-continuation+)
  (is-equal #x1 cauldron.ws:+opcode-text+)
  (is-equal #x2 cauldron.ws:+opcode-binary+)
  (is-equal #x8 cauldron.ws:+opcode-close+)
  (is-equal #x9 cauldron.ws:+opcode-ping+)
  (is-equal #xA cauldron.ws:+opcode-pong+))
