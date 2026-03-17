;;;; src/crypto/hmac.lisp — HMAC-SHA-256 implementation (RFC 2104)
(in-package :cauldron.crypto)

(defun hmac-sha256 (key message)
  "Compute HMAC-SHA-256 of MESSAGE using KEY. Both must be octet vectors (or strings).
Returns 32-byte octet vector."
  (let* ((key-octets (ensure-octets key))
         (msg-octets (ensure-octets message))
         (block-size 64)
         ;; If key > block size, hash it
         (actual-key (if (> (length key-octets) block-size)
                         (sha256 key-octets)
                         key-octets))
         ;; Pad key to block-size with zeros
         (padded-key (make-array block-size :element-type '(unsigned-byte 8) :initial-element 0))
         (ipad (make-array block-size :element-type '(unsigned-byte 8)))
         (opad (make-array block-size :element-type '(unsigned-byte 8))))
    ;; Copy key into padded-key
    (replace padded-key actual-key)
    ;; Compute ipad = key XOR 0x36, opad = key XOR 0x5c
    (loop for i from 0 below block-size
          do (setf (aref ipad i) (logxor (aref padded-key i) #x36))
             (setf (aref opad i) (logxor (aref padded-key i) #x5c)))
    ;; HMAC = SHA-256(opad || SHA-256(ipad || message))
    (let* ((inner-input (make-array (+ block-size (length msg-octets))
                                    :element-type '(unsigned-byte 8)))
           (inner-hash (progn
                         (replace inner-input ipad)
                         (replace inner-input msg-octets :start1 block-size)
                         (sha256 inner-input)))
           (outer-input (make-array (+ block-size 32)
                                    :element-type '(unsigned-byte 8))))
      (replace outer-input opad)
      (replace outer-input inner-hash :start1 block-size)
      (sha256 outer-input))))
