;;;; src/crypto/random.lisp — Cryptographic random number generation
(in-package :cauldron.crypto)

(defun secure-random-bytes (count)
  "Generate COUNT cryptographically secure random bytes by reading /dev/urandom.
Returns an octet vector."
  (let ((result (make-array count :element-type '(unsigned-byte 8))))
    (with-open-file (stream "/dev/urandom" :element-type '(unsigned-byte 8))
      (let ((bytes-read (read-sequence result stream)))
        (unless (= bytes-read count)
          (error "Failed to read ~D bytes from /dev/urandom (got ~D)" count bytes-read))))
    result))

(defun generate-token (&key (length 32))
  "Generate LENGTH random bytes and return as a lowercase hex string."
  (let* ((bytes (secure-random-bytes length))
         (hex (make-string (* length 2))))
    (loop for i from 0 below length
          for byte = (aref bytes i)
          for hi = (ash byte -4)
          for lo = (logand byte #x0F)
          for pos = (* i 2)
          do (setf (char hex pos)       (char "0123456789abcdef" hi))
             (setf (char hex (+ pos 1)) (char "0123456789abcdef" lo)))
    hex))

(defun secure-equal (a b)
  "Constant-time comparison of A and B (octet vectors or strings).
Always compares ALL bytes to prevent timing attacks. Returns T or NIL."
  (let ((a-octets (ensure-octets a))
        (b-octets (ensure-octets b)))
    (if (/= (length a-octets) (length b-octets))
        nil
        (zerop (loop for i from 0 below (length a-octets)
                     sum (logxor (aref a-octets i) (aref b-octets i)))))))
