;;;; src/runtime/request-id.lisp — Request ID generation and propagation
(in-package :cauldron.runtime)

;;; --- Request ID context ---

(defvar *current-request-id* nil
  "The request ID for the current request context.
Bind with LET in request-handling code to propagate through the call stack.")

(defun generate-request-id ()
  "Generate a unique request ID as a 32-character lowercase hex string.
Uses cauldron.crypto:generate-token when available, falls back to
reading /dev/urandom directly."
  (let ((crypto-pkg (find-package :cauldron.crypto)))
    (if (and crypto-pkg (fboundp (find-symbol "GENERATE-TOKEN" crypto-pkg)))
        (funcall (find-symbol "GENERATE-TOKEN" crypto-pkg) :length 16)
        ;; Fallback: read 16 bytes from /dev/urandom and hex-encode
        (let* ((bytes (make-array 16 :element-type '(unsigned-byte 8)))
               (hex (make-string 32)))
          (with-open-file (stream "/dev/urandom" :element-type '(unsigned-byte 8))
            (read-sequence bytes stream))
          (loop for i from 0 below 16
                for byte = (aref bytes i)
                for hi = (ash byte -4)
                for lo = (logand byte #x0F)
                for pos = (* i 2)
                do (setf (char hex pos)       (char "0123456789abcdef" hi))
                   (setf (char hex (+ pos 1)) (char "0123456789abcdef" lo)))
          hex))))
