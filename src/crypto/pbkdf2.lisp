;;;; src/crypto/pbkdf2.lisp — PBKDF2-HMAC-SHA-256 key derivation (RFC 2898)
(in-package :cauldron.crypto)

(defun pbkdf2-sha256 (password salt &key (iterations 100000) (key-length 32))
  "Derive a key of KEY-LENGTH bytes from PASSWORD and SALT using PBKDF2-HMAC-SHA-256
with ITERATIONS rounds. PASSWORD may be a string or octet vector. SALT must be an octet vector."
  (let* ((password-octets (ensure-octets password))
         (salt-octets (ensure-octets salt))
         (hash-len 32) ;; SHA-256 output size
         (num-blocks (ceiling key-length hash-len))
         (dk (make-array key-length :element-type '(unsigned-byte 8) :initial-element 0))
         (dk-offset 0))
    (loop for block-idx from 1 to num-blocks
          do (let* (;; U1 = HMAC(password, salt || INT32BE(block-idx))
                    (salt-plus-idx (make-array (+ (length salt-octets) 4)
                                              :element-type '(unsigned-byte 8)))
                    (u-prev nil)
                    (t-block (make-array hash-len :element-type '(unsigned-byte 8)
                                         :initial-element 0)))
               ;; Build salt || INT32BE(i)
               (replace salt-plus-idx salt-octets)
               (let ((idx-offset (length salt-octets)))
                 (setf (aref salt-plus-idx idx-offset)       (logand (ash block-idx -24) #xFF))
                 (setf (aref salt-plus-idx (+ idx-offset 1)) (logand (ash block-idx -16) #xFF))
                 (setf (aref salt-plus-idx (+ idx-offset 2)) (logand (ash block-idx -8)  #xFF))
                 (setf (aref salt-plus-idx (+ idx-offset 3)) (logand block-idx #xFF)))
               ;; U1
               (setf u-prev (hmac-sha256 password-octets salt-plus-idx))
               ;; T = U1
               (replace t-block u-prev)
               ;; U2..Uc
               (loop for j from 2 to iterations
                     do (let ((u-next (hmac-sha256 password-octets u-prev)))
                          ;; T = T XOR Uj
                          (loop for k from 0 below hash-len
                                do (setf (aref t-block k)
                                         (logxor (aref t-block k) (aref u-next k))))
                          (setf u-prev u-next)))
               ;; Copy T into DK
               (let ((copy-len (min hash-len (- key-length dk-offset))))
                 (replace dk t-block :start1 dk-offset :end2 copy-len)
                 (incf dk-offset copy-len))))
    dk))
