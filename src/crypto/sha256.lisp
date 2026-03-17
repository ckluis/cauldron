;;;; src/crypto/sha256.lisp — SHA-256 hash implementation (FIPS 180-4)
;;;; THE critical crypto primitive. Foundation of the entire auth system.
(in-package :cauldron.crypto)

;;; ---------- Constants ----------

;; 64 round constants: first 32 bits of the fractional parts of
;; the cube roots of the first 64 primes (2..311).
(defvar +sha256-k+
  #(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5
    #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
    #xd807aa98 #x12835b01 #x243185be #x550c7dc3
    #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
    #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc
    #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
    #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7
    #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
    #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13
    #x650a7354 #x766a0abb #x81c2c92e #x92722c85
    #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3
    #xd192e819 #xd6990624 #xf40e3585 #x106aa070
    #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5
    #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
    #x748f82ee #x78a5636f #x84c87814 #x8cc70208
    #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))

;; Initial hash values: first 32 bits of the fractional parts of
;; the square roots of the first 8 primes (2..19).
(defvar +sha256-h-init+
  #(#x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
    #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19))

;;; ---------- 32-bit Helpers ----------

(declaim (inline mod32+ rotr32))

(defun mod32+ (&rest args)
  "Add arguments modulo 2^32."
  (logand #xFFFFFFFF (apply #'+ args)))

(defun rotr32 (x n)
  "Right-rotate 32-bit value X by N bits."
  (logand #xFFFFFFFF
          (logior (ash x (- n))
                  (ash x (- 32 n)))))

(defun shr32 (x n)
  "Right-shift 32-bit value X by N bits."
  (ash x (- n)))

;;; ---------- SHA-256 Functions ----------

(declaim (inline sha256-ch sha256-maj sha256-big-sigma0 sha256-big-sigma1
                 sha256-small-sigma0 sha256-small-sigma1))

(defun sha256-ch (x y z)
  "Ch(x,y,z) = (x AND y) XOR (NOT x AND z)"
  (logxor (logand x y)
          (logand (logand #xFFFFFFFF (lognot x)) z)))

(defun sha256-maj (x y z)
  "Maj(x,y,z) = (x AND y) XOR (x AND z) XOR (y AND z)"
  (logxor (logand x y) (logand x z) (logand y z)))

(defun sha256-big-sigma0 (x)
  "Sigma-0(x) = ROTR(2,x) XOR ROTR(13,x) XOR ROTR(22,x)"
  (logxor (rotr32 x 2) (rotr32 x 13) (rotr32 x 22)))

(defun sha256-big-sigma1 (x)
  "Sigma-1(x) = ROTR(6,x) XOR ROTR(11,x) XOR ROTR(25,x)"
  (logxor (rotr32 x 6) (rotr32 x 11) (rotr32 x 25)))

(defun sha256-small-sigma0 (x)
  "sigma-0(x) = ROTR(7,x) XOR ROTR(18,x) XOR SHR(3,x)"
  (logxor (rotr32 x 7) (rotr32 x 18) (shr32 x 3)))

(defun sha256-small-sigma1 (x)
  "sigma-1(x) = ROTR(17,x) XOR ROTR(19,x) XOR SHR(10,x)"
  (logxor (rotr32 x 17) (rotr32 x 19) (shr32 x 10)))

;;; ---------- Padding ----------

(defun sha256-pad-message (message)
  "Pad MESSAGE per SHA-256 spec: append 0x80, zeros, 64-bit BE bit-length."
  (let* ((msg-len (length message))
         (bit-len (* msg-len 8))
         ;; Need room for 1 byte (0x80) + padding zeros + 8 bytes (length)
         ;; Total must be multiple of 64
         (r (mod (+ msg-len 1) 64))
         (pad-zeros (if (<= r 56) (- 56 r) (- 120 r)))
         (total-len (+ msg-len 1 pad-zeros 8))
         (padded (make-array total-len :element-type '(unsigned-byte 8) :initial-element 0)))
    (replace padded message)
    (setf (aref padded msg-len) #x80)
    ;; 64-bit big-endian bit-length at end
    (loop for i from 0 below 8
          do (setf (aref padded (- total-len 1 i))
                   (logand (ash bit-len (* -8 i)) #xFF)))
    padded))

;;; ---------- Core ----------

(defun sha256 (data)
  "Compute SHA-256 hash of DATA (octet vector or string). Returns 32-byte octet vector."
  (let* ((message (ensure-octets data))
         (padded (sha256-pad-message message))
         ;; Copy initial hash values
         (h (make-array 8 :element-type '(unsigned-byte 32)))
         (w (make-array 64 :element-type '(unsigned-byte 32) :initial-element 0)))
    ;; Initialize H from constants
    (loop for i from 0 below 8
          do (setf (aref h i) (aref +sha256-h-init+ i)))
    ;; Process each 512-bit (64-byte) block
    (loop for block-start from 0 below (length padded) by 64
          do ;; Prepare message schedule W[0..63]
             ;; W[0..15]: 32-bit words from the block (big-endian)
             (loop for t-idx from 0 below 16
                   for offset = (+ block-start (* t-idx 4))
                   do (setf (aref w t-idx)
                            (logior (ash (aref padded offset) 24)
                                    (ash (aref padded (+ offset 1)) 16)
                                    (ash (aref padded (+ offset 2)) 8)
                                    (aref padded (+ offset 3)))))
             ;; W[16..63]
             (loop for t-idx from 16 below 64
                   do (setf (aref w t-idx)
                            (mod32+ (sha256-small-sigma1 (aref w (- t-idx 2)))
                                    (aref w (- t-idx 7))
                                    (sha256-small-sigma0 (aref w (- t-idx 15)))
                                    (aref w (- t-idx 16)))))
             ;; Initialize working variables
             (let ((a (aref h 0)) (b (aref h 1)) (c (aref h 2)) (d (aref h 3))
                   (e (aref h 4)) (f (aref h 5)) (g (aref h 6)) (hh (aref h 7)))
               ;; 64 rounds
               (loop for t-idx from 0 below 64
                     for t1 = (mod32+ hh
                                      (sha256-big-sigma1 e)
                                      (sha256-ch e f g)
                                      (aref +sha256-k+ t-idx)
                                      (aref w t-idx))
                     for t2 = (mod32+ (sha256-big-sigma0 a)
                                      (sha256-maj a b c))
                     do (setf hh g
                              g f
                              f e
                              e (mod32+ d t1)
                              d c
                              c b
                              b a
                              a (mod32+ t1 t2)))
               ;; Update hash values
               (setf (aref h 0) (mod32+ (aref h 0) a)
                     (aref h 1) (mod32+ (aref h 1) b)
                     (aref h 2) (mod32+ (aref h 2) c)
                     (aref h 3) (mod32+ (aref h 3) d)
                     (aref h 4) (mod32+ (aref h 4) e)
                     (aref h 5) (mod32+ (aref h 5) f)
                     (aref h 6) (mod32+ (aref h 6) g)
                     (aref h 7) (mod32+ (aref h 7) hh))))
    ;; Produce 32-byte digest (big-endian)
    (let ((digest (make-array 32 :element-type '(unsigned-byte 8))))
      (loop for i from 0 below 8
            for val = (aref h i)
            for offset = (* i 4)
            do (setf (aref digest offset)       (logand (ash val -24) #xFF))
               (setf (aref digest (+ offset 1)) (logand (ash val -16) #xFF))
               (setf (aref digest (+ offset 2)) (logand (ash val -8)  #xFF))
               (setf (aref digest (+ offset 3)) (logand val #xFF)))
      digest)))

(defun sha256-string (string)
  "Compute SHA-256 of STRING (UTF-8 encoded). Returns 32-byte octet vector."
  (sha256 (ensure-octets string)))
