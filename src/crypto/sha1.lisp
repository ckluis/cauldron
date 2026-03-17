;;;; src/crypto/sha1.lisp — SHA-1 hash implementation (FIPS 180-4)
;;;; Used ONLY for WebSocket handshake (RFC 6455), NOT for security.
(in-package :cauldron.crypto)

(defun sha1-pad-message (message)
  "Pad MESSAGE (octet vector) per SHA-1 spec: append bit 1, zeros, 64-bit BE length."
  (let* ((msg-len (length message))
         (bit-len (* msg-len 8))
         ;; Pad to 64-byte boundary, with room for 1+zeros+8 bytes
         (pad-len (let ((r (mod (+ msg-len 1) 64)))
                    (if (<= r 56)
                        (- 56 r)
                        (- 120 r))))
         (total-len (+ msg-len 1 pad-len 8))
         (padded (make-array total-len :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; Copy message
    (replace padded message)
    ;; Append bit 1 (0x80)
    (setf (aref padded msg-len) #x80)
    ;; Append 64-bit big-endian length at the end
    (loop for i from 0 below 8
          do (setf (aref padded (- total-len 1 i))
                   (logand (ash bit-len (* -8 i)) #xFF)))
    padded))

(defun sha1-left-rotate (x n)
  "Left-rotate 32-bit integer X by N bits."
  (logand #xFFFFFFFF
          (logior (ash x n)
                  (ash x (- n 32)))))

(defun sha1 (data)
  "Compute SHA-1 hash of DATA (octet vector or string). Returns 20-byte octet vector."
  (let* ((message (ensure-octets data))
         (padded (sha1-pad-message message))
         ;; Initial hash values
         (h0 #x67452301)
         (h1 #xEFCDAB89)
         (h2 #x98BADCFE)
         (h3 #x10325476)
         (h4 #xC3D2E1F0)
         (w (make-array 80 :element-type '(unsigned-byte 32) :initial-element 0)))
    ;; Process each 512-bit (64-byte) block
    (loop for block-start from 0 below (length padded) by 64
          do ;; Prepare message schedule
             (loop for t-idx from 0 below 16
                   for offset = (+ block-start (* t-idx 4))
                   do (setf (aref w t-idx)
                            (logior (ash (aref padded offset) 24)
                                    (ash (aref padded (+ offset 1)) 16)
                                    (ash (aref padded (+ offset 2)) 8)
                                    (aref padded (+ offset 3)))))
             (loop for t-idx from 16 below 80
                   do (setf (aref w t-idx)
                            (sha1-left-rotate
                             (logxor (aref w (- t-idx 3))
                                     (aref w (- t-idx 8))
                                     (aref w (- t-idx 14))
                                     (aref w (- t-idx 16)))
                             1)))
             ;; Initialize working variables
             (let ((a h0) (b h1) (c h2) (d h3) (e h4))
               ;; 80 rounds
               (loop for t-idx from 0 below 80
                     for f = (cond
                               ((< t-idx 20)
                                (logior (logand b c)
                                        (logand (logand #xFFFFFFFF (lognot b)) d)))
                               ((< t-idx 40)
                                (logxor b c d))
                               ((< t-idx 60)
                                (logior (logand b c) (logand b d) (logand c d)))
                               (t
                                (logxor b c d)))
                     for k = (cond
                               ((< t-idx 20) #x5A827999)
                               ((< t-idx 40) #x6ED9EBA1)
                               ((< t-idx 60) #x8F1BBCDC)
                               (t            #xCA62C1D6))
                     for temp = (logand #xFFFFFFFF
                                        (+ (sha1-left-rotate a 5)
                                           f e k (aref w t-idx)))
                     do (setf e d
                              d c
                              c (sha1-left-rotate b 30)
                              b a
                              a temp))
               ;; Update hash values
               (setf h0 (logand #xFFFFFFFF (+ h0 a))
                     h1 (logand #xFFFFFFFF (+ h1 b))
                     h2 (logand #xFFFFFFFF (+ h2 c))
                     h3 (logand #xFFFFFFFF (+ h3 d))
                     h4 (logand #xFFFFFFFF (+ h4 e)))))
    ;; Produce 20-byte digest
    (let ((digest (make-array 20 :element-type '(unsigned-byte 8))))
      (loop for h in (list h0 h1 h2 h3 h4)
            for offset from 0 by 4
            do (setf (aref digest offset)       (logand (ash h -24) #xFF))
               (setf (aref digest (+ offset 1)) (logand (ash h -16) #xFF))
               (setf (aref digest (+ offset 2)) (logand (ash h -8)  #xFF))
               (setf (aref digest (+ offset 3)) (logand h #xFF)))
      digest)))
