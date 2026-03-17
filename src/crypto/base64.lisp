;;;; src/crypto/base64.lisp — Base64 and Base64URL encoding/decoding (RFC 4648)
(in-package :cauldron.crypto)

;;; ---------- Standard Base64 Alphabet ----------

(defvar +base64-encode-table+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defvar *base64-decode-table*
  (let ((table (make-array 256 :element-type '(signed-byte 8) :initial-element -1)))
    (loop for i from 0 below 64
          do (setf (aref table (char-code (char +base64-encode-table+ i))) i))
    table))

;;; ---------- Standard Base64 ----------

(defun ensure-octets (data)
  "Convert DATA to an octet vector. If DATA is a string, UTF-8 encode it."
  (etypecase data
    ((simple-array (unsigned-byte 8) (*)) data)
    ((vector (unsigned-byte 8)) (coerce data '(simple-array (unsigned-byte 8) (*))))
    (string (let* ((str data)
                   (len (length str))
                   ;; First pass: compute byte count
                   (byte-count (loop for i below len
                                     for code = (char-code (char str i))
                                     sum (cond ((<= code #x7F) 1)
                                               ((<= code #x7FF) 2)
                                               ((<= code #xFFFF) 3)
                                               (t 4)))))
              (let ((result (make-array byte-count :element-type '(unsigned-byte 8)))
                    (pos 0))
                (loop for i below len
                      for code = (char-code (char str i))
                      do (cond
                           ((<= code #x7F)
                            (setf (aref result pos) code)
                            (incf pos))
                           ((<= code #x7FF)
                            (setf (aref result pos) (logior #xC0 (ash code -6)))
                            (setf (aref result (+ pos 1)) (logior #x80 (logand code #x3F)))
                            (incf pos 2))
                           ((<= code #xFFFF)
                            (setf (aref result pos) (logior #xE0 (ash code -12)))
                            (setf (aref result (+ pos 1)) (logior #x80 (logand (ash code -6) #x3F)))
                            (setf (aref result (+ pos 2)) (logior #x80 (logand code #x3F)))
                            (incf pos 3))
                           (t
                            (setf (aref result pos) (logior #xF0 (ash code -18)))
                            (setf (aref result (+ pos 1)) (logior #x80 (logand (ash code -12) #x3F)))
                            (setf (aref result (+ pos 2)) (logior #x80 (logand (ash code -6) #x3F)))
                            (setf (aref result (+ pos 3)) (logior #x80 (logand code #x3F)))
                            (incf pos 4))))
                result)))))

(defun base64-encode (data)
  "Encode DATA (octet vector or string) to base64 string with padding."
  (let* ((octets (ensure-octets data))
         (len (length octets))
         (out-len (* 4 (ceiling len 3)))
         (result (make-string out-len))
         (pos 0))
    (loop for i from 0 below len by 3
          for b0 = (aref octets i)
          for b1 = (if (< (+ i 1) len) (aref octets (+ i 1)) 0)
          for b2 = (if (< (+ i 2) len) (aref octets (+ i 2)) 0)
          for triplet = (logior (ash b0 16) (ash b1 8) b2)
          for remaining = (- len i)
          do (setf (char result pos)
                   (char +base64-encode-table+ (logand (ash triplet -18) #x3F)))
             (setf (char result (+ pos 1))
                   (char +base64-encode-table+ (logand (ash triplet -12) #x3F)))
             (setf (char result (+ pos 2))
                   (if (>= remaining 2)
                       (char +base64-encode-table+ (logand (ash triplet -6) #x3F))
                       #\=))
             (setf (char result (+ pos 3))
                   (if (>= remaining 3)
                       (char +base64-encode-table+ (logand triplet #x3F))
                       #\=))
             (incf pos 4))
    result))

(defun strip-whitespace (string)
  "Remove whitespace characters from STRING."
  (remove-if (lambda (c) (or (char= c #\Space) (char= c #\Tab)
                              (char= c #\Newline) (char= c #\Return)))
             string))

(defun base64-decode (string)
  "Decode a base64 STRING to an octet vector. Handles padding and whitespace."
  (let* ((clean (strip-whitespace string))
         (len (length clean))
         ;; Count padding
         (pad (cond ((and (> len 0) (char= (char clean (- len 1)) #\=)
                         (> len 1) (char= (char clean (- len 2)) #\=)) 2)
                    ((and (> len 0) (char= (char clean (- len 1)) #\=)) 1)
                    (t 0)))
         (out-len (- (* 3 (floor len 4)) pad))
         (result (make-array (max 0 out-len) :element-type '(unsigned-byte 8)))
         (pos 0))
    (when (> len 0)
      (loop for i from 0 below len by 4
            for c0 = (aref *base64-decode-table* (char-code (char clean i)))
            for c1 = (if (< (+ i 1) len)
                         (aref *base64-decode-table* (char-code (char clean (+ i 1)))) 0)
            for c2 = (if (and (< (+ i 2) len) (char/= (char clean (+ i 2)) #\=))
                         (aref *base64-decode-table* (char-code (char clean (+ i 2)))) 0)
            for c3 = (if (and (< (+ i 3) len) (char/= (char clean (+ i 3)) #\=))
                         (aref *base64-decode-table* (char-code (char clean (+ i 3)))) 0)
            for triplet = (logior (ash c0 18) (ash c1 12) (ash c2 6) c3)
            do (when (< pos out-len)
                 (setf (aref result pos) (logand (ash triplet -16) #xFF))
                 (incf pos))
               (when (< pos out-len)
                 (setf (aref result pos) (logand (ash triplet -8) #xFF))
                 (incf pos))
               (when (< pos out-len)
                 (setf (aref result pos) (logand triplet #xFF))
                 (incf pos))))
    result))

;;; ---------- Base64URL ----------

(defvar +base64url-encode-table+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")

(defvar *base64url-decode-table*
  (let ((table (make-array 256 :element-type '(signed-byte 8) :initial-element -1)))
    (loop for i from 0 below 64
          do (setf (aref table (char-code (char +base64url-encode-table+ i))) i))
    table))

(defun base64url-encode (data)
  "Encode DATA to URL-safe base64 string without padding."
  (let* ((octets (ensure-octets data))
         (len (length octets))
         ;; Compute output length without padding
         (full-groups (floor len 3))
         (remaining (mod len 3))
         (out-len (+ (* full-groups 4)
                     (case remaining (0 0) (1 2) (2 3))))
         (result (make-string out-len))
         (pos 0))
    (loop for i from 0 below len by 3
          for b0 = (aref octets i)
          for b1 = (if (< (+ i 1) len) (aref octets (+ i 1)) 0)
          for b2 = (if (< (+ i 2) len) (aref octets (+ i 2)) 0)
          for triplet = (logior (ash b0 16) (ash b1 8) b2)
          for rem = (- len i)
          do ;; Always emit first 2 chars
             (setf (char result pos)
                   (char +base64url-encode-table+ (logand (ash triplet -18) #x3F)))
             (incf pos)
             (setf (char result pos)
                   (char +base64url-encode-table+ (logand (ash triplet -12) #x3F)))
             (incf pos)
             ;; Third char if >= 2 bytes remain
             (when (>= rem 2)
               (setf (char result pos)
                     (char +base64url-encode-table+ (logand (ash triplet -6) #x3F)))
               (incf pos))
             ;; Fourth char if >= 3 bytes remain
             (when (>= rem 3)
               (setf (char result pos)
                     (char +base64url-encode-table+ (logand triplet #x3F)))
               (incf pos)))
    result))

(defun base64url-decode (string)
  "Decode a URL-safe base64 STRING to an octet vector. Handles missing padding."
  (let* ((clean (strip-whitespace string))
         (len (length clean))
         ;; Add padding if needed
         (pad-needed (mod (- 4 (mod len 4)) 4))
         (padded (if (and (> pad-needed 0) (< pad-needed 4))
                     (concatenate 'string clean (make-string pad-needed :initial-element #\=))
                     clean))
         (padded-len (length padded))
         ;; Count effective padding for output size
         (pad (cond ((and (> padded-len 0) (char= (char padded (- padded-len 1)) #\=)
                         (> padded-len 1) (char= (char padded (- padded-len 2)) #\=)) 2)
                    ((and (> padded-len 0) (char= (char padded (- padded-len 1)) #\=)) 1)
                    (t 0)))
         (out-len (- (* 3 (floor padded-len 4)) pad))
         (result (make-array (max 0 out-len) :element-type '(unsigned-byte 8)))
         (pos 0))
    (when (> padded-len 0)
      (loop for i from 0 below padded-len by 4
            for c0 = (aref *base64url-decode-table* (char-code (char padded i)))
            for c1 = (if (< (+ i 1) padded-len)
                         (aref *base64url-decode-table* (char-code (char padded (+ i 1)))) 0)
            for c2 = (if (and (< (+ i 2) padded-len) (char/= (char padded (+ i 2)) #\=))
                         (aref *base64url-decode-table* (char-code (char padded (+ i 2)))) 0)
            for c3 = (if (and (< (+ i 3) padded-len) (char/= (char padded (+ i 3)) #\=))
                         (aref *base64url-decode-table* (char-code (char padded (+ i 3)))) 0)
            for triplet = (logior (ash c0 18) (ash c1 12) (ash c2 6) c3)
            do (when (< pos out-len)
                 (setf (aref result pos) (logand (ash triplet -16) #xFF))
                 (incf pos))
               (when (< pos out-len)
                 (setf (aref result pos) (logand (ash triplet -8) #xFF))
                 (incf pos))
               (when (< pos out-len)
                 (setf (aref result pos) (logand triplet #xFF))
                 (incf pos))))
    result))
