;;;; src/http/compress.lisp — Pure CL DEFLATE/gzip compression (RFC 1951/1952)
;;;; Zero-dependency compression for response bodies.
(in-package :cauldron.http)

;;; ============================================================
;;; CRC-32 (ISO 3309, used by gzip)
;;; ============================================================

(defvar *crc32-table*
  (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
    (dotimes (i 256)
      (let ((c i))
        (dotimes (k 8)
          (if (logbitp 0 c)
              (setf c (logxor (ash c -1) #xEDB88320))
              (setf c (ash c -1))))
        (setf (aref table i) (logand c #xFFFFFFFF))))
    table)
  "Pre-computed CRC-32 lookup table.")

(defun crc32 (octets)
  "Compute CRC-32 checksum of OCTETS (vector of (unsigned-byte 8)).
Returns an (unsigned-byte 32)."
  (let ((crc #xFFFFFFFF))
    (loop for byte across octets
          do (setf crc (logxor (aref *crc32-table*
                                     (logand (logxor crc byte) #xFF))
                               (ash crc -8)))
          finally (return (logand (logxor crc #xFFFFFFFF) #xFFFFFFFF)))))

;;; ============================================================
;;; DEFLATE compression (RFC 1951) — fixed Huffman codes only
;;; ============================================================

;;; This implements DEFLATE with fixed Huffman codes (BTYPE=01).
;;; No dynamic trees, no LZ77 back-references for simplicity.
;;; Each block stores literal bytes only (no length/distance pairs).
;;; Good enough for text compression; can be replaced with FFI later.

(defun deflate-compress (octets &key (level 6))
  "Compress OCTETS using DEFLATE (RFC 1951) with fixed Huffman codes.
LEVEL is accepted for API compatibility but currently ignored.
Returns a vector of (unsigned-byte 8)."
  (declare (ignore level))
  (let ((out (make-array (+ (length octets) 64) :element-type '(unsigned-byte 8)
                         :fill-pointer 0 :adjustable t))
        (pos 0)
        (len (length octets))
        (bit-buffer 0)
        (bits-in-buffer 0))
    (labels ((emit-bits (value nbits)
               "Write NBITS of VALUE (LSB first) to output."
               (setf bit-buffer (logior bit-buffer (ash value bits-in-buffer)))
               (incf bits-in-buffer nbits)
               (loop while (>= bits-in-buffer 8)
                     do (vector-push-extend (logand bit-buffer #xFF) out)
                        (setf bit-buffer (ash bit-buffer -8))
                        (decf bits-in-buffer 8)))
             (flush-bits ()
               "Flush remaining bits (pad with zeros)."
               (when (> bits-in-buffer 0)
                 (vector-push-extend (logand bit-buffer #xFF) out)
                 (setf bit-buffer 0 bits-in-buffer 0)))
             (emit-literal (byte)
               "Emit a literal byte using fixed Huffman codes."
               (cond
                 ;; 0-143: 8-bit codes 00110000 - 10111111 (reversed)
                 ((<= byte 143)
                  (let ((code (+ #x30 byte))) ; 48 + byte
                    (emit-bits (reverse-bits code 8) 8)))
                 ;; 144-255: 9-bit codes 110010000 - 111111111 (reversed)
                 (t
                  (let ((code (+ #x190 (- byte 144)))) ; 400 + (byte - 144)
                    (emit-bits (reverse-bits code 9) 9)))))
             (emit-end-of-block ()
               "Emit end-of-block marker (code 256 = 0000000 in 7 bits)."
               (emit-bits 0 7))
             (reverse-bits (value nbits)
               "Reverse the bottom NBITS of VALUE."
               (let ((result 0))
                 (dotimes (i nbits result)
                   (setf result (logior (ash result 1) (logand value 1)))
                   (setf value (ash value -1))))))
      ;; Write blocks
      (loop while (< pos len)
            for block-end = (min len (+ pos 65535))
            for is-final = (= block-end len)
            do ;; Block header: BFINAL (1 bit) + BTYPE=01 (2 bits)
               (emit-bits (if is-final 1 0) 1)  ; BFINAL
               (emit-bits 1 2)                    ; BTYPE = 01 (fixed Huffman)
               ;; Emit literal bytes
               (loop for i from pos below block-end
                     do (emit-literal (aref octets i)))
               (emit-end-of-block)
               (setf pos block-end))
      ;; Handle empty input
      (when (= len 0)
        (emit-bits 1 1)   ; BFINAL
        (emit-bits 1 2)   ; BTYPE = 01
        (emit-end-of-block))
      (flush-bits)
      (coerce out '(simple-array (unsigned-byte 8) (*))))))

;;; ============================================================
;;; gzip compression (RFC 1952)
;;; ============================================================

(defun gzip-compress (octets &key (level 6))
  "Compress OCTETS as gzip (RFC 1952).
Returns a vector of (unsigned-byte 8)."
  (let* ((deflated (deflate-compress octets :level level))
         (crc (crc32 octets))
         (isize (logand (length octets) #xFFFFFFFF))
         (out (make-array (+ 10 (length deflated) 8)
                          :element-type '(unsigned-byte 8)
                          :fill-pointer 0)))
    ;; gzip header (10 bytes)
    (vector-push-extend #x1F out)  ; ID1
    (vector-push-extend #x8B out)  ; ID2
    (vector-push-extend #x08 out)  ; CM (deflate)
    (vector-push-extend #x00 out)  ; FLG (no extras)
    (dotimes (i 4) (vector-push-extend 0 out))  ; MTIME
    (vector-push-extend 0 out)     ; XFL
    (vector-push-extend #xFF out)  ; OS (unknown)
    ;; Compressed data
    (loop for byte across deflated
          do (vector-push-extend byte out))
    ;; gzip trailer: CRC32 + ISIZE (both little-endian 32-bit)
    (dotimes (i 4) (vector-push-extend (logand (ash crc (* i -8)) #xFF) out))
    (dotimes (i 4) (vector-push-extend (logand (ash isize (* i -8)) #xFF) out))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))
