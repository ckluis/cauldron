;;;; src/http/multipart.lisp — Multipart form-data parsing
;;;; Parses multipart/form-data bodies into parts with headers, names,
;;;; filenames, content-types, and body data.

(in-package :cauldron.http)

;;; ============================================================
;;; Multipart Part Struct
;;; ============================================================

(defstruct multipart-part
  "A single part from a multipart/form-data body."
  (name     nil :type (or null string))     ; Form field name
  (filename nil :type (or null string))     ; Original filename (if file upload)
  (content-type nil :type (or null string)) ; MIME type of this part
  (headers  '() :type list)                 ; All part headers as alist
  (body     nil))                           ; Octet vector or string

;;; ============================================================
;;; Boundary Extraction
;;; ============================================================

(defun extract-boundary (content-type)
  "Extract the boundary string from a multipart/form-data Content-Type header.
Returns the boundary string or NIL if not found."
  (when (and content-type (search "multipart/form-data" content-type :test #'char-equal))
    (let ((pos (search "boundary=" content-type :test #'char-equal)))
      (when pos
        (let* ((start (+ pos 9))
               (value (subseq content-type start)))
          ;; Remove quotes if present
          (when (and (> (length value) 0) (char= (char value 0) #\"))
            (setf value (subseq value 1))
            (let ((end-quote (position #\" value)))
              (when end-quote
                (setf value (subseq value 0 end-quote)))))
          ;; Trim trailing whitespace/semicolons
          (setf value (string-trim '(#\Space #\Tab #\; #\Return #\Newline) value))
          (when (> (length value) 0)
            value))))))

;;; ============================================================
;;; Multipart Parsing
;;; ============================================================

(defun parse-multipart (body boundary)
  "Parse a multipart body (octet vector or string) using BOUNDARY.
Returns a list of multipart-part structs."
  (let* ((body-octets (ensure-body-octets body))
         (delimiter (string-to-octets (format nil "--~A" boundary)))
         (final-delimiter (string-to-octets (format nil "--~A--" boundary)))
         (crlf (string-to-octets (format nil "~C~C" #\Return #\Newline)))
         (parts '())
         (pos 0))
    ;; Skip preamble: find first delimiter
    (let ((first-delim (find-subsequence delimiter body-octets pos)))
      (unless first-delim
        (return-from parse-multipart nil))
      ;; Move past delimiter + CRLF
      (setf pos (+ first-delim (length delimiter)))
      (setf pos (skip-crlf body-octets pos)))
    ;; Parse each part
    (loop
      ;; Check if we're at the final delimiter
      (when (>= pos (length body-octets))
        (return))
      (when (at-final-delimiter-p body-octets pos final-delimiter)
        (return))
      ;; Parse part headers
      (multiple-value-bind (headers header-end)
          (parse-part-headers body-octets pos)
        (setf pos header-end)
        ;; Find next delimiter to determine body extent
        (let ((next-delim (find-subsequence delimiter body-octets pos)))
          (unless next-delim
            (return))
          ;; Body is everything between header-end and next delimiter
          ;; Minus trailing CRLF before delimiter
          (let* ((body-end (- next-delim 2)) ; strip CRLF before delimiter
                 (body-end (max body-end pos))
                 (part-body (subseq body-octets pos body-end)))
            ;; Extract name, filename, content-type from headers
            (let* ((disposition (cdr (assoc "Content-Disposition" headers :test #'string-equal)))
                   (name (extract-header-param disposition "name"))
                   (filename (extract-header-param disposition "filename"))
                   (ct (cdr (assoc "Content-Type" headers :test #'string-equal))))
              (push (make-multipart-part
                     :name name
                     :filename filename
                     :content-type ct
                     :headers headers
                     :body (if filename
                                part-body
                                (octets-to-string-safe part-body)))
                    parts)))
          ;; Move past delimiter + CRLF
          (setf pos (+ next-delim (length delimiter)))
          (setf pos (skip-crlf body-octets pos)))))
    (nreverse parts)))

;;; ============================================================
;;; Internal Helpers
;;; ============================================================

(defun ensure-body-octets (body)
  "Convert body to octet vector if it's a string."
  (etypecase body
    ((vector (unsigned-byte 8)) body)
    (string (string-to-octets body))))

(defun string-to-octets (string)
  "Convert string to UTF-8 octet vector."
  (let* ((len (length string))
         (octets (make-array len :element-type '(unsigned-byte 8))))
    (dotimes (i len octets)
      (setf (aref octets i) (char-code (char string i))))))

(defun octets-to-string-safe (octets)
  "Convert octet vector to string, assuming ASCII/UTF-8."
  (let* ((len (length octets))
         (str (make-string len)))
    (dotimes (i len str)
      (setf (char str i) (code-char (aref octets i))))))

(defun find-subsequence (needle haystack &optional (start 0))
  "Find the first occurrence of NEEDLE (octet vector) in HAYSTACK starting at START.
Returns the index or NIL."
  (let ((nlen (length needle))
        (hlen (length haystack)))
    (when (<= nlen (- hlen start))
      (loop for i from start to (- hlen nlen)
            when (loop for j from 0 below nlen
                       always (= (aref haystack (+ i j)) (aref needle j)))
              return i))))

(defun skip-crlf (octets pos)
  "Skip CRLF at POS in OCTETS. Returns new position."
  (cond
    ((and (<= (+ pos 2) (length octets))
          (= (aref octets pos) 13)      ; CR
          (= (aref octets (1+ pos)) 10)) ; LF
     (+ pos 2))
    ((and (<= (+ pos 1) (length octets))
          (= (aref octets pos) 10))     ; Just LF
     (1+ pos))
    (t pos)))

(defun at-final-delimiter-p (octets pos final-delimiter)
  "Check if OCTETS at POS starts with FINAL-DELIMITER."
  (let ((flen (length final-delimiter)))
    (and (<= (+ pos flen) (length octets))
         (loop for i from 0 below flen
               always (= (aref octets (+ pos i)) (aref final-delimiter i))))))

(defun parse-part-headers (octets pos)
  "Parse MIME headers from OCTETS starting at POS.
Returns (values headers-alist new-pos) where new-pos is after the blank line."
  (let ((headers '()))
    (loop
      ;; Check for empty line (end of headers)
      (when (>= pos (length octets))
        (return))
      (when (and (<= (+ pos 2) (length octets))
                 (= (aref octets pos) 13)
                 (= (aref octets (1+ pos)) 10))
        (incf pos 2)
        (return))
      (when (and (<= (+ pos 1) (length octets))
                 (= (aref octets pos) 10))
        (incf pos)
        (return))
      ;; Read header line
      (let ((line-end (or (find-subsequence (string-to-octets (format nil "~C~C" #\Return #\Newline))
                                            octets pos)
                          (position 10 octets :start pos)
                          (length octets))))
        (let* ((line-octets (subseq octets pos line-end))
               (line (octets-to-string-safe line-octets))
               (colon (position #\: line)))
          (when colon
            (push (cons (string-trim '(#\Space #\Tab) (subseq line 0 colon))
                        (string-trim '(#\Space #\Tab) (subseq line (1+ colon))))
                  headers)))
        ;; Move past line + CRLF
        (setf pos (skip-crlf octets
                             (if (and (< line-end (length octets))
                                      (= (aref octets line-end) 13))
                                 line-end
                                 line-end)))))
    (values (nreverse headers) pos)))

(defun extract-header-param (header param-name)
  "Extract a parameter value from a header like Content-Disposition.
E.g., extract \"name\" from 'form-data; name=\"field1\"'."
  (when header
    (let* ((search-str (format nil "~A=" param-name))
           (pos (search search-str header :test #'char-equal)))
      (when pos
        (let* ((val-start (+ pos (length search-str)))
               (quoted (and (< val-start (length header))
                            (char= (char header val-start) #\"))))
          (if quoted
              ;; Quoted value
              (let ((end (position #\" header :start (1+ val-start))))
                (when end
                  (subseq header (1+ val-start) end)))
              ;; Unquoted value — ends at ; or end
              (let ((end (or (position #\; header :start val-start)
                             (length header))))
                (string-trim '(#\Space #\Tab) (subseq header val-start end)))))))))
