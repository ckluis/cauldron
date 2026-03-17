;;;; src/json/encoder.lisp — JSON encoder
;;;; Zero external dependencies. Targets SBCL.
(in-package :cauldron.json)

;;; --------------------------------------------------------------------------
;;; Conditions
;;; --------------------------------------------------------------------------

(define-condition json-encode-error (simple-error)
  ((value :initarg :value :reader json-encode-error-value))
  (:report (lambda (c stream)
             (format stream "JSON encode error for value ~S: ~A"
                     (json-encode-error-value c)
                     (simple-condition-format-control c)))))

;;; --------------------------------------------------------------------------
;;; String escaping
;;; --------------------------------------------------------------------------

(defun write-escaped-string (string stream)
  "Write STRING to STREAM as a JSON-escaped, double-quoted string."
  (write-char #\" stream)
  (loop for char across string
        for code = (char-code char)
        do (cond
             ((char= char #\") (write-string "\\\"" stream))
             ((char= char #\\) (write-string "\\\\" stream))
             ((char= char #\/) (write-string "\\/" stream))
             ((char= char #\Backspace) (write-string "\\b" stream))
             ((char= char #\Page) (write-string "\\f" stream))
             ((char= char #\Newline) (write-string "\\n" stream))
             ((char= char #\Return) (write-string "\\r" stream))
             ((char= char #\Tab) (write-string "\\t" stream))
             ((< code #x20)
              ;; Other control characters → \uXXXX
              (format stream "\\u~4,'0X" code))
             ((> code #xFFFF)
              ;; Above BMP → surrogate pair
              (let* ((adjusted (- code #x10000))
                     (high (+ #xD800 (ash adjusted -10)))
                     (low  (+ #xDC00 (logand adjusted #x3FF))))
                (format stream "\\u~4,'0X\\u~4,'0X" high low)))
             (t
              (write-char char stream))))
  (write-char #\" stream))

;;; --------------------------------------------------------------------------
;;; Plist detection
;;; --------------------------------------------------------------------------

(defun plist-p (list)
  "Return T if LIST looks like a property list (keyword keys at even positions).
   An empty list is NOT a plist — it encodes as an empty array."
  (and (consp list)
       (keywordp (car list))
       (evenp (length list))
       (loop for (k v) on list by #'cddr
             always (keywordp k))))

;;; --------------------------------------------------------------------------
;;; Core encoder — writes to stream
;;; --------------------------------------------------------------------------

(defun %encode (value stream)
  "Encode VALUE as JSON and write to STREAM."
  (cond
    ;; Explicit markers first
    ((eq value :null)
     (write-string "null" stream))
    ((eq value :false)
     (write-string "false" stream))
    ((eq value t)
     (write-string "true" stream))

    ;; nil → null (NOT false)
    ((null value)
     (write-string "null" stream))

    ;; Integer
    ((integerp value)
     (princ value stream))

    ;; Float
    ((floatp value)
     ;; Reject NaN and Infinity — not valid JSON
     (when (or (sb-ext:float-nan-p value)
               (sb-ext:float-infinity-p value))
       (error 'json-encode-error
              :value value
              :format-control "NaN and Infinity are not valid JSON numbers"))
     (write-string (write-json-float value) stream))

    ;; String
    ((stringp value)
     (write-escaped-string value stream))

    ;; Vector (but not string — caught above)
    ((vectorp value)
     (write-char #\[ stream)
     (loop for i from 0 below (length value)
           when (> i 0) do (write-char #\, stream)
           do (%encode (aref value i) stream))
     (write-char #\] stream))

    ;; Hash-table → JSON object
    ((hash-table-p value)
     (write-char #\{ stream)
     ;; Collect and sort keys for deterministic output
     (let ((keys (sort (loop for k being the hash-keys of value collect k)
                       #'string<)))
       (loop for first = t then nil
             for key in keys
             do (progn
                  (unless (stringp key)
                    (error 'json-encode-error
                           :value key
                           :format-control "Hash-table keys must be strings for JSON encoding"))
                  (unless first (write-char #\, stream))
                  (write-escaped-string key stream)
                  (write-char #\: stream)
                  (%encode (gethash key value) stream))))
     (write-char #\} stream))

    ;; List
    ((listp value)
     (cond
       ;; :json-array marker for disambiguation
       ((eq (car value) :json-array)
        (write-char #\[ stream)
        (loop for rest on (cdr value)
              for first = t then nil
              do (progn
                   (unless first (write-char #\, stream))
                   (%encode (car rest) stream)))
        (write-char #\] stream))

       ;; Plist → JSON object
       ((plist-p value)
        (write-char #\{ stream)
        (loop for (k v) on value by #'cddr
              for first = t then nil
              do (progn
                   (unless first (write-char #\, stream))
                   (write-escaped-string (string-downcase (symbol-name k)) stream)
                   (write-char #\: stream)
                   (%encode v stream)))
        (write-char #\} stream))

       ;; Regular list → JSON array
       (t
        (write-char #\[ stream)
        (loop for rest on value
              for first = t then nil
              do (progn
                   (unless first (write-char #\, stream))
                   (%encode (car rest) stream)))
        (write-char #\] stream))))

    ;; Symbol → lowercase string
    ((symbolp value)
     (write-escaped-string (string-downcase (symbol-name value)) stream))

    ;; Anything else is an error
    (t
     (error 'json-encode-error
            :value value
            :format-control "Cannot encode value of type ~A to JSON"))))

;;; --------------------------------------------------------------------------
;;; Float formatting
;;; --------------------------------------------------------------------------

(defun write-json-float (value)
  "Format a float VALUE as a JSON number string.
   Uses CL's printer with sufficient precision, then cleans up to valid JSON format."
  (let* ((dval (coerce value 'double-float))
         (*read-default-float-format* 'double-float)
         ;; Use write-to-string which gives us a clean representation.
         ;; SBCL prints double-floats like: 1.5d0, 1.0d-7, 3.14159d0
         (raw (let ((*print-readably* nil))
                (write-to-string dval))))
    ;; SBCL format: [-]digits.digits[d[+-]exp]
    ;; We need to convert 'd' exponent marker to 'e' for JSON
    ;; and handle the case where there's no exponent
    (clean-lisp-float raw)))

(defun clean-lisp-float (raw)
  "Convert an SBCL double-float printed representation to valid JSON.
   Input examples: '1.5d0', '1.0d-7', '-3.14d+2', '0.0d0'."
  ;; Strip any leading/trailing whitespace
  (let ((str (string-trim '(#\Space #\Tab) raw)))
    ;; Find the 'd' or 'D' exponent marker
    (let ((d-pos (or (position #\d str) (position #\D str))))
      (if d-pos
          ;; Has exponent
          (let* ((mantissa (subseq str 0 d-pos))
                 (exp-str (subseq str (1+ d-pos)))
                 (exponent (parse-integer exp-str)))
            (if (zerop exponent)
                ;; d0 means no exponent needed — just the mantissa
                (clean-mantissa mantissa)
                ;; Non-zero exponent: shift decimal point or use 'e' notation
                (reformat-with-exponent mantissa exponent)))
          ;; No exponent marker — might be like "1.5" already
          ;; Ensure it has a decimal point
          (if (position #\. str)
              (clean-mantissa str)
              (concatenate 'string str ".0"))))))

(defun clean-mantissa (mantissa)
  "Ensure mantissa has a decimal point and trim trailing zeros (keeping at least x.0)."
  (let ((dot-pos (position #\. mantissa)))
    (if (null dot-pos)
        (concatenate 'string mantissa ".0")
        (let* ((int-part (subseq mantissa 0 dot-pos))
               (frac-part (subseq mantissa (1+ dot-pos)))
               (trimmed-frac (string-right-trim "0" frac-part)))
          (when (string= trimmed-frac "")
            (setf trimmed-frac "0"))
          (concatenate 'string int-part "." trimmed-frac)))))

(defun reformat-with-exponent (mantissa exponent)
  "Reformat a mantissa string + integer exponent into valid JSON number format.
   Tries to produce a non-exponential form when practical, otherwise uses 'e' notation."
  (let* ((neg (and (> (length mantissa) 0) (char= (char mantissa 0) #\-)))
         (abs-mantissa (if neg (subseq mantissa 1) mantissa))
         (dot-pos (position #\. abs-mantissa)))
    ;; Extract all significant digits from mantissa
    (let* ((int-part (if dot-pos (subseq abs-mantissa 0 dot-pos) abs-mantissa))
           (frac-part (if dot-pos (subseq abs-mantissa (1+ dot-pos)) ""))
           ;; Remove trailing zeros from frac
           (trimmed-frac (string-right-trim "0" frac-part))
           (all-digits (concatenate 'string int-part trimmed-frac))
           ;; decimal-pos = number of digits before decimal in final number
           ;; Original: int-part digits before decimal, so decimal shifts by exponent
           (decimal-pos (+ (length int-part) exponent)))
      (when (string= all-digits "") (setf all-digits "0"))
      (let ((result
              (cond
                ;; Decimal falls within the digit string
                ((and (>= decimal-pos 1)
                      (<= decimal-pos (length all-digits))
                      (<= decimal-pos 20))
                 (let ((before (subseq all-digits 0 decimal-pos))
                       (after (string-right-trim "0" (subseq all-digits decimal-pos))))
                   (when (string= after "") (setf after "0"))
                   (concatenate 'string before "." after)))

                ;; Decimal is to the right — integer-like value
                ((and (> decimal-pos (length all-digits))
                      (<= decimal-pos 20))
                 (concatenate 'string
                              all-digits
                              (make-string (- decimal-pos (length all-digits))
                                           :initial-element #\0)
                              ".0"))

                ;; Decimal is to the left — small number 0.00...
                ((and (< decimal-pos 1) (>= decimal-pos -6))
                 (let ((leading-zeros (- decimal-pos))
                       (trimmed (string-right-trim "0" all-digits)))
                   (when (string= trimmed "") (setf trimmed "0"))
                   (concatenate 'string "0."
                                (make-string leading-zeros :initial-element #\0)
                                trimmed)))

                ;; Use exponential notation
                (t
                 (let* ((first (subseq all-digits 0 1))
                        (rest (string-right-trim "0" (subseq all-digits 1)))
                        (e (1- decimal-pos)))
                   (when (string= rest "") (setf rest "0"))
                   (if (>= e 0)
                       (format nil "~A.~Ae+~D" first rest e)
                       (format nil "~A.~Ae~D" first rest e)))))))
        (if neg
            (concatenate 'string "-" result)
            result)))))

;;; --------------------------------------------------------------------------
;;; Public API
;;; --------------------------------------------------------------------------

(defun encode (value)
  "Encode VALUE to a JSON string."
  (with-output-to-string (stream)
    (%encode value stream)))

(defun encode-to-stream (value stream)
  "Encode VALUE as JSON and write directly to STREAM. Zero-copy for HTTP responses."
  (%encode value stream))
