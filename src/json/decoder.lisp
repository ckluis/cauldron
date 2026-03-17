;;;; src/json/decoder.lisp — JSON decoder (recursive descent parser)
;;;; Zero external dependencies. Targets SBCL.
(in-package :cauldron.json)

;;; --------------------------------------------------------------------------
;;; Conditions
;;; --------------------------------------------------------------------------

(define-condition json-parse-error (simple-error)
  ((position :initarg :position :reader json-parse-error-position
             :initform nil))
  (:report (lambda (c stream)
             (format stream "JSON parse error~@[ at position ~D~]: ~?"
                     (json-parse-error-position c)
                     (simple-condition-format-control c)
                     (simple-condition-format-arguments c)))))

(defun json-error (position format-control &rest format-arguments)
  "Signal a json-parse-error at POSITION."
  (error 'json-parse-error
         :position position
         :format-control format-control
         :format-arguments format-arguments))

;;; --------------------------------------------------------------------------
;;; Constants
;;; --------------------------------------------------------------------------

(defconstant +max-nesting-depth+ 1000
  "Maximum nesting depth for arrays and objects to prevent stack overflow.")

;;; --------------------------------------------------------------------------
;;; Whitespace handling
;;; --------------------------------------------------------------------------

(declaim (inline whitespace-p))
(defun whitespace-p (char)
  "Return T if CHAR is JSON whitespace (space, tab, newline, carriage return)."
  (or (char= char #\Space)
      (char= char #\Tab)
      (char= char #\Newline)
      (char= char #\Return)))

(declaim (inline skip-whitespace))
(defun skip-whitespace (string index end)
  "Skip whitespace characters starting at INDEX. Return new index."
  (declare (type simple-string string)
           (type fixnum index end))
  (loop while (and (< index end) (whitespace-p (char string index)))
        do (incf index))
  index)

;;; --------------------------------------------------------------------------
;;; Peek / advance helpers
;;; --------------------------------------------------------------------------

(declaim (inline peek-char*))
(defun peek-char* (string index end)
  "Return the character at INDEX or NIL if at end."
  (declare (type simple-string string)
           (type fixnum index end))
  (when (< index end)
    (char string index)))

(declaim (inline expect-char))
(defun expect-char (string index end expected)
  "Assert that the character at INDEX equals EXPECTED, then return index+1."
  (declare (type simple-string string)
           (type fixnum index end))
  (if (and (< index end) (char= (char string index) expected))
      (1+ index)
      (json-error index "Expected '~C' but ~:[reached end of input~;found '~C'~]"
                  expected (< index end)
                  (when (< index end) (char string index)))))

;;; --------------------------------------------------------------------------
;;; String parsing
;;; --------------------------------------------------------------------------

(defun parse-hex4 (string index end)
  "Parse exactly 4 hex digits starting at INDEX. Return (values code-point new-index)."
  (when (> (+ index 4) end)
    (json-error index "Incomplete \\u escape sequence"))
  (let ((result 0))
    (loop for i from index below (+ index 4)
          for ch = (char string i)
          for digit = (digit-char-p ch 16)
          do (if digit
                 (setf result (+ (ash result 4) digit))
                 (json-error i "Invalid hex digit '~C' in \\u escape" ch)))
    (values result (+ index 4))))

(defun parse-string (string index end)
  "Parse a JSON string starting at INDEX (must point to opening quote).
   Returns (values parsed-string new-index)."
  (declare (type simple-string string)
           (type fixnum index end))
  (setf index (expect-char string index end #\"))
  (let ((buf (make-array 64 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop
      (when (>= index end)
        (json-error index "Unterminated string"))
      (let ((ch (char string index)))
        (cond
          ;; End of string
          ((char= ch #\")
           (return (values (copy-seq buf) (1+ index))))

          ;; Unescaped control characters are invalid in JSON strings
          ((< (char-code ch) #x20)
           (json-error index "Unescaped control character U+~4,'0X in string" (char-code ch)))

          ;; Escape sequence
          ((char= ch #\\)
           (incf index)
           (when (>= index end)
             (json-error index "Unterminated escape sequence in string"))
           (let ((esc (char string index)))
             (incf index)
             (case esc
               (#\" (vector-push-extend #\" buf))
               (#\\ (vector-push-extend #\\ buf))
               (#\/ (vector-push-extend #\/ buf))
               (#\b (vector-push-extend #\Backspace buf))
               (#\f (vector-push-extend #\Page buf))
               (#\n (vector-push-extend #\Newline buf))
               (#\r (vector-push-extend #\Return buf))
               (#\t (vector-push-extend #\Tab buf))
               (#\u
                ;; Parse \uXXXX, handle surrogate pairs
                (multiple-value-bind (code new-idx) (parse-hex4 string index end)
                  (setf index new-idx)
                  (cond
                    ;; High surrogate — must be followed by \uDC00-\uDFFF
                    ((<= #xD800 code #xDBFF)
                     (unless (and (<= (+ index 1) end)
                                  (< index end)
                                  (char= (char string index) #\\)
                                  (< (1+ index) end)
                                  (char= (char string (1+ index)) #\u))
                       (json-error index "High surrogate U+~4,'0X not followed by low surrogate" code))
                     (incf index 2) ; skip \u
                     (multiple-value-bind (low new-idx2) (parse-hex4 string index end)
                       (setf index new-idx2)
                       (unless (<= #xDC00 low #xDFFF)
                         (json-error (- index 6)
                                     "High surrogate U+~4,'0X followed by non-low-surrogate U+~4,'0X"
                                     code low))
                       ;; Decode surrogate pair
                       (let ((codepoint (+ #x10000
                                           (ash (- code #xD800) 10)
                                           (- low #xDC00))))
                         (vector-push-extend (code-char codepoint) buf))))

                    ;; Lone low surrogate is invalid
                    ((<= #xDC00 code #xDFFF)
                     (json-error (- index 4) "Lone low surrogate U+~4,'0X" code))

                    ;; Normal BMP character
                    (t
                     (vector-push-extend (code-char code) buf)))))
               (t
                (json-error (1- index) "Invalid escape character '~C'" esc)))))

          ;; Normal character
          (t
           (vector-push-extend ch buf)
           (incf index)))))))

;;; --------------------------------------------------------------------------
;;; Number parsing
;;; --------------------------------------------------------------------------

(defun parse-number (string index end)
  "Parse a JSON number starting at INDEX.
   Returns (values number new-index)."
  (declare (type simple-string string)
           (type fixnum index end))
  (let ((start index)
        (is-float nil))

    ;; Optional leading minus
    (when (and (< index end) (char= (char string index) #\-))
      (incf index))

    ;; Integer part
    (when (or (>= index end)
              (not (digit-char-p (char string index))))
      (json-error start "Invalid number"))

    ;; Reject leading zeros (except 0 itself)
    (when (char= (char string index) #\0)
      (incf index)
      (when (and (< index end)
                 (digit-char-p (char string index)))
        (json-error start "Leading zeros are not allowed in JSON numbers")))

    ;; Consume remaining integer digits (the leading-zero case already
    ;; consumed the '0' and validated no further digits follow, so this
    ;; loop only runs for non-zero leading digits like 1-9)
    (loop while (and (< index end) (digit-char-p (char string index)))
          do (incf index))

    ;; Fractional part
    (when (and (< index end) (char= (char string index) #\.))
      (setf is-float t)
      (incf index)
      (let ((frac-start index))
        (loop while (and (< index end) (digit-char-p (char string index)))
              do (incf index))
        (when (= index frac-start)
          (json-error index "Expected digit after decimal point"))))

    ;; Exponent part
    (when (and (< index end)
               (let ((ch (char string index)))
                 (or (char= ch #\e) (char= ch #\E))))
      (setf is-float t)
      (incf index)
      ;; Optional sign
      (when (and (< index end)
                 (let ((ch (char string index)))
                   (or (char= ch #\+) (char= ch #\-))))
        (incf index))
      (let ((exp-start index))
        (loop while (and (< index end) (digit-char-p (char string index)))
              do (incf index))
        (when (= index exp-start)
          (json-error index "Expected digit in exponent"))))

    ;; Extract the number substring and parse
    (let ((num-str (subseq string start index)))
      (if is-float
          ;; Parse as double-float
          (let ((result (handler-case
                            (let ((*read-default-float-format* 'double-float))
                              (read-from-string num-str))
                          (error ()
                            (json-error start "Invalid or overflowing number: ~A" num-str)))))
            (when (or (sb-ext:float-infinity-p result)
                      (sb-ext:float-nan-p result))
              (json-error start "Number overflow: ~A" num-str))
            (values result index))
          ;; Parse as integer
          (let ((result (handler-case
                            (parse-integer num-str)
                          (error ()
                            ;; Might be too large for parse-integer with :junk-allowed nil
                            ;; Try read-from-string for bignums
                            (handler-case
                                (let ((val (read-from-string num-str)))
                                  (if (integerp val) val
                                      (json-error start "Invalid number: ~A" num-str)))
                              (error ()
                                (json-error start "Invalid number: ~A" num-str)))))))
            (values result index))))))

;;; --------------------------------------------------------------------------
;;; Value parsing
;;; --------------------------------------------------------------------------

(defun parse-value (string index end depth)
  "Parse a JSON value starting at INDEX.
   Returns (values value new-index)."
  (declare (type simple-string string)
           (type fixnum index end depth))
  (setf index (skip-whitespace string index end))
  (when (>= index end)
    (json-error index "Unexpected end of input"))

  (let ((ch (char string index)))
    (cond
      ;; String
      ((char= ch #\")
       (parse-string string index end))

      ;; Number (digit or minus)
      ((or (digit-char-p ch) (char= ch #\-))
       (parse-number string index end))

      ;; Object
      ((char= ch #\{)
       (parse-object string index end depth))

      ;; Array
      ((char= ch #\[)
       (parse-array string index end depth))

      ;; true
      ((char= ch #\t)
       (if (and (<= (+ index 4) end)
                (string= string "true" :start1 index :end1 (+ index 4)))
           (values t (+ index 4))
           (json-error index "Invalid token starting with 't'")))

      ;; false
      ((char= ch #\f)
       (if (and (<= (+ index 5) end)
                (string= string "false" :start1 index :end1 (+ index 5)))
           (values :false (+ index 5))
           (json-error index "Invalid token starting with 'f'")))

      ;; null
      ((char= ch #\n)
       (if (and (<= (+ index 4) end)
                (string= string "null" :start1 index :end1 (+ index 4)))
           (values :null (+ index 4))
           (json-error index "Invalid token starting with 'n'")))

      ;; Single quotes not allowed
      ((char= ch #\')
       (json-error index "Single quotes are not valid in JSON; use double quotes"))

      ;; Comments not allowed
      ((char= ch #\/)
       (json-error index "Comments are not allowed in JSON"))

      (t
       (json-error index "Unexpected character '~C'" ch)))))

;;; --------------------------------------------------------------------------
;;; Array parsing
;;; --------------------------------------------------------------------------

(defun parse-array (string index end depth)
  "Parse a JSON array starting at INDEX (must point to '[').
   Returns (values vector new-index)."
  (declare (type simple-string string)
           (type fixnum index end depth))
  (when (>= depth +max-nesting-depth+)
    (json-error index "Maximum nesting depth (~D) exceeded" +max-nesting-depth+))

  (setf index (expect-char string index end #\[))
  (setf index (skip-whitespace string index end))

  ;; Empty array
  (when (and (< index end) (char= (char string index) #\]))
    (return-from parse-array (values (vector) (1+ index))))

  (let ((elements (make-array 8 :adjustable t :fill-pointer 0)))
    (loop
      ;; Parse element
      (multiple-value-bind (val new-index)
          (parse-value string index end (1+ depth))
        (vector-push-extend val elements)
        (setf index (skip-whitespace string new-index end)))

      (when (>= index end)
        (json-error index "Unterminated array"))

      (let ((ch (char string index)))
        (cond
          ((char= ch #\])
           (return (values (copy-seq elements) (1+ index))))
          ((char= ch #\,)
           (incf index)
           ;; Check for trailing comma
           (setf index (skip-whitespace string index end))
           (when (and (< index end) (char= (char string index) #\]))
             (json-error index "Trailing comma in array")))
          (t
           (json-error index "Expected ',' or ']' in array, found '~C'" ch)))))))

;;; --------------------------------------------------------------------------
;;; Object parsing
;;; --------------------------------------------------------------------------

(defun parse-object (string index end depth)
  "Parse a JSON object starting at INDEX (must point to '{').
   Returns (values hash-table new-index)."
  (declare (type simple-string string)
           (type fixnum index end depth))
  (when (>= depth +max-nesting-depth+)
    (json-error index "Maximum nesting depth (~D) exceeded" +max-nesting-depth+))

  (setf index (expect-char string index end #\{))
  (setf index (skip-whitespace string index end))

  ;; Empty object
  (when (and (< index end) (char= (char string index) #\}))
    (return-from parse-object
      (values (make-hash-table :test 'equal) (1+ index))))

  (let ((table (make-hash-table :test 'equal)))
    (loop
      ;; Parse key (must be string)
      (setf index (skip-whitespace string index end))
      (when (>= index end)
        (json-error index "Unterminated object"))
      (unless (char= (char string index) #\")
        (json-error index "Expected string key in object, found '~C'" (char string index)))

      (multiple-value-bind (key new-index)
          (parse-string string index end)
        (setf index (skip-whitespace string new-index end))

        ;; Expect colon
        (setf index (expect-char string index end #\:))

        ;; Parse value
        (multiple-value-bind (val val-index)
            (parse-value string index end (1+ depth))
          (setf (gethash key table) val)
          (setf index (skip-whitespace string val-index end))))

      (when (>= index end)
        (json-error index "Unterminated object"))

      (let ((ch (char string index)))
        (cond
          ((char= ch #\})
           (return (values table (1+ index))))
          ((char= ch #\,)
           (incf index)
           ;; Check for trailing comma
           (setf index (skip-whitespace string index end))
           (when (and (< index end) (char= (char string index) #\}))
             (json-error index "Trailing comma in object")))
          (t
           (json-error index "Expected ',' or '}' in object, found '~C'" ch)))))))

;;; --------------------------------------------------------------------------
;;; Public API
;;; --------------------------------------------------------------------------

(defun decode (string)
  "Decode a JSON STRING into Lisp data structures.

   Type mapping:
     null    → :null
     true    → t
     false   → :false
     number  → integer or double-float
     string  → CL string
     array   → CL vector
     object  → hash-table (:test 'equal)

   Signals json-parse-error on invalid JSON."
  (check-type string string)
  (let* ((str (if (typep string 'simple-string)
                  string
                  (copy-seq string)))
         (end (length str)))
    (when (zerop end)
      (json-error 0 "Empty input"))
    (multiple-value-bind (value new-index)
        (parse-value str 0 end 0)
      ;; Check for trailing content
      (let ((final-index (skip-whitespace str new-index end)))
        (when (< final-index end)
          (json-error final-index "Unexpected trailing content: '~C'"
                      (char str final-index))))
      value)))
