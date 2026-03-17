;;;; format.lisp — Stripe-style canonical log line formatting and parsing
;;;; Part of Phase 37B: Canonical Logging Module

(in-package :cauldron.logging)

;;; --- Formatting ---

(defun keyword-to-snake-case (keyword)
  "Convert a keyword like :REQUEST-ID to the string \"request_id\"."
  (let* ((name (symbol-name keyword))
         (lower (string-downcase name)))
    (substitute #\_ #\- lower)))

(defun value-needs-quoting-p (string)
  "Return T if STRING contains spaces and thus needs quoting in canonical output."
  (find #\Space string))

(defun format-value (value)
  "Format a single VALUE for canonical log line output.
Integers print as-is, NIL becomes \"nil\", strings with spaces are quoted."
  (cond
    ((null value) "nil")
    ((integerp value) (write-to-string value))
    ((stringp value)
     (if (value-needs-quoting-p value)
         (format nil "\"~A\"" value)
         value))
    (t (let ((s (write-to-string value)))
         (if (value-needs-quoting-p s)
             (format nil "\"~A\"" s)
             s)))))

(defun format-canonical-line (alist)
  "Format an alist of (key . value) pairs into a canonical log line string.
Example output: \"request_id=req_a1b2 method=POST path=/api/records status=200 duration_ms=42\"
Keys are converted from keywords to snake_case strings. Values: strings unquoted
unless they contain spaces, nil becomes \"nil\", integers printed as-is."
  (let ((parts '()))
    (dolist (pair alist)
      (let ((key (keyword-to-snake-case (car pair)))
            (val (format-value (cdr pair))))
        (push (format nil "~A=~A" key val) parts)))
    (format nil "~{~A~^ ~}" (nreverse parts))))

;;; --- Parsing ---

(defun snake-case-to-keyword (string)
  "Convert a snake_case string like \"request_id\" to the keyword :REQUEST-ID."
  (intern (string-upcase (substitute #\- #\_ string)) :keyword))

(defun parse-value (string)
  "Parse a value string from a canonical log line.
Integer-looking strings become integers, \"nil\" becomes NIL, otherwise returns the string."
  (cond
    ((string= string "nil") nil)
    ((and (> (length string) 0)
          (every (lambda (c) (or (digit-char-p c) (char= c #\-)))
                 string)
          ;; Ensure not just a bare hyphen
          (not (string= string "-")))
     (parse-integer string :junk-allowed nil))
    ;; Strip surrounding quotes if present
    ((and (>= (length string) 2)
          (char= (char string 0) #\")
          (char= (char string (1- (length string))) #\"))
     (subseq string 1 (1- (length string))))
    (t string)))

(defun tokenize-canonical-line (line)
  "Split a canonical log line into key=value token strings.
Handles quoted values that contain spaces."
  (let ((tokens '())
        (current (make-string-output-stream))
        (in-quotes nil)
        (i 0)
        (len (length line)))
    (loop while (< i len)
          for ch = (char line i)
          do (cond
               ;; Toggle quote state
               ((char= ch #\")
                (setf in-quotes (not in-quotes))
                (write-char ch current))
               ;; Space outside quotes = token boundary
               ((and (char= ch #\Space) (not in-quotes))
                (let ((tok (get-output-stream-string current)))
                  (when (> (length tok) 0)
                    (push tok tokens)))
                (setf current (make-string-output-stream)))
               (t
                (write-char ch current)))
             (incf i))
    ;; Final token
    (let ((tok (get-output-stream-string current)))
      (when (> (length tok) 0)
        (push tok tokens)))
    (nreverse tokens)))

(defun parse-canonical-line (line)
  "Parse a canonical log line string back into an alist of (keyword . value) pairs.
Values that look like integers are parsed as integers. \"nil\" becomes NIL."
  (let ((tokens (tokenize-canonical-line line))
        (result '()))
    (dolist (token tokens)
      (let ((eq-pos (position #\= token)))
        (when eq-pos
          (let* ((key-str (subseq token 0 eq-pos))
                 (val-str (subseq token (1+ eq-pos)))
                 (key (snake-case-to-keyword key-str))
                 (val (parse-value val-str)))
            (push (cons key val) result)))))
    (nreverse result)))
