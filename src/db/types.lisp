;;;; src/db/types.lisp — PostgreSQL type OID mapping and value encoding/decoding
(in-package :cauldron.db)

;;; ---------- Type Info ----------

(defstruct pg-type-info
  "Metadata for a PostgreSQL type."
  (name    "" :type string)
  (oid     0  :type (unsigned-byte 32))
  (decoder nil :type (or null function))
  (category :string :type keyword))

;;; ---------- Decoders ----------

(defun decode-bool (text)
  "Decode PostgreSQL boolean text representation."
  (cond ((or (string= text "t") (string= text "true") (string= text "TRUE")) t)
        ((or (string= text "f") (string= text "false") (string= text "FALSE")) nil)
        (t (error "Invalid boolean value: ~S" text))))

(defun decode-integer (text)
  "Decode PostgreSQL integer text representation."
  (parse-integer text))

(defun decode-float4 (text)
  "Decode PostgreSQL float4 text representation."
  (coerce (read-from-string text) 'single-float))

(defun decode-float8 (text)
  "Decode PostgreSQL float8 text representation."
  (coerce (read-from-string text) 'double-float))

(defun decode-text (text)
  "Identity decoder for text types."
  text)

(defun decode-bytea-hex (text)
  "Decode PostgreSQL bytea hex format (\\x prefix)."
  (let* ((hex-str (if (and (>= (length text) 2)
                           (char= (char text 0) #\\)
                           (char= (char text 1) #\x))
                      (subseq text 2)
                      text))
         (len (floor (length hex-str) 2))
         (result (make-array len :element-type '(unsigned-byte 8))))
    (loop for i from 0 below len
          for pos = (* i 2)
          do (setf (aref result i)
                   (parse-integer hex-str :start pos :end (+ pos 2) :radix 16)))
    result))

(defun decode-text-array (text)
  "Decode PostgreSQL text[] format {\"a\",\"b\",\"c\"} into a list of strings.
Handles simple cases; does not handle nested arrays or special escaping."
  (if (or (string= text "{}") (string= text ""))
      nil
      ;; Strip outer braces
      (let ((inner (subseq text 1 (1- (length text)))))
        (parse-pg-array-elements inner))))

(defun parse-pg-array-elements (s)
  "Parse comma-separated PG array elements. Handles double-quoted strings."
  (let ((result '())
        (pos 0)
        (len (length s)))
    (loop while (< pos len)
          do (cond
               ;; Quoted element
               ((char= (char s pos) #\")
                (incf pos) ; skip opening quote
                (let ((buf (make-array 0 :element-type 'character
                                         :adjustable t :fill-pointer 0)))
                  (loop while (< pos len)
                        for ch = (char s pos)
                        do (cond
                             ((and (char= ch #\\) (< (1+ pos) len))
                              ;; Escaped character
                              (incf pos)
                              (vector-push-extend (char s pos) buf)
                              (incf pos))
                             ((char= ch #\")
                              (incf pos) ; skip closing quote
                              (return))
                             (t
                              (vector-push-extend ch buf)
                              (incf pos))))
                  (push (coerce buf 'string) result)
                  ;; Skip comma
                  (when (and (< pos len) (char= (char s pos) #\,))
                    (incf pos))))
               ;; Unquoted element
               (t
                (let ((comma-pos (position #\, s :start pos)))
                  (let ((elem (subseq s pos (or comma-pos len))))
                    (push (if (string= elem "NULL") nil elem) result))
                  (setf pos (if comma-pos (1+ comma-pos) len))))))
    (nreverse result)))

;;; ---------- OID Table ----------

(defvar *pg-type-oids* (make-hash-table :test #'eql)
  "Hash table mapping PostgreSQL OID → pg-type-info struct.")

(defun register-pg-type (oid name decoder &optional (category :string))
  "Register a PG type OID with its decoder."
  (setf (gethash oid *pg-type-oids*)
        (make-pg-type-info :name name :oid oid
                           :decoder decoder :category category)))

;; Register all supported types
(register-pg-type 16   "bool"        #'decode-bool       :boolean)
(register-pg-type 20   "int8"        #'decode-integer     :integer)
(register-pg-type 21   "int2"        #'decode-integer     :integer)
(register-pg-type 23   "int4"        #'decode-integer     :integer)
(register-pg-type 25   "text"        #'decode-text        :string)
(register-pg-type 700  "float4"      #'decode-float4      :float)
(register-pg-type 701  "float8"      #'decode-float8      :float)
(register-pg-type 1043 "varchar"     #'decode-text        :string)
(register-pg-type 1114 "timestamp"   #'decode-text        :string)
(register-pg-type 1184 "timestamptz" #'decode-text        :string)
(register-pg-type 2950 "uuid"        #'decode-text        :string)
(register-pg-type 3802 "jsonb"       #'decode-text        :string)
(register-pg-type 17   "bytea"       #'decode-bytea-hex   :binary)
(register-pg-type 1700 "numeric"     #'decode-text        :string)
(register-pg-type 1009 "text[]"      #'decode-text-array  :array)

;;; ---------- Public API ----------

(defun decode-pg-value (oid text-value)
  "Decode TEXT-VALUE from PostgreSQL using the type decoder registered for OID.
If OID is unknown, returns TEXT-VALUE as-is (string)."
  (when (null text-value)
    (return-from decode-pg-value nil))
  (let ((type-info (gethash oid *pg-type-oids*)))
    (if (and type-info (pg-type-info-decoder type-info))
        (funcall (pg-type-info-decoder type-info) text-value)
        ;; Unknown OID — return as string
        text-value)))

(defun encode-pg-value (value)
  "Encode a Lisp VALUE to a text string suitable for PG parameter binding.
Returns NIL for NIL (SQL NULL)."
  (etypecase value
    (null nil)
    (string value)
    (integer (princ-to-string value))
    (single-float (format nil "~F" value))
    (double-float (format nil "~F" value))
    ((eql t) "t")
    ;; Boolean NIL is handled above as null — use :false for explicit false
    (keyword (cond ((eq value :false) "f")
                   ((eq value :true) "t")
                   ((eq value :null) nil)
                   (t (string-downcase (symbol-name value)))))
    ((vector (unsigned-byte 8))
     ;; Bytea: encode as hex
     (let* ((len (length value))
            (hex (make-string (+ 2 (* len 2)))))
       (setf (char hex 0) #\\)
       (setf (char hex 1) #\x)
       (loop for i from 0 below len
             for byte = (aref value i)
             for pos = (+ 2 (* i 2))
             do (setf (char hex pos)       (char "0123456789abcdef" (ash byte -4)))
                (setf (char hex (1+ pos))  (char "0123456789abcdef" (logand byte #x0F))))
       hex))
    (list
     ;; List → PG array literal
     (format nil "{~{~A~^,~}}"
             (mapcar (lambda (elem)
                       (if (null elem) "NULL"
                           (format nil "\"~A\"" (encode-pg-value elem))))
                     value)))))
