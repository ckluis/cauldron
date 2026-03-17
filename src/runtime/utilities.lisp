;;;; src/runtime/utilities.lisp — General-purpose utility functions and macros
(in-package :cauldron.runtime)

(defun ht (&rest pairs)
  "Build a hash-table from alternating key value pairs.
Example: (ht \"name\" \"foo\" \"type\" \"string\")"
  (let ((ht (make-hash-table :test 'equal)))
    (loop for (k v) on pairs by #'cddr
          do (setf (gethash k ht) v))
    ht))

(defun ht-get (ht key &optional default)
  "Get value from a hash-table or alist by string key."
  (cond
    ((hash-table-p ht) (or (gethash key ht) default))
    ((listp ht) (or (cdr (assoc key ht :test #'string=)) default))
    (t default)))

(defun split-sequence (delimiter sequence &key (start 0) end)
  "Split SEQUENCE by DELIMITER element. Returns a list of subsequences.
START and END bound the region of SEQUENCE to consider."
  (let ((limit (or end (length sequence)))
        (result '())
        (current-start start))
    (loop for i from start below limit
          when (eql (elt sequence i) delimiter)
            do (push (subseq sequence current-start i) result)
               (setf current-start (1+ i))
          finally (push (subseq sequence current-start limit) result))
    (nreverse result)))

(defun string-prefix-p (prefix string)
  "Return T if STRING starts with PREFIX."
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun string-suffix-p (suffix string)
  "Return T if STRING ends with SUFFIX."
  (and (<= (length suffix) (length string))
       (string= suffix string :start2 (- (length string) (length suffix)))))

(defmacro when-let ((var form) &body body)
  "Bind VAR to the result of FORM; execute BODY only if the value is non-nil."
  `(let ((,var ,form))
     (when ,var ,@body)))

(defmacro if-let ((var form) then &optional else)
  "Bind VAR to the result of FORM; evaluate THEN if non-nil, otherwise ELSE."
  `(let ((,var ,form))
     (if ,var ,then ,else)))

(defun ensure-list (thing)
  "If THING is already a list, return it; otherwise wrap it in a fresh list."
  (if (listp thing) thing (list thing)))

(defun make-keyword (string)
  "Intern the upper-cased form of STRING into the KEYWORD package."
  (intern (string-upcase string) :keyword))

(defun octets-to-integer (octets)
  "Convert a vector of octets (big-endian) to an unsigned integer."
  (let ((result 0))
    (loop for octet across octets
          do (setf result (logior (ash result 8) octet)))
    result))

(defun integer-to-octets (integer &optional (n-bytes 4))
  "Convert unsigned INTEGER to a big-endian octet vector of N-BYTES length."
  (let ((result (make-array n-bytes :element-type '(unsigned-byte 8))))
    (loop for i from (1- n-bytes) downto 0
          for shift from 0 by 8
          do (setf (aref result i) (ldb (byte 8 shift) integer)))
    result))
