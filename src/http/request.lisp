;;;; src/http/request.lisp — HTTP/1.1 request parsing
;;;; No regex. Manual parsing with strict limits.

(in-package :cauldron.http)

;;; --- Constants ---

(defconstant +max-request-line+ 8192
  "Maximum length of the request line in bytes.")

(defconstant +max-header-size+ 32768
  "Maximum total size of all headers in bytes.")

(defconstant +max-headers+ 100
  "Maximum number of headers.")

(defconstant +max-body-size+ (* 10 1024 1024)
  "Default maximum body size: 10MB.")

;;; --- Request struct ---

(defstruct request
  "Parsed HTTP/1.1 request."
  (method :get :type keyword)
  (path "/" :type string)
  (query-string nil :type (or null string))
  (http-version "HTTP/1.1" :type string)
  (headers '() :type list)           ; alist of (name . value)
  (body nil)                          ; string, octet vector, or nil
  (content-type nil :type (or null string))
  (content-length nil :type (or null integer))
  (remote-addr nil :type (or null string)))

(defun request-header (request name)
  "Get header value by NAME (case-insensitive)."
  (cdr (assoc name (request-headers request) :test #'string-equal)))

;;; --- URL decoding ---

(defun hex-digit-value (char)
  "Return numeric value of hex digit, or NIL."
  (cond
    ((char<= #\0 char #\9) (- (char-code char) (char-code #\0)))
    ((char<= #\a char #\f) (+ 10 (- (char-code char) (char-code #\a))))
    ((char<= #\A char #\F) (+ 10 (- (char-code char) (char-code #\A))))
    (t nil)))

(defun url-decode (string)
  "Decode a percent-encoded URL string."
  (with-output-to-string (out)
    (let ((i 0) (len (length string)))
      (loop while (< i len)
            do (let ((char (char string i)))
                 (cond
                   ((char= char #\+)
                    (write-char #\Space out)
                    (incf i))
                   ((and (char= char #\%)
                         (< (+ i 2) len))
                    (let ((hi (hex-digit-value (char string (+ i 1))))
                          (lo (hex-digit-value (char string (+ i 2)))))
                      (if (and hi lo)
                          (progn
                            (write-char (code-char (+ (* hi 16) lo)) out)
                            (incf i 3))
                          (progn
                            (write-char char out)
                            (incf i)))))
                   (t
                    (write-char char out)
                    (incf i))))))))

(defun parse-query-string (qs)
  "Parse a query string into an alist of (key . value) pairs."
  (when (and qs (> (length qs) 0))
    (loop for pair in (split-string qs #\&)
          when (> (length pair) 0)
            collect (let ((eq-pos (position #\= pair)))
                      (if eq-pos
                          (cons (url-decode (subseq pair 0 eq-pos))
                                (url-decode (subseq pair (1+ eq-pos))))
                          (cons (url-decode pair) ""))))))

(defun split-string (string delimiter)
  "Split STRING by DELIMITER character."
  (loop for start = 0 then (1+ end)
        for end = (position delimiter string :start start)
        collect (subseq string start (or end (length string)))
        while end))

;;; --- Request parsing ---

(defun read-line-crlf (stream &optional (max-length +max-request-line+))
  "Read a CRLF-terminated line from STREAM. Returns string without CRLF.
Signals error if line exceeds MAX-LENGTH."
  (let ((buffer (make-array 256 :element-type 'character :adjustable t :fill-pointer 0))
        (prev-cr nil))
    (loop for byte = (read-byte stream nil nil)
          while byte
          do (let ((char (code-char byte)))
               (when (> (length buffer) max-length)
                 (error "Request line too long (max ~D bytes)" max-length))
               (cond
                 ((and prev-cr (char= char #\Linefeed))
                  ;; End of line — remove trailing CR
                  (when (> (fill-pointer buffer) 0)
                    (decf (fill-pointer buffer)))
                  (return (copy-seq buffer)))
                 (t
                  (setf prev-cr (char= char #\Return))
                  (vector-push-extend char buffer)))))
    ;; EOF — return what we have or nil
    (when (> (fill-pointer buffer) 0)
      (copy-seq buffer))))

(defun parse-request-line (line)
  "Parse 'GET /path?query HTTP/1.1' into (values method path query-string version)."
  (unless line
    (error "Empty request"))
  (let* ((sp1 (position #\Space line))
         (sp2 (and sp1 (position #\Space line :start (1+ sp1)))))
    (unless (and sp1 sp2)
      (error "Malformed request line: ~A" line))
    (let* ((method-str (subseq line 0 sp1))
           (uri (subseq line (1+ sp1) sp2))
           (version (subseq line (1+ sp2)))
           (qmark (position #\? uri))
           (path (if qmark (subseq uri 0 qmark) uri))
           (query (when qmark (subseq uri (1+ qmark))))
           (method (intern (string-upcase method-str) :keyword)))
      (values method path query version))))

(defun parse-headers (stream)
  "Parse HTTP headers from STREAM. Returns alist of (name . value)."
  (let ((headers '())
        (total-size 0)
        (count 0))
    (loop for line = (read-line-crlf stream +max-header-size+)
          while (and line (> (length line) 0))
          do (incf total-size (length line))
             (incf count)
             (when (> total-size +max-header-size+)
               (error "Headers too large (max ~D bytes)" +max-header-size+))
             (when (> count +max-headers+)
               (error "Too many headers (max ~D)" +max-headers+))
             (let ((colon (position #\: line)))
               (unless colon
                 (error "Malformed header: ~A" line))
               (let ((name (subseq line 0 colon))
                     (value (string-trim '(#\Space #\Tab) (subseq line (1+ colon)))))
                 (push (cons name value) headers))))
    (nreverse headers)))

(defun parse-request (stream &key remote-addr (max-body-size +max-body-size+))
  "Parse a complete HTTP/1.1 request from STREAM.
Returns a REQUEST struct or signals an error."
  ;; Request line
  (let ((request-line (read-line-crlf stream)))
    (unless request-line
      (return-from parse-request nil))
    (multiple-value-bind (method path query-string version)
        (parse-request-line request-line)
      ;; Security: reject mixed Content-Length + Transfer-Encoding (request smuggling)
      (let* ((headers (parse-headers stream))
             (has-cl (assoc "Content-Length" headers :test #'string-equal))
             (has-te (assoc "Transfer-Encoding" headers :test #'string-equal))
             (content-type (cdr (assoc "Content-Type" headers :test #'string-equal)))
             (content-length (when has-cl
                               (parse-integer (cdr has-cl) :junk-allowed t))))
        ;; Request smuggling defense
        (when (and has-cl has-te)
          (error "Rejecting request with both Content-Length and Transfer-Encoding"))
        ;; Read body
        (let ((body nil))
          (cond
            ;; Chunked transfer encoding
            ((and has-te (search "chunked" (cdr has-te) :test #'char-equal))
             (setf body (read-chunked-body stream max-body-size)))
            ;; Content-Length body
            (content-length
             (when (> content-length max-body-size)
               (error "Request body too large: ~D bytes (max ~D)" content-length max-body-size))
             (when (> content-length 0)
               (let ((buf (make-array content-length :element-type '(unsigned-byte 8))))
                 (read-sequence buf stream)
                 (setf body buf)))))
          (make-request :method method
                        :path path
                        :query-string query-string
                        :http-version version
                        :headers headers
                        :body body
                        :content-type content-type
                        :content-length content-length
                        :remote-addr remote-addr))))))

(defun read-chunked-body (stream max-size)
  "Read a chunked transfer-encoded body."
  (let ((output (make-array 1024 :element-type '(unsigned-byte 8)
                                 :adjustable t :fill-pointer 0))
        (total 0))
    (loop
      (let* ((size-line (read-line-crlf stream))
             (chunk-size (parse-integer size-line :radix 16 :junk-allowed t)))
        (unless chunk-size
          (error "Malformed chunk size: ~A" size-line))
        (when (zerop chunk-size)
          ;; Read trailing CRLF after last chunk
          (read-line-crlf stream)
          (return))
        (incf total chunk-size)
        (when (> total max-size)
          (error "Chunked body too large: ~D bytes (max ~D)" total max-size))
        ;; Read chunk data
        (let ((buf (make-array chunk-size :element-type '(unsigned-byte 8))))
          (read-sequence buf stream)
          (loop for byte across buf
                do (vector-push-extend byte output)))
        ;; Read CRLF after chunk
        (read-line-crlf stream)))
    (copy-seq output)))
