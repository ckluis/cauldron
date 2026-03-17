;;;; src/http/response.lisp — HTTP response construction and serialization

(in-package :cauldron.http)

;;; --- Status codes ---

(defparameter *status-texts*
  '((100 . "Continue")
    (101 . "Switching Protocols")
    (200 . "OK")
    (201 . "Created")
    (204 . "No Content")
    (301 . "Moved Permanently")
    (302 . "Found")
    (303 . "See Other")
    (304 . "Not Modified")
    (307 . "Temporary Redirect")
    (308 . "Permanent Redirect")
    (400 . "Bad Request")
    (401 . "Unauthorized")
    (403 . "Forbidden")
    (404 . "Not Found")
    (405 . "Method Not Allowed")
    (408 . "Request Timeout")
    (413 . "Content Too Large")
    (415 . "Unsupported Media Type")
    (422 . "Unprocessable Content")
    (429 . "Too Many Requests")
    (500 . "Internal Server Error")
    (502 . "Bad Gateway")
    (503 . "Service Unavailable")
    (504 . "Gateway Timeout")))

(defun status-text (code)
  "Get the standard text for HTTP status CODE."
  (or (cdr (assoc code *status-texts*))
      "Unknown"))

;;; --- Response struct ---

(defstruct response
  "HTTP response to be serialized to the wire."
  (status 200 :type integer)
  (headers '() :type list)          ; alist of (name . value)
  (body nil))                        ; string, octet vector, or nil

(defun response-header (response name)
  "Get a response header value by NAME."
  (cdr (assoc name (response-headers response) :test #'string-equal)))

(defun (setf response-header) (value response name)
  "Set a response header."
  (let ((existing (assoc name (response-headers response) :test #'string-equal)))
    (if existing
        (setf (cdr existing) value)
        (push (cons name value) (response-headers response))))
  value)

;;; --- Response writing ---

(defun write-crlf (stream)
  "Write CRLF to stream."
  (write-byte 13 stream)   ; CR
  (write-byte 10 stream))  ; LF

(defun write-ascii (string stream)
  "Write an ASCII string to a binary stream."
  (loop for char across string
        do (write-byte (char-code char) stream)))

(defun body-to-octets (body)
  "Convert response body to octets."
  (cond
    ((null body) #())
    ((typep body '(simple-array (unsigned-byte 8) (*))) body)
    ((stringp body)
     (let* ((len (length body))
            (octets (make-array len :element-type '(unsigned-byte 8))))
       (dotimes (i len octets)
         (setf (aref octets i) (char-code (char body i))))))
    ((typep body '(vector (unsigned-byte 8))) (coerce body '(simple-array (unsigned-byte 8) (*))))
    (t (body-to-octets (princ-to-string body)))))

(defun write-response (response stream &key (head-only nil))
  "Serialize RESPONSE to binary STREAM as HTTP/1.1.
If HEAD-ONLY, skip the body (for HEAD requests)."
  (let* ((body-octets (unless head-only (body-to-octets (response-body response))))
         (body-length (if body-octets (length body-octets) 0)))
    ;; Status line
    (write-ascii (format nil "HTTP/1.1 ~D ~A"
                         (response-status response)
                         (status-text (response-status response)))
                 stream)
    (write-crlf stream)
    ;; Ensure Content-Length header
    (unless (assoc "Content-Length" (response-headers response) :test #'string-equal)
      (push (cons "Content-Length" (princ-to-string body-length))
            (response-headers response)))
    ;; Headers
    (dolist (header (response-headers response))
      (write-ascii (car header) stream)
      (write-ascii ": " stream)
      (write-ascii (cdr header) stream)
      (write-crlf stream))
    ;; Blank line
    (write-crlf stream)
    ;; Body
    (when (and body-octets (> body-length 0) (not head-only))
      (write-sequence body-octets stream))
    (force-output stream)))

;;; --- Convenience constructors ---

(defun make-text-response (body &key (status 200) headers)
  "Create a text/plain response."
  (make-response :status status
                 :headers (append (list (cons "Content-Type" "text/plain; charset=utf-8"))
                                  headers)
                 :body body))

(defun make-html-response (body &key (status 200) headers)
  "Create a text/html response."
  (make-response :status status
                 :headers (append (list (cons "Content-Type" "text/html; charset=utf-8"))
                                  headers)
                 :body body))

(defun make-json-response (body &key (status 200) headers)
  "Create an application/json response. BODY should be a JSON string."
  (make-response :status status
                 :headers (append (list (cons "Content-Type" "application/json"))
                                  headers)
                 :body body))

(defun make-redirect (url &key (status 302) headers)
  "Create a redirect response."
  (make-response :status status
                 :headers (append (list (cons "Location" url)) headers)
                 :body ""))
