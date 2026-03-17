;;;; src/crucible/compress.lisp — Response compression plug
(in-package :cauldron.crucible)

(defvar *compressible-types*
  '("text/html" "text/css" "text/plain" "text/xml" "text/javascript"
    "application/json" "application/xml" "application/javascript"
    "application/xhtml+xml" "image/svg+xml")
  "Content-Type values eligible for compression.")

(defvar *compression-min-size* 256
  "Minimum response body size (bytes) to compress.")

(defun compress-response (conn)
  "Compress conn response body if eligible. Called from bridge after dispatch.
Checks Accept-Encoding, Content-Type, body size. Sets Content-Encoding + Vary."
  (let* ((body (conn-resp-body conn))
         (accept-encoding (cdr (assoc "Accept-Encoding" (conn-headers conn)
                                      :test #'string-equal)))
         (content-type (cdr (assoc "Content-Type" (conn-resp-headers conn)
                                   :test #'string-equal))))
    ;; Check eligibility
    (if (and accept-encoding
             body
             (stringp body)
             (> (length body) *compression-min-size*)
             content-type
             (compressible-type-p content-type)
             (not (assoc "Content-Encoding" (conn-resp-headers conn)
                         :test #'string-equal))
             (search "gzip" accept-encoding :test #'char-equal))
        ;; Compress
        (let* ((body-octets (sb-ext:string-to-octets body :external-format :utf-8))
               (compressed (cauldron.http:gzip-compress body-octets)))
          (conn-put-resp-body conn compressed)
          (conn-put-resp-header conn "Content-Encoding" "gzip")
          (conn-put-resp-header conn "Vary" "Accept-Encoding")
          conn)
        ;; Not eligible — return unchanged
        conn)))

(defun compressible-type-p (content-type)
  "Return T if CONTENT-TYPE is compressible.
Strips charset parameters for comparison."
  (let ((base-type (string-trim '(#\Space)
                                (subseq content-type 0
                                        (or (position #\; content-type)
                                            (length content-type))))))
    (member base-type *compressible-types* :test #'string-equal)))
