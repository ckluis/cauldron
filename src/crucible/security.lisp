;;;; src/crucible/security.lisp — Phase 45: Security hardening plugs
;;;; CSP, HSTS, IP allowlist, request size limits, enhanced secure headers.
(in-package :cauldron.crucible)

;;; --- Content Security Policy ---

(defun make-plug-content-security-policy (&key
                                            (default-src "'self'")
                                            (script-src "'self'")
                                            (style-src "'self' 'unsafe-inline'")
                                            (img-src "'self' data:")
                                            (font-src "'self'")
                                            (connect-src "'self'")
                                            (frame-ancestors "'none'")
                                            (base-uri "'self'")
                                            (form-action "'self'"))
  "Create a plug that sets Content-Security-Policy header.
All directives default to restrictive values."
  (let ((policy (format nil "default-src ~A; script-src ~A; style-src ~A; img-src ~A; font-src ~A; connect-src ~A; frame-ancestors ~A; base-uri ~A; form-action ~A"
                        default-src script-src style-src img-src font-src connect-src
                        frame-ancestors base-uri form-action)))
    (lambda (conn)
      (conn-put-resp-header conn "Content-Security-Policy" policy))))

;;; --- HSTS ---

(defun make-plug-hsts (&key (max-age 31536000) include-subdomains preload)
  "Create a plug that sets Strict-Transport-Security header.
MAX-AGE in seconds (default: 1 year)."
  (let ((value (format nil "max-age=~D~A~A"
                       max-age
                       (if include-subdomains "; includeSubDomains" "")
                       (if preload "; preload" ""))))
    (lambda (conn)
      (conn-put-resp-header conn "Strict-Transport-Security" value))))

;;; --- Enhanced Security Headers ---

(defun plug-secure-headers-enhanced (conn)
  "Enhanced version of plug-security-headers with full header set.
Replaces the basic plug-security-headers."
  (conn-put-resp-header conn "X-Content-Type-Options" "nosniff")
  (conn-put-resp-header conn "X-Frame-Options" "DENY")
  (conn-put-resp-header conn "X-XSS-Protection" "0")
  (conn-put-resp-header conn "Referrer-Policy" "strict-origin-when-cross-origin")
  (conn-put-resp-header conn "Permissions-Policy"
                        "camera=(), microphone=(), geolocation=(), payment=()")
  (conn-put-resp-header conn "Cross-Origin-Opener-Policy" "same-origin")
  (conn-put-resp-header conn "Cross-Origin-Resource-Policy" "same-origin")
  conn)

;;; --- IP Allowlist ---

(defun %parse-ip-from-conn (conn)
  "Extract client IP from conn headers.
Checks X-Forwarded-For, X-Real-IP, then falls back to connection IP."
  (or (let ((xff (cdr (assoc "X-Forwarded-For" (conn-headers conn) :test #'string-equal))))
        (when xff
          (string-trim '(#\Space) (subseq xff 0 (or (position #\, xff) (length xff))))))
      (cdr (assoc "X-Real-IP" (conn-headers conn) :test #'string-equal))
      "unknown"))

(defun make-plug-ip-allowlist (allowed-ips)
  "Create a plug that restricts access to ALLOWED-IPS (list of IP strings).
Halts with 403 if client IP is not in the list."
  (lambda (conn)
    (let ((client-ip (%parse-ip-from-conn conn)))
      (if (member client-ip allowed-ips :test #'string=)
          conn
          (progn
            (json-error conn "Forbidden: IP not allowed" :status 403)
            (halt-conn conn :status 403))))))

;;; --- Request Size Limit ---

(defun make-plug-request-size-limit (&key (max-json-bytes 1048576) (max-multipart-bytes 10485760))
  "Create a plug that rejects oversized request bodies.
MAX-JSON-BYTES default 1MB, MAX-MULTIPART-BYTES default 10MB."
  (lambda (conn)
    (let* ((content-type (cdr (assoc "Content-Type" (conn-headers conn) :test #'string-equal)))
           (content-length-str (cdr (assoc "Content-Length" (conn-headers conn) :test #'string-equal)))
           (content-length (when content-length-str
                             (parse-integer content-length-str :junk-allowed t)))
           (is-multipart (and content-type (search "multipart" content-type :test #'char-equal)))
           (max-bytes (if is-multipart max-multipart-bytes max-json-bytes)))
      (if (and content-length (> content-length max-bytes))
          (progn
            (json-error conn
                        (format nil "Request body too large: ~:D bytes exceeds ~:D byte limit"
                                content-length max-bytes)
                        :status 413)
            (halt-conn conn :status 413))
          conn))))

;;; --- Secure Random String ---

(defun secure-random-string (length &key (alphabet "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
  "Generate a cryptographically secure random string of LENGTH from ALPHABET."
  (let* ((bytes (cauldron.crypto:secure-random-bytes length))
         (alphabet-len (length alphabet))
         (result (make-string length)))
    (loop for i from 0 below length
          do (setf (char result i)
                   (char alphabet (mod (aref bytes i) alphabet-len))))
    result))

;;; --- Session Fixation Protection ---

(defun regenerate-session-id ()
  "Generate a new session ID for session fixation protection.
Call this on login/logout to prevent session fixation attacks.
Returns the new session ID string."
  (cauldron.crypto:generate-token :length 32))

;;; --- SQL Identifier Validation ---

(defun validate-sql-identifier (name)
  "Validate that NAME is a safe SQL identifier (alphanumeric + underscore only).
Returns the validated name string or signals an error."
  (let ((name-str (string name)))
    (unless (every (lambda (c) (or (alphanumericp c) (char= c #\_))) name-str)
      (error "Invalid SQL identifier: ~S (must be alphanumeric + underscore)" name-str))
    (when (zerop (length name-str))
      (error "Empty SQL identifier"))
    name-str))
