;;;; src/crucible/plugs.lisp — Standard plugs (middleware) for Crucible pipelines
;;;; Each plug is a function (conn) → conn that can inspect, modify, or halt.

(in-package :cauldron.crucible)

;;; ============================================================
;;; Body parsing
;;; ============================================================

(defun plug-parse-body (conn)
  "Parse the request body based on Content-Type.
Sets conn body to:
  - alist for application/x-www-form-urlencoded
  - parsed JSON (via cauldron.json:decode) for application/json
  - raw body otherwise
Stores original raw body in :raw-body assign."
  (let* ((content-type (cdr (assoc "Content-Type" (conn-headers conn)
                                   :test #'string-equal)))
         (raw-body (conn-body conn)))
    (conn-put-assign conn :raw-body raw-body)
    (when (and raw-body content-type)
      (cond
        ;; URL-encoded form data
        ((search "application/x-www-form-urlencoded" content-type :test #'char-equal)
         (let ((body-string (if (stringp raw-body)
                                raw-body
                                (octets-to-string raw-body))))
           (setf (conn-body conn)
                 (cauldron.http:parse-query-string body-string))))
        ;; JSON
        ((search "application/json" content-type :test #'char-equal)
         (let ((body-string (if (stringp raw-body)
                                raw-body
                                (octets-to-string raw-body))))
           (when (and body-string (> (length body-string) 0))
             (setf (conn-body conn)
                   (cauldron.json:decode body-string)))))
        ;; Multipart form-data
        ((search "multipart/form-data" content-type :test #'char-equal)
         (let ((boundary (cauldron.http:extract-boundary content-type)))
           (when (and boundary raw-body)
             (let ((parts (cauldron.http:parse-multipart raw-body boundary)))
               ;; Store parts in :multipart-parts assign
               (conn-put-assign conn :multipart-parts parts)
               ;; Also build an alist of name→value for non-file fields
               (setf (conn-body conn)
                     (loop for part in parts
                           when (and (cauldron.http:multipart-part-name part)
                                     (not (cauldron.http:multipart-part-filename part)))
                             collect (cons (cauldron.http:multipart-part-name part)
                                           (cauldron.http:multipart-part-body part))))))))
        ;; Leave as-is for other content types
        ))
    conn))

(defun octets-to-string (octets)
  "Convert an octet vector to a string (assuming UTF-8/ASCII)."
  (when octets
    (let* ((len (length octets))
           (str (make-string len)))
      (dotimes (i len str)
        (setf (char str i) (code-char (aref octets i)))))))

;;; ============================================================
;;; Cookie parsing
;;; ============================================================

(defun plug-parse-cookies (conn)
  "Parse the Cookie header into an alist stored in :cookies assign.
Each cookie is (name . value)."
  (let* ((cookie-header (cdr (assoc "Cookie" (conn-headers conn)
                                    :test #'string-equal)))
         (cookies (when cookie-header (parse-cookie-header cookie-header))))
    (conn-put-assign conn :cookies cookies)
    conn))

(defun parse-cookie-header (header)
  "Parse a Cookie header string into an alist of (name . value)."
  (let ((pairs '()))
    (loop for part in (split-cookie-string header #\;)
          do (let* ((trimmed (string-trim '(#\Space #\Tab) part))
                    (eq-pos (position #\= trimmed)))
               (when (and eq-pos (> eq-pos 0))
                 (push (cons (subseq trimmed 0 eq-pos)
                             (subseq trimmed (1+ eq-pos)))
                       pairs))))
    (nreverse pairs)))

(defun split-cookie-string (string delimiter)
  "Split STRING by DELIMITER character."
  (loop for start = 0 then (1+ end)
        for end = (position delimiter string :start start)
        collect (subseq string start (or end (length string)))
        while end))

;;; ============================================================
;;; Set-Cookie response helper
;;; ============================================================

(defun plug-set-cookie (conn name value &key path domain max-age
                                              secure http-only same-site)
  "Set a cookie on the response. This is a utility, not a pipeline plug.
Call it from handlers or other plugs."
  (let ((parts (list (format nil "~A=~A" name value))))
    (when path (push (format nil "Path=~A" path) parts))
    (when domain (push (format nil "Domain=~A" domain) parts))
    (when max-age (push (format nil "Max-Age=~D" max-age) parts))
    (when secure (push "Secure" parts))
    (when http-only (push "HttpOnly" parts))
    (when same-site (push (format nil "SameSite=~A" same-site) parts))
    (conn-put-resp-header conn "Set-Cookie"
                          (format nil "~{~A~^; ~}" (nreverse parts)))
    conn))

;;; ============================================================
;;; Session (HMAC-signed cookie, in-memory store)
;;; ============================================================

(defvar *session-store* (make-hash-table :test 'equal)
  "In-memory session store: session-id → hash-table of session data.")

(defvar *session-secret* nil
  "Secret key for signing session cookies. Must be set before use.")

(defvar *session-cookie-name* "_cauldron_session"
  "Name of the session cookie.")

(defvar *session-max-age* 86400
  "Session cookie max-age in seconds (default: 24 hours).")

(defun sign-session-id (session-id)
  "Sign a session ID with HMAC-SHA256. Returns 'id.signature' string."
  (unless *session-secret*
    (error "Session secret not set. Set cauldron.crucible:*session-secret* before using sessions."))
  (let* ((mac (cauldron.crypto:hmac-sha256 *session-secret* session-id))
         (sig (hex-encode mac)))
    (format nil "~A.~A" session-id sig)))

(defun verify-session-cookie (cookie-value)
  "Verify a signed session cookie. Returns session-id if valid, NIL otherwise.
Signals error if *session-secret* is not set."
  (unless *session-secret*
    (error "Session secret not set. Set cauldron.crucible:*session-secret* before using sessions."))
  (when cookie-value
    (let ((dot-pos (position #\. cookie-value)))
      (when dot-pos
        (let* ((session-id (subseq cookie-value 0 dot-pos))
               (provided-sig (subseq cookie-value (1+ dot-pos)))
               (expected-mac (cauldron.crypto:hmac-sha256 *session-secret* session-id))
               (expected-sig (hex-encode expected-mac)))
          (when (cauldron.crypto:secure-equal provided-sig expected-sig)
            session-id))))))

(defun hex-encode (octets)
  "Encode octet vector as lowercase hex string."
  (let* ((len (length octets))
         (hex (make-string (* len 2))))
    (loop for i from 0 below len
          for byte = (aref octets i)
          for pos = (* i 2)
          do (setf (char hex pos) (char "0123456789abcdef" (ash byte -4)))
             (setf (char hex (+ pos 1)) (char "0123456789abcdef" (logand byte #x0F))))
    hex))

(defun plug-session (conn)
  "Session plug: loads or creates a session.
Reads session cookie, verifies HMAC signature, loads session data from store.
Sets :session-id and :session assigns on conn.
Sets session cookie on response if new session created."
  (let* ((cookies (conn-get-assign conn :cookies))
         (cookie-value (cdr (assoc *session-cookie-name* cookies :test #'string=)))
         (session-id (when cookie-value (verify-session-cookie cookie-value)))
         (session-data nil)
         (new-session-p nil))
    ;; Try to load existing session
    (when session-id
      (setf session-data (gethash session-id *session-store*)))
    ;; Create new session if needed
    (unless session-data
      (setf session-id (cauldron.crypto:generate-token :length 32))
      (setf session-data (make-hash-table :test 'equal))
      (setf (gethash session-id *session-store*) session-data)
      (setf new-session-p t))
    ;; Store on conn
    (conn-put-assign conn :session-id session-id)
    (conn-put-assign conn :session session-data)
    ;; Set cookie if new session
    (when new-session-p
      (let ((signed (sign-session-id session-id)))
        (plug-set-cookie conn *session-cookie-name* signed
                         :path "/" :http-only t
                         :same-site "Lax"
                         :max-age *session-max-age*)))
    conn))

;;; Session data access helpers

(defun session-get (conn key)
  "Get a value from the current session."
  (let ((session (conn-get-assign conn :session)))
    (when session (gethash key session))))

(defun session-put (conn key value)
  "Set a value in the current session. Returns conn."
  (let ((session (conn-get-assign conn :session)))
    (when session (setf (gethash key session) value)))
  conn)

(defun session-delete (conn key)
  "Remove a key from the current session. Returns conn."
  (let ((session (conn-get-assign conn :session)))
    (when session (remhash key session)))
  conn)

(defun session-clear (conn)
  "Clear all data from the current session. Returns conn."
  (let ((session (conn-get-assign conn :session)))
    (when session (clrhash session)))
  conn)

;;; ============================================================
;;; CSRF protection
;;; ============================================================

(defun plug-csrf (conn)
  "CSRF protection plug.
- For GET/HEAD/OPTIONS: generates a CSRF token, stores in session, sets on conn.
- For POST/PUT/PATCH/DELETE: validates token from form body or header.
  Halts with 403 if token is missing or invalid."
  (let ((method (conn-method conn)))
    (if (member method '(:get :head :options))
        ;; Generate token for safe methods
        (let ((token (cauldron.crypto:generate-token :length 32)))
          (session-put conn "_csrf_token" token)
          (conn-put-assign conn :csrf-token token)
          ;; Wire to alembic form helpers
          (setf cauldron.alembic:*csrf-token* token)
          conn)
        ;; Validate token for unsafe methods
        (let* ((session-token (session-get conn "_csrf_token"))
               (submitted-token (or (get-body-param conn "_csrf_token")
                                    (cdr (assoc "X-CSRF-Token" (conn-headers conn)
                                                :test #'string-equal)))))
          (if (and session-token submitted-token
                   (cauldron.crypto:secure-equal session-token submitted-token))
              (progn
                ;; Re-set for template access in response
                (conn-put-assign conn :csrf-token session-token)
                (setf cauldron.alembic:*csrf-token* session-token)
                conn)
              (halt-conn conn :status 403
                              :body "Forbidden: Invalid CSRF token"))))))

(defun get-body-param (conn name)
  "Get a parameter from parsed form body (alist). Returns value or NIL."
  (let ((body (conn-body conn)))
    (when (and body (listp body))
      (cdr (assoc name body :test #'string=)))))

;;; ============================================================
;;; Flash messages
;;; ============================================================

(defun plug-flash (conn)
  "Flash message plug.
Loads flash messages from session into :flash assign, then clears them
from the session. Flash messages survive exactly one request."
  (let ((flash (session-get conn "_flash")))
    (conn-put-assign conn :flash flash)
    ;; Clear flash from session after reading
    (session-delete conn "_flash")
    conn))

(defun put-flash (conn key message)
  "Set a flash message for the next request.
KEY is typically :info, :error, :success, etc."
  (let ((flash (or (session-get conn "_flash") '())))
    (push (cons key message) flash)
    (session-put conn "_flash" flash))
  conn)

;;; ============================================================
;;; Static file serving
;;; ============================================================

(defvar *static-root* nil
  "Root directory for static files. Must be set before using plug-static.")

(defparameter *mime-types*
  '(("html" . "text/html; charset=utf-8")
    ("htm"  . "text/html; charset=utf-8")
    ("css"  . "text/css; charset=utf-8")
    ("js"   . "application/javascript; charset=utf-8")
    ("json" . "application/json")
    ("png"  . "image/png")
    ("jpg"  . "image/jpeg")
    ("jpeg" . "image/jpeg")
    ("gif"  . "image/gif")
    ("svg"  . "image/svg+xml")
    ("ico"  . "image/x-icon")
    ("woff" . "font/woff")
    ("woff2" . "font/woff2")
    ("ttf"  . "font/ttf")
    ("otf"  . "font/otf")
    ("eot"  . "application/vnd.ms-fontobject")
    ("map"  . "application/json")
    ("xml"  . "application/xml")
    ("txt"  . "text/plain; charset=utf-8")
    ("pdf"  . "application/pdf")
    ("zip"  . "application/zip")
    ("webp" . "image/webp")
    ("mp4"  . "video/mp4")
    ("webm" . "video/webm")
    ("mp3"  . "audio/mpeg")
    ("wav"  . "audio/wav"))
  "Map of file extensions to MIME types.")

(defun guess-mime-type (path)
  "Guess the MIME type from a file path extension."
  (let* ((dot-pos (position #\. path :from-end t))
         (ext (when dot-pos (string-downcase (subseq path (1+ dot-pos))))))
    (or (cdr (assoc ext *mime-types* :test #'string=))
        "application/octet-stream")))

(defun path-traversal-p (path)
  "Return T if PATH contains path traversal sequences."
  (or (search ".." path)
      (search "//" path)
      (and (> (length path) 0) (char= (char path 0) #\/))
      (search "~" path)))

(defun make-plug-static (prefix &key (root *static-root*))
  "Create a static file serving plug for files under PREFIX.
Returns a plug function. Files are served from ROOT directory.

Usage:
  (defpipeline :browser
    :plugs (list (make-plug-static \"/static\" :root \"/path/to/static/\")))"
  (lambda (conn)
    (let ((path (conn-path conn)))
      (if (and (eql (conn-method conn) :get)
               (>= (length path) (length prefix))
               (string= path prefix :end1 (length prefix)))
          ;; Strip prefix to get relative path
          (let ((relative (subseq path (length prefix))))
            ;; Remove leading slash
            (when (and (> (length relative) 0) (char= (char relative 0) #\/))
              (setf relative (subseq relative 1)))
            ;; Security: prevent path traversal
            (if (or (path-traversal-p relative)
                    (= (length relative) 0))
                conn  ; Let request continue to router
                ;; Try to serve file
                (let ((file-path (format nil "~A/~A" (string-right-trim "/" root) relative)))
                  (if (and (probe-file file-path)
                           (not (directory-pathname-p (pathname file-path))))
                      ;; Serve the file
                      (let ((content (read-file-octets file-path))
                            (mime (guess-mime-type file-path)))
                        (conn-put-status conn 200)
                        (conn-put-resp-header conn "Content-Type" mime)
                        (conn-put-resp-header conn "Cache-Control" "public, max-age=3600")
                        (conn-put-resp-body conn content)
                        (halt-conn conn))
                      ;; File not found — continue to router
                      conn))))
          ;; Not a static path — continue
          conn))))

(defun directory-pathname-p (pathname)
  "Return T if PATHNAME refers to a directory."
  (and (null (pathname-name pathname))
       (null (pathname-type pathname))))

(defun read-file-octets (path)
  "Read an entire file as an octet vector."
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let* ((size (file-length stream))
           (buffer (make-array size :element-type '(unsigned-byte 8))))
      (read-sequence buffer stream)
      buffer)))

;;; ============================================================
;;; Content-type plugs
;;; ============================================================

(defun plug-json-content-type (conn)
  "Set Content-Type to application/json."
  (conn-put-resp-header conn "Content-Type" "application/json; charset=utf-8")
  conn)

(defun plug-html-content-type (conn)
  "Set Content-Type to text/html; charset=utf-8."
  (conn-put-resp-header conn "Content-Type" "text/html; charset=utf-8")
  conn)

;;; ============================================================
;;; Redirect with flash
;;; ============================================================

(defun redirect-with-flash (conn path flash-type message)
  "Redirect to PATH with a flash message."
  (let ((conn (put-flash conn flash-type message)))
    (conn-put-status conn 303)
    (conn-put-resp-header conn "Location" path)
    (halt-conn conn)))

;;; ============================================================
;;; Security headers
;;; ============================================================

(defun plug-security-headers (conn)
  "Add standard security headers to the response."
  (conn-put-resp-header conn "X-Content-Type-Options" "nosniff")
  (conn-put-resp-header conn "X-Frame-Options" "DENY")
  (conn-put-resp-header conn "X-XSS-Protection" "0")
  (conn-put-resp-header conn "Referrer-Policy" "strict-origin-when-cross-origin")
  (conn-put-resp-header conn "Permissions-Policy"
                        "camera=(), microphone=(), geolocation=()")
  conn)
