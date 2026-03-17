;;;; src/http-client/client.lisp — Outbound HTTP client via curl
(in-package :cauldron.http-client)

(defvar *log-external-call-fn* nil
  "When non-nil, called after each HTTP request with a plist:
  (:url url :method method :status status-code :duration-ms milliseconds).
  Useful for canonical logging of external service calls.")

;;; ---------------------------------------------------------------------------
;;; Internal helpers
;;; ---------------------------------------------------------------------------

(defun %read-stream-to-string (stream)
  "Read all characters from STREAM and return as a string."
  (let ((result (make-array 0 :element-type 'character
                              :adjustable t :fill-pointer 0)))
    (loop for c = (read-char stream nil nil)
          while c do (vector-push-extend c result))
    (coerce result 'string)))

(defun %method-string (method)
  "Convert a keyword method to the curl -X string."
  (ecase method
    (:get    "GET")
    (:post   "POST")
    (:put    "PUT")
    (:patch  "PATCH")
    (:delete "DELETE")))

(defun %parse-response-headers (header-block)
  "Parse raw HTTP response headers into an alist.
HEADER-BLOCK is the string before the first blank line in curl -i output.
Returns an alist of (name . value) pairs. Skips the status line."
  (let ((headers '()))
    (with-input-from-string (s header-block)
      (loop for line = (read-line s nil nil)
            while line
            for trimmed = (string-trim '(#\Return) line)
            ;; Skip empty lines and the HTTP status line (e.g. HTTP/1.1 200 OK)
            when (and (plusp (length trimmed))
                      (not (and (>= (length trimmed) 5)
                                (string= "HTTP/" trimmed :end2 5))))
              do (let ((colon-pos (position #\: trimmed)))
                   (when colon-pos
                     (push (cons (string-trim " " (subseq trimmed 0 colon-pos))
                                 (string-trim " " (subseq trimmed (1+ colon-pos))))
                           headers)))))
    (nreverse headers)))

(defun %split-headers-and-body (raw-output)
  "Split curl -i output into (values header-block body-string).
Headers and body are separated by a blank line (CRLFCRLF or LFLF).
Handles multiple header blocks (e.g. 100 Continue) by finding the last
header/body separator before the final body content."
  ;; Try CRLFCRLF first, then LFLF
  (let ((sep (search (coerce '(#\Return #\Newline #\Return #\Newline) 'string)
                     raw-output)))
    (if sep
        (values (subseq raw-output 0 sep)
                (subseq raw-output (+ sep 4)))
        ;; Fallback to LFLF
        (let ((sep2 (search (coerce '(#\Newline #\Newline) 'string)
                            raw-output)))
          (if sep2
              (values (subseq raw-output 0 sep2)
                      (subseq raw-output (+ sep2 2)))
              ;; No separator found — treat everything as body
              (values "" raw-output))))))

;;; ---------------------------------------------------------------------------
;;; Core request function
;;; ---------------------------------------------------------------------------

(defun http-request (url &key (method :get) headers body timeout)
  "General HTTP client via curl.
URL is the target URL string.
METHOD is a keyword — :GET, :POST, :PUT, :PATCH, or :DELETE (default :GET).
HEADERS is an alist of (header-name . header-value) pairs.
BODY is an optional request body string.
TIMEOUT is an optional timeout in seconds (integer).

Returns (values body-string status-code response-headers-alist)."
  (let* ((start-time (get-internal-real-time))
         (header-args (loop for (k . v) in headers
                            collect "-H"
                            collect (format nil "~A: ~A" k v)))
         (timeout-args (when timeout
                         (list "--max-time" (format nil "~A" timeout))))
         (body-args (when body
                      (list "-d" "@-")))
         (args (append (list "curl" "-s" "-i" "-w" "\n%{http_code}" "-X" (%method-string method))
                       header-args
                       timeout-args
                       body-args
                       (list url)))
         (process (sb-ext:run-program (first args) (rest args)
                                      :input (if body :stream nil)
                                      :output :stream
                                      :error :stream
                                      :wait nil
                                      :search t)))
    ;; Write body to stdin if provided
    (when body
      (let ((stdin (sb-ext:process-input process)))
        (write-string body stdin)
        (finish-output stdin)
        (close stdin)))
    (sb-ext:process-wait process)
    (let* ((raw-output (%read-stream-to-string (sb-ext:process-output process)))
           (_error-output (%read-stream-to-string (sb-ext:process-error process)))
           ;; Last line is the HTTP status code from -w
           (last-newline (position #\Newline raw-output :from-end t))
           (status-str (if last-newline
                           (subseq raw-output (1+ last-newline))
                           "0"))
           (status (or (parse-integer status-str :junk-allowed t) 0))
           ;; Everything before the status code line is headers+body from -i
           (output-without-status (if last-newline
                                      (subseq raw-output 0 last-newline)
                                      raw-output)))
      (declare (ignore _error-output))
      (sb-ext:process-close process)
      (multiple-value-bind (header-block body-string)
          (%split-headers-and-body output-without-status)
        (let* ((response-headers (%parse-response-headers header-block))
               (end-time (get-internal-real-time))
               (duration-ms (round (* 1000 (/ (- end-time start-time)
                                              internal-time-units-per-second)))))
          ;; Logging hook
          (when *log-external-call-fn*
            (funcall *log-external-call-fn*
                     (list :url url
                           :method method
                           :status status
                           :duration-ms duration-ms)))
          (values body-string status response-headers))))))

;;; ---------------------------------------------------------------------------
;;; Convenience wrappers
;;; ---------------------------------------------------------------------------

(defun http-get (url &key headers timeout)
  "Perform an HTTP GET request. See HTTP-REQUEST for return values."
  (http-request url :method :get :headers headers :timeout timeout))

(defun http-post (url &key headers body timeout)
  "Perform an HTTP POST request. See HTTP-REQUEST for return values."
  (http-request url :method :post :headers headers :body body :timeout timeout))

(defun http-put (url &key headers body timeout)
  "Perform an HTTP PUT request. See HTTP-REQUEST for return values."
  (http-request url :method :put :headers headers :body body :timeout timeout))

(defun http-patch (url &key headers body timeout)
  "Perform an HTTP PATCH request. See HTTP-REQUEST for return values."
  (http-request url :method :patch :headers headers :body body :timeout timeout))

(defun http-delete (url &key headers body timeout)
  "Perform an HTTP DELETE request. See HTTP-REQUEST for return values."
  (http-request url :method :delete :headers headers :body body :timeout timeout))

;;; ---------------------------------------------------------------------------
;;; JSON convenience
;;; ---------------------------------------------------------------------------

(defun http-request-json (url &key (method :get) headers body timeout)
  "HTTP request with automatic JSON encoding/decoding.
BODY, if a hash-table or list, is auto-encoded via CAULDRON.JSON:ENCODE.
If BODY is a string, it is sent as-is.
Content-Type: application/json is auto-added to headers.
The response body is auto-decoded via CAULDRON.JSON:DECODE.

Returns (values parsed-json status-code response-headers-alist)."
  (let* ((json-headers (if (assoc "Content-Type" headers :test #'string-equal)
                           headers
                           (cons '("Content-Type" . "application/json") headers)))
         (body-string (cond
                        ((null body) nil)
                        ((stringp body) body)
                        (t (cauldron.json:encode body)))))
    (multiple-value-bind (response-body status response-headers)
        (http-request url :method method :headers json-headers
                          :body body-string :timeout timeout)
      (let ((parsed (if (and response-body (plusp (length response-body)))
                        (handler-case (cauldron.json:decode response-body)
                          (error () response-body))
                        response-body)))
        (values parsed status response-headers)))))

;;; ---------------------------------------------------------------------------
;;; Backward-compatible wrapper
;;; ---------------------------------------------------------------------------

(defun http-post-json (url headers body-string)
  "POST JSON via curl. Returns (values response-string status-code).
HEADERS is an alist of (header-name . header-value) pairs.
Backward-compatible — delegates to HTTP-REQUEST."
  (multiple-value-bind (body status _response-headers)
      (http-request url :method :post :headers headers :body body-string)
    (declare (ignore _response-headers))
    (values body status)))
