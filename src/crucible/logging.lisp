;;;; src/crucible/logging.lisp — Request logging for Crucible
(in-package :cauldron.crucible)

(defvar *request-log-stream* *standard-output*
  "Stream to write request logs to.")

(defvar *request-log-enabled* t
  "When NIL, request logging is suppressed.")

(defun plug-request-log (conn)
  "Record request start time in :request-start-time assign."
  (conn-put-assign conn :request-start-time (get-internal-real-time)))

(defun log-request-completion (conn)
  "Log method, path, status, duration-ms to *request-log-stream*.
Reads :request-start-time from assigns. No-op if logging disabled.
Also populates canonical log context with duration_ms."
  (let* ((start (conn-get-assign conn :request-start-time))
         (now (get-internal-real-time))
         (duration-ms (if start
                          (round (* 1000 (/ (- now start)
                                            internal-time-units-per-second)))
                          0)))
    ;; Feed duration into canonical logging context
    (cauldron.logging:log-set :duration-ms duration-ms)
    (when *request-log-enabled*
      (let ((method (conn-method conn))
            (path (conn-path conn))
            (status (or (conn-status conn) 200)))
        (format *request-log-stream* "~&~A~%"
                (format-request-log method path status duration-ms)))))
  conn)

(defun format-request-log (method path status duration-ms)
  "Format log line. Returns string like: GET /users 200 12ms"
  (format nil "~A ~A ~D ~Dms" (string-upcase (string method)) path status duration-ms))
