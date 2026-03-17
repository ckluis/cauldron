;;;; src/integration/retry.lisp — Retry with exponential backoff
(in-package :cauldron.integration)

(defun compute-backoff-ms (attempt base-delay-ms max-delay-ms)
  "Compute backoff delay: min(max, base * 2^attempt) + random 0-25% jitter."
  (let* ((exponential (* base-delay-ms (expt 2 attempt)))
         (capped (min exponential max-delay-ms))
         (jitter (random (max 1 (floor capped 4)))))
    (+ capped jitter)))

(defun retryable-status-p (status)
  "Return T if HTTP STATUS is retryable (server error or rate-limited)."
  (member status '(429 500 502 503 504) :test #'=))

(defmacro with-retry ((&key (max-retries 3) (base-delay-ms 500) (max-delay-ms 30000)) &body body)
  "Execute BODY, retrying on retryable HTTP status codes.
BODY must return (values result status ...). Retries on retryable status.
Returns the final values from BODY."
  (let ((attempt (gensym "ATTEMPT"))
        (all-vals (gensym "VALS"))
        (status (gensym "STATUS")))
    `(loop for ,attempt from 0 to ,max-retries
           for ,all-vals = (multiple-value-list (progn ,@body))
           for ,status = (second ,all-vals)
           when (or (= ,attempt ,max-retries)
                    (not (retryable-status-p ,status)))
             return (values-list ,all-vals)
           do (sleep (/ (compute-backoff-ms ,attempt ,base-delay-ms ,max-delay-ms) 1000.0)))))
