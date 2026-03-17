;;;; context.lisp — Request-scoped context accumulator for canonical logging
;;;; Part of Phase 37B: Canonical Logging Module

(in-package :cauldron.logging)

(defvar *request-context* nil
  "Dynamic variable bound to a hash-table for the current request's log context.")

(defun make-context ()
  "Create a fresh log context (hash-table with keyword keys)."
  (make-hash-table :test 'eq))

(defun log-set (key value)
  "Set KEY to VALUE in the current request context. No-op if no context."
  (when *request-context*
    (setf (gethash key *request-context*) value)))

(defun log-inc (key &optional (delta 1))
  "Increment counter KEY by DELTA in the current request context."
  (when *request-context*
    (let ((current (or (gethash key *request-context*) 0)))
      (setf (gethash key *request-context*) (+ current delta)))))

(defun log-timing (key duration-ms)
  "Accumulate timing for KEY in the current request context."
  (when *request-context*
    (let ((current (or (gethash key *request-context*) 0)))
      (setf (gethash key *request-context*) (+ current duration-ms)))))

(defun log-emit ()
  "Format the current request context as a canonical log line and write it.
Sorts keys for deterministic output. Writes to *canonical-log-stream*."
  (when (and *request-context* *canonical-log-enabled*)
    (let* ((pairs (let ((result '()))
                    (maphash (lambda (k v) (push (cons k v) result))
                             *request-context*)
                    ;; Sort for deterministic output
                    (sort result #'string< :key (lambda (p) (symbol-name (car p))))))
           (line (format-canonical-line pairs)))
      (format *canonical-log-stream* "~&canonical-log-line ~A~%" line))))

(defmacro with-log-context ((&key initial-fields) &body body)
  "Execute BODY with a fresh request log context. Emits the canonical log line at the end.
INITIAL-FIELDS is an alist of (key . value) to pre-populate."
  (let ((ctx (gensym "CTX")))
    `(let* ((,ctx (make-context))
            (*request-context* ,ctx))
       ,@(when initial-fields
           `((loop for (k . v) in ,initial-fields
                   do (setf (gethash k ,ctx) v))))
       (unwind-protect (progn ,@body)
         (log-emit)))))
