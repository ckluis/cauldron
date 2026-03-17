;;;; packages.lisp — Package definition for canonical structured logging
;;;; Part of Phase 37B: Canonical Logging Module

(defpackage :cauldron.logging
  (:use :cl)
  (:export
   ;; Format
   #:format-canonical-line
   #:parse-canonical-line
   ;; Context
   #:*request-context*
   #:make-context
   #:log-set
   #:log-inc
   #:log-timing
   #:log-emit
   #:with-log-context
   ;; Stream
   #:*canonical-log-stream*
   #:*canonical-log-enabled*))
