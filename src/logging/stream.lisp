;;;; stream.lisp — Output target configuration for canonical logging
;;;; Part of Phase 37B: Canonical Logging Module

(in-package :cauldron.logging)

(defvar *canonical-log-stream* *standard-output*
  "Stream to write canonical log lines to. Defaults to stdout.")

(defvar *canonical-log-enabled* t
  "When NIL, canonical logging is suppressed entirely.")
