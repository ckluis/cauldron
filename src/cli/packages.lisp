;;;; src/cli/packages.lisp — Package definitions for CLI infrastructure
(defpackage :cauldron.cli
  (:use :cl)
  (:export
   ;; Command definition
   #:defcommand
   #:*commands*
   #:find-command
   ;; Argument parsing
   #:parse-args
   #:get-arg
   #:get-flag
   #:get-positional
   ;; Command dispatch
   #:dispatch-command
   #:run-cli
   #:suggest-command
   #:print-command-usage
   ;; Output
   #:emit-json
   #:emit-table
   #:emit
   #:emit-error
   #:*output-format*
   #:*emit-start-time*
   #:with-timing
   ;; DB pool
   #:*cli-pool*
   #:with-cli-pool))
