;;;; src/scry/packages.lisp — Package definitions for the LiveView layer
(defpackage :cauldron.scry
  (:use :cl)
  (:export
   ;; Diff engine
   #:diff-trees
   #:apply-diff
   ;; Lifecycle
   #:defscry
   #:scry-session
   #:make-scry-session
   #:scry-session-id
   #:scry-session-assigns
   #:scry-session-view
   #:scry-session-socket
   #:scry-session-subscriptions
   #:scry-mount
   #:scry-render
   #:scry-handle-event
   ;; Transport
   #:scry-ws-handler
   #:encode-patches
   #:decode-client-message))
