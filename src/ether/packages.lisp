;;;; src/ether/packages.lisp — Package definitions for the PubSub layer
(defpackage :cauldron.ether
  (:use :cl)
  (:export
   ;; In-process PubSub
   #:*pubsub*
   #:make-pubsub
   #:subscribe
   #:unsubscribe
   #:broadcast
   #:pubsub-topics
   #:pubsub-subscriber-count
   ;; PG LISTEN/NOTIFY
   #:start-pg-pubsub
   #:stop-pg-pubsub))
