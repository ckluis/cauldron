;;;; src/runtime/packages.lisp — Package definitions for the runtime layer

(defpackage :cauldron.runtime
  (:use :cl)
  (:export
   ;; Utilities
   #:ht
   #:ht-get
   #:split-sequence
   #:string-prefix-p
   #:string-suffix-p
   #:when-let
   #:if-let
   #:ensure-list
   #:make-keyword
   #:octets-to-integer
   #:integer-to-octets
   ;; Threads
   #:spawn
   #:current-thread
   #:thread-alive-p
   #:thread-name
   #:kill-thread
   #:join-thread
   #:make-lock
   #:with-lock
   #:make-semaphore
   #:signal-semaphore
   #:wait-semaphore
   ;; Queue
   #:make-queue
   #:enqueue
   #:dequeue
   #:queue-empty-p
   #:queue-length
   ;; Thread pool
   #:make-thread-pool
   #:submit-task
   #:shutdown-pool
   #:thread-pool-active-count
   #:thread-pool-queued-count
   ;; Sockets
   #:make-tcp-listener
   #:accept-connection
   #:make-tcp-connection
   #:socket-close
   #:socket-read
   #:socket-write
   #:socket-read-line
   #:socket-read-sequence
   #:socket-write-sequence
   #:socket-peer-address
   ;; Timer
   #:make-scheduler
   #:schedule-once
   #:schedule-recurring
   #:cancel-timer
   #:shutdown-scheduler
   ;; Config
   #:get-env
   #:coerce-config-value
   #:defconfig
   #:validate-config
   #:config-summary
   ;; Shutdown
   #:*shutdown-hooks*
   #:register-shutdown-hook
   #:graceful-shutdown
   #:install-signal-handlers
   ;; Request ID
   #:generate-request-id
   #:*current-request-id*))
