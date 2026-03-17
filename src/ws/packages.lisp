;;;; src/ws/packages.lisp — Package definitions for the WebSocket layer

(defpackage :cauldron.ws
  (:use :cl)
  (:export
   ;; Upgrade
   #:ws-upgrade-p
   #:ws-accept-key
   #:ws-upgrade-response
   ;; Frames
   #:+opcode-continuation+
   #:+opcode-text+
   #:+opcode-binary+
   #:+opcode-close+
   #:+opcode-ping+
   #:+opcode-pong+
   #:ws-frame
   #:make-ws-frame
   #:ws-frame-fin
   #:ws-frame-opcode
   #:ws-frame-payload
   #:ws-read-frame
   #:ws-write-frame
   #:ws-read-message
   #:ws-write-message
   ;; Connection
   #:ws-connection
   #:make-ws-connection
   #:ws-connection-stream
   #:ws-connection-state
   #:ws-close
   #:ws-send
   #:ws-send-binary
   #:ws-ping
   #:ws-pong
   #:ws-handle-control-frame))
