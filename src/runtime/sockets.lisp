;;;; src/runtime/sockets.lisp — TCP socket wrappers around sb-bsd-sockets
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-bsd-sockets))
(in-package :cauldron.runtime)

(defun %parse-host (host)
  "Convert a host string like \"127.0.0.1\" to the vector #(127 0 0 1).
Accepts both string and vector forms."
  (etypecase host
    (string
     (if (string= host "0.0.0.0")
         #(0 0 0 0)
         (let ((parts (split-sequence #\. host)))
           (make-array 4 :element-type '(unsigned-byte 8)
                         :initial-contents (mapcar #'parse-integer parts)))))
    (vector host)))

(defun %make-socket-stream (socket)
  "Create a bidirectional binary/character stream from a socket's file descriptor."
  (sb-bsd-sockets:socket-make-stream
   socket
   :input t
   :output t
   :buffering :full
   :element-type '(unsigned-byte 8)))

(defun make-tcp-listener (host port &key (backlog 128))
  "Create a TCP server socket bound to HOST:PORT and listening with BACKLOG.
HOST is a string like \"0.0.0.0\" or \"127.0.0.1\".
Returns the listener socket."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream
                               :protocol :tcp)))
    ;; Allow address reuse to avoid bind errors on restart
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket (%parse-host host) port)
    (sb-bsd-sockets:socket-listen socket backlog)
    socket))

(defun accept-connection (listener)
  "Accept a connection on LISTENER socket.
Returns (VALUES client-socket stream) where stream is a bidirectional
binary stream suitable for reading/writing octets."
  (let ((client-socket (sb-bsd-sockets:socket-accept listener)))
    (let ((stream (%make-socket-stream client-socket)))
      (values client-socket stream))))

(defun make-tcp-connection (host port)
  "Open a TCP connection to HOST:PORT.
Returns (VALUES socket stream)."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream
                               :protocol :tcp)))
    (sb-bsd-sockets:socket-connect socket (%parse-host host) port)
    (let ((stream (%make-socket-stream socket)))
      (values socket stream))))

(defun socket-close (socket)
  "Close SOCKET safely. Ignores errors if the socket is already closed."
  (handler-case
      (sb-bsd-sockets:socket-close socket)
    (error () nil)))

(defun socket-read-line (stream)
  "Read a CRLF-terminated line from STREAM (a socket stream).
Returns the line as a string without the trailing CRLF.
Returns NIL on end-of-stream."
  (let ((bytes (make-array 0 :element-type '(unsigned-byte 8)
                             :adjustable t :fill-pointer 0)))
    (handler-case
        (loop
          (let ((byte (read-byte stream)))
            (when (= byte 13) ; CR
              (let ((next (read-byte stream nil nil)))
                (when (and next (= next 10)) ; LF
                  (return (sb-ext:octets-to-string bytes :external-format :utf-8)))
                ;; If not LF, include the CR and the next byte
                (vector-push-extend byte bytes)
                (when next (vector-push-extend next bytes))))
            (unless (= byte 13)
              (vector-push-extend byte bytes))))
      (end-of-file ()
        (if (> (length bytes) 0)
            (sb-ext:octets-to-string bytes :external-format :utf-8)
            nil)))))

(defun socket-read-sequence (stream length)
  "Read exactly LENGTH bytes from STREAM into a fresh octet vector.
Returns the octet vector. Signals an error if fewer bytes are available."
  (let ((buffer (make-array length :element-type '(unsigned-byte 8))))
    (let ((bytes-read (read-sequence buffer stream)))
      (unless (= bytes-read length)
        (error "Short read: expected ~D bytes but got ~D" length bytes-read))
      buffer)))

(defun socket-write-sequence (stream data)
  "Write the octet vector DATA to STREAM and flush."
  (write-sequence data stream)
  (force-output stream)
  (values))

(defun socket-read (stream buffer &key (start 0) end)
  "Read bytes from STREAM into BUFFER between START and END.
Returns the number of bytes actually read."
  (let ((end (or end (length buffer))))
    (read-sequence buffer stream :start start :end end)))

(defun socket-write (stream buffer &key (start 0) end)
  "Write bytes from BUFFER to STREAM between START and END. Flushes output."
  (let ((end (or end (length buffer))))
    (write-sequence buffer stream :start start :end end)
    (force-output stream)
    (- end start)))

(defun socket-peer-address (socket)
  "Return (VALUES host-string port) of the remote peer connected to SOCKET."
  (multiple-value-bind (address port)
      (sb-bsd-sockets:socket-peername socket)
    (values (format nil "~{~D~^.~}" (coerce address 'list))
            port)))
