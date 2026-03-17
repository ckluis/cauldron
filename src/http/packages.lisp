;;;; src/http/packages.lisp — Package definitions for the HTTP/1.1 server

(defpackage :cauldron.http
  (:use :cl)
  (:export
   ;; Request
   #:request
   #:make-request
   #:request-method
   #:request-path
   #:request-query-string
   #:request-http-version
   #:request-headers
   #:request-body
   #:request-content-type
   #:request-content-length
   #:request-header
   #:request-remote-addr
   #:parse-request
   ;; Response
   #:response
   #:make-response
   #:response-status
   #:response-headers
   #:response-body
   #:write-response
   #:status-text
   ;; Server
   #:start-server
   #:stop-server
   #:server-running-p
   ;; Utilities
   #:parse-query-string
   #:url-decode
   ;; Compression
   #:deflate-compress
   #:gzip-compress
   #:crc32
   ;; Multipart
   #:multipart-part
   #:make-multipart-part
   #:multipart-part-name
   #:multipart-part-filename
   #:multipart-part-content-type
   #:multipart-part-headers
   #:multipart-part-body
   #:extract-boundary
   #:parse-multipart))
