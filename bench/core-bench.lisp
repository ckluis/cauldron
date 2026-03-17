;;;; bench/core-bench.lisp — Core benchmarks for Cauldron
;;;; Benchmarks JSON, crypto, HTTP parsing, query builder, and token generation.

(in-package :cauldron.bench)

;;; ===================================================================
;;; JSON Benchmarks
;;; ===================================================================

;; 1KB object: small hash table with string keys and mixed values
(let ((small-obj (let ((ht (make-hash-table :test 'equal)))
                   (setf (gethash "name" ht) "John Doe")
                   (setf (gethash "age" ht) 42)
                   (setf (gethash "email" ht) "john@example.com")
                   (setf (gethash "active" ht) t)
                   (setf (gethash "score" ht) 98.6)
                   (setf (gethash "bio" ht) "A somewhat longer string that adds some size to the object for realistic benchmarking purposes. This simulates a typical API response field.")
                   (setf (gethash "tags" ht) (list "user" "admin" "verified"))
                   (setf (gethash "address" ht)
                         (let ((addr (make-hash-table :test 'equal)))
                           (setf (gethash "street" addr) "123 Main St")
                           (setf (gethash "city" addr) "Springfield")
                           (setf (gethash "state" addr) "IL")
                           (setf (gethash "zip" addr) "62701")
                           addr))
                   ht)))
  (defbench json-encode-1kb 5000
    "Encode a ~1KB JSON object"
    (cauldron.json:encode small-obj))

  (let ((json-str (cauldron.json:encode small-obj)))
    (defbench json-decode-1kb 5000
      "Decode a ~1KB JSON string"
      (cauldron.json:decode json-str))))

;; 10KB array: list of 50 small objects
(let* ((items (loop for i from 1 to 50
                    collect (let ((ht (make-hash-table :test 'equal)))
                              (setf (gethash "id" ht) i)
                              (setf (gethash "name" ht) (format nil "Item ~D" i))
                              (setf (gethash "price" ht) (* i 9.99))
                              (setf (gethash "in_stock" ht) (oddp i))
                              (setf (gethash "category" ht) "widgets")
                              ht))))
  (defbench json-encode-10kb 1000
    "Encode a ~10KB JSON array (50 objects)"
    (cauldron.json:encode items))

  (let ((json-str (cauldron.json:encode items)))
    (defbench json-decode-10kb 1000
      "Decode a ~10KB JSON string (50 objects)"
      (cauldron.json:decode json-str))))

;;; ===================================================================
;;; Crypto Benchmarks
;;; ===================================================================

(let ((data-100 (make-array 100 :element-type '(unsigned-byte 8) :initial-element 65)))
  (defbench sha256-100b 10000
    "SHA-256 hash of 100 bytes"
    (cauldron.crypto:sha256 data-100)))

(let ((data-1000 (make-array 1000 :element-type '(unsigned-byte 8) :initial-element 66)))
  (defbench sha256-1000b 5000
    "SHA-256 hash of 1000 bytes"
    (cauldron.crypto:sha256 data-1000)))

;;; ===================================================================
;;; Base64 Benchmarks
;;; ===================================================================

(let ((data-256 (make-array 256 :element-type '(unsigned-byte 8) :initial-element 77)))
  (defbench base64-encode-256b 10000
    "Base64 encode 256 bytes"
    (cauldron.crypto:base64-encode data-256))

  (let ((encoded (cauldron.crypto:base64-encode data-256)))
    (defbench base64-decode-256b 10000
      "Base64 decode ~344 char string"
      (cauldron.crypto:base64-decode encoded))))

;;; ===================================================================
;;; HTTP Request Parsing Benchmarks
;;; ===================================================================

(defun %octets-from-string (string)
  "Convert STRING to an octet vector using char-code (ASCII)."
  (let ((octets (make-array (length string) :element-type '(unsigned-byte 8))))
    (loop for i from 0 below (length string)
          do (setf (aref octets i) (char-code (char string i))))
    octets))

(defclass octet-input-stream (sb-gray:fundamental-binary-input-stream)
  ((data :initarg :data :type (simple-array (unsigned-byte 8) (*)))
   (position :initform 0 :type fixnum))
  (:documentation "Simple binary input stream backed by an octet vector."))

(defmethod sb-gray:stream-read-byte ((stream octet-input-stream))
  (with-slots (data position) stream
    (if (< position (length data))
        (prog1 (aref data position)
          (incf position))
        :eof)))

(defmethod sb-gray:stream-read-sequence ((stream octet-input-stream) sequence start end &key)
  (with-slots (data position) stream
    (let* ((available (- (length data) position))
           (requested (- end start))
           (to-copy (min available requested)))
      (replace sequence data
               :start1 start :end1 (+ start to-copy)
               :start2 position :end2 (+ position to-copy))
      (incf position to-copy)
      (+ start to-copy))))

(let ((get-request-octets (%octets-from-string
                           (format nil "GET /api/v1/users?page=1&limit=20 HTTP/1.1~C~CHost: example.com~C~CAccept: application/json~C~CAuthorization: Bearer tok_abc123~C~C~C~C"
                                   #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed))))
  (defbench http-parse-get 5000
    "Parse simple GET request with 3 headers"
    (let ((stream (make-instance 'octet-input-stream :data get-request-octets)))
      (cauldron.http:parse-request stream))))

(let* ((body-str "{\"name\":\"John\",\"email\":\"john@example.com\",\"age\":42}")
       (post-request-octets (%octets-from-string
                             (format nil "POST /api/v1/users HTTP/1.1~C~CHost: example.com~C~CContent-Type: application/json~C~CContent-Length: ~D~C~C~C~C~A"
                                     #\Return #\Linefeed #\Return #\Linefeed (length body-str) #\Return #\Linefeed #\Return #\Linefeed body-str))))
  (defbench http-parse-post 5000
    "Parse POST request with JSON body"
    (let ((stream (make-instance 'octet-input-stream :data post-request-octets)))
      (cauldron.http:parse-request stream))))

;;; ===================================================================
;;; Query Builder Benchmarks
;;; ===================================================================

(defbench query-chain 5000
  "Build query: from -> where -> order -> limit -> to-sql"
  (let ((q (cauldron.grimoire:from "users")))
    (setf q (cauldron.grimoire:where-clause q '(:= "status" "active")))
    (setf q (cauldron.grimoire:order-by q '("created_at" :desc)))
    (setf q (cauldron.grimoire:limit-query q 25))
    (cauldron.grimoire:to-sql q)))

(defbench query-complex-chain 2000
  "Build complex query: from -> select -> where x3 -> join -> order -> group -> limit -> to-sql"
  (let ((q (cauldron.grimoire:from "orders")))
    (setf q (cauldron.grimoire:select-fields q "orders.id" "users.name" "orders.total"))
    (setf q (cauldron.grimoire:join-query q :inner "users" "orders.user_id = users.id"))
    (setf q (cauldron.grimoire:where-clause q '(:= "orders.status" "completed")))
    (setf q (cauldron.grimoire:where-clause q '(:> "orders.total" 100)))
    (setf q (cauldron.grimoire:where-clause q '(:<> "users.role" "admin")))
    (setf q (cauldron.grimoire:order-by q '("orders.total" :desc)))
    (setf q (cauldron.grimoire:group-by q "users.name"))
    (setf q (cauldron.grimoire:limit-query q 50))
    (cauldron.grimoire:to-sql q)))

;;; ===================================================================
;;; Token Generation Benchmark
;;; ===================================================================

(defbench generate-token-32 1000
  "Generate 32-byte random hex token (64 chars)"
  (cauldron.crypto:generate-token :length 32))

(defbench generate-token-16 1000
  "Generate 16-byte random hex token (32 chars) — request ID size"
  (cauldron.crypto:generate-token :length 16))
