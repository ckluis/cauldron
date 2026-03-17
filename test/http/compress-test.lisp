;;;; test/http/compress-test.lisp — Tests for DEFLATE/gzip compression
(in-package :cauldron.test)

(defsuite :http-compress)

(deftest test-crc32-known-value
  ;; CRC-32 of empty input is 0
  (is-equal 0 (cauldron.http:crc32 (make-array 0 :element-type '(unsigned-byte 8)))))

(deftest test-crc32-known-string
  ;; CRC-32 of "123456789" = 0xCBF43926
  (let ((octets (sb-ext:string-to-octets "123456789" :external-format :ascii)))
    (is-equal #xCBF43926 (cauldron.http:crc32 octets))))

(deftest test-gzip-header-bytes
  ;; gzip output starts with magic bytes 1F 8B
  (let* ((input (sb-ext:string-to-octets "Hello" :external-format :utf-8))
         (compressed (cauldron.http:gzip-compress input)))
    (is-equal #x1F (aref compressed 0))
    (is-equal #x8B (aref compressed 1))
    (is-equal #x08 (aref compressed 2))))

(deftest test-deflate-output-nonempty
  ;; DEFLATE should produce some output for non-empty input
  (let* ((input (sb-ext:string-to-octets "Hello, World!" :external-format :utf-8))
         (compressed (cauldron.http:deflate-compress input)))
    (is-true (> (length compressed) 0))))

(deftest test-deflate-empty-input
  ;; DEFLATE of empty input should produce small but valid output
  (let* ((input (make-array 0 :element-type '(unsigned-byte 8)))
         (compressed (cauldron.http:deflate-compress input)))
    (is-true (> (length compressed) 0))))
