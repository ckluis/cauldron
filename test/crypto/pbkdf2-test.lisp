;;;; test/crypto/pbkdf2-test.lisp — PBKDF2-SHA-256 tests (RFC 6070 vectors)
(in-package :cauldron.test)

(defsuite :crypto-pbkdf2)

;;; RFC 6070 test vectors (adapted for SHA-256)
;;; Note: RFC 6070 specifies PBKDF2-HMAC-SHA1, but we use the
;;; equivalent SHA-256 vectors from RFC 7914 and other sources.

(deftest test-pbkdf2-c1
  ;; password="password", salt="salt", c=1, dkLen=32
  (let* ((salt (make-array 4 :element-type '(unsigned-byte 8)
                             :initial-contents (map 'list #'char-code "salt")))
         (result (hex-string
                  (cauldron.crypto:pbkdf2-sha256 "password" salt
                                                  :iterations 1
                                                  :key-length 32))))
    (is-equal "120fb6cffcf8b32c43e7225256c4f837a86548c92ccc35480805987cb70be17b"
              result)))

(deftest test-pbkdf2-c2
  ;; password="password", salt="salt", c=2, dkLen=32
  (let* ((salt (make-array 4 :element-type '(unsigned-byte 8)
                             :initial-contents (map 'list #'char-code "salt")))
         (result (hex-string
                  (cauldron.crypto:pbkdf2-sha256 "password" salt
                                                  :iterations 2
                                                  :key-length 32))))
    (is-equal "ae4d0c95af6b46d32d0adff928f06dd02a303f8ef3c251dfd6e2d85a95474c43"
              result)))

(deftest test-pbkdf2-c4096
  ;; password="password", salt="salt", c=4096, dkLen=32
  (let* ((salt (make-array 4 :element-type '(unsigned-byte 8)
                             :initial-contents (map 'list #'char-code "salt")))
         (result (hex-string
                  (cauldron.crypto:pbkdf2-sha256 "password" salt
                                                  :iterations 4096
                                                  :key-length 32))))
    (is-equal "c5e478d59288c841aa530db6845c4c8d962893a001ce4e11a4963873aa98134a"
              result)))

(deftest test-pbkdf2-returns-correct-length
  (let* ((salt (make-array 4 :element-type '(unsigned-byte 8)
                             :initial-contents '(1 2 3 4)))
         (result (cauldron.crypto:pbkdf2-sha256 "pass" salt
                                                 :iterations 1
                                                 :key-length 20)))
    (is-equal 20 (length result))))
