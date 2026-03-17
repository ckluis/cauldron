;;;; test/crypto/sha256-test.lisp — SHA-256 tests (NIST CAVP vectors)
;;;; THE critical test. Everything depends on this.
(in-package :cauldron.test)

(defsuite :crypto-sha256)

;; hex-string is defined in sha1-test.lisp and available in this package

;;; NIST CAVP test vectors

(deftest test-sha256-empty
  (is-equal "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
            (hex-string (cauldron.crypto:sha256 ""))))

(deftest test-sha256-abc
  (is-equal "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
            (hex-string (cauldron.crypto:sha256 "abc"))))

(deftest test-sha256-448bit
  (is-equal "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
            (hex-string (cauldron.crypto:sha256
                         "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))))

(deftest test-sha256-million-a
  ;; 1 million 'a' characters
  (let ((input (make-string 1000000 :initial-element #\a)))
    (is-equal "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0"
              (hex-string (cauldron.crypto:sha256 input)))))

;;; Additional vectors

(deftest test-sha256-string-api
  ;; sha256-string should produce the same result
  (is-equalp (cauldron.crypto:sha256 "hello")
             (cauldron.crypto:sha256-string "hello")))

(deftest test-sha256-returns-32-bytes
  (is-equal 32 (length (cauldron.crypto:sha256 "test"))))

(deftest test-sha256-octets-input
  ;; Should accept octet vectors directly
  (let ((octets (make-array 3 :element-type '(unsigned-byte 8)
                              :initial-contents '(97 98 99)))) ; "abc"
    (is-equal "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
              (hex-string (cauldron.crypto:sha256 octets)))))
