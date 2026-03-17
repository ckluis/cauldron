;;;; test/crypto/sha1-test.lisp — SHA-1 tests (FIPS 180-4 vectors)
(in-package :cauldron.test)

(defsuite :crypto-sha1)

(defun hex-string (octets)
  "Convert octet vector to lowercase hex string."
  (with-output-to-string (s)
    (loop for byte across octets
          do (format s "~(~2,'0x~)" byte))))

;;; FIPS 180-4 test vectors

(deftest test-sha1-empty
  (is-equal "da39a3ee5e6b4b0d3255bfef95601890afd80709"
            (hex-string (cauldron.crypto:sha1 ""))))

(deftest test-sha1-abc
  (is-equal "a9993e364706816aba3e25717850c26c9cd0d89d"
            (hex-string (cauldron.crypto:sha1 "abc"))))

(deftest test-sha1-448bit
  (is-equal "84983e441c3bd26ebaae4aa1f95129e5e54670f1"
            (hex-string (cauldron.crypto:sha1
                         "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))))
