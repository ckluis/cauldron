;;;; test/crypto/hmac-test.lisp — HMAC-SHA-256 tests (RFC 4231 vectors)
(in-package :cauldron.test)

(defsuite :crypto-hmac)

(defun make-octet-vector (&rest bytes)
  "Create an octet vector from a list of bytes."
  (make-array (length bytes) :element-type '(unsigned-byte 8)
                              :initial-contents bytes))

(defun make-repeated-byte (byte count)
  "Create an octet vector of COUNT copies of BYTE."
  (make-array count :element-type '(unsigned-byte 8)
                     :initial-element byte))

;;; RFC 4231 Test Case 1
(deftest test-hmac-tc1
  ;; Key = 20 bytes of 0x0b
  ;; Data = "Hi There"
  (let* ((key (make-repeated-byte #x0b 20))
         (data "Hi There")
         (result (hex-string (cauldron.crypto:hmac-sha256 key data))))
    (is-equal "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7"
              result)))

;;; RFC 4231 Test Case 2
(deftest test-hmac-tc2
  ;; Key = "Jefe"
  ;; Data = "what do ya want for nothing?"
  (let ((result (hex-string (cauldron.crypto:hmac-sha256 "Jefe" "what do ya want for nothing?"))))
    (is-equal "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843"
              result)))

;;; RFC 4231 Test Case 3
(deftest test-hmac-tc3
  ;; Key = 20 bytes of 0xaa
  ;; Data = 50 bytes of 0xdd
  (let* ((key (make-repeated-byte #xaa 20))
         (data (make-repeated-byte #xdd 50))
         (result (hex-string (cauldron.crypto:hmac-sha256 key data))))
    (is-equal "773ea91e36800e46854db8ebd09181a72959098b3ef8c122d9635514ced565fe"
              result)))

;;; RFC 4231 Test Case 4
(deftest test-hmac-tc4
  ;; Key = 0x0102...19 (25 bytes)
  ;; Data = 50 bytes of 0xcd
  (let* ((key (make-array 25 :element-type '(unsigned-byte 8)
                             :initial-contents (loop for i from 1 to 25 collect i)))
         (data (make-repeated-byte #xcd 50))
         (result (hex-string (cauldron.crypto:hmac-sha256 key data))))
    (is-equal "82558a389a443c0ea4cc819899f2083a85f0faa3e578f8077a2e3ff46729665b"
              result)))

;;; RFC 4231 Test Case 6 (key > block size)
(deftest test-hmac-tc6
  ;; Key = 131 bytes of 0xaa (longer than SHA-256 block size)
  ;; Data = "Test Using Larger Than Block-Size Key - Hash Key First"
  (let* ((key (make-repeated-byte #xaa 131))
         (data "Test Using Larger Than Block-Size Key - Hash Key First")
         (result (hex-string (cauldron.crypto:hmac-sha256 key data))))
    (is-equal "60e431591ee0b67f0d8a26aacbf5b77f8e0bc6213728c5140546040f0ee37f54"
              result)))

;;; RFC 4231 Test Case 7 (key > block size, different data)
(deftest test-hmac-tc7
  (let* ((key (make-repeated-byte #xaa 131))
         (data "This is a test using a larger than block-size key and a larger than block-size data. The key needs to be hashed before being used by the HMAC algorithm.")
         (result (hex-string (cauldron.crypto:hmac-sha256 key data))))
    (is-equal "9b09ffa71b942fcb27635fbcd5b0e944bfdc63644f0713938a7f51535c3a35e2"
              result)))
