;;;; test/crypto/aes-test.lisp — AES-256-CBC tests including NIST FIPS 197 vectors
(in-package :cauldron.test)

(defsuite :crypto-aes)

;;; --- Helper: parse hex string to octet vector ---

(defun hex-to-octets (hex-string)
  "Convert a hex string to an octet vector."
  (let* ((len (/ (length hex-string) 2))
         (result (make-array len :element-type '(unsigned-byte 8))))
    (loop for i from 0 below len
          for hi = (digit-char-p (char hex-string (* i 2)) 16)
          for lo = (digit-char-p (char hex-string (+ (* i 2) 1)) 16)
          do (setf (aref result i) (+ (* hi 16) lo)))
    result))

;;; --- NIST FIPS 197 Appendix C.3: AES-256 Known Answer Test ---

(deftest test-aes-256-nist-vector
  ;; FIPS 197 Appendix C.3: AES-256 (Nk=8, Nr=14)
  ;; Key:       000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f
  ;; Plaintext: 00112233445566778899aabbccddeeff
  ;; Expected:  8ea2b7ca516745bfeafc49904b496089
  (let* ((key (hex-to-octets "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"))
         (plaintext (hex-to-octets "00112233445566778899aabbccddeeff"))
         (expected "8ea2b7ca516745bfeafc49904b496089")
         (encrypted (cauldron.crypto::aes-256-encrypt-block plaintext key))
         (result (hex-string encrypted)))
    (is-equal expected result "NIST FIPS 197 AES-256 encrypt")))

(deftest test-aes-256-nist-decrypt
  ;; Verify decrypt of the NIST vector
  (let* ((key (hex-to-octets "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"))
         (ciphertext (hex-to-octets "8ea2b7ca516745bfeafc49904b496089"))
         (expected "00112233445566778899aabbccddeeff")
         (decrypted (cauldron.crypto::aes-256-decrypt-block ciphertext key))
         (result (hex-string decrypted)))
    (is-equal expected result "NIST FIPS 197 AES-256 decrypt")))

;;; --- CBC mode roundtrip ---

(deftest test-aes-256-cbc-roundtrip
  ;; Encrypt then decrypt, verify we get back the original
  (let* ((key (hex-to-octets "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"))
         (plaintext "Hello, AES-256-CBC! This is a test message.")
         (plain-octets (cauldron.crypto:ensure-octets plaintext)))
    (multiple-value-bind (ciphertext iv)
        (cauldron.crypto:aes-256-cbc-encrypt plaintext key)
      (declare (ignore iv))
      ;; Ciphertext should be longer than plaintext (IV + padding)
      (is (> (length ciphertext) (length plain-octets))
          "Ciphertext includes IV and is padded")
      ;; Decrypt
      (let ((decrypted (cauldron.crypto:aes-256-cbc-decrypt ciphertext key)))
        (is-equal plaintext (map 'string #'code-char decrypted)
                  "CBC roundtrip produces original plaintext")))))

(deftest test-aes-256-cbc-roundtrip-short
  ;; Test with short plaintext (less than one block)
  (let* ((key (hex-to-octets "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"))
         (plaintext "Hi"))
    (multiple-value-bind (ciphertext iv)
        (cauldron.crypto:aes-256-cbc-encrypt plaintext key)
      (declare (ignore iv))
      ;; IV (16) + 1 padded block (16) = 32 bytes
      (is-equal 32 (length ciphertext) "Short plaintext produces IV + 1 block")
      (let ((decrypted (cauldron.crypto:aes-256-cbc-decrypt ciphertext key)))
        (is-equal "Hi" (map 'string #'code-char decrypted))))))

(deftest test-aes-256-cbc-iv-uniqueness
  ;; Two encryptions of the same plaintext should produce different ciphertexts
  ;; because IV is randomly generated
  (let* ((key (hex-to-octets "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"))
         (plaintext "same message"))
    (multiple-value-bind (ct1 iv1) (cauldron.crypto:aes-256-cbc-encrypt plaintext key)
      (multiple-value-bind (ct2 iv2) (cauldron.crypto:aes-256-cbc-encrypt plaintext key)
        ;; IVs should differ
        (is (not (equalp iv1 iv2)) "Random IVs should differ")
        ;; Ciphertexts should differ
        (is (not (equalp ct1 ct2)) "Ciphertexts with different IVs should differ")
        ;; But both should decrypt to the same plaintext
        (let ((p1 (map 'string #'code-char (cauldron.crypto:aes-256-cbc-decrypt ct1 key)))
              (p2 (map 'string #'code-char (cauldron.crypto:aes-256-cbc-decrypt ct2 key))))
          (is-equal p1 p2 "Both decrypt to same plaintext"))))))

;;; --- PKCS7 padding ---

(deftest test-pkcs7-pad-unpad
  ;; Padding then unpadding should be identity
  (let* ((data (cauldron.crypto:ensure-octets "test"))
         (padded (cauldron.crypto::pkcs7-pad data 16)))
    ;; padded should be 16 bytes (4 data + 12 padding)
    (is-equal 16 (length padded) "Padded to block size")
    (is-equal 12 (aref padded 15) "Padding byte is 12")
    (let ((unpadded (cauldron.crypto::pkcs7-unpad padded)))
      (is-equalp data unpadded "Unpad restores original"))))

(deftest test-pkcs7-pad-full-block
  ;; Data exactly one block should get a full block of padding
  (let* ((data (make-array 16 :element-type '(unsigned-byte 8) :initial-element #x41))
         (padded (cauldron.crypto::pkcs7-pad data 16)))
    ;; Should be 32 bytes: 16 data + 16 padding
    (is-equal 32 (length padded) "Full block data gets extra padding block")
    (is-equal 16 (aref padded 31) "Padding byte is 16")))

(deftest test-pkcs7-unpad-invalid
  ;; Invalid padding should signal an error
  (let ((bad-data (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; Pad byte of 0 is invalid
    (signals-condition error
      (cauldron.crypto::pkcs7-unpad bad-data)
      "Zero pad byte signals error")))

(deftest test-pkcs7-unpad-constant-time-error
  ;; Both types of invalid padding must produce identical error message
  (let ((out-of-range (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0))
        (wrong-byte (make-array 16 :element-type '(unsigned-byte 8) :initial-element 5)))
    ;; Pad byte 0 (out of range)
    (setf (aref out-of-range 15) 0)
    ;; Pad byte 3, but not all pad bytes match
    (setf (aref wrong-byte 15) 3)
    (setf (aref wrong-byte 14) 3)
    (setf (aref wrong-byte 13) 99)  ; Wrong padding byte
    (let ((err1 (handler-case (progn (cauldron.crypto::pkcs7-unpad out-of-range) nil)
                  (error (e) (format nil "~A" e))))
          (err2 (handler-case (progn (cauldron.crypto::pkcs7-unpad wrong-byte) nil)
                  (error (e) (format nil "~A" e)))))
      (is-not-nil err1 "Out-of-range signals error")
      (is-not-nil err2 "Wrong pad byte signals error")
      (is-equal err1 err2 "Both produce identical error message"))))

;;; --- GF lookup table correctness ---

(deftest test-gf-lookup-tables-match-computed
  ;; Verify lookup tables match gf-mul for all 256 inputs
  (loop for constant in '(2 3 9 11 13 14)
        for table in (list cauldron.crypto::+gf-mul-2+ cauldron.crypto::+gf-mul-3+
                          cauldron.crypto::+gf-mul-9+ cauldron.crypto::+gf-mul-11+
                          cauldron.crypto::+gf-mul-13+ cauldron.crypto::+gf-mul-14+)
        do (loop for i from 0 below 256
                 for expected = (cauldron.crypto::gf-mul constant i)
                 for actual = (aref table i)
                 do (is-equal expected actual
                             (format nil "GF mul ~D * ~D: expected ~D got ~D"
                                     constant i expected actual)))))
