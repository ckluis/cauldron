;;;; test/crypto/property-test.lisp — Property-based crypto tests
(in-package :cauldron.test)

(defsuite :crypto-property)

;;; --- SHA-256 properties ---

(deftest test-sha256-deterministic
  "SHA-256 of the same input always produces the same output."
  (dolist (input '("hello" "world" "" "test123" "a longer string with spaces"))
    (let ((h1 (cauldron.crypto:sha256 input))
          (h2 (cauldron.crypto:sha256 input)))
      (is-equalp h1 h2 (format nil "SHA-256 deterministic for ~S" input)))))

(deftest test-sha256-different-inputs-different-hashes
  "Different inputs produce different SHA-256 hashes."
  (let ((inputs '("a" "b" "ab" "ba" "abc" "cba" "" " ")))
    (let ((hashes (mapcar #'cauldron.crypto:sha256 inputs)))
      ;; Check all pairs are different
      (loop for i from 0 below (length hashes)
            do (loop for j from (1+ i) below (length hashes)
                     do (is (not (equalp (nth i hashes) (nth j hashes)))
                            (format nil "~S and ~S produce different hashes"
                                    (nth i inputs) (nth j inputs))))))))

(deftest test-sha256-output-length
  "SHA-256 always produces 32 bytes regardless of input."
  (dolist (input (list "" "x" (make-string 1000 :initial-element #\a)))
    (is-equal 32 (length (cauldron.crypto:sha256 input)))))

;;; --- HMAC properties ---

(deftest test-hmac-sha256-deterministic
  "HMAC-SHA256 is deterministic for same key+message."
  (let ((key "secret-key")
        (msg "hello world"))
    (let ((h1 (cauldron.crypto:hmac-sha256 key msg))
          (h2 (cauldron.crypto:hmac-sha256 key msg)))
      (is-equalp h1 h2))))

(deftest test-hmac-sha256-different-keys
  "Different keys produce different HMACs for the same message."
  (let ((msg "same message"))
    (let ((h1 (cauldron.crypto:hmac-sha256 "key1" msg))
          (h2 (cauldron.crypto:hmac-sha256 "key2" msg)))
      (is (not (equalp h1 h2))))))

(deftest test-hmac-sha256-different-messages
  "Same key with different messages produces different HMACs."
  (let ((key "same-key"))
    (let ((h1 (cauldron.crypto:hmac-sha256 key "msg1"))
          (h2 (cauldron.crypto:hmac-sha256 key "msg2")))
      (is (not (equalp h1 h2))))))

(deftest test-hmac-sha256-output-length
  "HMAC-SHA256 always produces 32 bytes."
  (dolist (msg '("" "short" "a medium length message for testing"))
    (is-equal 32 (length (cauldron.crypto:hmac-sha256 "key" msg)))))

;;; --- Base64 roundtrip ---

(deftest test-base64-roundtrip-strings
  "Base64 encode then decode returns the original string bytes."
  (dolist (input '("hello" "world" "" "abc123" "special chars: !@#$%"))
    (let* ((encoded (cauldron.crypto:base64-encode input))
           (decoded (cauldron.crypto:base64-decode encoded)))
      ;; decoded is octets; compare to original octets
      (is-equalp (cauldron.crypto:ensure-octets input) decoded
                 (format nil "base64 roundtrip for ~S" input)))))

(deftest test-base64-roundtrip-octets
  "Base64 encode then decode preserves arbitrary byte sequences."
  (dolist (size '(0 1 2 3 4 16 32 100))
    (let* ((octets (make-array size :element-type '(unsigned-byte 8)
                                    :initial-element (mod size 256)))
           (encoded (cauldron.crypto:base64-encode octets))
           (decoded (cauldron.crypto:base64-decode encoded)))
      (is-equalp octets decoded
                 (format nil "base64 roundtrip for ~D bytes" size)))))

(deftest test-base64url-roundtrip
  "Base64URL encode then decode returns original."
  (dolist (input '("hello" "" "test/value+123"))
    (let* ((encoded (cauldron.crypto:base64url-encode input))
           (decoded (cauldron.crypto:base64url-decode encoded)))
      (is-equalp (cauldron.crypto:ensure-octets input) decoded
                 (format nil "base64url roundtrip for ~S" input)))))

;;; --- generate-token properties ---

(deftest test-generate-token-default-length
  "Default token is 64 hex characters (32 bytes)."
  (let ((token (cauldron.crypto:generate-token)))
    (is-equal 64 (length token))))

(deftest test-generate-token-custom-lengths
  "Custom lengths produce correct hex string sizes."
  (dolist (len '(1 8 16 32 64))
    (let ((token (cauldron.crypto:generate-token :length len)))
      (is-equal (* 2 len) (length token)
                (format nil "token length ~D produces ~D hex chars" len (* 2 len))))))

(deftest test-generate-token-all-hex-chars
  "Token contains only lowercase hex characters."
  (dotimes (i 3)
    (let ((token (cauldron.crypto:generate-token)))
      (is (every (lambda (c) (or (digit-char-p c) (find c "abcdef")))
                 token)
          "all chars are hex"))))

;;; --- bcrypt hash/verify roundtrip ---

(deftest test-bcrypt-roundtrip-various-passwords
  "bcrypt-hash then bcrypt-verify returns T for several passwords."
  (dolist (pw '("password" "123456" "" "a" "long password with spaces and symbols!@#"))
    (let ((hash (cauldron.crypto:bcrypt-hash pw :cost 4)))
      (is-true (cauldron.crypto:bcrypt-verify pw hash)
               (format nil "bcrypt roundtrip for ~S" pw)))))

(deftest test-bcrypt-wrong-password-fails
  "bcrypt-verify with wrong password returns NIL."
  (let ((hash (cauldron.crypto:bcrypt-hash "correct-pw" :cost 4)))
    (dolist (wrong '("wrong" "Correct-pw" "correct-p" "correct-pw "))
      (is-false (cauldron.crypto:bcrypt-verify wrong hash)
                (format nil "bcrypt rejects ~S" wrong)))))

;;; --- secure-equal properties ---

(deftest test-secure-equal-reflexive
  "secure-equal with same data is always T."
  (dolist (input '("hello" "world" "" "test"))
    (is-true (cauldron.crypto:secure-equal input input)
             (format nil "secure-equal reflexive for ~S" input))))

(deftest test-secure-equal-symmetric
  "secure-equal(a,b) = secure-equal(b,a)."
  (let ((a "hello")
        (b "hello")
        (c "world"))
    (is-equal (cauldron.crypto:secure-equal a b)
              (cauldron.crypto:secure-equal b a)
              "symmetric for equal strings")
    (is-equal (cauldron.crypto:secure-equal a c)
              (cauldron.crypto:secure-equal c a)
              "symmetric for different strings")))
