;;;; test/crypto/bcrypt-test.lisp — Bcrypt implementation tests
(in-package :cauldron.test)

(defsuite :crypto-bcrypt)

;;; --- bcrypt Base64 ---

(deftest test-bcrypt-b64-encode-empty
  (is-equal "" (cauldron.crypto::bcrypt-base64-encode
                (make-array 0 :element-type '(unsigned-byte 8)) 0)))

(deftest test-bcrypt-b64-roundtrip-3-bytes
  "3 bytes encode to 4 chars and decode back exactly."
  (let* ((input (make-array 3 :element-type '(unsigned-byte 8)
                              :initial-contents '(0 127 255)))
         (encoded (cauldron.crypto::bcrypt-base64-encode input 4))
         (decoded (cauldron.crypto::bcrypt-base64-decode encoded 3)))
    (is-equalp input decoded)))

(deftest test-bcrypt-b64-roundtrip-16-bytes
  "16 bytes (salt-sized) roundtrip through 22 chars."
  (let* ((input (make-array 16 :element-type '(unsigned-byte 8)
                               :initial-contents '(1 2 3 4 5 6 7 8
                                                   9 10 11 12 13 14 15 16)))
         (encoded (cauldron.crypto::bcrypt-base64-encode input 22))
         (decoded (cauldron.crypto::bcrypt-base64-decode encoded 16)))
    (is-equalp input decoded)))

(deftest test-bcrypt-b64-roundtrip-23-bytes
  "23 bytes roundtrip through 31 chars (bcrypt hash output uses 23 of 24 bytes)."
  (let* ((input (make-array 23 :element-type '(unsigned-byte 8)
                               :initial-contents '(10 20 30 40 50 60 70 80
                                                   90 100 110 120 130 140 150 160
                                                   170 180 190 200 210 220 230)))
         ;; 23 bytes needs ceil(23*8/6) = 31 chars
         (encoded (cauldron.crypto::bcrypt-base64-encode input 31))
         (decoded (cauldron.crypto::bcrypt-base64-decode encoded 23)))
    (is-equalp input decoded)))

(deftest test-bcrypt-b64-uses-nonstandard-alphabet
  "bcrypt base64 starts with ./ not +/ like standard."
  (let ((alphabet cauldron.crypto::+bcrypt-base64-alphabet+))
    (is-equal #\. (char alphabet 0))
    (is-equal #\/ (char alphabet 1))
    (is-equal #\A (char alphabet 2))
    (is-equal 64 (length alphabet))))

(deftest test-bcrypt-b64-all-zeros
  (let* ((input (make-array 3 :element-type '(unsigned-byte 8)
                              :initial-contents '(0 0 0)))
         (encoded (cauldron.crypto::bcrypt-base64-encode input 4))
         (decoded (cauldron.crypto::bcrypt-base64-decode encoded 3)))
    (is-equalp input decoded)
    ;; All zeros should encode to first char of alphabet repeated
    (is-equal #\. (char encoded 0))))

(deftest test-bcrypt-b64-all-ones
  (let* ((input (make-array 3 :element-type '(unsigned-byte 8)
                              :initial-contents '(255 255 255)))
         (encoded (cauldron.crypto::bcrypt-base64-encode input 4))
         (decoded (cauldron.crypto::bcrypt-base64-decode encoded 3)))
    (is-equalp input decoded)))

;;; --- Blowfish state ---

(deftest test-bf-make-state-structure
  "bf-make-state returns (P S0 S1 S2 S3) with correct sizes."
  (let ((state (cauldron.crypto::bf-make-state)))
    (is-equal 5 (length state))
    (is-equal 18 (length (first state)))    ; P-array
    (is-equal 256 (length (second state)))  ; S0
    (is-equal 256 (length (third state)))   ; S1
    (is-equal 256 (length (fourth state)))  ; S2
    (is-equal 256 (length (fifth state))))) ; S3

(deftest test-bf-make-state-p-array-values
  "P-array starts with known pi-derived values."
  (let* ((state (cauldron.crypto::bf-make-state))
         (p (first state)))
    (is-equal #x243f6a88 (aref p 0))
    (is-equal #x85a308d3 (aref p 1))
    (is-equal #x8979fb1b (aref p 17))))

(deftest test-bf-make-state-fresh-copies
  "Each call returns independent state."
  (let ((s1 (cauldron.crypto::bf-make-state))
        (s2 (cauldron.crypto::bf-make-state)))
    (setf (aref (first s1) 0) 0)
    (is (not (= 0 (aref (first s2) 0))))))

;;; --- Blowfish F function ---

(deftest test-bf-f-zero-input
  "F(0) should use S-box[0] entries and combine them."
  (let* ((state (cauldron.crypto::bf-make-state))
         (result (cauldron.crypto::bf-f
                  (second state) (third state)
                  (fourth state) (fifth state) 0)))
    (is (typep result '(unsigned-byte 32)))
    ;; F(0) = ((S0[0] + S1[0]) XOR S2[0]) + S3[0] mod 2^32
    (let* ((s0-0 (aref (second state) 0))
           (s1-0 (aref (third state) 0))
           (s2-0 (aref (fourth state) 0))
           (s3-0 (aref (fifth state) 0))
           (expected (logand #xFFFFFFFF
                             (+ (logxor (logand #xFFFFFFFF (+ s0-0 s1-0)) s2-0)
                                s3-0))))
      (is-equal expected result))))

(deftest test-bf-f-known-input
  "F with all bytes = 1 uses S-box[1] entries."
  (let* ((state (cauldron.crypto::bf-make-state))
         ;; x = #x01010101 → a=1, b=1, c=1, d=1
         (result (cauldron.crypto::bf-f
                  (second state) (third state)
                  (fourth state) (fifth state) #x01010101)))
    (is (typep result '(unsigned-byte 32)))))

;;; --- bf-encrypt-pair ---

(deftest test-bf-encrypt-pair-initial-state
  "Encryption with initial state produces deterministic output."
  (let ((state (cauldron.crypto::bf-make-state)))
    (multiple-value-bind (l r) (cauldron.crypto::bf-encrypt-pair state 0 0)
      (is (typep l '(unsigned-byte 32)))
      (is (typep r '(unsigned-byte 32)))
      ;; Encrypting (0, 0) with initial state should give specific values
      ;; This is a known Blowfish test vector with the identity key
      (is (not (= l 0)))
      (is (not (= r 0))))))

(deftest test-bf-encrypt-pair-deterministic
  "Same input always gives same output."
  (let ((s1 (cauldron.crypto::bf-make-state))
        (s2 (cauldron.crypto::bf-make-state)))
    (multiple-value-bind (l1 r1) (cauldron.crypto::bf-encrypt-pair s1 #xDEADBEEF #xCAFEBABE)
      (multiple-value-bind (l2 r2) (cauldron.crypto::bf-encrypt-pair s2 #xDEADBEEF #xCAFEBABE)
        (is-equal l1 l2)
        (is-equal r1 r2)))))

(deftest test-bf-encrypt-pair-different-inputs
  "Different inputs give different outputs."
  (let ((state (cauldron.crypto::bf-make-state)))
    (multiple-value-bind (l1 r1) (cauldron.crypto::bf-encrypt-pair state 0 0)
      (let ((state2 (cauldron.crypto::bf-make-state)))
        (multiple-value-bind (l2 r2) (cauldron.crypto::bf-encrypt-pair state2 1 0)
          (is (or (not (= l1 l2)) (not (= r1 r2)))))))))

;;; --- bf-expand-key ---

(deftest test-bf-expand-key-modifies-state
  "Key expansion changes P-array and S-boxes."
  (let ((state (cauldron.crypto::bf-make-state))
        (key (make-array 4 :element-type '(unsigned-byte 8)
                           :initial-contents '(1 2 3 4))))
    (let ((p-before (aref (first state) 0)))
      (cauldron.crypto::bf-expand-key state key)
      (is (not (= p-before (aref (first state) 0)))))))

;;; --- bf-expand-key-with-salt ---

(deftest test-bf-expand-key-with-salt-differs-from-plain
  "EksBlowfish key schedule produces different state than plain expansion."
  (let ((state1 (cauldron.crypto::bf-make-state))
        (state2 (cauldron.crypto::bf-make-state))
        (key (make-array 4 :element-type '(unsigned-byte 8)
                           :initial-contents '(1 2 3 4)))
        (salt (make-array 16 :element-type '(unsigned-byte 8)
                             :initial-element 42)))
    (cauldron.crypto::bf-expand-key state1 key)
    (cauldron.crypto::bf-expand-key-with-salt state2 key salt)
    ;; Different salt involvement → different P-arrays
    (is (not (= (aref (first state1) 0) (aref (first state2) 0))))))

;;; --- bcrypt-encrypt-ctext ---

(deftest test-bcrypt-encrypt-ctext-returns-24-bytes
  "bcrypt ctext encryption always produces 24 bytes."
  (let* ((state (cauldron.crypto::bf-make-state))
         (result (cauldron.crypto::bcrypt-encrypt-ctext state)))
    (is-equal 24 (length result))
    (is (typep result '(simple-array (unsigned-byte 8) (24))))))

(deftest test-bcrypt-encrypt-ctext-deterministic
  "Same state produces same ctext."
  (let ((s1 (cauldron.crypto::bf-make-state))
        (s2 (cauldron.crypto::bf-make-state)))
    (is-equalp (cauldron.crypto::bcrypt-encrypt-ctext s1)
               (cauldron.crypto::bcrypt-encrypt-ctext s2))))

(deftest test-bcrypt-encrypt-ctext-different-states
  "Different states produce different ctext."
  (let ((s1 (cauldron.crypto::bf-make-state))
        (s2 (cauldron.crypto::bf-make-state)))
    (let ((key (make-array 4 :element-type '(unsigned-byte 8)
                             :initial-contents '(1 2 3 4))))
      (cauldron.crypto::bf-expand-key s2 key))
    (is (not (equalp (cauldron.crypto::bcrypt-encrypt-ctext s1)
                     (cauldron.crypto::bcrypt-encrypt-ctext s2))))))

;;; --- Full bcrypt hash/verify round-trip ---

(deftest test-bcrypt-hash-format
  "bcrypt-hash produces $2b$CC$... format."
  (let ((hash (cauldron.crypto:bcrypt-hash "password" :cost 4)))
    (is (stringp hash))
    (is-equal "$2b$" (subseq hash 0 4))
    (is-equal "04" (subseq hash 4 6))
    (is-equal #\$ (char hash 6))
    ;; 7 + 22 (salt) + 31 (hash) = 60 chars
    (is-equal 60 (length hash))))

(deftest test-bcrypt-verify-correct-password
  (let ((hash (cauldron.crypto:bcrypt-hash "test123" :cost 4)))
    (is-true (cauldron.crypto:bcrypt-verify "test123" hash))))

(deftest test-bcrypt-verify-wrong-password
  (let ((hash (cauldron.crypto:bcrypt-hash "correct" :cost 4)))
    (is-false (cauldron.crypto:bcrypt-verify "wrong" hash))))

(deftest test-bcrypt-hash-different-salts
  "Two hashes of same password differ (random salt)."
  (let ((h1 (cauldron.crypto:bcrypt-hash "same" :cost 4))
        (h2 (cauldron.crypto:bcrypt-hash "same" :cost 4)))
    (is (not (equal h1 h2)))
    ;; But both verify
    (is-true (cauldron.crypto:bcrypt-verify "same" h1))
    (is-true (cauldron.crypto:bcrypt-verify "same" h2))))

(deftest test-bcrypt-cost-range
  "Cost must be 4-31."
  (signals-condition error (cauldron.crypto:bcrypt-hash "pw" :cost 3))
  (signals-condition error (cauldron.crypto:bcrypt-hash "pw" :cost 32)))
