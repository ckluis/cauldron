;;;; test/crypto/random-test.lisp — Random generation tests
(in-package :cauldron.test)

(defsuite :crypto-random)

(deftest test-secure-random-bytes-length
  (let ((result (cauldron.crypto:secure-random-bytes 32)))
    (is-equal 32 (length result))
    (is (typep result '(vector (unsigned-byte 8))))))

(deftest test-secure-random-bytes-zero
  (let ((result (cauldron.crypto:secure-random-bytes 0)))
    (is-equal 0 (length result))))

(deftest test-secure-random-bytes-various-sizes
  (dolist (size '(1 16 64 256))
    (is-equal size (length (cauldron.crypto:secure-random-bytes size)))))

(deftest test-generate-token-length
  ;; Default: 32 bytes → 64 hex chars
  (let ((token (cauldron.crypto:generate-token)))
    (is-equal 64 (length token))))

(deftest test-generate-token-custom-length
  (let ((token (cauldron.crypto:generate-token :length 16)))
    (is-equal 32 (length token))))

(deftest test-generate-token-hex-chars
  (let ((token (cauldron.crypto:generate-token)))
    (is (every (lambda (c) (or (digit-char-p c) (find c "abcdef")))
               token))))

(deftest test-generate-token-unique
  ;; Two tokens should be different (probability of collision is negligible)
  (let ((t1 (cauldron.crypto:generate-token))
        (t2 (cauldron.crypto:generate-token)))
    (is (not (string= t1 t2)))))

;;; --- secure-equal ---

(deftest test-secure-equal-same
  (let ((a (make-array 4 :element-type '(unsigned-byte 8)
                         :initial-contents '(1 2 3 4)))
        (b (make-array 4 :element-type '(unsigned-byte 8)
                         :initial-contents '(1 2 3 4))))
    (is-true (cauldron.crypto:secure-equal a b))))

(deftest test-secure-equal-different
  (let ((a (make-array 4 :element-type '(unsigned-byte 8)
                         :initial-contents '(1 2 3 4)))
        (b (make-array 4 :element-type '(unsigned-byte 8)
                         :initial-contents '(1 2 3 5))))
    (is-nil (cauldron.crypto:secure-equal a b))))

(deftest test-secure-equal-different-lengths
  (let ((a (make-array 4 :element-type '(unsigned-byte 8)
                         :initial-contents '(1 2 3 4)))
        (b (make-array 3 :element-type '(unsigned-byte 8)
                         :initial-contents '(1 2 3))))
    (is-nil (cauldron.crypto:secure-equal a b))))

(deftest test-secure-equal-strings
  (is-true (cauldron.crypto:secure-equal "hello" "hello"))
  (is-nil (cauldron.crypto:secure-equal "hello" "world")))
