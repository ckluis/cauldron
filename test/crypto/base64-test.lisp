;;;; test/crypto/base64-test.lisp — Base64 tests (RFC 4648 vectors)
(in-package :cauldron.test)

(defsuite :crypto-base64)

;;; --- RFC 4648 test vectors ---

(deftest test-base64-encode-empty
  (is-equal "" (cauldron.crypto:base64-encode "")))

(deftest test-base64-encode-f
  (is-equal "Zg==" (cauldron.crypto:base64-encode "f")))

(deftest test-base64-encode-fo
  (is-equal "Zm8=" (cauldron.crypto:base64-encode "fo")))

(deftest test-base64-encode-foo
  (is-equal "Zm9v" (cauldron.crypto:base64-encode "foo")))

(deftest test-base64-encode-foob
  (is-equal "Zm9vYg==" (cauldron.crypto:base64-encode "foob")))

(deftest test-base64-encode-fooba
  (is-equal "Zm9vYmE=" (cauldron.crypto:base64-encode "fooba")))

(deftest test-base64-encode-foobar
  (is-equal "Zm9vYmFy" (cauldron.crypto:base64-encode "foobar")))

;;; --- Decode ---

(deftest test-base64-decode-empty
  (is-equalp #() (cauldron.crypto:base64-decode "")))

(deftest test-base64-decode-foobar
  (let ((result (cauldron.crypto:base64-decode "Zm9vYmFy")))
    (is-equal "foobar"
              (map 'string #'code-char result))))

;;; --- Round-trip ---

(deftest test-base64-roundtrip
  (let* ((data (make-array 5 :element-type '(unsigned-byte 8)
                             :initial-contents '(0 127 128 255 42)))
         (encoded (cauldron.crypto:base64-encode data))
         (decoded (cauldron.crypto:base64-decode encoded)))
    (is-equalp data decoded)))

;;; --- Whitespace handling ---

(deftest test-base64-decode-whitespace
  (let ((result (cauldron.crypto:base64-decode "Zm9v YmFy")))
    (is-equal "foobar" (map 'string #'code-char result))))

(deftest test-base64-decode-newlines
  (let ((result (cauldron.crypto:base64-decode (format nil "Zm9v~%YmFy"))))
    (is-equal "foobar" (map 'string #'code-char result))))

;;; --- Base64URL ---

(deftest test-base64url-encode-no-padding
  ;; "f" in standard = "Zg==", in URL = "Zg" (no padding)
  (is-equal "Zg" (cauldron.crypto:base64url-encode "f")))

(deftest test-base64url-encode-foobar
  (is-equal "Zm9vYmFy" (cauldron.crypto:base64url-encode "foobar")))

(deftest test-base64url-roundtrip
  (let* ((data (make-array 3 :element-type '(unsigned-byte 8)
                             :initial-contents '(251 239 190)))
         (encoded (cauldron.crypto:base64url-encode data))
         (decoded (cauldron.crypto:base64url-decode encoded)))
    (is-equalp data decoded)
    ;; Should use - and _ instead of + and /
    (is-false (find #\+ encoded))
    (is-false (find #\/ encoded))))

(deftest test-base64url-uses-url-safe-chars
  ;; Bytes that would produce + and / in standard base64
  (let* ((data (make-array 3 :element-type '(unsigned-byte 8)
                             :initial-contents '(#xFB #xEF #xBE)))
         (encoded (cauldron.crypto:base64url-encode data)))
    (is-false (find #\+ encoded))
    (is-false (find #\/ encoded))
    (is-false (find #\= encoded))))
