;;;; test/db/auth-test.lisp — MD5 + SCRAM parsing tests
(in-package :cauldron.test)

(defsuite :db-auth)

;;; --- md5-mod32+ ---

(deftest test-md5-mod32+-basic
  (is-equal 3 (cauldron.db::md5-mod32+ 1 2)))

(deftest test-md5-mod32+-overflow-wraps
  "Overflow wraps to 0."
  (is-equal 0 (cauldron.db::md5-mod32+ #xFFFFFFFF 1)))

(deftest test-md5-mod32+-multiple-args
  (is-equal 10 (cauldron.db::md5-mod32+ 1 2 3 4)))

(deftest test-md5-mod32+-zero
  (is-equal 0 (cauldron.db::md5-mod32+ 0 0)))

(deftest test-md5-mod32+-max
  (is-equal #xFFFFFFFF (cauldron.db::md5-mod32+ #xFFFFFFFF 0)))

;;; --- md5-left-rotate ---

(deftest test-md5-left-rotate-1-by-1
  (is-equal 2 (cauldron.db::md5-left-rotate 1 1)))

(deftest test-md5-left-rotate-1-by-0
  (is-equal 1 (cauldron.db::md5-left-rotate 1 0)))

(deftest test-md5-left-rotate-high-bit
  "Rotating 1 left by 31 gives #x80000000."
  (is-equal #x80000000 (cauldron.db::md5-left-rotate 1 31)))

(deftest test-md5-left-rotate-wrap
  "Rotating 1 left by 32 gives 1 (full rotation)."
  (is-equal 1 (cauldron.db::md5-left-rotate 1 32)))

;;; --- md5-pad-message ---

(deftest test-md5-pad-empty-message
  "Empty message pads to 64 bytes."
  (let ((padded (cauldron.db::md5-pad-message
                 (make-array 0 :element-type '(unsigned-byte 8)))))
    (is-equal 64 (length padded))
    ;; First byte is #x80
    (is-equal #x80 (aref padded 0))
    ;; Length bits = 0, so last 8 bytes are all 0
    (is-equal 0 (aref padded 56))
    (is-equal 0 (aref padded 63))))

(deftest test-md5-pad-56-byte-message
  "56-byte message pads to 128 bytes (can't fit length in same block)."
  (let* ((msg (make-array 56 :element-type '(unsigned-byte 8) :initial-element #x41))
         (padded (cauldron.db::md5-pad-message msg)))
    (is-equal 128 (length padded))
    (is-equal #x80 (aref padded 56))))

(deftest test-md5-pad-preserves-content
  (let* ((msg (make-array 3 :element-type '(unsigned-byte 8)
                            :initial-contents '(65 66 67)))
         (padded (cauldron.db::md5-pad-message msg)))
    (is-equal 65 (aref padded 0))
    (is-equal 66 (aref padded 1))
    (is-equal 67 (aref padded 2))
    (is-equal #x80 (aref padded 3))))

;;; --- md5-get-le32 ---

(deftest test-md5-get-le32-known
  (is-equal #x12345678
            (cauldron.db::md5-get-le32
             (make-array 4 :element-type '(unsigned-byte 8)
                           :initial-contents '(#x78 #x56 #x34 #x12))
             0)))

(deftest test-md5-get-le32-offset
  (let ((buf (make-array 8 :element-type '(unsigned-byte 8)
                           :initial-contents '(0 0 0 0 #x01 #x00 #x00 #x00))))
    (is-equal 1 (cauldron.db::md5-get-le32 buf 4))))

(deftest test-md5-get-le32-zeros
  (is-equal 0
            (cauldron.db::md5-get-le32
             (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0)
             0)))

;;; --- md5 (RFC 1321 test vectors) ---

(deftest test-md5-empty-string
  (is-equal "d41d8cd98f00b204e9800998ecf8427e"
            (cauldron.db::md5-hex "")))

(deftest test-md5-abc
  (is-equal "900150983cd24fb0d6963f7d28e17f72"
            (cauldron.db::md5-hex "abc")))

(deftest test-md5-message-digest
  (is-equal "f96b697d7cb7938d525a2f31aaf161d0"
            (cauldron.db::md5-hex "message digest")))

(deftest test-md5-alphabet
  (is-equal "c3fcd3d76192e4007dfb496cca67e13b"
            (cauldron.db::md5-hex "abcdefghijklmnopqrstuvwxyz")))

(deftest test-md5-a
  (is-equal "0cc175b9c0f1b6a831c399e269772661"
            (cauldron.db::md5-hex "a")))

;;; --- md5-hex returns string ---

(deftest test-md5-hex-returns-string
  (is (stringp (cauldron.db::md5-hex "test")))
  (is-equal 32 (length (cauldron.db::md5-hex "test"))))

;;; --- md5 returns octets ---

(deftest test-md5-returns-16-bytes
  (let ((digest (cauldron.db::md5 "")))
    (is-equal 16 (length digest))
    (is (typep digest '(simple-array (unsigned-byte 8) (16))))))

;;; --- split-scram-string ---

(deftest test-split-scram-basic
  (is-equal '("a=1" "b=2" "c=3")
            (cauldron.db::split-scram-string "a=1,b=2,c=3" #\,)))

(deftest test-split-scram-single
  (is-equal '("hello")
            (cauldron.db::split-scram-string "hello" #\,)))

(deftest test-split-scram-empty
  (is-equal '("")
            (cauldron.db::split-scram-string "" #\,)))

(deftest test-split-scram-trailing-comma
  (is-equal '("a" "b" "")
            (cauldron.db::split-scram-string "a,b," #\,)))

;;; --- parse-scram-attributes ---

(deftest test-parse-scram-typical
  (let ((result (cauldron.db::parse-scram-attributes "r=nonce,s=salt,i=4096")))
    (is-equal "nonce" (cdr (assoc #\r result)))
    (is-equal "salt" (cdr (assoc #\s result)))
    (is-equal "4096" (cdr (assoc #\i result)))))

(deftest test-parse-scram-single-attr
  (let ((result (cauldron.db::parse-scram-attributes "r=abc")))
    (is-equal 1 (length result))
    (is-equal "abc" (cdr (assoc #\r result)))))

(deftest test-parse-scram-skips-short
  "Parts shorter than 2 chars are skipped."
  (let ((result (cauldron.db::parse-scram-attributes "r=x,a")))
    (is-equal 1 (length result))
    (is-equal "x" (cdr (assoc #\r result)))))

(deftest test-parse-scram-empty
  (let ((result (cauldron.db::parse-scram-attributes "")))
    (is-equal 0 (length result))))
