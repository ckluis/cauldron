;;;; test/db/protocol-test.lisp — PG wire protocol builder tests
(in-package :cauldron.test)

(defsuite :db-protocol)

;;; --- payload-read-int32 ---

(deftest test-payload-read-int32
  (let ((buf (make-array 4 :element-type '(unsigned-byte 8)
                           :initial-contents '(0 0 1 0))))
    (multiple-value-bind (val offset) (cauldron.db::payload-read-int32 buf 0)
      (is-equal 256 val)
      (is-equal 4 offset))))

(deftest test-payload-read-int32-large
  (let ((buf (make-array 4 :element-type '(unsigned-byte 8)
                           :initial-contents '(0 1 0 0))))
    (is-equal 65536 (cauldron.db::payload-read-int32 buf 0))))

;;; --- payload-read-int16 ---

(deftest test-payload-read-int16
  (let ((buf (make-array 2 :element-type '(unsigned-byte 8)
                           :initial-contents '(0 42))))
    (multiple-value-bind (val offset) (cauldron.db::payload-read-int16 buf 0)
      (is-equal 42 val)
      (is-equal 2 offset))))

;;; --- payload-read-int32-signed ---

(deftest test-payload-read-int32-signed-positive
  (let ((buf (make-array 4 :element-type '(unsigned-byte 8)
                           :initial-contents '(0 0 0 1))))
    (is-equal 1 (cauldron.db::payload-read-int32-signed buf 0))))

(deftest test-payload-read-int32-signed-negative
  ;; -1 in 32-bit two's complement = #xFFFFFFFF
  (let ((buf (make-array 4 :element-type '(unsigned-byte 8)
                           :initial-contents '(#xFF #xFF #xFF #xFF))))
    (is-equal -1 (cauldron.db::payload-read-int32-signed buf 0))))

;;; --- payload-read-cstring ---

(deftest test-payload-read-cstring
  (let ((buf (make-array 6 :element-type '(unsigned-byte 8)
                           :initial-contents (list 104 101 108 108 111 0)))) ; "hello\0"
    (multiple-value-bind (str offset) (cauldron.db::payload-read-cstring buf 0)
      (is-equal "hello" str)
      (is-equal 6 offset))))

;;; --- payload-read-bytes ---

(deftest test-payload-read-bytes
  (let ((buf (make-array 5 :element-type '(unsigned-byte 8)
                           :initial-contents '(1 2 3 4 5))))
    (multiple-value-bind (bytes offset) (cauldron.db::payload-read-bytes buf 1 3)
      (is-equal 3 (length bytes))
      (is-equal 2 (aref bytes 0))
      (is-equal 4 offset))))

;;; --- build-query-payload ---

(deftest test-build-query-payload
  (let ((payload (cauldron.db::build-query-payload "SELECT 1")))
    (is (typep payload '(vector (unsigned-byte 8))))
    ;; Should end with null byte
    (is-equal 0 (aref payload (1- (length payload))))
    ;; Length should be string length + 1 (null)
    (is-equal 9 (length payload))))

;;; --- build-parse-payload ---

(deftest test-build-parse-payload
  (let ((payload (cauldron.db::build-parse-payload "" "SELECT $1" '(25))))
    (is (typep payload '(vector (unsigned-byte 8))))
    ;; Should contain the SQL string
    (let ((sql-bytes (sb-ext:string-to-octets "SELECT $1" :external-format :utf-8)))
      ;; After stmt name null, find SQL
      (is (search sql-bytes payload)))))

;;; --- build-bind-payload ---

(deftest test-build-bind-payload-with-params
  (let ((payload (cauldron.db::build-bind-payload "" "" '("hello"))))
    (is (typep payload '(vector (unsigned-byte 8))))
    ;; Should contain the param value
    (let ((val-bytes (sb-ext:string-to-octets "hello" :external-format :utf-8)))
      (is (search val-bytes payload)))))

(deftest test-build-bind-payload-with-null
  (let ((payload (cauldron.db::build-bind-payload "" "" '(nil))))
    ;; NULL is encoded as 4 bytes of 0xFF (-1 as int32)
    (is (search #(#xFF #xFF #xFF #xFF) payload))))

;;; --- build-describe-payload ---

(deftest test-build-describe-payload
  (let ((payload (cauldron.db::build-describe-payload #\S "stmt1")))
    ;; First byte should be char-code of S
    (is-equal (char-code #\S) (aref payload 0))
    ;; Should end with null
    (is-equal 0 (aref payload (1- (length payload))))))

;;; --- build-execute-payload ---

(deftest test-build-execute-payload
  (let ((payload (cauldron.db::build-execute-payload "" 0)))
    ;; Empty portal name (null byte) + 4 bytes max-rows
    (is-equal 5 (length payload))
    ;; First byte is null (empty portal name)
    (is-equal 0 (aref payload 0))
    ;; Max rows 0 = all zeros
    (is-equal 0 (aref payload 1))
    (is-equal 0 (aref payload 4))))

(deftest test-build-execute-payload-with-limit
  (let ((payload (cauldron.db::build-execute-payload "" 100)))
    ;; Max rows = 100 as int32
    (is-equal 0 (aref payload 1))
    (is-equal 0 (aref payload 2))
    (is-equal 0 (aref payload 3))
    (is-equal 100 (aref payload 4))))
