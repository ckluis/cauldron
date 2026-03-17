;;;; test/runtime/sockets-test.lisp — Socket utility tests (pure functions only)
(in-package :cauldron.test)

(defsuite :runtime-sockets)

;;; ============================================================
;;; %parse-host
;;; ============================================================

(deftest test-parse-host-localhost
  (let ((result (cauldron.runtime::%parse-host "127.0.0.1")))
    (is (typep result '(vector (unsigned-byte 8))))
    (is-equal 4 (length result))
    (is-equal 127 (aref result 0))
    (is-equal 0 (aref result 1))
    (is-equal 0 (aref result 2))
    (is-equal 1 (aref result 3))))

(deftest test-parse-host-all-zeros
  (let ((result (cauldron.runtime::%parse-host "0.0.0.0")))
    (is-equalp #(0 0 0 0) result)))

(deftest test-parse-host-private-network
  (let ((result (cauldron.runtime::%parse-host "192.168.1.100")))
    (is-equal 192 (aref result 0))
    (is-equal 168 (aref result 1))
    (is-equal 1 (aref result 2))
    (is-equal 100 (aref result 3))))

(deftest test-parse-host-ten-network
  (let ((result (cauldron.runtime::%parse-host "10.0.0.5")))
    (is-equal 10 (aref result 0))
    (is-equal 0 (aref result 1))
    (is-equal 0 (aref result 2))
    (is-equal 5 (aref result 3))))

(deftest test-parse-host-vector-passthrough
  "When given a vector, should return it unchanged."
  (let* ((input (make-array 4 :element-type '(unsigned-byte 8)
                              :initial-contents '(172 16 0 1)))
         (result (cauldron.runtime::%parse-host input)))
    (is (eq input result))))
