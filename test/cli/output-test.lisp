;;;; test/cli/output-test.lisp — Tests for CLI output formatting
(in-package :cauldron.test)

(defsuite :cli-output)

;;; ============================================================
;;; Helper: make a row hash-table
;;; ============================================================

(defun make-test-row (name value)
  "Create a hash-table row with name and value columns."
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "name" ht) name)
    (setf (gethash "value" ht) value)
    ht))

;;; ============================================================
;;; Overflow mode
;;; ============================================================

(deftest test-overflow-truncates-large-output
  "When rows > 200, output is truncated and a temp file is created."
  (let* ((rows (loop for i from 1 to 210
                     collect (make-test-row (format nil "item-~D" i)
                                            (format nil "val-~D" i))))
         (out (make-string-output-stream))
         (cauldron.cli::*overflow-counter* 0))
    (cauldron.cli:emit-table rows :stream out)
    (let ((output (get-output-stream-string out)))
      ;; Should show truncation notice
      (is-not-nil (search "Showing 200 of 210 rows" output))
      ;; Should reference a temp file
      (is-not-nil (search "/tmp/cauldron-output-" output))
      ;; Should NOT contain item-201 in the main output before the notice
      ;; (but the temp file reference line will be there)
      ;; Verify the temp file was created
      (is-not-nil (probe-file "/tmp/cauldron-output-1.txt")))))

(deftest test-no-overflow-for-small-output
  "When rows <= 200, no truncation occurs."
  (let* ((rows (loop for i from 1 to 5
                     collect (make-test-row (format nil "item-~D" i)
                                            (format nil "val-~D" i))))
         (out (make-string-output-stream)))
    (cauldron.cli:emit-table rows :stream out)
    (let ((output (get-output-stream-string out)))
      ;; No overflow notice
      (is-nil (search "Showing" output))
      ;; All rows present
      (is-not-nil (search "item-1" output))
      (is-not-nil (search "item-5" output)))))

;;; ============================================================
;;; Metadata footer
;;; ============================================================

(deftest test-metadata-footer-with-timing
  "When *emit-start-time* is bound, footer shows rows and ms."
  (let* ((rows (list (make-test-row "a" "1") (make-test-row "b" "2")))
         (out (make-string-output-stream)))
    (cauldron.cli:with-timing
      (cauldron.cli:emit-table rows :stream out))
    (let ((output (get-output-stream-string out)))
      ;; Should have rows:2 in footer
      (is-not-nil (search "rows:2" output))
      ;; Should have ms indicator
      (is-not-nil (search "ms]" output)))))

(deftest test-no-metadata-footer-without-timing
  "When *emit-start-time* is not bound, no footer is printed."
  (let* ((rows (list (make-test-row "a" "1")))
         (out (make-string-output-stream))
         (cauldron.cli:*emit-start-time* nil))
    (cauldron.cli:emit-table rows :stream out)
    (let ((output (get-output-stream-string out)))
      (is-nil (search "rows:" output)))))

;;; ============================================================
;;; emit-error
;;; ============================================================

(deftest test-emit-error-json-output
  "emit-error in JSON mode produces error envelope."
  (let ((out (make-string-output-stream))
        (cauldron.cli:*output-format* :json))
    (let ((code (cauldron.cli:emit-error "something broke" :stream out)))
      (is-equal 1 code)
      (let ((output (get-output-stream-string out)))
        (is-not-nil (search "error" output))
        (is-not-nil (search "something broke" output))))))

(deftest test-emit-error-table-output
  "emit-error in table mode prints Error: message."
  (let ((out (make-string-output-stream))
        (cauldron.cli:*output-format* :table))
    (let ((code (cauldron.cli:emit-error "bad input" :stream out)))
      (is-equal 1 code)
      (let ((output (get-output-stream-string out)))
        (is-not-nil (search "Error: bad input" output))))))

(deftest test-emit-error-with-use-hint
  "emit-error with :use-hint appends usage hint."
  (let ((out (make-string-output-stream))
        (cauldron.cli:*output-format* :table))
    (cauldron.cli:emit-error "missing name" :use-hint "cauldron company create --name NAME" :stream out)
    (let ((output (get-output-stream-string out)))
      (is-not-nil (search "Error: missing name" output))
      (is-not-nil (search "Use: cauldron company create --name NAME" output)))))

;;; ============================================================
;;; with-timing
;;; ============================================================

(deftest test-with-timing-binds-start-time
  "with-timing binds *emit-start-time* to a non-nil value."
  (is-nil cauldron.cli:*emit-start-time*)
  (cauldron.cli:with-timing
    (is-not-nil cauldron.cli:*emit-start-time*)))
