;;;; test/logging/context-test.lisp — Tests for request-scoped logging context
(in-package :cauldron.test)

(defsuite :logging-context)

;;; --- make-context ---

(deftest test-make-context-returns-hash-table
  (let ((ctx (cauldron.logging:make-context)))
    (is (hash-table-p ctx))
    (is-equal 0 (hash-table-count ctx))))

;;; --- log-set ---

(deftest test-log-set-stores-value
  (let ((cauldron.logging:*request-context* (cauldron.logging:make-context)))
    (cauldron.logging:log-set :method "GET")
    (is-equal "GET" (gethash :method cauldron.logging:*request-context*))))

(deftest test-log-set-noop-without-context
  "log-set should be a no-op when *request-context* is nil."
  (let ((cauldron.logging:*request-context* nil))
    ;; Should not error
    (cauldron.logging:log-set :foo "bar")
    (is-true t)))

(deftest test-log-set-overwrites
  (let ((cauldron.logging:*request-context* (cauldron.logging:make-context)))
    (cauldron.logging:log-set :status 200)
    (cauldron.logging:log-set :status 404)
    (is-equal 404 (gethash :status cauldron.logging:*request-context*))))

;;; --- log-inc ---

(deftest test-log-inc-initial
  (let ((cauldron.logging:*request-context* (cauldron.logging:make-context)))
    (cauldron.logging:log-inc :sql-count)
    (is-equal 1 (gethash :sql-count cauldron.logging:*request-context*))))

(deftest test-log-inc-accumulates
  (let ((cauldron.logging:*request-context* (cauldron.logging:make-context)))
    (cauldron.logging:log-inc :sql-count)
    (cauldron.logging:log-inc :sql-count)
    (cauldron.logging:log-inc :sql-count)
    (is-equal 3 (gethash :sql-count cauldron.logging:*request-context*))))

(deftest test-log-inc-custom-delta
  (let ((cauldron.logging:*request-context* (cauldron.logging:make-context)))
    (cauldron.logging:log-inc :sql-count 5)
    (is-equal 5 (gethash :sql-count cauldron.logging:*request-context*))))

(deftest test-log-inc-noop-without-context
  (let ((cauldron.logging:*request-context* nil))
    (cauldron.logging:log-inc :sql-count)
    (is-true t)))

;;; --- log-timing ---

(deftest test-log-timing-initial
  (let ((cauldron.logging:*request-context* (cauldron.logging:make-context)))
    (cauldron.logging:log-timing :sql-duration-ms 10)
    (is-equal 10 (gethash :sql-duration-ms cauldron.logging:*request-context*))))

(deftest test-log-timing-accumulates
  (let ((cauldron.logging:*request-context* (cauldron.logging:make-context)))
    (cauldron.logging:log-timing :sql-duration-ms 10)
    (cauldron.logging:log-timing :sql-duration-ms 8)
    (is-equal 18 (gethash :sql-duration-ms cauldron.logging:*request-context*))))

;;; --- log-emit ---

(deftest test-log-emit-writes-to-stream
  (let* ((output (make-string-output-stream))
         (cauldron.logging:*canonical-log-stream* output)
         (cauldron.logging:*canonical-log-enabled* t)
         (cauldron.logging:*request-context* (cauldron.logging:make-context)))
    (cauldron.logging:log-set :method "GET")
    (cauldron.logging:log-set :status 200)
    (cauldron.logging:log-emit)
    (let ((logged (get-output-stream-string output)))
      (is-not-nil (search "canonical-log-line" logged))
      (is-not-nil (search "method=GET" logged))
      (is-not-nil (search "status=200" logged)))))

(deftest test-log-emit-suppressed-when-disabled
  (let* ((output (make-string-output-stream))
         (cauldron.logging:*canonical-log-stream* output)
         (cauldron.logging:*canonical-log-enabled* nil)
         (cauldron.logging:*request-context* (cauldron.logging:make-context)))
    (cauldron.logging:log-set :method "GET")
    (cauldron.logging:log-emit)
    (is-equal "" (get-output-stream-string output))))

(deftest test-log-emit-noop-without-context
  (let* ((output (make-string-output-stream))
         (cauldron.logging:*canonical-log-stream* output)
         (cauldron.logging:*request-context* nil))
    (cauldron.logging:log-emit)
    (is-equal "" (get-output-stream-string output))))

(deftest test-log-emit-deterministic-key-order
  "Keys should be sorted alphabetically for deterministic output."
  (let* ((output (make-string-output-stream))
         (cauldron.logging:*canonical-log-stream* output)
         (cauldron.logging:*canonical-log-enabled* t)
         (cauldron.logging:*request-context* (cauldron.logging:make-context)))
    (cauldron.logging:log-set :z-last "last")
    (cauldron.logging:log-set :a-first "first")
    (cauldron.logging:log-emit)
    (let* ((logged (get-output-stream-string output))
           (pos-a (search "a_first" logged))
           (pos-z (search "z_last" logged)))
      (is-not-nil pos-a)
      (is-not-nil pos-z)
      (is (< pos-a pos-z)))))

;;; --- with-log-context ---

(deftest test-with-log-context-creates-context
  (let* ((output (make-string-output-stream))
         (cauldron.logging:*canonical-log-stream* output)
         (cauldron.logging:*canonical-log-enabled* t))
    (cauldron.logging:with-log-context ()
      (cauldron.logging:log-set :test "yes"))
    (let ((logged (get-output-stream-string output)))
      (is-not-nil (search "test=yes" logged)))))

(deftest test-with-log-context-emits-on-error
  "Context should emit even when body signals an error."
  (let* ((output (make-string-output-stream))
         (cauldron.logging:*canonical-log-stream* output)
         (cauldron.logging:*canonical-log-enabled* t))
    (ignore-errors
      (cauldron.logging:with-log-context ()
        (cauldron.logging:log-set :status 500)
        (error "boom")))
    (let ((logged (get-output-stream-string output)))
      (is-not-nil (search "status=500" logged)))))
