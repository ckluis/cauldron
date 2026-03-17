;;;; test/runtime/request-id-test.lisp — Tests for request ID generation
(in-package :cauldron.test)

(defsuite :request-id)

;;; --- generate-request-id ---

(deftest test-generate-request-id-returns-string
  (let ((id (cauldron.runtime:generate-request-id)))
    (is-true (stringp id))))

(deftest test-generate-request-id-length-32
  (let ((id (cauldron.runtime:generate-request-id)))
    (is-equal 32 (length id))))

(deftest test-generate-request-id-hex-chars-only
  (let ((id (cauldron.runtime:generate-request-id)))
    (is-true (every (lambda (ch)
                      (or (and (char>= ch #\0) (char<= ch #\9))
                          (and (char>= ch #\a) (char<= ch #\f))))
                    id))))

(deftest test-generate-request-id-lowercase
  (let ((id (cauldron.runtime:generate-request-id)))
    (is-equal id (string-downcase id))))

(deftest test-two-ids-are-different
  (let ((id1 (cauldron.runtime:generate-request-id))
        (id2 (cauldron.runtime:generate-request-id)))
    (is-true (not (string= id1 id2)))))

;;; --- *current-request-id* ---

(deftest test-current-request-id-default-nil
  (let ((cauldron.runtime:*current-request-id* nil))
    (is-nil cauldron.runtime:*current-request-id*)))

(deftest test-current-request-id-can-be-bound
  (let ((cauldron.runtime:*current-request-id* "test-id-123"))
    (is-equal "test-id-123" cauldron.runtime:*current-request-id*)))

(deftest test-current-request-id-dynamic-binding
  (let ((cauldron.runtime:*current-request-id* nil))
    (is-nil cauldron.runtime:*current-request-id*)
    (let ((cauldron.runtime:*current-request-id* "inner-id"))
      (is-equal "inner-id" cauldron.runtime:*current-request-id*))
    ;; After inner let exits, should be nil again
    (is-nil cauldron.runtime:*current-request-id*)))

(deftest test-current-request-id-with-generated-id
  (let ((cauldron.runtime:*current-request-id* (cauldron.runtime:generate-request-id)))
    (is-true (stringp cauldron.runtime:*current-request-id*))
    (is-equal 32 (length cauldron.runtime:*current-request-id*))))

(deftest test-generate-multiple-ids-all-unique
  (let ((ids (loop repeat 10 collect (cauldron.runtime:generate-request-id))))
    (is-equal 10 (length (remove-duplicates ids :test #'string=)))))
