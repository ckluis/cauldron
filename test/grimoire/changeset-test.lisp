;;;; test/grimoire/changeset-test.lisp — Changeset validation tests
(in-package :cauldron.test)

(defsuite :grimoire-changeset)

;;; --- Cast ---

(deftest test-cast-creates-changeset
  (let* ((params (let ((ht (make-hash-table :test 'equal)))
                   (setf (gethash "name" ht) "Alice")
                   (setf (gethash "email" ht) "alice@test.com")
                   (setf (gethash "admin" ht) t) ; not in allowed fields
                   ht))
         (cs (cauldron.grimoire:cast nil params '("name" "email")
                                     :action :insert
                                     :source "users")))
    (is-true (cauldron.grimoire:changeset-valid-p cs))
    ;; "admin" should be filtered out
    (let ((changes (cauldron.grimoire:changeset-changes cs)))
      (is-equal "Alice" (gethash "name" changes))
      (is-equal "alice@test.com" (gethash "email" changes))
      (is-nil (gethash "admin" changes)))))

;;; --- validate-required ---

(deftest test-validate-required-present
  (let* ((params (let ((ht (make-hash-table :test 'equal)))
                   (setf (gethash "name" ht) "Alice")
                   ht))
         (cs (cauldron.grimoire:cast nil params '("name") :action :insert)))
    (cauldron.grimoire:validate-required cs "name")
    (is-true (cauldron.grimoire:changeset-valid-p cs))))

(deftest test-validate-required-missing
  (let* ((params (make-hash-table :test 'equal))
         (cs (cauldron.grimoire:cast nil params '("name") :action :insert)))
    (cauldron.grimoire:validate-required cs "name")
    (is-nil (cauldron.grimoire:changeset-valid-p cs))
    (is (> (length (cauldron.grimoire:changeset-errors cs)) 0))))

(deftest test-validate-required-empty-string
  (let* ((params (let ((ht (make-hash-table :test 'equal)))
                   (setf (gethash "name" ht) "")
                   ht))
         (cs (cauldron.grimoire:cast nil params '("name") :action :insert)))
    (cauldron.grimoire:validate-required cs "name")
    (is-nil (cauldron.grimoire:changeset-valid-p cs))))

;;; --- validate-length ---

(deftest test-validate-length-min
  (let* ((params (let ((ht (make-hash-table :test 'equal)))
                   (setf (gethash "name" ht) "ab")
                   ht))
         (cs (cauldron.grimoire:cast nil params '("name") :action :insert)))
    (cauldron.grimoire:validate-length cs "name" :min 3)
    (is-nil (cauldron.grimoire:changeset-valid-p cs))))

(deftest test-validate-length-max
  (let* ((params (let ((ht (make-hash-table :test 'equal)))
                   (setf (gethash "name" ht) "a very long name that exceeds limits")
                   ht))
         (cs (cauldron.grimoire:cast nil params '("name") :action :insert)))
    (cauldron.grimoire:validate-length cs "name" :max 10)
    (is-nil (cauldron.grimoire:changeset-valid-p cs))))

(deftest test-validate-length-ok
  (let* ((params (let ((ht (make-hash-table :test 'equal)))
                   (setf (gethash "name" ht) "Alice")
                   ht))
         (cs (cauldron.grimoire:cast nil params '("name") :action :insert)))
    (cauldron.grimoire:validate-length cs "name" :min 2 :max 50)
    (is-true (cauldron.grimoire:changeset-valid-p cs))))

;;; --- validate-inclusion ---

(deftest test-validate-inclusion-valid
  (let* ((params (let ((ht (make-hash-table :test 'equal)))
                   (setf (gethash "role" ht) "admin")
                   ht))
         (cs (cauldron.grimoire:cast nil params '("role") :action :insert)))
    (cauldron.grimoire:validate-inclusion cs "role" '("admin" "user" "mod"))
    (is-true (cauldron.grimoire:changeset-valid-p cs))))

(deftest test-validate-inclusion-invalid
  (let* ((params (let ((ht (make-hash-table :test 'equal)))
                   (setf (gethash "role" ht) "superadmin")
                   ht))
         (cs (cauldron.grimoire:cast nil params '("role") :action :insert)))
    (cauldron.grimoire:validate-inclusion cs "role" '("admin" "user" "mod"))
    (is-nil (cauldron.grimoire:changeset-valid-p cs))))

;;; --- Valid changeset → changeset-valid-p true ---

(deftest test-valid-changeset
  (let* ((params (let ((ht (make-hash-table :test 'equal)))
                   (setf (gethash "name" ht) "Alice")
                   (setf (gethash "email" ht) "alice@test.com")
                   ht))
         (cs (cauldron.grimoire:cast nil params '("name" "email")
                                     :action :insert :source "users")))
    (cauldron.grimoire:validate-required cs "name" "email")
    (cauldron.grimoire:validate-length cs "name" :min 1 :max 100)
    (is-true (cauldron.grimoire:changeset-valid-p cs))))

;;; --- Invalid changeset → apply-changeset signals error ---

(deftest test-apply-invalid-changeset
  (let* ((params (make-hash-table :test 'equal))
         (cs (cauldron.grimoire:cast nil params '("name") :action :insert :source "users")))
    (cauldron.grimoire:validate-required cs "name")
    (signals-condition cauldron.grimoire:query-error
      (cauldron.grimoire:apply-changeset cs))))

;;; --- apply-changeset generates query ---

(deftest test-apply-valid-changeset
  (let* ((params (let ((ht (make-hash-table :test 'equal)))
                   (setf (gethash "name" ht) "Alice")
                   ht))
         (cs (cauldron.grimoire:cast nil params '("name")
                                     :action :insert :source "users")))
    (let ((query (cauldron.grimoire:apply-changeset cs)))
      (multiple-value-bind (sql query-params) (cauldron.grimoire:to-sql query)
        (is (search "INSERT INTO users" sql))
        (is (search "$1" sql))
        (is (member "Alice" query-params :test #'equal))))))
