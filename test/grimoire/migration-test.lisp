;;;; test/grimoire/migration-test.lisp — Grimoire migration system tests
(in-package :cauldron.test)

(defsuite :grimoire-migration)

;;; --- applied-versions ---

(deftest test-applied-versions-basic
  "applied-versions extracts version strings from query results."
  (let ((result (cauldron.grimoire::applied-versions
                 (lambda (sql)
                   (declare (ignore sql))
                   (let ((r1 (make-hash-table :test 'equal))
                         (r2 (make-hash-table :test 'equal)))
                     (setf (gethash "version" r1) "20240101120000")
                     (setf (gethash "version" r2) "20240201120000")
                     (list r1 r2))))))
    (is-equal '("20240101120000" "20240201120000") result)))

(deftest test-applied-versions-empty
  (let ((result (cauldron.grimoire::applied-versions
                 (lambda (sql) (declare (ignore sql)) '()))))
    (is-equal '() result)))

;;; --- migration-status ---

(deftest test-migration-status-mixed
  "migration-status shows pending/applied for each migration."
  (let ((cauldron.grimoire::*migrations*
          (list (cauldron.grimoire::make-migration-def
                 :name 'create-users :version "20240101120000")
                (cauldron.grimoire::make-migration-def
                 :name 'create-posts :version "20240201120000")
                (cauldron.grimoire::make-migration-def
                 :name 'add-tags :version "20240301120000"))))
    (let ((result (cauldron.grimoire:migration-status
                   (lambda (sql)
                     (declare (ignore sql))
                     (let ((r1 (make-hash-table :test 'equal)))
                       (setf (gethash "version" r1) "20240101120000")
                       (list r1))))))
      (is-equal 3 (length result))
      ;; First migration is applied
      (is-equal 'create-users (first (first result)))
      (is-equal :applied (third (first result)))
      ;; Second and third are pending
      (is-equal :pending (third (second result)))
      (is-equal :pending (third (third result))))))

(deftest test-migration-status-all-applied
  (let ((cauldron.grimoire::*migrations*
          (list (cauldron.grimoire::make-migration-def
                 :name 'mig-a :version "20240101000000")
                (cauldron.grimoire::make-migration-def
                 :name 'mig-b :version "20240201000000"))))
    (let ((result (cauldron.grimoire:migration-status
                   (lambda (sql)
                     (declare (ignore sql))
                     (let ((r1 (make-hash-table :test 'equal))
                           (r2 (make-hash-table :test 'equal)))
                       (setf (gethash "version" r1) "20240101000000")
                       (setf (gethash "version" r2) "20240201000000")
                       (list r1 r2))))))
      (is-equal :applied (third (first result)))
      (is-equal :applied (third (second result))))))

(deftest test-migration-status-all-pending
  (let ((cauldron.grimoire::*migrations*
          (list (cauldron.grimoire::make-migration-def
                 :name 'mig-x :version "20240101000000"))))
    (let ((result (cauldron.grimoire:migration-status
                   (lambda (sql) (declare (ignore sql)) '()))))
      (is-equal 1 (length result))
      (is-equal :pending (third (first result))))))

(deftest test-migration-status-empty-migrations
  (let ((cauldron.grimoire::*migrations* '()))
    (let ((result (cauldron.grimoire:migration-status
                   (lambda (sql) (declare (ignore sql)) '()))))
      (is-equal '() result))))

(deftest test-migration-status-version-in-result
  (let ((cauldron.grimoire::*migrations*
          (list (cauldron.grimoire::make-migration-def
                 :name 'create-users :version "20240515093000"))))
    (let ((result (cauldron.grimoire:migration-status
                   (lambda (sql) (declare (ignore sql)) '()))))
      (is-equal "20240515093000" (second (first result))))))
