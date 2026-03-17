;;;; test/reagent/registry-test.lisp — Resource registry tests
(in-package :cauldron.test)

(defsuite :reagent-registry)

;;; --- register-resource + find-resource-class ---

(deftest test-registry-register-and-find
  (let ((saved (make-hash-table :test 'eq)))
    ;; Save existing state
    (maphash (lambda (k v) (setf (gethash k saved) v))
             (cauldron.reagent:resource-registry))
    (unwind-protect
        (progn
          ;; Register a fake resource
          (cauldron.reagent:register-resource 'test-resource-xyz :fake-class)
          (is-equal :fake-class
                    (cauldron.reagent:find-resource-class 'test-resource-xyz)
                    "roundtrip register and find"))
      ;; Restore: remove test entry
      (remhash 'test-resource-xyz (cauldron.reagent:resource-registry)))))

(deftest test-registry-find-unknown
  (is-nil (cauldron.reagent:find-resource-class 'completely-unknown-resource-xyz)
          "unknown resource returns NIL"))

;;; --- all-resources ---

(deftest test-registry-all-resources
  (let ((saved (make-hash-table :test 'eq)))
    (maphash (lambda (k v) (setf (gethash k saved) v))
             (cauldron.reagent:resource-registry))
    (unwind-protect
        (progn
          (cauldron.reagent:register-resource 'res-a-test :class-a)
          (cauldron.reagent:register-resource 'res-b-test :class-b)
          (let ((all (cauldron.reagent:all-resources)))
            (is (listp all) "all-resources returns a list")
            (is (assoc 'res-a-test all) "contains res-a-test")
            (is (assoc 'res-b-test all) "contains res-b-test")
            (is-equal :class-a (cdr (assoc 'res-a-test all)))
            (is-equal :class-b (cdr (assoc 'res-b-test all)))))
      ;; Cleanup
      (remhash 'res-a-test (cauldron.reagent:resource-registry))
      (remhash 'res-b-test (cauldron.reagent:resource-registry)))))

;;; --- resource-registry ---

(deftest test-registry-returns-hash-table
  (let ((reg (cauldron.reagent:resource-registry)))
    (is (hash-table-p reg) "resource-registry returns a hash-table")))

;;; --- Isolation ---

(deftest test-registry-isolation
  "Registering in one test doesn't leak to next."
  (is-nil (cauldron.reagent:find-resource-class 'test-resource-xyz)
          "test-resource-xyz not present from previous test"))
