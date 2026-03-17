;;;; test/forge/drift-test.lisp — Forge schema drift detection tests
(in-package :cauldron.test)

(defsuite :forge-drift)

;;; Helper to make a minimal resource config for testing
(defun %make-test-resource-config (name attributes)
  (cauldron.forge::make-forge-resource-config
   :name name
   :display-name (symbol-name name)
   :table-name (format nil "~As" (string-downcase (symbol-name name)))
   :attributes attributes))

;;; --- detect-drift: missing in DB ---

(deftest test-drift-missing-in-db
  (let* ((rc (%make-test-resource-config 'user '(name email role)))
         (db-columns '(("name" "varchar" nil) ("email" "varchar" nil)))
         (drifts (cauldron.forge:detect-drift rc db-columns)))
    (is-equal 1 (length drifts))
    (is-equal :missing-in-db (first (first drifts)))
    (is-equal "role" (second (first drifts)))))

;;; --- detect-drift: missing in code ---

(deftest test-drift-missing-in-code
  (let* ((rc (%make-test-resource-config 'user '(name)))
         (db-columns '(("name" "varchar" nil) ("extra_col" "text" nil)))
         (drifts (cauldron.forge:detect-drift rc db-columns)))
    (is-equal 1 (length drifts))
    (is-equal :missing-in-code (first (first drifts)))
    (is-equal "extra_col" (second (first drifts)))))

;;; --- detect-drift: standard columns excluded ---

(deftest test-drift-standard-columns-excluded
  (let* ((rc (%make-test-resource-config 'user '(name)))
         (db-columns '(("name" "varchar" nil)
                       ("id" "uuid" nil)
                       ("created_at" "timestamptz" nil)
                       ("updated_at" "timestamptz" nil)))
         (drifts (cauldron.forge:detect-drift rc db-columns)))
    (is-nil drifts)))

;;; --- detect-drift: no drift ---

(deftest test-drift-no-drift
  (let* ((rc (%make-test-resource-config 'user '(name email)))
         (db-columns '(("name" "varchar" nil) ("email" "varchar" nil)
                       ("id" "uuid" nil)))
         (drifts (cauldron.forge:detect-drift rc db-columns)))
    (is-nil drifts)))

;;; --- detect-drift: both directions ---

(deftest test-drift-both-directions
  (let* ((rc (%make-test-resource-config 'user '(name age)))
         (db-columns '(("name" "varchar" nil) ("bio" "text" nil)))
         (drifts (cauldron.forge:detect-drift rc db-columns)))
    (is-equal 2 (length drifts))
    ;; age is missing in DB
    (is (find :missing-in-db drifts :key #'first))
    ;; bio is missing in code
    (is (find :missing-in-code drifts :key #'first))))

;;; --- detect-drift: inserted_at is standard ---

(deftest test-drift-inserted-at-excluded
  (let* ((rc (%make-test-resource-config 'user '(name)))
         (db-columns '(("name" "varchar" nil) ("inserted_at" "timestamptz" nil)))
         (drifts (cauldron.forge:detect-drift rc db-columns)))
    (is-nil drifts)))
