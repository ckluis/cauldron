;;;; test/forge/schema-browser-test.lisp — Schema browser + drift detection tests
(in-package :cauldron.test)

(defsuite :forge-schema-browser)

;;; Reuse helper from drift-test or define locally
(defun %make-drift-resource-config (name attributes)
  (cauldron.forge::make-forge-resource-config
   :name name
   :display-name (symbol-name name)
   :table-name (format nil "~As" (string-downcase (symbol-name name)))
   :attributes attributes))

;;; ============================================================
;;; detect-drift — comprehensive tests
;;; ============================================================

(deftest test-detect-drift-no-drift
  (let* ((rc (%make-drift-resource-config 'user '(name email)))
         (db-cols '(("name" "varchar" nil) ("email" "varchar" nil)
                    ("id" "uuid" nil)))
         (drifts (cauldron.forge:detect-drift rc db-cols)))
    (is-nil drifts)))

(deftest test-detect-drift-missing-in-db
  (let* ((rc (%make-drift-resource-config 'user '(name email phone)))
         (db-cols '(("name" "varchar" nil) ("email" "varchar" nil)
                    ("id" "uuid" nil)))
         (drifts (cauldron.forge:detect-drift rc db-cols)))
    (is-equal 1 (length drifts))
    (is-equal :missing-in-db (first (first drifts)))
    (is-equal "phone" (second (first drifts)))))

(deftest test-detect-drift-missing-in-code
  (let* ((rc (%make-drift-resource-config 'user '(name)))
         (db-cols '(("name" "varchar" nil) ("legacy_field" "text" nil)))
         (drifts (cauldron.forge:detect-drift rc db-cols)))
    (is-equal 1 (length drifts))
    (is-equal :missing-in-code (first (first drifts)))
    (is-equal "legacy_field" (second (first drifts)))))

(deftest test-detect-drift-standard-cols-excluded
  "Standard columns (id, created_at, updated_at, inserted_at) should not be flagged."
  (let* ((rc (%make-drift-resource-config 'user '(name)))
         (db-cols '(("name" "varchar" nil)
                    ("id" "uuid" nil)
                    ("created_at" "timestamptz" nil)
                    ("updated_at" "timestamptz" nil)
                    ("inserted_at" "timestamptz" nil)))
         (drifts (cauldron.forge:detect-drift rc db-cols)))
    (is-nil drifts)))

(deftest test-detect-drift-multiple-drifts
  (let* ((rc (%make-drift-resource-config 'user '(name age bio)))
         (db-cols '(("name" "varchar" nil) ("legacy" "text" nil)
                    ("old_field" "int" nil)))
         (drifts (cauldron.forge:detect-drift rc db-cols)))
    ;; age and bio missing in DB, legacy and old_field missing in code
    (is-equal 4 (length drifts))
    (is-equal 2 (count :missing-in-db drifts :key #'first))
    (is-equal 2 (count :missing-in-code drifts :key #'first))))

(deftest test-detect-drift-empty-resource
  "Resource with no attributes — only standard cols present → no drifts."
  (let* ((rc (%make-drift-resource-config 'empty '()))
         (db-cols '(("id" "uuid" nil) ("created_at" "timestamptz" nil)))
         (drifts (cauldron.forge:detect-drift rc db-cols)))
    (is-nil drifts)))

(deftest test-detect-drift-empty-db-columns
  "All defined cols should be reported as missing in DB."
  (let* ((rc (%make-drift-resource-config 'user '(name email)))
         (db-cols '())
         (drifts (cauldron.forge:detect-drift rc db-cols)))
    (is-equal 2 (length drifts))
    (is (every (lambda (d) (eq :missing-in-db (first d))) drifts))))

(deftest test-detect-drift-order-preserved
  "missing-in-db drifts should appear before missing-in-code (due to nreverse)."
  (let* ((rc (%make-drift-resource-config 'user '(name new-col)))
         (db-cols '(("name" "varchar" nil) ("old-col" "text" nil)))
         (drifts (cauldron.forge:detect-drift rc db-cols)))
    ;; new-col missing in DB comes first, old-col missing in code comes second
    (is-equal :missing-in-db (first (first drifts)))
    (is-equal :missing-in-code (first (second drifts)))))

;;; ============================================================
;;; schema-browser-view
;;; ============================================================

(deftest test-schema-browser-view-returns-conn
  (let* ((conn (cauldron.crucible:make-conn :method :get :path "/forge/schema"))
         (result (cauldron.forge:schema-browser-view conn nil)))
    (is-not-nil result)))

(deftest test-schema-browser-view-contains-heading
  (let* ((conn (cauldron.crucible:make-conn :method :get :path "/forge/schema"))
         (result (cauldron.forge:schema-browser-view conn nil))
         (body (cauldron.crucible:conn-resp-body result)))
    (is (search "Schema Browser" body))))

(deftest test-schema-browser-view-contains-table-structure
  (let* ((conn (cauldron.crucible:make-conn :method :get :path "/forge/schema"))
         (result (cauldron.forge:schema-browser-view conn nil))
         (body (cauldron.crucible:conn-resp-body result)))
    (is (search "Resource" body))
    (is (search "Table" body))
    (is (search "Defined Columns" body))
    (is (search "Status" body))))
