;;;; test/forge/crud-test.lisp — Forge CRUD rendering tests
(in-package :cauldron.test)

(defsuite :forge-crud)

;;; --- render-form-field ---

(deftest test-render-form-field-textarea
  (let* ((field (cauldron.forge::make-forge-field-config
                 :name 'bio :type :textarea :label "Bio"))
         (result (cauldron.forge::render-form-field field)))
    (is-not-nil result)
    (is (listp result))))

(deftest test-render-form-field-select
  (let* ((field (cauldron.forge::make-forge-field-config
                 :name 'role :type :select :label "Role"
                 :options '((:admin . "Admin") (:user . "User"))))
         (result (cauldron.forge::render-form-field field)))
    (is-not-nil result)))

(deftest test-render-form-field-checkbox
  (let* ((field (cauldron.forge::make-forge-field-config
                 :name 'active :type :checkbox :label "Active"))
         (result (cauldron.forge::render-form-field field)))
    (is-not-nil result)))

(deftest test-render-form-field-number
  (let* ((field (cauldron.forge::make-forge-field-config
                 :name 'age :type :number :label "Age"))
         (result (cauldron.forge::render-form-field field)))
    (is-not-nil result)))

(deftest test-render-form-field-default-text
  (let* ((field (cauldron.forge::make-forge-field-config
                 :name 'name :type :text :label "Name"))
         (result (cauldron.forge::render-form-field field)))
    (is-not-nil result)))

(deftest test-render-form-field-required
  (let* ((field (cauldron.forge::make-forge-field-config
                 :name 'email :type :text :label "Email" :required t))
         (result (cauldron.forge::render-form-field field)))
    (is-not-nil result)))

;;; --- Full field pipeline: attribute → config → render ---

(deftest test-attr-to-render-pipeline-text
  "attribute-to-form-field → render-form-field roundtrip for text."
  (let* ((field (cauldron.forge::attribute-to-form-field 'email))
         (rendered (cauldron.forge::render-form-field field)))
    (is-not-nil rendered)))

(deftest test-attr-to-render-pipeline-textarea
  (let* ((field (cauldron.forge::attribute-to-form-field '(bio :type text)))
         (rendered (cauldron.forge::render-form-field field)))
    (is-not-nil rendered)))

(deftest test-attr-to-render-pipeline-select
  (let* ((field (cauldron.forge::attribute-to-form-field '(role :one-of (:admin :user))))
         (rendered (cauldron.forge::render-form-field field)))
    (is-not-nil rendered)))

(deftest test-attr-to-render-pipeline-checkbox
  (let* ((field (cauldron.forge::attribute-to-form-field '(active :type boolean)))
         (rendered (cauldron.forge::render-form-field field)))
    (is-not-nil rendered)))

(deftest test-attr-to-render-pipeline-number
  (let* ((field (cauldron.forge::attribute-to-form-field '(count :type integer)))
         (rendered (cauldron.forge::render-form-field field)))
    (is-not-nil rendered)))
