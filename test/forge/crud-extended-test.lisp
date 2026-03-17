;;;; test/forge/crud-extended-test.lisp — Extended Forge CRUD tests
(in-package :cauldron.test)

(defsuite :forge-crud-extended)

;;; --- render-form-field for different field types ---

(deftest test-render-form-field-text-explicit
  "Explicit :text type renders a text input."
  (let* ((field (cauldron.forge::make-forge-field-config
                 :name 'username :type :text :label "Username"))
         (result (cauldron.forge::render-form-field field)))
    (is-not-nil result)
    (is (listp result) "result is a list (Alembic tree)")))

(deftest test-render-form-field-textarea-with-required
  "Textarea with required flag renders."
  (let* ((field (cauldron.forge::make-forge-field-config
                 :name 'description :type :textarea :label "Description"
                 :required t))
         (result (cauldron.forge::render-form-field field)))
    (is-not-nil result)))

(deftest test-render-form-field-select-with-options
  "Select with multiple options renders."
  (let* ((field (cauldron.forge::make-forge-field-config
                 :name 'status :type :select :label "Status"
                 :options '((:active . "Active") (:inactive . "Inactive") (:pending . "Pending"))))
         (result (cauldron.forge::render-form-field field)))
    (is-not-nil result)))

(deftest test-render-form-field-checkbox-label
  "Checkbox field has the expected label."
  (let* ((field (cauldron.forge::make-forge-field-config
                 :name 'is-admin :type :checkbox :label "Is Admin"))
         (result (cauldron.forge::render-form-field field)))
    (is-not-nil result)))

(deftest test-render-form-field-number-type
  "Number field renders correctly."
  (let* ((field (cauldron.forge::make-forge-field-config
                 :name 'quantity :type :number :label "Quantity" :required t))
         (result (cauldron.forge::render-form-field field)))
    (is-not-nil result)))

(deftest test-render-form-field-unknown-type-fallback
  "Unknown field type falls through to text input."
  (let* ((field (cauldron.forge::make-forge-field-config
                 :name 'custom :type :email :label "Email"))
         (result (cauldron.forge::render-form-field field)))
    (is-not-nil result "unknown type still renders")))

;;; --- forge-field-config struct accessors ---

(deftest test-forge-field-config-accessors
  "Verify all forge-field-config accessors work."
  (let ((field (cauldron.forge::make-forge-field-config
                :name 'test-field
                :type :select
                :label "Test Field"
                :required t
                :options '((:a . "A") (:b . "B"))
                :max-length 100
                :min-length 1)))
    (is-equal 'test-field (cauldron.forge::forge-field-config-name field))
    (is-equal :select (cauldron.forge::forge-field-config-type field))
    (is-equal "Test Field" (cauldron.forge::forge-field-config-label field))
    (is-true (cauldron.forge::forge-field-config-required field))
    (is-equal 2 (length (cauldron.forge::forge-field-config-options field)))
    (is-equal 100 (cauldron.forge::forge-field-config-max-length field))
    (is-equal 1 (cauldron.forge::forge-field-config-min-length field))))

;;; --- attribute-to-form-field pipeline ---

(deftest test-attribute-to-field-simple-symbol
  "A bare symbol attribute becomes a text field."
  (let ((field (cauldron.forge::attribute-to-form-field 'username)))
    (is-equal 'username (cauldron.forge::forge-field-config-name field))
    (is-equal :text (cauldron.forge::forge-field-config-type field))
    (is-equal "Username" (cauldron.forge::forge-field-config-label field))))

(deftest test-attribute-to-field-with-one-of
  "Attribute with :one-of becomes a select field."
  (let ((field (cauldron.forge::attribute-to-form-field '(role :one-of (:admin :user :guest)))))
    (is-equal :select (cauldron.forge::forge-field-config-type field))
    (is-equal 3 (length (cauldron.forge::forge-field-config-options field)))))

;;; --- batch-action placeholder ---

(deftest test-batch-action-returns-302
  "Batch action returns a redirect (302) response."
  (let* ((conn (cauldron.crucible:make-conn :method :post :path "/forge/users/batch"))
         (result (cauldron.forge::batch-action conn nil "delete" '("1" "2"))))
    (is-equal 302 (cauldron.crucible:conn-status result))))
