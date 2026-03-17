;;;; test/reagent/attributes-test.lisp — Reagent attribute type system tests
(in-package :cauldron.test)

(defsuite :reagent-attributes)

;;; --- attribute-pg-type ---

(deftest test-attribute-pg-type-string
  (is-equal "VARCHAR(255)" (cauldron.reagent:attribute-pg-type :string)))

(deftest test-attribute-pg-type-text
  (is-equal "TEXT" (cauldron.reagent:attribute-pg-type :text)))

(deftest test-attribute-pg-type-integer
  (is-equal "INTEGER" (cauldron.reagent:attribute-pg-type :integer)))

(deftest test-attribute-pg-type-boolean
  (is-equal "BOOLEAN" (cauldron.reagent:attribute-pg-type :boolean)))

(deftest test-attribute-pg-type-uuid
  (is-equal "UUID" (cauldron.reagent:attribute-pg-type :uuid)))

(deftest test-attribute-pg-type-timestamp
  (is-equal "TIMESTAMP WITH TIME ZONE" (cauldron.reagent:attribute-pg-type :timestamp)))

(deftest test-attribute-pg-type-unknown
  (signals-condition error
    (cauldron.reagent:attribute-pg-type :nonexistent)))

;;; --- attribute-form-field ---

(deftest test-attribute-form-field-string
  (is-equal :text-input (cauldron.reagent:attribute-form-field :string)))

(deftest test-attribute-form-field-text
  (is-equal :textarea (cauldron.reagent:attribute-form-field :text)))

(deftest test-attribute-form-field-boolean
  (is-equal :checkbox-input (cauldron.reagent:attribute-form-field :boolean)))

(deftest test-attribute-form-field-unknown
  (is-equal :text-input (cauldron.reagent:attribute-form-field :nonexistent)))

;;; --- parse-attribute-spec ---

(deftest test-parse-attribute-spec-simple
  (let ((attr (cauldron.reagent::parse-attribute-spec '(name))))
    (is-equal 'name (cauldron.reagent::attribute-def-name attr))
    (is-equal :string (cauldron.reagent::attribute-def-type attr))
    (is-false (cauldron.reagent::attribute-def-required attr))))

(deftest test-parse-attribute-spec-full
  (let ((attr (cauldron.reagent::parse-attribute-spec
               '(email :type :string :required t :max-length 255))))
    (is-equal 'email (cauldron.reagent::attribute-def-name attr))
    (is-equal :string (cauldron.reagent::attribute-def-type attr))
    (is-true (cauldron.reagent::attribute-def-required attr))
    (is-equal 255 (cauldron.reagent::attribute-def-max-length attr))
    (is-equal "VARCHAR(255)" (cauldron.reagent::attribute-def-pg-type attr))
    (is-equal :text-input (cauldron.reagent::attribute-def-form-field attr))))

(deftest test-parse-attribute-spec-with-constraints
  (let ((attr (cauldron.reagent::parse-attribute-spec
               '(role :type :keyword :one-of (:user :admin) :default :user))))
    (is-equal :keyword (cauldron.reagent::attribute-def-type attr))
    (is-equal '(:user :admin) (cauldron.reagent::attribute-def-one-of attr))
    (is-equal :user (cauldron.reagent::attribute-def-default attr))))

;;; --- parse-action-spec ---

(deftest test-parse-action-spec
  (let ((result (cauldron.reagent::parse-action-spec
                 '(create :accept (name email) :validate check-fn))))
    (is-equal 'create (getf result :name))
    (is-equal '(name email) (getf result :accept))
    (is-equal 'check-fn (getf result :validate))))

;;; --- parse-policy-spec ---

(deftest test-parse-policy-spec
  (let ((result (cauldron.reagent::parse-policy-spec '(allow :read :always))))
    (is-equal 'allow (getf result :disposition))
    (is-equal :read (getf result :action))
    (is-equal :always (getf result :condition))))

;;; --- parse-relationship-spec ---

(deftest test-parse-relationship-spec-belongs-to
  (let ((result (cauldron.reagent::parse-relationship-spec
                 '(:belongs-to user :foreign-key :user-id))))
    (is-equal :belongs-to (getf result :type))
    (is-equal 'user (getf result :target))
    (is-equal :user-id (getf result :foreign-key))))

(deftest test-parse-relationship-spec-has-many
  (let ((result (cauldron.reagent::parse-relationship-spec
                 '(:has-many comments))))
    (is-equal :has-many (getf result :type))
    (is-equal 'comments (getf result :target))
    (is-nil (getf result :foreign-key))))

(deftest test-parse-relationship-spec-many-to-many
  (let ((result (cauldron.reagent::parse-relationship-spec
                 '(:many-to-many tags :through :post-tags))))
    (is-equal :many-to-many (getf result :type))
    (is-equal 'tags (getf result :target))
    (is-equal :post-tags (getf result :through))))
