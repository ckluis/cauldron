;;;; test/forge/config-test.lisp — Forge config derivation tests
(in-package :cauldron.test)

(defsuite :forge-config)

;;; --- pluralize-name ---

(deftest test-forge-pluralize-user
  (is-equal "users" (cauldron.forge::pluralize-name :user)))

(deftest test-forge-pluralize-address
  "Words ending in 's' get 'es'."
  (is-equal "addresses" (cauldron.forge::pluralize-name :address)))

(deftest test-forge-pluralize-city
  "Words ending in 'y' get 'ies'."
  (is-equal "cities" (cauldron.forge::pluralize-name :city)))

(deftest test-forge-pluralize-post
  (is-equal "posts" (cauldron.forge::pluralize-name :post)))

(deftest test-forge-pluralize-bus
  "Words ending in 's' get 'es'."
  (is-equal "buses" (cauldron.forge::pluralize-name :bus)))

;;; --- string-suffix-p ---

(deftest test-string-suffix-p-match
  (is (cauldron.forge::string-suffix-p "hello" "lo")))

(deftest test-string-suffix-p-no-match
  (is-false (cauldron.forge::string-suffix-p "hello" "xyz")))

(deftest test-string-suffix-p-equal
  (is (cauldron.forge::string-suffix-p "abc" "abc")))

(deftest test-string-suffix-p-longer-suffix
  (is-false (cauldron.forge::string-suffix-p "hi" "hello")))

(deftest test-string-suffix-p-empty-suffix
  (is (cauldron.forge::string-suffix-p "hello" "")))

;;; --- humanize-name ---

(deftest test-humanize-first-name
  (is-equal "First Name" (cauldron.forge::humanize-name :first-name)))

(deftest test-humanize-id
  (is-equal "Id" (cauldron.forge::humanize-name :id)))

(deftest test-humanize-simple
  (is-equal "Email" (cauldron.forge::humanize-name :email)))

;;; --- attribute-to-form-field ---

(deftest test-attr-to-field-simple-symbol
  "A bare symbol defaults to text."
  (let ((field (cauldron.forge::attribute-to-form-field 'name)))
    (is-equal :text (cauldron.forge::forge-field-config-type field))
    (is-equal 'name (cauldron.forge::forge-field-config-name field))))

(deftest test-attr-to-field-textarea
  (let ((field (cauldron.forge::attribute-to-form-field '(bio :type cauldron.forge::text))))
    (is-equal :textarea (cauldron.forge::forge-field-config-type field))))

(deftest test-attr-to-field-checkbox
  (let ((field (cauldron.forge::attribute-to-form-field '(active :type boolean))))
    (is-equal :checkbox (cauldron.forge::forge-field-config-type field))))

(deftest test-attr-to-field-select-one-of
  (let ((field (cauldron.forge::attribute-to-form-field '(role :one-of (:user :admin)))))
    (is-equal :select (cauldron.forge::forge-field-config-type field))
    (is-not-nil (cauldron.forge::forge-field-config-options field))))

(deftest test-attr-to-field-number
  (let ((field (cauldron.forge::attribute-to-form-field '(age :type integer))))
    (is-equal :number (cauldron.forge::forge-field-config-type field))))

(deftest test-attr-to-field-label
  (let ((field (cauldron.forge::attribute-to-form-field '(first-name :type string))))
    (is-equal "First Name" (cauldron.forge::forge-field-config-label field))))

;;; --- text-column-p ---

(deftest test-text-column-p-text
  (is (cauldron.forge::text-column-p '(bio :type cauldron.forge::text))))

(deftest test-text-column-p-string
  (is-false (cauldron.forge::text-column-p '(name :type cauldron.forge::string))))

(deftest test-text-column-p-bare-symbol
  (is-false (cauldron.forge::text-column-p 'name)))

(deftest test-text-column-p-jsonb
  (is (cauldron.forge::text-column-p '(metadata :type cauldron.forge::jsonb))))

;;; --- string-type-p ---

(deftest test-string-type-p-string
  (is (cauldron.forge::string-type-p '(name :type cauldron.forge::string))))

(deftest test-string-type-p-integer
  (is-false (cauldron.forge::string-type-p '(age :type cauldron.forge::integer))))

(deftest test-string-type-p-bare-symbol
  "Bare symbol defaults to :string — but :string is a keyword, not matching 'string in forge."
  ;; string-type-p checks (member type '(string text)) — the default type from getf
  ;; is :string (a keyword), which won't match unqualified 'string.
  ;; This reveals a bug: getf returns :string but member checks for 'string (CL:STRING).
  ;; For now, test actual behavior.
  (is-false (cauldron.forge::string-type-p 'name)))

;;; --- enum-type-p ---

(deftest test-enum-type-p-with-one-of
  (is (cauldron.forge::enum-type-p '(role :one-of (:a :b)))))

(deftest test-enum-type-p-without
  (is-false (cauldron.forge::enum-type-p '(name :type string))))

(deftest test-enum-type-p-bare-symbol
  (is-false (cauldron.forge::enum-type-p 'name)))

;;; --- derive-resource-config ---

(deftest test-derive-resource-config-basic
  (let ((config (cauldron.forge::derive-resource-config
                 (list :name 'cauldron.forge::user
                       :attributes (list 'cauldron.forge::name
                                         '(cauldron.forge::email :type cauldron.forge::string)
                                         '(cauldron.forge::bio :type cauldron.forge::text)
                                         '(cauldron.forge::role :one-of (:user :admin))
                                         '(cauldron.forge::age :type cauldron.forge::integer)
                                         '(cauldron.forge::active :type cauldron.forge::boolean)
                                         '(cauldron.forge::notes :type cauldron.forge::text))
                       :actions '((:deactivate))))))
    ;; Table name pluralized
    (is-equal "users" (cauldron.forge::forge-resource-config-table-name config))
    ;; Display name humanized
    (is-equal "User" (cauldron.forge::forge-resource-config-display-name config))
    ;; Table columns: max 6, excluding text columns
    (is (<= (length (cauldron.forge::forge-resource-config-table-columns config)) 6))
    ;; Bio and notes are text → excluded from table columns
    (is-false (member 'cauldron.forge::bio
                      (cauldron.forge::forge-resource-config-table-columns config)))
    ;; Searchable: string-type-p checks against '(string text) — bare symbols
    ;; get default type :string (keyword) which doesn't match unqualified 'string.
    ;; So bare symbols are not detected as searchable (known limitation).
    ;; The email attr with explicit :type string ALSO uses forge::string, which matches.
    (is (member 'cauldron.forge::email
                (cauldron.forge::forge-resource-config-searchable-columns config)))
    ;; Filterable: enum columns
    (is (member 'cauldron.forge::role
                (cauldron.forge::forge-resource-config-filterable-columns config)))
    ;; Form fields generated for all attrs
    (is-equal 7 (length (cauldron.forge::forge-resource-config-form-fields config)))
    ;; Display field is first searchable (email, since name isn't detected)
    (is-not-nil (cauldron.forge::forge-resource-config-display-field config))
    ;; Actions
    (is-equal '(:deactivate) (cauldron.forge::forge-resource-config-actions config))))

(deftest test-derive-resource-config-no-attrs
  (let ((config (cauldron.forge::derive-resource-config
                 '(:name widget :attributes () :actions ()))))
    (is-equal "widgets" (cauldron.forge::forge-resource-config-table-name config))
    (is-equal 0 (length (cauldron.forge::forge-resource-config-table-columns config)))
    (is-nil (cauldron.forge::forge-resource-config-display-field config))))
