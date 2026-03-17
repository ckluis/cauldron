;;;; test/crucible/api-test.lisp — API response helper tests
;;;; Phase 34D
(in-package :cauldron.test)

(defsuite :crucible-api)

;;; --- json-response ---

(deftest test-json-response-exists
  (is (fboundp 'cauldron.crucible:json-response)))

;;; --- json-error ---

(deftest test-json-error-exists
  (is (fboundp 'cauldron.crucible:json-error)))

;;; --- generate-example-value ---

(deftest test-crucible-generate-example-email
  (is-equal "name@example.com"
            (cauldron.crucible:generate-example-value "email" "email")))

(deftest test-crucible-generate-example-phone
  (is-equal "555-0100"
            (cauldron.crucible:generate-example-value "phone" "phone")))

(deftest test-crucible-generate-example-url
  (is-equal "https://example.com"
            (cauldron.crucible:generate-example-value "url" "website")))

(deftest test-crucible-generate-example-integer
  (is-equal "42"
            (cauldron.crucible:generate-example-value "integer" "count")))

(deftest test-crucible-generate-example-boolean
  (is-equal "true"
            (cauldron.crucible:generate-example-value "boolean" "active")))

;;; --- describe-validation ---

(deftest test-crucible-describe-validation-email
  (let ((desc (cauldron.crucible:describe-validation "email" nil)))
    (is (search "@" desc))))

(deftest test-crucible-describe-validation-required
  (let ((desc (cauldron.crucible:describe-validation "text" t)))
    (is (search "Required" desc))))

(deftest test-crucible-describe-validation-optional
  (let ((desc (cauldron.crucible:describe-validation "text" nil)))
    (is-nil (search "Required" desc))))

;;; --- role-allows-p ---

(deftest test-crucible-role-allows-viewer-list
  (is (cauldron.crucible:role-allows-p "viewer" :list)))

(deftest test-crucible-role-allows-viewer-get
  (is (cauldron.crucible:role-allows-p "viewer" :get)))

(deftest test-crucible-role-denies-viewer-create
  (is-false (cauldron.crucible:role-allows-p "viewer" :create)))

(deftest test-crucible-role-allows-member-create
  (is (cauldron.crucible:role-allows-p "member" :create)))

(deftest test-crucible-role-allows-admin-delete
  (is (cauldron.crucible:role-allows-p "admin" :delete)))

;;; --- build-help-actions ---

(deftest test-crucible-help-actions-viewer-count
  (let ((actions (cauldron.crucible:build-help-actions "viewer" "/api/acme" "contacts" nil)))
    (is-equal 2 (length actions))))

(deftest test-crucible-help-actions-member-count
  (let ((actions (cauldron.crucible:build-help-actions "member" "/api/acme" "contacts" nil)))
    (is-equal 7 (length actions))))

;;; --- build-example-payload ---

(deftest test-crucible-build-example-payload-empty
  (let ((payload (cauldron.crucible:build-example-payload nil)))
    (is-nil payload)))

(deftest test-crucible-build-example-payload-with-fields
  (let* ((field1 (let ((ht (make-hash-table :test 'equal)))
                   (setf (gethash "name" ht) "email"
                         (gethash "field_type" ht) "email")
                   ht))
         (payload (cauldron.crucible:build-example-payload (list field1))))
    (is-equal 1 (length payload))
    (is-equal "name@example.com" (cdr (assoc "email" payload :test #'string=)))))
