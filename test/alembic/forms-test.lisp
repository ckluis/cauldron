;;;; test/alembic/forms-test.lisp — Alembic form helper tests
(in-package :cauldron.test)

(defsuite :alembic-forms)

;;; --- text-input ---

(deftest test-text-input-basic
  "text-input generates a div with label and input."
  (let ((result (cauldron.alembic:text-input "username" :label "Username")))
    (is (listp result))
    (is-equal :div (first result))
    ;; Should contain an :input element
    (is (find :input (rest result) :key (lambda (x) (when (listp x) (first x)))))))

(deftest test-text-input-with-value
  (let ((result (cauldron.alembic:text-input "name" :value "John")))
    ;; Find the :input element and check :value
    (let ((input (find :input (rest result) :key (lambda (x) (when (listp x) (first x))))))
      (is-not-nil input)
      (is-not-nil (member :value input)))))

(deftest test-text-input-with-required
  (let ((result (cauldron.alembic:text-input "email" :required t)))
    (let ((input (find :input (rest result) :key (lambda (x) (when (listp x) (first x))))))
      (is-not-nil (member :required input)))))

(deftest test-text-input-with-label
  (let ((result (cauldron.alembic:text-input "name" :label "Full Name")))
    ;; Should contain a :label element
    (is (find :label (rest result) :key (lambda (x) (when (listp x) (first x)))))))

(deftest test-text-input-without-label
  (let ((result (cauldron.alembic:text-input "hidden_field")))
    ;; No label element when :label not provided
    (is-false (find :label (rest result) :key (lambda (x) (when (listp x) (first x)))))))

(deftest test-text-input-with-errors
  (let ((result (cauldron.alembic:text-input "email" :errors '("is required"))))
    ;; Should have field-error in class
    (let ((class-val (getf (rest result) :class)))
      (is (search "field-error" class-val)))
    ;; Should contain error span
    (is (find :span (rest result) :key (lambda (x) (when (listp x) (first x)))))))

(deftest test-text-input-custom-id
  (let ((result (cauldron.alembic:text-input "name" :id "custom-id")))
    (let ((input (find :input (rest result) :key (lambda (x) (when (listp x) (first x))))))
      (is-equal "custom-id" (getf (rest input) :id)))))

(deftest test-text-input-generated-id
  (let ((result (cauldron.alembic:text-input "username")))
    (let ((input (find :input (rest result) :key (lambda (x) (when (listp x) (first x))))))
      (is-equal "field-username" (getf (rest input) :id)))))

;;; --- hidden-input ---

(deftest test-hidden-input
  (let ((result (cauldron.alembic:hidden-input "token" "abc123")))
    (is-equal :input (first result))
    (is-equal "hidden" (getf (rest result) :type))
    (is-equal "token" (getf (rest result) :name))
    (is-equal "abc123" (getf (rest result) :value))))

;;; --- textarea ---

(deftest test-textarea-basic
  (let ((result (cauldron.alembic:textarea "bio" :label "Bio")))
    (is (listp result))
    (is-equal :div (first result))
    (is (find :textarea (rest result) :key (lambda (x) (when (listp x) (first x)))))))

(deftest test-textarea-with-value
  (let ((result (cauldron.alembic:textarea "bio" :value "Hello world")))
    (let ((ta (find :textarea (rest result) :key (lambda (x) (when (listp x) (first x))))))
      (is-not-nil ta)
      ;; Last element should be the value
      (is-equal "Hello world" (car (last ta))))))

;;; --- select-input ---

(deftest test-select-input-basic
  (let ((result (cauldron.alembic:select-input "color"
                  '("red" "green" "blue")
                  :label "Color")))
    (is-equal :div (first result))
    (is (find :select (rest result) :key (lambda (x) (when (listp x) (first x)))))))

(deftest test-select-input-with-pairs
  (let ((result (cauldron.alembic:select-input "status"
                  '(("active" . "Active") ("inactive" . "Inactive")))))
    (let ((sel (find :select (rest result) :key (lambda (x) (when (listp x) (first x))))))
      (is-not-nil sel)
      ;; Should have :option elements
      (let ((options (remove-if-not (lambda (x) (and (listp x) (eq :option (first x)))) sel)))
        (is-equal 2 (length options))))))

(deftest test-select-input-selected
  (let ((result (cauldron.alembic:select-input "size"
                  '("S" "M" "L")
                  :selected "M")))
    (let ((sel (find :select (rest result) :key (lambda (x) (when (listp x) (first x))))))
      (let ((options (remove-if-not (lambda (x) (and (listp x) (eq :option (first x)))) sel)))
        ;; "M" option should have :selected t
        (let ((m-opt (find "M" options :test #'equal
                           :key (lambda (x) (getf (rest x) :value)))))
          (is-not-nil (member :selected m-opt)))))))

;;; --- checkbox-input ---

(deftest test-checkbox-input-basic
  (let ((result (cauldron.alembic:checkbox-input "agree" :label "I agree")))
    (is-equal :div (first result))
    (let ((input (find :input (rest result) :key (lambda (x) (when (listp x) (first x))))))
      (is-equal "checkbox" (getf (rest input) :type)))))

(deftest test-checkbox-input-checked
  (let ((result (cauldron.alembic:checkbox-input "agree" :checked t)))
    (let ((input (find :input (rest result) :key (lambda (x) (when (listp x) (first x))))))
      (is-not-nil (member :checked input)))))

;;; --- number-input ---

(deftest test-number-input
  "number-input delegates to text-input with type=number."
  (let ((result (cauldron.alembic:number-input "age" :label "Age")))
    (is-equal :div (first result))
    (let ((input (find :input (rest result) :key (lambda (x) (when (listp x) (first x))))))
      (is-equal "number" (getf (rest input) :type)))))

;;; --- submit-button ---

(deftest test-submit-button-default
  (let ((result (cauldron.alembic:submit-button)))
    (is-equal :button (first result))
    (is-equal "submit" (getf (rest result) :type))
    ;; Last element is the label
    (is-equal "Submit" (car (last result)))))

(deftest test-submit-button-custom-label
  (let ((result (cauldron.alembic:submit-button :label "Save")))
    (is-equal "Save" (car (last result)))))

(deftest test-submit-button-disabled
  (let ((result (cauldron.alembic:submit-button :disabled t)))
    (is-not-nil (member :disabled result))))

;;; --- form-for ---

(deftest test-form-for-basic
  "form-for with no body fields produces :form."
  (let ((cauldron.alembic::*csrf-token* nil))
    (let ((result (cauldron.alembic:form-for "/submit")))
      (is-equal :form (first result))
      (is-equal "/submit" (getf (rest result) :action)))))

(deftest test-form-for-default-method-post
  (let ((cauldron.alembic::*csrf-token* nil))
    (let ((result (cauldron.alembic:form-for "/create")))
      (is-equal "POST" (getf (rest result) :method)))))

(deftest test-form-for-with-csrf
  (let ((cauldron.alembic::*csrf-token* "token123"))
    (let ((result (cauldron.alembic:form-for "/action")))
      ;; Should contain a hidden input with the CSRF token
      (is (find "_csrf_token" (rest result)
                :test (lambda (name x)
                        (and (listp x)
                             (eq :input (first x))
                             (equal name (getf (rest x) :name)))))))))
