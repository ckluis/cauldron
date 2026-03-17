;;;; test/reagent/policies-test.lisp — Reagent authorization policy tests
(in-package :cauldron.test)

(defsuite :reagent-policies)

;;; --- policy-matches-action-p ---

(deftest test-policy-matches-exact-action
  (let ((rule (cauldron.reagent::make-policy-rule :action :read)))
    (is-true (cauldron.reagent::policy-matches-action-p rule :read))))

(deftest test-policy-does-not-match-different-action
  (let ((rule (cauldron.reagent::make-policy-rule :action :read)))
    (is-false (cauldron.reagent::policy-matches-action-p rule :write))))

(deftest test-policy-all-matches-any-action
  (let ((rule (cauldron.reagent::make-policy-rule :action :all)))
    (is-true (cauldron.reagent::policy-matches-action-p rule :read))
    (is-true (cauldron.reagent::policy-matches-action-p rule :write))
    (is-true (cauldron.reagent::policy-matches-action-p rule :delete))))

;;; --- evaluate-condition ---

(deftest test-evaluate-condition-always
  (is-true (cauldron.reagent::evaluate-condition :always nil nil)))

(deftest test-evaluate-condition-never
  (is-false (cauldron.reagent::evaluate-condition :never nil nil)))

(deftest test-evaluate-condition-function
  (is-true (cauldron.reagent::evaluate-condition
            (lambda (actor resource) (declare (ignore resource)) (eq actor :admin))
            :admin nil)))

(deftest test-evaluate-condition-function-false
  (is-false (cauldron.reagent::evaluate-condition
             (lambda (actor resource) (declare (ignore resource)) (eq actor :admin))
             :user nil)))

(deftest test-evaluate-condition-unknown
  "Unknown condition type evaluates to NIL."
  (is-false (cauldron.reagent::evaluate-condition 42 nil nil)))

;;; --- check-policy ---

(deftest test-check-policy-allow-always
  (let ((policies (list (cauldron.reagent::make-policy-rule
                         :disposition :allow :action :read :condition :always))))
    (multiple-value-bind (allowed rule)
        (cauldron.reagent::check-policy policies :read nil)
      (is-true allowed)
      (is-not-nil rule))))

(deftest test-check-policy-deny-always
  (let ((policies (list (cauldron.reagent::make-policy-rule
                         :disposition :deny :action :read :condition :always))))
    (multiple-value-bind (allowed rule)
        (cauldron.reagent::check-policy policies :read nil)
      (is-false allowed)
      (is-not-nil rule))))

(deftest test-check-policy-first-match-wins
  "First matching rule determines the result."
  (let ((policies (list (cauldron.reagent::make-policy-rule
                         :disposition :deny :action :read :condition :always)
                        (cauldron.reagent::make-policy-rule
                         :disposition :allow :action :read :condition :always))))
    (multiple-value-bind (allowed rule)
        (cauldron.reagent::check-policy policies :read nil)
      (is-false allowed)
      (is-equal :deny (cauldron.reagent::policy-rule-disposition rule)))))

(deftest test-check-policy-no-match-denies
  "No matching rule defaults to deny."
  (let ((policies (list (cauldron.reagent::make-policy-rule
                         :disposition :allow :action :write :condition :always))))
    (multiple-value-bind (allowed rule)
        (cauldron.reagent::check-policy policies :read nil)
      (is-false allowed)
      (is-nil rule))))

(deftest test-check-policy-condition-evaluated
  "Rule only matches if condition passes."
  (let ((policies (list (cauldron.reagent::make-policy-rule
                         :disposition :allow :action :read
                         :condition (lambda (actor resource)
                                     (declare (ignore resource))
                                     (eq actor :admin))))))
    ;; Admin passes
    (is-true (cauldron.reagent::check-policy policies :read :admin))
    ;; Non-admin fails (no matching rule → deny)
    (is-false (cauldron.reagent::check-policy policies :read :user))))

(deftest test-check-policy-skips-non-matching-actions
  "Rules for different actions are skipped."
  (let ((policies (list (cauldron.reagent::make-policy-rule
                         :disposition :deny :action :write :condition :always)
                        (cauldron.reagent::make-policy-rule
                         :disposition :allow :action :read :condition :always))))
    (is-true (cauldron.reagent::check-policy policies :read nil))))

(deftest test-check-policy-empty-list
  "Empty policy list denies by default."
  (multiple-value-bind (allowed rule)
      (cauldron.reagent::check-policy '() :read nil)
    (is-false allowed)
    (is-nil rule)))

;;; --- authenticated-p ---

(deftest test-authenticated-p-with-actor
  (is-true (cauldron.reagent::authenticated-p '(:id 1 :name "Alice"))))

(deftest test-authenticated-p-nil-actor
  (is-false (cauldron.reagent::authenticated-p nil)))

(deftest test-authenticated-p-ignores-resource
  (is-true (cauldron.reagent::authenticated-p :user :some-resource)))

;;; --- role-is ---

(deftest test-role-is-matches
  (let ((pred (cauldron.reagent::role-is :admin)))
    (is-true (funcall pred '(:id 1 :role :admin)))))

(deftest test-role-is-no-match
  (let ((pred (cauldron.reagent::role-is :admin)))
    (is-false (funcall pred '(:id 1 :role :user)))))

(deftest test-role-is-nil-actor
  (let ((pred (cauldron.reagent::role-is :admin)))
    (is-false (funcall pred nil))))

;;; --- owner-p ---

(deftest test-owner-p-matches
  (is-true (cauldron.reagent::owner-p
            '(:id 42)
            '(:id 1 :user-id 42))))

(deftest test-owner-p-no-match
  (is-false (cauldron.reagent::owner-p
             '(:id 42)
             '(:id 1 :user-id 99))))

(deftest test-owner-p-nil-actor
  (is-false (cauldron.reagent::owner-p nil '(:id 1 :user-id 42))))

(deftest test-owner-p-nil-resource
  (is-false (cauldron.reagent::owner-p '(:id 42) nil)))

;;; --- allow/deny convenience ---

(deftest test-allow-creates-allow-rule
  (let ((rule (cauldron.reagent:allow :read :always)))
    (is-equal :allow (cauldron.reagent::policy-rule-disposition rule))
    (is-equal :read (cauldron.reagent::policy-rule-action rule))
    (is-equal :always (cauldron.reagent::policy-rule-condition rule))))

(deftest test-deny-creates-deny-rule
  (let ((rule (cauldron.reagent:deny :write :never)))
    (is-equal :deny (cauldron.reagent::policy-rule-disposition rule))
    (is-equal :write (cauldron.reagent::policy-rule-action rule))))
