;;;; test/reagent/actions-test.lisp — Reagent action system tests
(in-package :cauldron.test)

(defsuite :reagent-actions)

;;; --- action-key ---

(deftest test-action-key-creates-cons
  (let ((key (cauldron.reagent::action-key 'user 'create)))
    (is (consp key))
    (is-equal 'user (car key))
    (is-equal 'create (cdr key))))

(deftest test-action-key-different-pairs
  (let ((k1 (cauldron.reagent::action-key 'user 'create))
        (k2 (cauldron.reagent::action-key 'post 'create))
        (k3 (cauldron.reagent::action-key 'user 'delete)))
    (is (not (equal k1 k2)))
    (is (not (equal k1 k3)))))

(deftest test-action-key-same-inputs-equal
  (let ((k1 (cauldron.reagent::action-key 'user 'create))
        (k2 (cauldron.reagent::action-key 'user 'create)))
    (is (equal k1 k2))))

;;; --- filter-params ---

(deftest test-filter-params-keeps-accepted
  (let ((result (cauldron.reagent::filter-params
                 '(:name "Alice" :email "a@b.com" :role :admin)
                 '(name email))))
    (is-equal '(:name "Alice" :email "a@b.com") result)))

(deftest test-filter-params-removes-unaccepted
  (let ((result (cauldron.reagent::filter-params
                 '(:name "Alice" :password "secret" :email "a@b.com")
                 '(name email))))
    (is-false (member :password result))))

(deftest test-filter-params-empty-accept-list
  "Empty accept list returns all params (passthrough)."
  (let ((result (cauldron.reagent::filter-params
                 '(:name "Alice" :email "a@b.com")
                 nil)))
    (is-equal '(:name "Alice" :email "a@b.com") result)))

(deftest test-filter-params-empty-params
  (let ((result (cauldron.reagent::filter-params '() '(name email))))
    (is-equal '() result)))

(deftest test-filter-params-no-matching-keys
  (let ((result (cauldron.reagent::filter-params
                 '(:foo 1 :bar 2)
                 '(name email))))
    (is-equal '() result)))

(deftest test-filter-params-preserves-order
  (let ((result (cauldron.reagent::filter-params
                 '(:email "a@b.com" :name "Alice")
                 '(name email))))
    ;; Order follows params order, not accept-list order
    (is-equal :email (first result))
    (is-equal :name (third result))))

(deftest test-filter-params-keyword-matching
  "Accept list symbols are matched as keywords."
  (let ((result (cauldron.reagent::filter-params
                 '(:first-name "Alice" :age 30)
                 '(first-name))))
    (is-equal '(:first-name "Alice") result)))
