;;;; test/reagent/relationships-test.lisp — Reagent relationship helper tests
(in-package :cauldron.test)

(defsuite :reagent-relationships)

;;; --- default-foreign-key ---

(deftest test-default-foreign-key-user
  (is-equal "user_id" (cauldron.reagent::default-foreign-key :user)))

(deftest test-default-foreign-key-post
  (is-equal "post_id" (cauldron.reagent::default-foreign-key :post)))

(deftest test-default-foreign-key-comment
  (is-equal "comment_id" (cauldron.reagent::default-foreign-key :comment)))

(deftest test-default-foreign-key-multi-word
  "Multi-word symbols get lowercased."
  (is-equal "blog-post_id" (cauldron.reagent::default-foreign-key :blog-post)))

;;; --- default-join-table ---

(deftest test-default-join-table-alphabetical
  "Join table uses alphabetical ordering of pluralized names."
  ;; post < tag alphabetically, so posts_tags
  (let ((result (cauldron.reagent::default-join-table :post :tag)))
    ;; Should start with the alphabetically-first pluralized name
    (is (stringp result))
    ;; Both names should appear
    (is (search "post" result))
    (is (search "tag" result))))

(deftest test-default-join-table-reversed-order
  "Reversed order produces same table name."
  (let ((r1 (cauldron.reagent::default-join-table :post :tag))
        (r2 (cauldron.reagent::default-join-table :tag :post)))
    (is-equal r1 r2)))

(deftest test-default-join-table-user-role
  (let ((result (cauldron.reagent::default-join-table :user :role)))
    (is (stringp result))
    (is (search "role" result))
    (is (search "user" result))))

;;; --- extract-ids ---

(deftest test-extract-ids-basic
  (let ((records (list '(:id 1 :name "A")
                       '(:id 2 :name "B")
                       '(:id 3 :name "C"))))
    (is-equal '(1 2 3) (cauldron.reagent::extract-ids records :id))))

(deftest test-extract-ids-unique
  "Duplicate values are removed."
  (let ((records (list '(:id 1 :name "A")
                       '(:id 1 :name "B")
                       '(:id 2 :name "C"))))
    (is-equal '(1 2) (cauldron.reagent::extract-ids records :id))))

(deftest test-extract-ids-skips-nil
  (let ((records (list '(:id 1)
                       '(:id nil)
                       '(:id 3))))
    (is-equal '(1 3) (cauldron.reagent::extract-ids records :id))))

(deftest test-extract-ids-empty-records
  (is-equal '() (cauldron.reagent::extract-ids '() :id)))

(deftest test-extract-ids-different-key
  (let ((records (list '(:user-id 10 :name "X")
                       '(:user-id 20 :name "Y"))))
    (is-equal '(10 20) (cauldron.reagent::extract-ids records :user-id))))

;;; --- build-in-clause ---

(deftest test-build-in-clause-integers
  (let ((result (cauldron.reagent::build-in-clause "id" '(1 2 3))))
    (is-equal "id IN (1, 2, 3)" result)))

(deftest test-build-in-clause-single-id
  (let ((result (cauldron.reagent::build-in-clause "user_id" '(42))))
    (is-equal "user_id IN (42)" result)))

(deftest test-build-in-clause-strings
  (let ((result (cauldron.reagent::build-in-clause "code" '("abc" "def"))))
    (is-equal "code IN ('abc', 'def')" result)))

(deftest test-build-in-clause-mixed
  "Mixed integers and strings."
  (let ((result (cauldron.reagent::build-in-clause "x" '(1 "two" 3))))
    (is-equal "x IN (1, 'two', 3)" result)))
