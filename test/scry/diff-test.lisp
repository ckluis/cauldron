;;;; test/scry/diff-test.lisp — Scry diff engine tests
(in-package :cauldron.test)

(defsuite :scry-diff)

;;; --- Type predicates ---

(deftest test-tag-node-p
  (is-true (cauldron.scry::tag-node-p '(:div "hello")))
  (is-true (cauldron.scry::tag-node-p '(:span :class "x" "text")))
  (is-false (cauldron.scry::tag-node-p "hello"))
  (is-false (cauldron.scry::tag-node-p 42))
  (is-false (cauldron.scry::tag-node-p nil)))

(deftest test-text-node-p
  (is-true (cauldron.scry::text-node-p "hello"))
  (is-true (cauldron.scry::text-node-p 42))
  (is-true (cauldron.scry::text-node-p 3.14))
  (is-false (cauldron.scry::text-node-p '(:div)))
  (is-false (cauldron.scry::text-node-p nil)))

(deftest test-type-of-node
  (is-equal :tag (cauldron.scry::type-of-node '(:div "x")))
  (is-equal :text (cauldron.scry::type-of-node "hello"))
  (is-equal :text (cauldron.scry::type-of-node 42))
  (is-equal :nil (cauldron.scry::type-of-node nil)))

;;; --- Parse node ---

(deftest test-parse-node-simple
  (multiple-value-bind (tag attrs children)
      (cauldron.scry::parse-node '(:div "hello"))
    (is-equal :div tag)
    (is-nil attrs)
    (is-equal '("hello") children)))

(deftest test-parse-node-with-attrs
  (multiple-value-bind (tag attrs children)
      (cauldron.scry::parse-node '(:span :class "foo" :id "bar" "text"))
    (is-equal :span tag)
    (is-equal '(:class "foo" :id "bar") attrs)
    (is-equal '("text") children)))

(deftest test-parse-node-no-children
  (multiple-value-bind (tag attrs children)
      (cauldron.scry::parse-node '(:br))
    (is-equal :br tag)
    (is-nil attrs)
    (is-nil children)))

;;; --- Node text ---

(deftest test-node-text
  (is-equal "hello" (cauldron.scry::node-text "hello"))
  (is-equal "42" (cauldron.scry::node-text 42))
  (is-nil (cauldron.scry::node-text nil)))

;;; --- Attribute comparison ---

(deftest test-attrs-equal-p
  (is-true (cauldron.scry::attrs-equal-p '(:class "x") '(:class "x")))
  (is-false (cauldron.scry::attrs-equal-p '(:class "x") '(:class "y")))
  (is-true (cauldron.scry::attrs-equal-p '() '()))
  (is-false (cauldron.scry::attrs-equal-p '(:class "x") '())))

(deftest test-diff-attrs-changed
  (let ((result (cauldron.scry::diff-attrs '(:class "old") '(:class "new"))))
    (is-equal "new" (getf result :class))))

(deftest test-diff-attrs-added
  (let ((result (cauldron.scry::diff-attrs '() '(:class "new"))))
    (is-equal "new" (getf result :class))))

(deftest test-diff-attrs-removed
  (let ((result (cauldron.scry::diff-attrs '(:class "old") '())))
    (is-nil (getf result :class))
    (is (member :class result))))

(deftest test-diff-attrs-identical
  (is-nil (cauldron.scry::diff-attrs '(:class "x") '(:class "x"))))

;;; --- Merge attrs ---

(deftest test-merge-attrs
  (let ((result (cauldron.scry::merge-attrs '(:class "old" :id "keep") '(:class "new"))))
    (is-equal "new" (getf result :class))
    (is-equal "keep" (getf result :id))))

(deftest test-merge-attrs-remove
  (let ((result (cauldron.scry::merge-attrs '(:class "x" :id "y") '(:class nil))))
    (is-nil (getf result :class))
    (is-equal "y" (getf result :id))))

;;; --- Remove plist key ---

(deftest test-remove-plist-key
  (is-equal '(:id "y") (cauldron.scry::remove-plist-key '(:class "x" :id "y") :class))
  (is-equal '(:class "x" :id "y")
            (cauldron.scry::remove-plist-key '(:class "x" :id "y") :foo)))

;;; --- diff-trees ---

(deftest test-diff-trees-identical
  (let ((tree '(:div :class "x" "hello")))
    (is-nil (cauldron.scry:diff-trees tree tree))))

(deftest test-diff-trees-text-change
  (let ((patches (cauldron.scry:diff-trees "old" "new")))
    (is-equal 1 (length patches))
    (is-equal :update-text (first (first patches)))
    (is-equal "new" (third (first patches)))))

(deftest test-diff-trees-attr-change
  (let ((patches (cauldron.scry:diff-trees
                  '(:div :class "old" "text")
                  '(:div :class "new" "text"))))
    (is-equal 1 (length patches))
    (is-equal :update-attrs (first (first patches)))))

(deftest test-diff-trees-tag-replacement
  (let ((patches (cauldron.scry:diff-trees
                  '(:div "hello")
                  '(:span "hello"))))
    (is-equal 1 (length patches))
    (is-equal :replace (first (first patches)))))

(deftest test-diff-trees-child-insert
  (let ((patches (cauldron.scry:diff-trees
                  '(:ul (:li "a"))
                  '(:ul (:li "a") (:li "b")))))
    (is (find :insert patches :key #'first))))

(deftest test-diff-trees-child-remove
  (let ((patches (cauldron.scry:diff-trees
                  '(:ul (:li "a") (:li "b"))
                  '(:ul (:li "a")))))
    (is (find :remove patches :key #'first))))

(deftest test-diff-trees-nested-changes
  (let ((patches (cauldron.scry:diff-trees
                  '(:div (:p "old"))
                  '(:div (:p "new")))))
    (is-equal 1 (length patches))
    ;; Path should indicate nested position
    (is (> (length (second (first patches))) 0))))

(deftest test-diff-trees-table-efficiency
  "Changing 1 row in a 100-row table should produce a small patch set."
  (let* ((rows (loop for i from 0 below 100
                     collect `(:tr (:td ,(format nil "row-~D" i)))))
         (new-rows (copy-tree rows)))
    ;; Change row 50
    (setf (nth 50 new-rows) '(:tr (:td "CHANGED")))
    (let* ((old-tree (cons :tbody rows))
           (new-tree (cons :tbody new-rows))
           (patches (cauldron.scry:diff-trees old-tree new-tree)))
      ;; Should be a small number of patches, not 100
      (is (< (length patches) 5)))))

;;; --- apply-diff round-trip ---

(deftest test-apply-diff-roundtrip-text
  (let* ((old "hello")
         (new "world")
         (patches (cauldron.scry:diff-trees old new))
         (result (cauldron.scry:apply-diff old patches)))
    (is-equal new result)))

(deftest test-apply-diff-roundtrip-attrs
  (let* ((old '(:div :class "old" "text"))
         (new '(:div :class "new" "text"))
         (patches (cauldron.scry:diff-trees old new))
         (result (cauldron.scry:apply-diff old patches)))
    (is-equal new result)))

(deftest test-apply-diff-roundtrip-children
  (let* ((old '(:ul (:li "a") (:li "b")))
         (new '(:ul (:li "a") (:li "b") (:li "c")))
         (patches (cauldron.scry:diff-trees old new))
         (result (cauldron.scry:apply-diff old patches)))
    (is-equal new result)))

(deftest test-apply-diff-roundtrip-complex
  (let* ((old '(:div :class "container"
                (:h1 "Title")
                (:p "Old paragraph")
                (:ul (:li "item1") (:li "item2"))))
         (new '(:div :class "container"
                (:h1 "New Title")
                (:p "New paragraph")
                (:ul (:li "item1") (:li "item2") (:li "item3"))))
         (patches (cauldron.scry:diff-trees old new))
         (result (cauldron.scry:apply-diff old patches)))
    (is-equal new result)))

(deftest test-apply-diff-roundtrip-remove
  (let* ((old '(:ul (:li "a") (:li "b") (:li "c")))
         (new '(:ul (:li "a")))
         (patches (cauldron.scry:diff-trees old new))
         (result (cauldron.scry:apply-diff old patches)))
    (is-equal new result)))

(deftest test-apply-diff-roundtrip-replace
  (let* ((old '(:div (:span "old")))
         (new '(:div (:p "new")))
         (patches (cauldron.scry:diff-trees old new))
         (result (cauldron.scry:apply-diff old patches)))
    (is-equal new result)))
