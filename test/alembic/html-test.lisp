;;;; test/alembic/html-test.lisp — HTML generation tests
(in-package :cauldron.test)

(defsuite :alembic-html)

;;; --- Simple tags ---

(deftest test-simple-div
  (is-equal "<div>Hello</div>"
            (cauldron.alembic:html (:div "Hello"))))

(deftest test-h1-tag
  (is-equal "<h1>Title</h1>"
            (cauldron.alembic:html (:h1 "Title"))))

;;; --- Attributes ---

(deftest test-div-with-class
  (is-equal "<div class=\"container\">content</div>"
            (cauldron.alembic:html (:div :class "container" "content"))))

(deftest test-multiple-attributes
  (let ((result (cauldron.alembic:html (:a :href "/home" :class "nav" "Home"))))
    (is (search "href=\"/home\"" result))
    (is (search "class=\"nav\"" result))
    (is (search ">Home</a>" result))))

;;; --- Void elements ---

(deftest test-br-void
  (is-equal "<br>" (cauldron.alembic:html (:br))))

(deftest test-img-void
  (let ((result (cauldron.alembic:html (:img :src "pic.jpg"))))
    (is (search "<img" result))
    (is (search "src=\"pic.jpg\"" result))
    ;; Should NOT have </img>
    (is-nil (search "</img>" result))))

(deftest test-input-void
  (let ((result (cauldron.alembic:html (:input :type "text" :name "q"))))
    (is (search "<input" result))
    (is-nil (search "</input>" result))))

;;; --- Nesting ---

(deftest test-nested-tags
  (is-equal "<div><h1>Hello</h1><p>World</p></div>"
            (cauldron.alembic:html (:div (:h1 "Hello") (:p "World")))))

(deftest test-deep-nesting
  (is-equal "<div><ul><li>item</li></ul></div>"
            (cauldron.alembic:html (:div (:ul (:li "item"))))))

;;; --- Escaping ---

(deftest test-string-content-escaped
  (let ((result (cauldron.alembic:html (:p "<script>alert('xss')</script>"))))
    (is (search "&lt;script&gt;" result))
    (is-nil (search "<script>" result))))

(deftest test-raw-content-not-escaped
  (let ((result (cauldron.alembic:html (:p (cauldron.alembic:raw "<b>bold</b>")))))
    (is (search "<b>bold</b>" result))))

;;; --- Boolean attributes ---

(deftest test-boolean-attribute-true
  (let ((result (cauldron.alembic:html (:input :disabled t))))
    (is (search "disabled" result))
    ;; Should be just "disabled", not "disabled=\"true\""
    (is-nil (search "disabled=\"" result))))

(deftest test-boolean-attribute-nil
  (let ((result (cauldron.alembic:html (:input :disabled nil))))
    ;; nil attribute should be skipped
    (is-nil (search "disabled" result))))

;;; --- Nil skipping ---

(deftest test-nil-children-skipped
  (is-equal "<div></div>"
            (cauldron.alembic:html (:div nil))))

(deftest test-nil-among-children
  (is-equal "<div>hello</div>"
            (cauldron.alembic:html (:div nil "hello" nil))))
