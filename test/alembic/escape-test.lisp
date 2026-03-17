;;;; test/alembic/escape-test.lisp — HTML escaping tests
(in-package :cauldron.test)

(defsuite :alembic-escape)

(deftest test-escape-ampersand
  (is-equal "foo &amp; bar" (cauldron.alembic:escape-html "foo & bar")))

(deftest test-escape-less-than
  (is-equal "&lt;script&gt;" (cauldron.alembic:escape-html "<script>")))

(deftest test-escape-greater-than
  (is-equal "a &gt; b" (cauldron.alembic:escape-html "a > b")))

(deftest test-escape-double-quote
  (is-equal "&quot;hello&quot;" (cauldron.alembic:escape-html "\"hello\"")))

(deftest test-escape-single-quote
  (is-equal "it&#x27;s" (cauldron.alembic:escape-html "it's")))

(deftest test-escape-all-five
  (let ((result (cauldron.alembic:escape-html "<div class=\"x\" data-y='z'>&")))
    (is (search "&lt;" result))
    (is (search "&gt;" result))
    (is (search "&quot;" result))
    (is (search "&#x27;" result))
    (is (search "&amp;" result))))

(deftest test-escape-safe-string
  ;; Strings without special chars pass through unchanged
  (is-equal "hello world" (cauldron.alembic:escape-html "hello world")))

(deftest test-escape-empty-string
  (is-equal "" (cauldron.alembic:escape-html "")))

(deftest test-escape-non-string
  ;; Non-string input should be coerced
  (let ((result (cauldron.alembic:escape-html 42)))
    (is (stringp result))
    (is-equal "42" result)))
