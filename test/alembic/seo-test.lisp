;;;; test/alembic/seo-test.lisp — Tests for SEO helpers
(in-package :cauldron.test)

(defsuite :alembic-seo)

;;; ============================================================
;;; meta-tags
;;; ============================================================

(deftest test-meta-tags-title
  (let ((tags (cauldron.alembic:meta-tags :title "My Page")))
    (is-not-nil tags)
    (is (find '(:title "My Page") tags :test #'equal))))

(deftest test-meta-tags-description
  (let ((tags (cauldron.alembic:meta-tags :description "A great page")))
    (is-equal 1 (length tags))
    (let ((tag (first tags)))
      (is-equal :meta (first tag)))))

(deftest test-meta-tags-canonical
  (let ((tags (cauldron.alembic:meta-tags :canonical "https://example.com/page")))
    (is-equal 1 (length tags))
    (let ((tag (first tags)))
      (is-equal :link (first tag)))))

(deftest test-meta-tags-multiple
  (let ((tags (cauldron.alembic:meta-tags
               :title "Test" :description "Desc" :robots "noindex")))
    (is-equal 3 (length tags))))

(deftest test-meta-tags-empty
  (let ((tags (cauldron.alembic:meta-tags)))
    (is-nil tags)))

;;; ============================================================
;;; og-tags
;;; ============================================================

(deftest test-og-tags-basic
  (let ((tags (cauldron.alembic:og-tags :title "OG Title" :url "https://example.com")))
    (is-not-nil tags)
    (is (>= (length tags) 2))))

(deftest test-og-tags-type-defaults-to-website
  (let ((tags (cauldron.alembic:og-tags :title "Test")))
    ;; Should include og:type = website
    (is (find-if (lambda (tag)
                   (and (listp tag)
                        (member "og:type" tag :test #'string=)))
                 tags))))

;;; ============================================================
;;; canonical-link
;;; ============================================================

(deftest test-canonical-link
  (let ((link (cauldron.alembic:canonical-link "https://example.com/page")))
    (is-equal :link (first link))
    (is (member "canonical" link :test #'string=))))

;;; ============================================================
;;; sitemap-xml
;;; ============================================================

(deftest test-sitemap-xml-string-urls
  (let ((xml (cauldron.alembic:sitemap-xml '("https://example.com/" "https://example.com/about"))))
    (is (stringp xml))
    (is (search "urlset" xml))
    (is (search "https://example.com/" xml))
    (is (search "https://example.com/about" xml))))

(deftest test-sitemap-xml-alist-urls
  (let ((xml (cauldron.alembic:sitemap-xml
              (list (list (cons :loc "https://example.com/")
                          (cons :lastmod "2026-01-01")
                          (cons :changefreq "daily")
                          (cons :priority "1.0"))))))
    (is (search "lastmod" xml))
    (is (search "2026-01-01" xml))
    (is (search "daily" xml))))

(deftest test-sitemap-xml-empty
  (let ((xml (cauldron.alembic:sitemap-xml '())))
    (is (search "urlset" xml))
    (is-false (search "<url>" xml))))

(deftest test-sitemap-xml-escapes-special-chars
  (let ((xml (cauldron.alembic:sitemap-xml '("https://example.com/?a=1&b=2"))))
    (is (search "&amp;" xml))
    (is-false (search "?a=1&b=" xml))))

;;; ============================================================
;;; robots-txt
;;; ============================================================

(deftest test-robots-txt-basic
  (let ((txt (cauldron.alembic:robots-txt :disallow '("/admin" "/private"))))
    (is (search "User-agent: *" txt))
    (is (search "Disallow: /admin" txt))
    (is (search "Disallow: /private" txt))))

(deftest test-robots-txt-with-sitemap
  (let ((txt (cauldron.alembic:robots-txt
              :allow '("/") :sitemap "https://example.com/sitemap.xml")))
    (is (search "Allow: /" txt))
    (is (search "Sitemap: https://example.com/sitemap.xml" txt))))

;;; ============================================================
;;; rss-feed
;;; ============================================================

(deftest test-rss-feed-basic
  (let ((xml (cauldron.alembic:rss-feed
              :title "My Blog"
              :link "https://example.com"
              :description "A blog"
              :items (list (list (cons :title "Post 1")
                                (cons :link "https://example.com/post-1")
                                (cons :description "First post"))))))
    (is (search "<rss" xml))
    (is (search "My Blog" xml))
    (is (search "Post 1" xml))
    (is (search "https://example.com/post-1" xml))))

(deftest test-rss-feed-empty-items
  (let ((xml (cauldron.alembic:rss-feed :title "Empty" :items '())))
    (is (search "<rss" xml))
    (is-false (search "<item>" xml))))

;;; ============================================================
;;; escape-xml
;;; ============================================================

(deftest test-escape-xml-ampersand
  (is-equal "foo &amp; bar" (cauldron.alembic:escape-xml "foo & bar")))

(deftest test-escape-xml-angle-brackets
  (is-equal "&lt;script&gt;" (cauldron.alembic:escape-xml "<script>")))

(deftest test-escape-xml-clean-string
  (is-equal "hello world" (cauldron.alembic:escape-xml "hello world")))

;;; ============================================================
;;; json-ld
;;; ============================================================

(deftest test-json-ld-generates-script-tag
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "@type" ht) "Organization")
    (let ((tag (cauldron.alembic:json-ld ht)))
      (is-equal :script (first tag))
      (is (search "application/ld+json" (format nil "~S" tag))))))

(deftest test-json-ld-organization
  (let ((tag (cauldron.alembic:json-ld-organization :name "Acme" :url "https://acme.com")))
    (is-equal :script (first tag))))

(deftest test-json-ld-article
  (let ((tag (cauldron.alembic:json-ld-article :headline "My Article" :date-published "2026-01-01")))
    (is-equal :script (first tag))))
