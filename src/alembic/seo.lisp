;;;; src/alembic/seo.lisp — SEO helpers for HTML generation
;;;; Generates meta tags, Open Graph tags, JSON-LD, sitemaps, robots.txt, RSS feeds.

(in-package :cauldron.alembic)

;;; ============================================================
;;; Meta Tags
;;; ============================================================

(defun meta-tags (&key title description keywords author robots canonical)
  "Generate standard HTML meta tags as s-expression list.
Returns a list of meta tag forms suitable for inclusion in a <head>."
  (let ((tags '()))
    (when title
      (push `(:title ,title) tags))
    (when description
      (push `(:meta :name "description" :content ,description) tags))
    (when keywords
      (push `(:meta :name "keywords" :content ,keywords) tags))
    (when author
      (push `(:meta :name "author" :content ,author) tags))
    (when robots
      (push `(:meta :name "robots" :content ,robots) tags))
    (when canonical
      (push `(:link :rel "canonical" :href ,canonical) tags))
    (nreverse tags)))

;;; ============================================================
;;; Open Graph Tags
;;; ============================================================

(defun og-tags (&key title description url image type site-name locale)
  "Generate Open Graph meta tags for social media sharing.
Returns a list of meta tag forms."
  (let ((tags '()))
    (when title
      (push `(:meta :property "og:title" :content ,title) tags))
    (when description
      (push `(:meta :property "og:description" :content ,description) tags))
    (when url
      (push `(:meta :property "og:url" :content ,url) tags))
    (when image
      (push `(:meta :property "og:image" :content ,image) tags))
    (when (or type t)
      (push `(:meta :property "og:type" :content ,(or type "website")) tags))
    (when site-name
      (push `(:meta :property "og:site_name" :content ,site-name) tags))
    (when locale
      (push `(:meta :property "og:locale" :content ,locale) tags))
    (nreverse tags)))

;;; ============================================================
;;; JSON-LD Structured Data
;;; ============================================================

(defun json-ld (data)
  "Generate a <script type=\"application/ld+json\"> tag with DATA.
DATA should be a hash-table or alist that will be JSON-encoded."
  (let ((json-string (cauldron.json:encode data)))
    `(:script :type "application/ld+json"
              ,(raw json-string))))

(defun json-ld-organization (&key name url logo description)
  "Generate JSON-LD for an Organization schema."
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "@context" ht) "https://schema.org")
    (setf (gethash "@type" ht) "Organization")
    (when name (setf (gethash "name" ht) name))
    (when url (setf (gethash "url" ht) url))
    (when logo (setf (gethash "logo" ht) logo))
    (when description (setf (gethash "description" ht) description))
    (json-ld ht)))

(defun json-ld-article (&key headline description author date-published date-modified url image)
  "Generate JSON-LD for an Article schema."
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "@context" ht) "https://schema.org")
    (setf (gethash "@type" ht) "Article")
    (when headline (setf (gethash "headline" ht) headline))
    (when description (setf (gethash "description" ht) description))
    (when author (setf (gethash "author" ht) author))
    (when date-published (setf (gethash "datePublished" ht) date-published))
    (when date-modified (setf (gethash "dateModified" ht) date-modified))
    (when url (setf (gethash "url" ht) url))
    (when image (setf (gethash "image" ht) image))
    (json-ld ht)))

;;; ============================================================
;;; Canonical Link
;;; ============================================================

(defun canonical-link (url)
  "Generate a <link rel=\"canonical\"> tag."
  `(:link :rel "canonical" :href ,url))

;;; ============================================================
;;; Sitemap XML
;;; ============================================================

(defun sitemap-xml (urls)
  "Generate an XML sitemap string from URLS.
Each URL is either a string or an alist with keys: loc, lastmod, changefreq, priority."
  (with-output-to-string (out)
    (write-string "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" out)
    (write-string "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">" out)
    (dolist (url urls)
      (write-string "<url>" out)
      (etypecase url
        (string
         (format out "<loc>~A</loc>" (escape-xml url)))
        (list
         (let ((loc (cdr (assoc :loc url)))
               (lastmod (cdr (assoc :lastmod url)))
               (changefreq (cdr (assoc :changefreq url)))
               (priority (cdr (assoc :priority url))))
           (when loc (format out "<loc>~A</loc>" (escape-xml loc)))
           (when lastmod (format out "<lastmod>~A</lastmod>" (escape-xml lastmod)))
           (when changefreq (format out "<changefreq>~A</changefreq>" (escape-xml changefreq)))
           (when priority (format out "<priority>~A</priority>" priority)))))
      (write-string "</url>" out))
    (write-string "</urlset>" out)))

;;; ============================================================
;;; Robots.txt
;;; ============================================================

(defun robots-txt (&key (user-agent "*") allow disallow sitemap)
  "Generate a robots.txt string.
ALLOW and DISALLOW are lists of path strings.
SITEMAP is a URL string."
  (with-output-to-string (out)
    (format out "User-agent: ~A~%" user-agent)
    (dolist (path (if (listp allow) allow (list allow)))
      (when path (format out "Allow: ~A~%" path)))
    (dolist (path (if (listp disallow) disallow (list disallow)))
      (when path (format out "Disallow: ~A~%" path)))
    (when sitemap
      (format out "~%Sitemap: ~A~%" sitemap))))

;;; ============================================================
;;; RSS Feed
;;; ============================================================

(defun rss-feed (&key title link description language items)
  "Generate an RSS 2.0 XML feed string.
ITEMS is a list of alists with keys: title, link, description, pub-date, guid."
  (with-output-to-string (out)
    (write-string "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" out)
    (write-string "<rss version=\"2.0\">" out)
    (write-string "<channel>" out)
    (when title (format out "<title>~A</title>" (escape-xml title)))
    (when link (format out "<link>~A</link>" (escape-xml link)))
    (when description (format out "<description>~A</description>" (escape-xml description)))
    (when language (format out "<language>~A</language>" (escape-xml language)))
    (dolist (item items)
      (write-string "<item>" out)
      (let ((item-title (cdr (assoc :title item)))
            (item-link (cdr (assoc :link item)))
            (item-desc (cdr (assoc :description item)))
            (item-date (cdr (assoc :pub-date item)))
            (item-guid (cdr (assoc :guid item))))
        (when item-title (format out "<title>~A</title>" (escape-xml item-title)))
        (when item-link (format out "<link>~A</link>" (escape-xml item-link)))
        (when item-desc (format out "<description>~A</description>" (escape-xml item-desc)))
        (when item-date (format out "<pubDate>~A</pubDate>" (escape-xml item-date)))
        (when item-guid (format out "<guid>~A</guid>" (escape-xml item-guid))))
      (write-string "</item>" out))
    (write-string "</channel></rss>" out)))

;;; ============================================================
;;; XML Escaping Helper
;;; ============================================================

(defun escape-xml (string)
  "Escape special XML characters in STRING."
  (with-output-to-string (out)
    (loop for char across string
          do (case char
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\& (write-string "&amp;" out))
               (#\" (write-string "&quot;" out))
               (#\' (write-string "&apos;" out))
               (otherwise (write-char char out))))))
