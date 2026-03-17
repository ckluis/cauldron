;;;; src/alembic/packages.lisp — Package definitions for the HTML template engine

(defpackage :cauldron.alembic
  (:use :cl)
  (:export
   ;; Core HTML generation
   #:html
   #:html-to-string
   #:raw
   #:escape-html
   ;; Conditional/iteration helpers
   #:when-html
   #:if-html
   #:for-each
   ;; Form helpers
   #:form-for
   #:text-input
   #:textarea
   #:select-input
   #:submit-button
   #:hidden-input
   #:checkbox-input
   #:number-input
   ;; Scry attributes
   #:scry-click
   #:scry-change
   #:scry-submit
   ;; CSRF token (set by crucible plug, read by form helpers)
   #:*csrf-token*
   #:csrf-hidden-field
   ;; SEO helpers
   #:meta-tags
   #:og-tags
   #:json-ld
   #:json-ld-organization
   #:json-ld-article
   #:canonical-link
   #:sitemap-xml
   #:robots-txt
   #:rss-feed
   #:escape-xml))
