;;;; src/alembic/html.lisp — HTML generation from s-expression DSL
;;;; (html (:div :class "container" (:h1 "Hello") (:p "World")))
;;;; → "<div class=\"container\"><h1>Hello</h1><p>World</p></div>"

(in-package :cauldron.alembic)

;;; --- Void elements (self-closing) ---

(defparameter *void-elements*
  '(:area :base :br :col :embed :hr :img :input :link :meta
    :param :source :track :wbr)
  "HTML5 void elements that must not have closing tags.")

(defun void-element-p (tag)
  (member tag *void-elements*))

;;; --- Raw HTML wrapper ---

(defstruct raw-html
  "Wrapper for pre-escaped HTML content."
  (content "" :type string))

(defun raw (string)
  "Mark STRING as raw HTML — will not be escaped."
  (make-raw-html :content (if (stringp string) string (princ-to-string string))))

;;; --- HTML rendering ---

(defun render-attribute (name value stream)
  "Write a single HTML attribute to STREAM."
  (cond
    ((eq value t)
     ;; Boolean attribute: just the name
     (write-char #\Space stream)
     (write-string (string-downcase (symbol-name name)) stream))
    ((null value)
     ;; Skip nil attributes
     nil)
    (t
     (write-char #\Space stream)
     (write-string (string-downcase (symbol-name name)) stream)
     (write-string "=\"" stream)
     (write-string (escape-attribute-value value) stream)
     (write-char #\" stream))))

(defun parse-tag-form (form)
  "Parse a tag form (:tag attrs... children...).
Returns (values tag-name attributes children)."
  (let ((tag (first form))
        (rest (rest form))
        (attrs '())
        (children '()))
    ;; Collect keyword-value pairs as attributes
    (loop while (and rest (keywordp (first rest)))
          do (push (first rest) attrs)
             (push (second rest) attrs)
             (setf rest (cddr rest)))
    (setf attrs (nreverse attrs))
    (setf children rest)
    (values tag attrs children)))

(defun render-node (node stream)
  "Render a single HTML node to STREAM."
  (cond
    ;; NIL — skip
    ((null node) nil)

    ;; Raw HTML — write directly
    ((raw-html-p node)
     (write-string (raw-html-content node) stream))

    ;; String — escape and write
    ((stringp node)
     (write-string (escape-html node) stream))

    ;; Number — write as string
    ((numberp node)
     (princ node stream))

    ;; Keyword alone — treated as void element with no attrs
    ((keywordp node)
     (write-char #\< stream)
     (write-string (string-downcase (symbol-name node)) stream)
     (if (void-element-p node)
         (write-string ">" stream)
         (progn
           (write-string "></" stream)
           (write-string (string-downcase (symbol-name node)) stream)
           (write-char #\> stream))))

    ;; List starting with keyword — tag form
    ((and (consp node) (keywordp (first node)))
     (multiple-value-bind (tag attrs children)
         (parse-tag-form node)
       (let ((tag-name (string-downcase (symbol-name tag))))
         ;; Opening tag
         (write-char #\< stream)
         (write-string tag-name stream)
         ;; Attributes
         (loop for (name value) on attrs by #'cddr
               do (render-attribute name value stream))
         (write-char #\> stream)
         ;; Children (skip for void elements)
         (unless (void-element-p tag)
           (dolist (child children)
             (render-node child stream))
           ;; Closing tag
           (write-string "</" stream)
           (write-string tag-name stream)
           (write-char #\> stream)))))

    ;; List of nodes (spliced)
    ((and (consp node) (not (keywordp (first node))))
     (dolist (child node)
       (render-node child stream)))

    ;; Anything else — coerce to string and escape
    (t
     (write-string (escape-html (princ-to-string node)) stream))))

;;; --- Public API ---

(defun %transform-html-form (form)
  "Transform an HTML DSL form into code that produces the right data at runtime.
Tag forms (lists starting with keywords) are transformed recursively.
Other forms are left as-is for normal evaluation."
  (cond
    ;; NIL, strings, numbers, keywords — quote them as literals
    ((null form) nil)
    ((stringp form) form)
    ((numberp form) form)
    ((keywordp form) `',form)
    ;; List starting with keyword — this is a tag form like (:div :class "x" "body")
    ;; Transform into a quoted list, recursively transforming children
    ((and (consp form) (keywordp (first form)))
     (multiple-value-bind (tag attrs children)
         (parse-tag-form form)
       (let ((transformed-children (mapcar #'%transform-html-form children)))
         `(list ,`',tag ,@(loop for (k v) on attrs by #'cddr
                                collect `',k
                                collect (%transform-html-form v))
                ,@transformed-children))))
    ;; Any other form — evaluate it normally (raw, when-html, for-each, etc.)
    (t form)))

(defmacro html (&body body)
  "Generate HTML string from s-expression DSL.

Usage:
  (html (:div :class \"container\"
    (:h1 \"Hello\")
    (:p \"World\")))
  → \"<div class=\\\"container\\\"><h1>Hello</h1><p>World</p></div>\"

Tags are keywords. Attributes are keyword-value pairs.
Strings are escaped. Use (raw ...) for pre-escaped content.
NIL values are skipped."
  `(html-to-string (list ,@(mapcar #'%transform-html-form body))))

(defun html-to-string (nodes)
  "Render a list of HTML nodes to a string."
  (with-output-to-string (stream)
    (if (listp nodes)
        (dolist (node nodes)
          (render-node node stream))
        (render-node nodes stream))))

;;; --- Conditional/iteration helpers ---

(defmacro when-html (test &body body)
  "Return HTML body only when TEST is truthy, otherwise NIL (skipped in rendering)."
  `(when ,test
     (list ,@body)))

(defmacro if-html (test then &optional else)
  "Return THEN html when TEST is truthy, otherwise ELSE."
  `(if ,test ,then ,else))

(defmacro for-each ((var list-form) &body body)
  "Map over LIST-FORM, binding VAR, collecting HTML nodes."
  `(mapcar (lambda (,var) ,@body) ,list-form))
