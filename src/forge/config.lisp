;;;; src/forge/config.lisp — Derive Forge configuration from resource metadata

(in-package :cauldron.forge)

;;; --- Forge config structs ---

(defstruct forge-config
  "Top-level Forge configuration."
  (resources '() :type list)         ; List of forge-resource-config
  (mount-path "/forge" :type string)
  (title "Forge Admin" :type string))

(defstruct forge-resource-config
  "Configuration for a single resource in Forge."
  (name nil :type symbol)
  (display-name "" :type string)
  (table-name "" :type string)
  (table-columns '() :type list)     ; Max 6 columns for index view
  (searchable-columns '() :type list)
  (filterable-columns '() :type list)
  (sortable-columns '() :type list)
  (form-fields '() :type list)       ; List of forge-field-config
  (actions '() :type list)
  (display-field nil)                 ; Primary display field (e.g., :name, :title)
  (attributes '() :type list))       ; All attributes for show view

(defstruct forge-field-config
  "Configuration for a form field in Forge."
  (name nil :type symbol)
  (type :text :type keyword)         ; :text, :textarea, :select, :checkbox, :number, :hidden
  (label "" :type string)
  (required nil :type boolean)
  (options nil :type list)           ; For select fields
  (max-length nil)
  (min-length nil))

;;; --- Derivation ---

(defun pluralize-name (name)
  "Simple English pluralization for resource names."
  (let ((s (string-downcase (symbol-name name))))
    (cond
      ((string-suffix-p s "s") (concatenate 'string s "es"))
      ((string-suffix-p s "y")
       (concatenate 'string (subseq s 0 (1- (length s))) "ies"))
      (t (concatenate 'string s "s")))))

(defun string-suffix-p (string suffix)
  "Test if STRING ends with SUFFIX."
  (and (>= (length string) (length suffix))
       (string= string suffix :start1 (- (length string) (length suffix)))))

(defun humanize-name (name)
  "Convert a symbol name to a human-readable string."
  (substitute #\Space #\- (string-capitalize (symbol-name name))))

(defun attribute-to-form-field (attr)
  "Convert a resource attribute definition to a forge-field-config."
  (let* ((name (if (consp attr) (first attr) attr))
         (props (if (consp attr) (rest attr) nil))
         (type (getf props :type :string))
         (required (getf props :required))
         (one-of (getf props :one-of))
         (max-length (getf props :max-length))
         (min-length (getf props :min-length)))
    (make-forge-field-config
     :name name
     :type (cond
             (one-of :select)
             ((eq type 'text) :textarea)
             ((eq type 'boolean) :checkbox)
             ((member type '(integer float)) :number)
             ((eq type 'keyword) :select)
             (t :text))
     :label (humanize-name name)
     :required required
     :options (when one-of (mapcar (lambda (v) (cons v (humanize-name v))) one-of))
     :max-length max-length
     :min-length min-length)))

(defun text-column-p (attr)
  "Return T if attribute is a text/blob type (excluded from index table)."
  (let ((type (if (consp attr) (getf (rest attr) :type) nil)))
    (member type '(text jsonb bytea))))

(defun string-type-p (attr)
  "Return T if attribute is a string-like type (searchable)."
  (let ((type (if (consp attr) (getf (rest attr) :type) :string)))
    (member type '(string text))))

(defun enum-type-p (attr)
  "Return T if attribute has :one-of (filterable)."
  (and (consp attr) (getf (rest attr) :one-of)))

(defun derive-forge-config (&key resources (mount-path "/forge") (title "Forge Admin"))
  "Derive a complete Forge configuration from resource definitions.
RESOURCES is a list of resource class objects or specs."
  (make-forge-config
   :resources (mapcar #'derive-resource-config resources)
   :mount-path mount-path
   :title title))

(defun derive-resource-config (resource-spec)
  "Derive config for a single resource.
RESOURCE-SPEC is a plist: (:name NAME :attributes ATTRS :actions ACTIONS)"
  (let* ((name (getf resource-spec :name))
         (attrs (getf resource-spec :attributes '()))
         (actions (getf resource-spec :actions '()))
         ;; Table columns: non-text attrs, max 6
         (table-attrs (remove-if #'text-column-p attrs))
         (table-columns (mapcar (lambda (a) (if (consp a) (first a) a))
                                (subseq table-attrs 0 (min 6 (length table-attrs)))))
         ;; Searchable: string-type columns
         (searchable (mapcar (lambda (a) (if (consp a) (first a) a))
                             (remove-if-not #'string-type-p attrs)))
         ;; Filterable: enum columns + boolean
         (filterable (mapcar (lambda (a) (if (consp a) (first a) a))
                             (remove-if-not #'enum-type-p attrs)))
         ;; Display field: first string attribute, or first attribute
         (display-field (or (first searchable) (first table-columns))))
    (make-forge-resource-config
     :name name
     :display-name (humanize-name name)
     :table-name (pluralize-name name)
     :table-columns table-columns
     :searchable-columns searchable
     :filterable-columns filterable
     :sortable-columns table-columns
     :form-fields (mapcar #'attribute-to-form-field attrs)
     :actions (mapcar (lambda (a) (if (consp a) (first a) a)) actions)
     :display-field display-field
     :attributes (mapcar (lambda (a) (if (consp a) (first a) a)) attrs))))
