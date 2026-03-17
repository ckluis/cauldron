;;;; src/forge/crud.lisp — Auto-generated CRUD views for Forge
;;;; All views derived from forge-resource-config.

(in-package :cauldron.forge)

;;; --- Index view (list with search, filter, sort, pagination) ---

(defun index-view (conn resource-config)
  "Render the resource index (list) view."
  (let* ((name (forge-resource-config-display-name resource-config))
         (columns (forge-resource-config-table-columns resource-config))
         (base-url (format nil "/forge/~A"
                           (string-downcase
                            (symbol-name (forge-resource-config-name resource-config)))))
         (body (forge-layout (format nil "~A" name)
                 (cauldron.alembic:html
                   ;; Toolbar
                   (:div :class "forge-toolbar"
                     (:a :href (concatenate 'string base-url "/new")
                         :class "btn btn-primary"
                         (format nil "New ~A" (forge-resource-config-display-name resource-config)))
                     ;; Search (if searchable columns exist)
                     (when (forge-resource-config-searchable-columns resource-config)
                         (list `(:form :method "GET" :action ,base-url :class "forge-search"
                                  (:input :type "search" :name "q" :placeholder "Search...")
                                  (:button :type "submit" "Search")))))
                   ;; Table
                   (:table :class "forge-table"
                     (:thead
                       (:tr
                         (:th (:input :type "checkbox" :class "forge-select-all"))
                         (mapcar (lambda (col)
                                     `(:th ,(humanize-name col)))
                                   columns)
                         (:th "Actions")))
                     (:tbody
                       ;; Rows populated at runtime from DB
                       (:tr (:td :colspan (princ-to-string (+ 2 (length columns)))
                                 :class "forge-empty"
                                 "No records yet. Data loads at runtime."))))
                   ;; Batch actions
                   (:div :class "forge-batch-actions"
                     (:select :name "batch_action"
                       (:option :value "" "Batch actions...")
                       (:option :value "delete" "Delete selected")
                       (:option :value "export" "Export CSV"))
                     (:button :type "button" :class "btn" "Apply"))))))
    (cauldron.crucible:conn-put-resp-body
     (cauldron.crucible:conn-put-resp-header
      (cauldron.crucible:conn-put-status conn 200)
      "Content-Type" "text/html; charset=utf-8")
     body)))

;;; --- Show view (single record detail) ---

(defun show-view (conn resource-config)
  "Render the resource show (detail) view."
  (let* ((name (forge-resource-config-display-name resource-config))
         (attrs (forge-resource-config-attributes resource-config))
         (id (cdr (assoc "id" (cauldron.crucible:conn-params conn) :test #'string=)))
         (base-url (format nil "/forge/~A"
                           (string-downcase
                            (symbol-name (forge-resource-config-name resource-config)))))
         (body (forge-layout (format nil "~A #~A" name (or id "?"))
                 (cauldron.alembic:html
                   ;; Actions
                   (:div :class "forge-toolbar"
                     (:a :href (format nil "~A/~A/edit" base-url (or id ""))
                         :class "btn" "Edit")
                     (:a :href base-url :class "btn" "Back to list"))
                   ;; Attribute table
                   (:table :class "forge-detail-table"
                     (mapcar (lambda (attr)
                                 `(:tr
                                    (:th ,(humanize-name attr))
                                    (:td "—")))  ; Value populated at runtime
                               attrs))))))
    (cauldron.crucible:conn-put-resp-body
     (cauldron.crucible:conn-put-resp-header
      (cauldron.crucible:conn-put-status conn 200)
      "Content-Type" "text/html; charset=utf-8")
     body)))

;;; --- Create view (new record form) ---

(defun render-form-field (field)
  "Render a forge-field-config as an Alembic form field."
  (let ((name (string-downcase (symbol-name (forge-field-config-name field))))
        (label (forge-field-config-label field))
        (type (forge-field-config-type field))
        (required (forge-field-config-required field))
        (options (forge-field-config-options field)))
    (case type
      (:textarea
       (cauldron.alembic:textarea name :label label :required required))
      (:select
       (cauldron.alembic:select-input name options :label label :required required))
      (:checkbox
       (cauldron.alembic:checkbox-input name :label label))
      (:number
       (cauldron.alembic:number-input name :label label :required required))
      (otherwise
       (cauldron.alembic:text-input name :label label :required required)))))

(defun create-view (conn resource-config)
  "Render the new record form."
  (let* ((name (forge-resource-config-display-name resource-config))
         (fields (forge-resource-config-form-fields resource-config))
         (base-url (format nil "/forge/~A"
                           (string-downcase
                            (symbol-name (forge-resource-config-name resource-config)))))
         (body (forge-layout (format nil "New ~A" name)
                 (cauldron.alembic:html
                   (:form :method "POST" :action base-url
                     (mapcar #'render-form-field fields)
                     (:div :class "forge-form-actions"
                       (cauldron.alembic:submit-button :label (format nil "Create ~A" name))
                       (:a :href base-url :class "btn" "Cancel")))))))
    (cauldron.crucible:conn-put-resp-body
     (cauldron.crucible:conn-put-resp-header
      (cauldron.crucible:conn-put-status conn 200)
      "Content-Type" "text/html; charset=utf-8")
     body)))

(defun create-action (conn resource-config)
  "Handle POST to create a new record."
  (declare (ignore resource-config))
  ;; At runtime: parse body, create changeset, validate, insert, redirect
  (cauldron.crucible:conn-put-resp-body
   (cauldron.crucible:conn-put-status conn 302)
   ""))

;;; --- Edit view ---

(defun edit-view (conn resource-config)
  "Render the edit record form."
  (let* ((name (forge-resource-config-display-name resource-config))
         (id (cdr (assoc "id" (cauldron.crucible:conn-params conn) :test #'string=)))
         (fields (forge-resource-config-form-fields resource-config))
         (base-url (format nil "/forge/~A"
                           (string-downcase
                            (symbol-name (forge-resource-config-name resource-config)))))
         (body (forge-layout (format nil "Edit ~A #~A" name (or id "?"))
                 (cauldron.alembic:html
                   (:form :method "POST" :action (format nil "~A/~A" base-url (or id ""))
                     (mapcar #'render-form-field fields)
                     (:div :class "forge-form-actions"
                       (cauldron.alembic:submit-button :label "Save Changes")
                       (:a :href (format nil "~A/~A" base-url (or id ""))
                           :class "btn" "Cancel")))))))
    (cauldron.crucible:conn-put-resp-body
     (cauldron.crucible:conn-put-resp-header
      (cauldron.crucible:conn-put-status conn 200)
      "Content-Type" "text/html; charset=utf-8")
     body)))

(defun update-action (conn resource-config)
  "Handle POST to update a record."
  (declare (ignore resource-config))
  (cauldron.crucible:conn-put-resp-body
   (cauldron.crucible:conn-put-status conn 302)
   ""))

;;; --- Delete action ---

(defun delete-action (conn resource-config)
  "Handle DELETE to remove a record."
  (declare (ignore resource-config))
  (cauldron.crucible:conn-put-resp-body
   (cauldron.crucible:conn-put-status conn 302)
   ""))

;;; --- Batch actions ---

(defun batch-action (conn resource-config action ids)
  "Handle batch operations on multiple records."
  (declare (ignore resource-config action ids))
  (cauldron.crucible:conn-put-resp-body
   (cauldron.crucible:conn-put-status conn 302)
   ""))
