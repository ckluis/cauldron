;;;; src/crucible/api.lisp — Generic API response helpers
;;;; Phase 34D: Moved from Crucible Works to core framework
(in-package :cauldron.crucible)

;;; --- Default role hierarchy for RBAC ---

(defvar *role-hierarchy* '("owner" "admin" "member" "viewer")
  "Default role hierarchy from most to least privileged.")

(defun %role-at-least-p (user-role required-role)
  "Return T if USER-ROLE is at least as privileged as REQUIRED-ROLE."
  (let ((user-level (or (position user-role *role-hierarchy* :test #'string-equal)
                         (length *role-hierarchy*)))
        (required-level (or (position required-role *role-hierarchy* :test #'string-equal)
                             (length *role-hierarchy*))))
    (<= user-level required-level)))

;;; --- Core Response Helpers ---

(defun json-response (conn data &key (status 200))
  "Send a JSON response."
  (conn-put-status conn status)
  (conn-put-resp-body conn (cauldron.json:encode data))
  conn)

(defun json-error (conn message &key (status 400) hint help-link)
  "Send a JSON error response with optional LLM recovery hints."
  (let ((data (list (cons "error" message))))
    (when hint (push (cons "_hint" hint) (cdr (last data))))
    (when help-link
      (push (cons "_links" (list (cons "help" help-link))) (cdr (last data))))
    (json-response conn data :status status)))

;;; --- Hypermedia Helpers ---

(defun api-base-path (conn)
  "Return the base API path for the current company slug."
  (let ((slug (cdr (assoc "slug" (conn-params conn) :test #'string=))))
    (format nil "/api/~A" slug)))

(defun wrap-api-response (conn payload &key extra-links)
  "Wrap a payload with _links for navigation. Merges EXTRA-LINKS into default links."
  (let* ((base (api-base-path conn))
         (links (list (cons "api_root" base)
                      (cons "help" (format nil "~A/help" base)))))
    (dolist (link extra-links)
      (push link links))
    (append payload (list (cons "_links" (nreverse links))))))

(defun role-allows-p (role action)
  "Return T if ROLE is allowed to perform ACTION.
Viewers can only list/get. Members can CRUD. Admins/owners can do everything."
  (cond
    ((member action '(:list :get) :test #'eq) t)
    ((member action '(:create :update :delete :link :unlink) :test #'eq)
     (%role-at-least-p (or role "viewer") "member"))
    (t nil)))

;;; --- Example Generator ---

(defun generate-example-value (field-type field-name)
  "Generate a realistic example value for a field type/name combination."
  (cond
    ((string-equal field-type "email") "name@example.com")
    ((string-equal field-type "phone") "555-0100")
    ((string-equal field-type "url") "https://example.com")
    ((string-equal field-type "date") "2026-03-15")
    ((string-equal field-type "datetime") "2026-03-15T10:30:00Z")
    ((string-equal field-type "number") "99.99")
    ((string-equal field-type "money") "1500.00")
    ((string-equal field-type "integer") "42")
    ((string-equal field-type "boolean") "true")
    ((string-equal field-type "textarea") (format nil "Description of ~A..." field-name))
    (t (format nil "Example ~A" field-name))))

;;; --- Validation Descriptions ---

(defun describe-validation (field-type required-p)
  "Return a human-readable validation description for a field type."
  (let ((type-desc
          (cond
            ((string-equal field-type "email") "Must contain @ and be at least 4 characters")
            ((string-equal field-type "url") "Must start with http:// or https://")
            ((string-equal field-type "date") "Format: YYYY-MM-DD")
            ((string-equal field-type "datetime") "Format: ISO 8601 (YYYY-MM-DDTHH:MM:SSZ)")
            ((string-equal field-type "number") "Must be a valid number")
            ((string-equal field-type "money") "Must be a valid number (decimal)")
            ((string-equal field-type "integer") "Must be a whole number")
            ((string-equal field-type "boolean") "Must be true or false")
            ((string-equal field-type "phone") "Free-form phone number")
            (t "Free-form text"))))
    (if required-p
        (format nil "Required. ~A" type-desc)
        type-desc)))

;;; --- Example Payloads ---

(defun build-example-payload (fields)
  "Build an example JSON payload from a list of fields (hash-tables with name/field_type)."
  (let ((payload nil))
    (dolist (f fields)
      (let ((name (gethash "name" f))
            (ft (gethash "field_type" f)))
        (push (cons name (generate-example-value ft name)) payload)))
    (nreverse payload)))

;;; --- Help Actions ---

(defun build-help-actions (role base object-name fields)
  "Build the actions list for help detail, filtered by role."
  (let ((actions nil)
        (records-href (format nil "~A/records/~A" base object-name)))
    ;; List — always visible
    (push (list (cons "action" "list")
                (cons "method" "GET")
                (cons "href" records-href)
                (cons "parameters" (list (cons "limit" "integer (default 50)")
                                         (cons "offset" "integer (default 0)"))))
          actions)
    ;; Get — always visible
    (push (list (cons "action" "get")
                (cons "method" "GET")
                (cons "href_template" (format nil "~A/{id}" records-href)))
          actions)
    ;; Create — member+
    (when (role-allows-p role :create)
      (let ((example (build-example-payload fields)))
        (push (list (cons "action" "create")
                    (cons "method" "POST")
                    (cons "href" records-href)
                    (cons "example_body" example))
              actions)))
    ;; Update — member+
    (when (role-allows-p role :update)
      (push (list (cons "action" "update")
                  (cons "method" "PUT")
                  (cons "href_template" (format nil "~A/{id}" records-href)))
            actions))
    ;; Delete — member+
    (when (role-allows-p role :delete)
      (push (list (cons "action" "delete")
                  (cons "method" "DELETE")
                  (cons "href_template" (format nil "~A/{id}" records-href)))
            actions))
    ;; Link — member+
    (when (role-allows-p role :link)
      (push (list (cons "action" "link")
                  (cons "method" "POST")
                  (cons "href_template" (format nil "~A/{id}/link" records-href))
                  (cons "example_body" (list (cons "target_object" "other_object")
                                              (cons "target_id" 1)
                                              (cons "relation_type" "related"))))
            actions))
    ;; Unlink — member+
    (when (role-allows-p role :unlink)
      (push (list (cons "action" "unlink")
                  (cons "method" "DELETE")
                  (cons "href_template" (format nil "~A/{id}/link" records-href))
                  (cons "example_body" (list (cons "target_object" "other_object")
                                              (cons "target_id" 1))))
            actions))
    (nreverse actions)))
