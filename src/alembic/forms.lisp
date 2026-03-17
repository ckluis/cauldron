;;;; src/alembic/forms.lisp — Form helper components
;;;; Generates accessible HTML forms with CSRF protection and changeset error display.

(in-package :cauldron.alembic)

;;; --- CSRF token support ---

(defvar *csrf-token* nil
  "Current CSRF token. Set by the request pipeline.")

(defun csrf-hidden-field ()
  "Generate a hidden input with the current CSRF token."
  (when *csrf-token*
    `(:input :type "hidden" :name "_csrf_token" :value ,*csrf-token*)))

;;; --- Form helpers ---

(defun form-for (action &rest body)
  "Generate a form targeting ACTION (URL string).
BODY contains form field s-expressions.
Automatically includes CSRF token and method override."
  (let ((method (getf body :method "POST"))
        (fields (loop for item in body
                      unless (keywordp item)
                        collect item)))
    ;; Remove :method from body if present
    (remf body :method)
    `(:form :action ,action :method ,(if (member method '("GET" "get") :test #'string=)
                                         "GET" "POST")
       ,(csrf-hidden-field)
       ,@fields)))

(defun text-input (name &key label value placeholder required
                          (type "text") id class
                          errors scry-change scry-blur)
  "Generate a labeled text input.
NAME is the field name (string).
ERRORS is a list of error strings to display."
  (let ((input-id (or id (format nil "field-~A" name))))
    `(:div :class ,(format nil "field~@[ ~A~]~:[~; field-error~]" class errors)
       ,@(when label
           (list `(:label :for ,input-id ,label)))
       (:input :type ,type
               :id ,input-id
               :name ,name
               ,@(when value `(:value ,value))
               ,@(when placeholder `(:placeholder ,placeholder))
               ,@(when required '(:required t))
               ,@(when scry-change `(:scry-change ,scry-change))
               ,@(when scry-blur `(:scry-blur ,scry-blur)))
       ,@(when errors
           (mapcar (lambda (err)
                     `(:span :class "field-error-message" ,err))
                   errors)))))

(defun hidden-input (name value)
  "Generate a hidden input field."
  `(:input :type "hidden" :name ,name :value ,value))

(defun textarea (name &key label value rows placeholder required
                        id class errors scry-change)
  "Generate a labeled textarea."
  (let ((input-id (or id (format nil "field-~A" name))))
    `(:div :class ,(format nil "field~@[ ~A~]~:[~; field-error~]" class errors)
       ,@(when label
           (list `(:label :for ,input-id ,label)))
       (:textarea :id ,input-id
                  :name ,name
                  ,@(when rows `(:rows ,(princ-to-string rows)))
                  ,@(when placeholder `(:placeholder ,placeholder))
                  ,@(when required '(:required t))
                  ,@(when scry-change `(:scry-change ,scry-change))
                  ,(or value ""))
       ,@(when errors
           (mapcar (lambda (err)
                     `(:span :class "field-error-message" ,err))
                   errors)))))

(defun select-input (name options &key label selected required
                                       id class errors scry-change)
  "Generate a labeled select dropdown.
OPTIONS is a list of (value . display) pairs or plain strings."
  (let ((input-id (or id (format nil "field-~A" name))))
    `(:div :class ,(format nil "field~@[ ~A~]~:[~; field-error~]" class errors)
       ,@(when label
           (list `(:label :for ,input-id ,label)))
       (:select :id ,input-id
                :name ,name
                ,@(when required '(:required t))
                ,@(when scry-change `(:scry-change ,scry-change))
         ,@(mapcar (lambda (opt)
                     (let* ((val (if (consp opt) (car opt) opt))
                            (display (if (consp opt) (cdr opt) opt))
                            (val-str (if (stringp val) val (princ-to-string val))))
                       `(:option :value ,val-str
                                 ,@(when (equal val-str
                                                (if (stringp selected)
                                                    selected
                                                    (and selected (princ-to-string selected))))
                                     '(:selected t))
                                 ,(if (stringp display) display (princ-to-string display)))))
                   options))
       ,@(when errors
           (mapcar (lambda (err)
                     `(:span :class "field-error-message" ,err))
                   errors)))))

(defun checkbox-input (name &key label checked id class scry-change)
  "Generate a labeled checkbox."
  (let ((input-id (or id (format nil "field-~A" name))))
    `(:div :class ,(format nil "field field-checkbox~@[ ~A~]" class)
       (:input :type "checkbox"
               :id ,input-id
               :name ,name
               ,@(when checked '(:checked t))
               ,@(when scry-change `(:scry-change ,scry-change)))
       ,@(when label
           (list `(:label :for ,input-id ,label))))))

(defun number-input (name &key label value min max step placeholder
                               required id class errors scry-change)
  "Generate a labeled number input."
  (text-input name :label label :value value :placeholder placeholder
                   :required required :id id :class class
                   :errors errors :type "number"
                   :scry-change scry-change))

(defun submit-button (&key (label "Submit") class scry-click disabled)
  "Generate a submit button."
  `(:button :type "submit"
            ,@(when class `(:class ,class))
            ,@(when disabled '(:disabled t))
            ,@(when scry-click `(:scry-click ,scry-click))
            ,label))

;;; --- Scry attribute helpers ---

(defun scry-click (event-name)
  "Generate a scry-click attribute value."
  event-name)

(defun scry-change (event-name)
  "Generate a scry-change attribute value."
  event-name)

(defun scry-submit (event-name)
  "Generate a scry-submit attribute value."
  event-name)
