;;;; src/runtime/config.lisp — Environment-based configuration with type coercion
(in-package :cauldron.runtime)

;;; --- Config registry ---

(defvar *config-entries* (make-hash-table :test 'eq)
  "Registry of declared config entries: name → plist (:env-var :type :default :required :description :value).")

;;; --- Type coercion ---

(defun coerce-config-value (string-value type)
  "Coerce STRING-VALUE to TYPE.
TYPE is one of :string, :integer, :boolean, :float.
:boolean treats \"true\", \"1\", \"yes\" (case-insensitive) as T, everything else as NIL."
  (case type
    (:string string-value)
    (:integer (%parse-int string-value 0 (length string-value)))
    (:boolean (let ((lower (string-downcase string-value)))
                (or (string= lower "true")
                    (string= lower "1")
                    (string= lower "yes"))))
    (:float (let ((*read-eval* nil))
              (let ((val (read-from-string string-value)))
                (if (numberp val) (coerce val 'double-float)
                    (error "Cannot coerce ~S to float" string-value)))))
    (otherwise (error "Unknown config type: ~S" type))))

(defun %parse-int (string start end)
  "Parse an integer from STRING between START and END."
  (let ((negative nil)
        (result 0)
        (i start))
    ;; Skip whitespace
    (loop while (and (< i end) (member (char string i) '(#\Space #\Tab)))
          do (incf i))
    ;; Sign
    (when (and (< i end) (char= (char string i) #\-))
      (setf negative t)
      (incf i))
    (when (and (< i end) (char= (char string i) #\+))
      (incf i))
    ;; Digits
    (loop while (< i end)
          for ch = (char string i)
          while (digit-char-p ch)
          do (setf result (+ (* result 10) (digit-char-p ch)))
             (incf i))
    (if negative (- result) result)))

;;; --- Environment access ---

(defun get-env (name &key (type :string) default required)
  "Get environment variable NAME with type coercion.
TYPE is :string, :integer, :boolean, or :float.
Returns DEFAULT if NAME is not set.
Signals error if REQUIRED is true and NAME is not set with no DEFAULT."
  (let ((raw (sb-ext:posix-getenv name)))
    (cond
      (raw (coerce-config-value raw type))
      (default default)
      (required (error "Required environment variable ~A is not set" name))
      (t (case type
           (:boolean nil)
           (:integer 0)
           (:float 0.0d0)
           (otherwise nil))))))

;;; --- Declarative config ---

(defmacro defconfig (name env-var &key (type :string) default required description)
  "Declare a config entry. Registers for validation and provides a reader function.
NAME is a symbol, ENV-VAR is the environment variable string."
  `(progn
     (setf (gethash ',name *config-entries*)
           (list :env-var ,env-var
                 :type ,type
                 :default ,default
                 :required ,required
                 :description ,description))
     (defun ,name ()
       ,(or description (format nil "Read config ~A from env ~A" name env-var))
       (get-env ,env-var :type ,type :default ,default :required ,required))))

;;; --- Validation ---

(defun validate-config ()
  "Validate all registered config entries.
Signals error listing all missing required entries."
  (let ((missing '()))
    (maphash (lambda (name plist)
               (let ((env-var (getf plist :env-var))
                     (default (getf plist :default))
                     (required (getf plist :required)))
                 (when (and required
                            (null default)
                            (null (sb-ext:posix-getenv env-var)))
                   (push (cons name env-var) missing))))
             *config-entries*)
    (when missing
      (error "Missing required config:~%~{  ~A (~A)~%~}"
             (loop for (name . env-var) in (nreverse missing)
                   collect name collect env-var)))
    t))

;;; --- Summary ---

(defun config-summary ()
  "Return alist of (name value required) for all registered config entries.
Masks values for entries whose env-var contains SECRET, PASSWORD, or KEY."
  (let ((result '()))
    (maphash (lambda (name plist)
               (let* ((env-var (getf plist :env-var))
                      (type (getf plist :type))
                      (default (getf plist :default))
                      (required (getf plist :required))
                      (raw (sb-ext:posix-getenv env-var))
                      (value (if raw
                                 (coerce-config-value raw type)
                                 default))
                      (masked (if (and raw (sensitive-env-var-p env-var))
                                  "********"
                                  value)))
                 (push (list name masked required) result)))
             *config-entries*)
    (sort result #'string< :key (lambda (e) (symbol-name (first e))))))

(defun sensitive-env-var-p (env-var)
  "Return T if ENV-VAR name suggests a sensitive value."
  (let ((upper (string-upcase env-var)))
    (or (search "SECRET" upper)
        (search "PASSWORD" upper)
        (search "KEY" upper))))
