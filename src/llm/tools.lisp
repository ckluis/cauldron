;;;; src/llm/tools.lisp — Tool definition structs + Claude format conversion
(in-package :cauldron.llm)

;;; --- Data Structures ---

(defstruct agent-tool
  "A tool that an agent can invoke."
  (name "" :type string)
  (description "" :type string)
  (parameters nil :type list)       ; list of tool-parameter structs
  (handler nil :type function)      ; (lambda (conn args) ...) → result
  (destructive-p nil :type boolean))

(defstruct tool-parameter
  "A parameter for an agent tool."
  (name "" :type string)
  (type "string" :type string)      ; "string", "integer", "boolean"
  (description "" :type string)
  (required-p nil :type boolean)
  (enum nil :type list))

;;; --- Claude API Format Conversion ---

(defun %param-to-property (param)
  "Convert a tool-parameter to a (name . hash-table) pair for properties object."
  (let ((prop (cauldron.runtime:ht "type" (tool-parameter-type param)
                   "description" (tool-parameter-description param))))
    (when (tool-parameter-enum param)
      (setf (gethash "enum" prop) (tool-parameter-enum param)))
    (cons (tool-parameter-name param) prop)))

(defun tools-to-claude-format (tools)
  "Convert a list of agent-tools to Claude API tools JSON structure.
Returns a list of hash-tables ready for cauldron.json:encode."
  (mapcar (lambda (tool)
            (let* ((params (agent-tool-parameters tool))
                   (properties (let ((ht (make-hash-table :test 'equal)))
                                 (dolist (p params)
                                   (let ((pair (%param-to-property p)))
                                     (setf (gethash (car pair) ht) (cdr pair))))
                                 ht))
                   (required (coerce (mapcar #'tool-parameter-name
                                             (remove-if-not #'tool-parameter-required-p params))
                                     'vector)))
              (cauldron.runtime:ht "name" (agent-tool-name tool)
                   "description" (agent-tool-description tool)
                   "input_schema" (cauldron.runtime:ht "type" "object"
                                       "properties" properties
                                       "required" required))))
          tools))

;;; --- Tool Execution ---

(defun execute-tool (conn tool-name args tools)
  "Find tool by name, call its handler. Returns (values result-string success-p)."
  (let ((tool (find tool-name tools :key #'agent-tool-name :test #'string=)))
    (if (null tool)
        (values (format nil "Error: unknown tool ~S" tool-name) nil)
        (handler-case
            (let ((result (funcall (agent-tool-handler tool) conn args)))
              (values (if (stringp result)
                          result
                          (cauldron.json:encode result))
                      t))
          (error (e)
            (values (format nil "Error executing ~A: ~A" tool-name e) nil))))))
