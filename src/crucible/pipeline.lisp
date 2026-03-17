;;;; src/crucible/pipeline.lisp — Request processing pipelines
;;;; Pipelines are ordered sequences of "plugs" — functions that take
;;;; a conn and return a (possibly modified) conn. Any plug can halt.

(in-package :cauldron.crucible)

;;; --- Conn: the connection/request context ---

(defstruct conn
  "Represents an HTTP request/response lifecycle.
Flows through plugs and handlers, accumulating state."
  (method :get :type keyword)
  (path "/" :type string)
  (params '() :type list)           ; Route params alist
  (query-params '() :type list)     ; Query string params alist
  (body nil)                         ; Parsed request body
  (headers '() :type list)          ; Request headers alist
  (assigns (make-hash-table :test 'eq) :type hash-table)  ; Arbitrary assigns
  ;; Response
  (status 200 :type integer)
  (resp-headers '() :type list)     ; Response headers alist
  (resp-body nil)                    ; Response body (string, octets, or stream)
  ;; Control
  (halted nil :type boolean)        ; If true, pipeline stops
  (pipeline nil))                   ; Current pipeline name

(defun halt-conn (conn &key (status 200) body)
  "Halt the connection pipeline. No further plugs run."
  (setf (conn-halted conn) t)
  (when status (setf (conn-status conn) status))
  (when body (setf (conn-resp-body conn) body))
  conn)

(defun conn-halted-p (conn)
  "Return T if conn has been halted."
  (conn-halted conn))

(defun conn-get-assign (conn key)
  "Get an assign value from conn."
  (gethash key (conn-assigns conn)))

(defun conn-put-assign (conn key value)
  "Set an assign value on conn. Returns conn."
  (setf (gethash key (conn-assigns conn)) value)
  conn)

(defun conn-put-status (conn status)
  "Set the response status. Returns conn."
  (setf (conn-status conn) status)
  conn)

(defun conn-put-resp-header (conn name value)
  "Add a response header. Returns conn."
  (push (cons name value) (conn-resp-headers conn))
  conn)

(defun conn-put-resp-body (conn body)
  "Set the response body. Returns conn."
  (setf (conn-resp-body conn) body)
  conn)

;;; --- Pipeline ---

(defstruct pipeline-def
  "A named pipeline definition."
  (name nil :type symbol)
  (plugs '() :type list))           ; List of plug functions/specs

(defvar *pipelines* (make-hash-table :test 'eq)
  "Registry of named pipelines.")

(defmacro defpipeline (name &key plugs)
  "Define a named pipeline with ordered plugs.
Each plug is a function (conn) → conn or a list (function &rest args).

Example:
  (defpipeline :browser
    :plugs '(fetch-session
             (require-role :user)
             set-flash-scope))"
  `(setf (gethash ',name *pipelines*)
         (make-pipeline-def :name ',name :plugs ,plugs)))

(defun make-pipeline (name plugs)
  "Programmatically create a pipeline."
  (let ((pdef (make-pipeline-def :name name :plugs plugs)))
    (setf (gethash name *pipelines*) pdef)
    pdef))

(defun get-pipeline (name)
  "Look up a pipeline by name."
  (gethash name *pipelines*))

(defun call-plug (plug conn)
  "Call a single plug with CONN. Plug is either a function or (function &rest args)."
  (cond
    ((functionp plug)
     (funcall plug conn))
    ((and (consp plug) (functionp (first plug)))
     ;; Partial application: (fn arg1 arg2) → (fn conn arg1 arg2)
     (apply (first plug) conn (rest plug)))
    ((symbolp plug)
     (funcall (symbol-function plug) conn))
    ((and (consp plug) (symbolp (first plug)))
     (apply (symbol-function (first plug)) conn (rest plug)))
    (t
     (error "Invalid plug: ~S" plug))))

(defun run-pipeline (pipeline-name conn)
  "Run all plugs in the named pipeline against CONN.
Stops if any plug halts the connection.
Returns the final conn."
  (let ((pdef (get-pipeline pipeline-name)))
    (unless pdef
      (error "Unknown pipeline: ~A" pipeline-name))
    (setf (conn-pipeline conn) pipeline-name)
    (dolist (plug (pipeline-def-plugs pdef) conn)
      (when (conn-halted-p conn)
        (return conn))
      (setf conn (call-plug plug conn)))))
