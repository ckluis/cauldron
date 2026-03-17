;;;; src/scry/lifecycle.lisp — LiveView lifecycle management
(in-package :cauldron.scry)

;;; -------------------------------------------------------
;;; Scry LiveView Lifecycle
;;;
;;; Defines the LiveView component lifecycle: mount → render →
;;; handle events → re-render → diff → send patches.
;;; -------------------------------------------------------

;;; --- View Registry ---

(defstruct scry-view
  "Definition of a LiveView component."
  (name nil :type symbol)
  (mount-fn nil :type (or null function))
  (render-fn nil :type (or null function))
  (handle-event-fn nil :type (or null function))
  (handle-info-fn nil :type (or null function)))

(defvar *scry-views* (make-hash-table :test 'eq)
  "Registry of defined LiveView components: name → scry-view.")

(defmacro defscry (name &key mount render handle-event handle-info)
  "Define a LiveView component NAME with lifecycle callbacks.

:mount        — (lambda (params session socket) → assigns-plist)
                Called on initial connection. Returns initial assigns.

:render       — (lambda (assigns) → html-tree)
                Called to produce the HTML tree from current assigns.

:handle-event — (lambda (event assigns) → new-assigns-plist)
                Called when the client sends an event. Returns updated assigns.

:handle-info  — (lambda (info assigns) → new-assigns-plist)
                Called when a server-side message arrives (from PubSub).

Example:
  (defscry counter-live
    :mount (lambda (params session socket)
             (declare (ignore params session socket))
             (list :count 0))
    :render (lambda (assigns)
              (cauldron.alembic:html
                (:div (:h1 (format nil \"Count: ~D\" (getf assigns :count)))
                      (:button :scry-click \"increment\" \"+\"))))
    :handle-event (lambda (event assigns)
                    (case (intern (string-upcase event) :keyword)
                      (:increment (list :count (1+ (getf assigns :count))))
                      (t assigns))))"
  `(progn
     (setf (gethash ',name *scry-views*)
           (make-scry-view
            :name ',name
            :mount-fn ,mount
            :render-fn ,render
            :handle-event-fn ,handle-event
            :handle-info-fn ,handle-info))
     ',name))

(defun find-scry-view (name)
  "Look up a scry-view by NAME."
  (gethash name *scry-views*))

;;; --- Session Management ---

(defvar *session-counter* 0
  "Monotonically increasing session ID counter.")

(defvar *session-lock* (sb-thread:make-mutex :name "cauldron-scry-session-lock"))

(defun generate-session-id ()
  "Generate a unique session ID string. Thread-safe."
  (sb-thread:with-mutex (*session-lock*)
    (format nil "scry-~D-~D" (incf *session-counter*) (get-universal-time))))

(defstruct scry-session
  "State for an active LiveView session."
  (id (generate-session-id) :type string)
  (assigns nil :type list)          ; plist of current assigns
  (view nil :type (or null symbol)) ; name of the scry-view
  (socket nil)                      ; WebSocket connection
  (subscriptions nil :type list)    ; list of pubsub subscription IDs
  (last-tree nil)                   ; last rendered HTML tree (for diffing)
  (created-at (get-universal-time) :type integer))

(defvar *sessions* (make-hash-table :test 'equal)
  "Active sessions: session-id → scry-session.")

(defvar *sessions-lock* (sb-thread:make-mutex :name "cauldron-scry-sessions-lock"))

(defun register-session (session)
  "Register SESSION in the global sessions table. Thread-safe."
  (sb-thread:with-mutex (*sessions-lock*)
    (setf (gethash (scry-session-id session) *sessions*) session)))

(defun find-session (session-id)
  "Look up a session by ID. Thread-safe."
  (sb-thread:with-mutex (*sessions-lock*)
    (gethash session-id *sessions*)))

(defun remove-session (session-id)
  "Remove a session by ID. Thread-safe."
  (sb-thread:with-mutex (*sessions-lock*)
    (remhash session-id *sessions*)))

;;; --- Lifecycle Operations ---

(defun scry-mount (view-name params &key session socket)
  "Mount a LiveView, creating a new session.

1. Looks up the view definition
2. Calls the mount function to get initial assigns
3. Calls render to produce the initial HTML tree
4. Creates and registers a session
5. Returns (values session html-string)

VIEW-NAME — symbol naming the defscry view
PARAMS    — request parameters (plist)
SESSION   — HTTP session data (plist)
SOCKET    — WebSocket connection (or nil for initial HTTP render)"
  (let ((view (find-scry-view view-name)))
    (unless view
      (error "Unknown Scry view: ~S" view-name))
    ;; 1. Mount — get initial assigns
    (let* ((assigns (if (scry-view-mount-fn view)
                        (funcall (scry-view-mount-fn view) params session socket)
                        nil))
           ;; 2. Render initial tree
           (tree (when (scry-view-render-fn view)
                   (funcall (scry-view-render-fn view) assigns)))
           ;; 3. Create session
           (scry-sess (make-scry-session
                       :assigns assigns
                       :view view-name
                       :socket socket
                       :last-tree tree)))
      ;; 4. Register
      (register-session scry-sess)
      ;; 5. Return session + rendered HTML
      (let ((html (if tree
                      (cauldron.alembic:html-to-string tree)
                      "")))
        (values scry-sess html)))))

(defun scry-render (session)
  "Re-render the current assigns of SESSION, returning the HTML tree.
Updates the session's last-tree."
  (let ((view (find-scry-view (scry-session-view session))))
    (when (and view (scry-view-render-fn view))
      (let ((tree (funcall (scry-view-render-fn view)
                           (scry-session-assigns session))))
        (setf (scry-session-last-tree session) tree)
        tree))))

(defun scry-handle-event (session event &optional payload)
  "Process an event in a LiveView session.

1. Call the view's handle-event function with the event and current assigns
2. Update session assigns with the returned new assigns
3. Re-render to get new HTML tree
4. Diff old tree vs new tree
5. Return (values patches new-html-string)

SESSION — scry-session struct
EVENT   — event name string (from client)
PAYLOAD — optional event payload"
  (declare (ignore payload))
  (let ((view (find-scry-view (scry-session-view session))))
    (unless view
      (error "Session ~A references unknown view ~S"
             (scry-session-id session) (scry-session-view session)))
    (let ((old-tree (scry-session-last-tree session)))
      ;; 1. Handle event → new assigns
      (when (scry-view-handle-event-fn view)
        (let ((new-assigns (funcall (scry-view-handle-event-fn view)
                                    event
                                    (scry-session-assigns session))))
          (setf (scry-session-assigns session) new-assigns)))
      ;; 2. Re-render
      (let ((new-tree (scry-render session)))
        ;; 3. Diff
        (let ((patches (if (and old-tree new-tree)
                           (diff-trees old-tree new-tree)
                           (list (list :replace '() new-tree)))))
          ;; 4. Return patches and HTML
          (values patches
                  (if new-tree
                      (cauldron.alembic:html-to-string new-tree)
                      "")))))))

(defun scry-handle-info (session info)
  "Process a server-side info message in a LiveView session.
Same flow as handle-event but uses the handle-info callback."
  (let ((view (find-scry-view (scry-session-view session))))
    (when (and view (scry-view-handle-info-fn view))
      (let ((old-tree (scry-session-last-tree session)))
        (let ((new-assigns (funcall (scry-view-handle-info-fn view)
                                    info
                                    (scry-session-assigns session))))
          (setf (scry-session-assigns session) new-assigns))
        (let ((new-tree (scry-render session)))
          (let ((patches (if (and old-tree new-tree)
                             (diff-trees old-tree new-tree)
                             (list (list :replace '() new-tree)))))
            (values patches
                    (if new-tree
                        (cauldron.alembic:html-to-string new-tree)
                        ""))))))))
