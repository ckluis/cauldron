;;;; src/crucible/router.lisp — Router definition and dispatch

(in-package :cauldron.crucible)

;;; --- Router struct ---

(defstruct router
  "A Cauldron router with a route trie and pipeline assignments."
  (name nil :type symbol)
  (trie (make-trie) :type trie-node)
  (default-pipeline nil)            ; Pipeline applied to all routes
  (mounts '() :type list)           ; List of (prefix . sub-router)
  (not-found-handler nil)           ; Custom 404 handler
  (method-not-allowed-handler nil)) ; Custom 405 handler

(defvar *routers* (make-hash-table :test 'eq)
  "Registry of named routers.")

;;; --- Route addition ---

(defun add-route (router method path handler &key pipeline)
  "Add a route to ROUTER.
METHOD is :GET, :POST, :PUT, :PATCH, :DELETE, or :ANY.
PATH is a string like '/users/:id'.
HANDLER is a function (conn) → conn."
  (trie-insert (router-trie router) method path handler :pipeline pipeline)
  router)

(defun mount (router prefix sub-router &key pipeline)
  "Mount SUB-ROUTER under PREFIX in ROUTER.
All routes in SUB-ROUTER are accessible at PREFIX + original-path."
  ;; Store mount for dispatch-time prefix stripping
  (push (list prefix sub-router pipeline) (router-mounts router))
  router)

;;; --- Dispatch ---

(defun strip-prefix (path prefix)
  "Remove PREFIX from PATH. Returns stripped path or NIL if no match."
  (let ((plen (length prefix)))
    (cond
      ((string= path prefix) "/")
      ((and (> (length path) plen)
            (string= path prefix :end1 plen)
            (char= (char path plen) #\/))
       (subseq path plen))
      (t nil))))

(defun match-route (router method path)
  "Match METHOD and PATH against ROUTER.
Returns (values handler params pipeline) or NIL.
Checks mounts first (by prefix length, longest first), then own routes."
  ;; Check mounts (longest prefix first)
  (let ((sorted-mounts (sort (copy-list (router-mounts router))
                             #'> :key (lambda (m) (length (first m))))))
    (dolist (mount-entry sorted-mounts)
      (destructuring-bind (prefix sub-router mount-pipeline) mount-entry
        (let ((stripped (strip-prefix path prefix)))
          (when stripped
            (multiple-value-bind (handler params pipeline)
                (match-route sub-router method stripped)
              (when handler
                (return-from match-route
                  (values handler params (or pipeline mount-pipeline))))))))))
  ;; Check own routes
  (trie-match (router-trie router) method path))

(defun dispatch (router conn)
  "Dispatch CONN through ROUTER. Returns the final conn.
1. Match route
2. Set params on conn
3. Run pipeline (if any)
4. Call handler
5. Return conn with response set."
  (multiple-value-bind (handler params pipeline)
      (match-route router (conn-method conn) (conn-path conn))
    (cond
      ;; No match — 404
      ((null handler)
       (if (router-not-found-handler router)
           (funcall (router-not-found-handler router) conn)
           (progn
             (setf (conn-status conn) 404)
             (setf (conn-resp-body conn) "Not Found")
             conn)))

      (t
       ;; Set route params
       (setf (conn-params conn) params)
       ;; Run pipeline if assigned
       (let ((effective-pipeline (or pipeline (router-default-pipeline router))))
         (when effective-pipeline
           (setf conn (run-pipeline effective-pipeline conn))))
       ;; Call handler (unless pipeline halted)
       (unless (conn-halted-p conn)
         (setf conn (funcall handler conn)))
       conn))))

;;; --- defrouter macro ---

(defmacro defrouter (name &body route-defs)
  "Define a named router with routes.

Usage:
  (defrouter *app*
    (:get  \"/\"           #'index-handler)
    (:get  \"/users\"      #'users-index)
    (:get  \"/users/:id\"  #'users-show)
    (:post \"/users\"      #'users-create)
    (:any  \"/health\"     #'health-check))

Each route-def is (METHOD PATH HANDLER &key pipeline)."
  (let ((router-var (gensym "ROUTER")))
    `(progn
       (defvar ,name
         (let ((,router-var (make-router :name ',name)))
           ,@(mapcar (lambda (rdef)
                       (destructuring-bind (method path handler &rest opts) rdef
                         `(add-route ,router-var ,method ,path ,handler ,@opts)))
                     route-defs)
           (setf (gethash ',name *routers*) ,router-var)
           ,router-var)))))
