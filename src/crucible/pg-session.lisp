;;;; src/crucible/pg-session.lisp — PostgreSQL-backed session store
;;;; Replaces the in-memory session store with persistent PG storage.
;;;; Auto-creates the cauldron_sessions table on first use.

(in-package :cauldron.crucible)

;;; ============================================================
;;; PG Session Store
;;; ============================================================

(defstruct pg-session-store
  "PostgreSQL-backed session store."
  (pool nil)                                ; cauldron.db:pg-pool
  (table-name "cauldron_sessions" :type string)
  (initialized nil :type boolean))           ; Whether table has been created

(defvar *pg-session-store* nil
  "The active PG session store instance, or NIL if using in-memory.")

(defun make-pg-session-store-instance (pool &key (table-name "cauldron_sessions"))
  "Create a PG session store backed by POOL."
  (make-pg-session-store :pool pool :table-name table-name))

(defun ensure-session-table (store)
  "Create the sessions table if it doesn't exist."
  (unless (pg-session-store-initialized store)
    (cauldron.db:with-pool-connection (conn (pg-session-store-pool store))
      (cauldron.db:execute-sql conn
        (format nil "CREATE TABLE IF NOT EXISTS ~A (
           session_id TEXT PRIMARY KEY,
           data TEXT NOT NULL DEFAULT '{}',
           created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
           updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
         )" (pg-session-store-table-name store))))
    (setf (pg-session-store-initialized store) t)))

;;; ============================================================
;;; Store Operations
;;; ============================================================

(defun pg-session-load (store session-id)
  "Load session data from PG. Returns a hash-table or NIL if not found."
  (ensure-session-table store)
  (cauldron.db:with-pool-connection (conn (pg-session-store-pool store))
    (let ((rows (cauldron.db:query conn
                  (format nil "SELECT data FROM ~A WHERE session_id = $1"
                          (pg-session-store-table-name store))
                  session-id)))
      (when rows
        (let ((data-json (gethash "data" (first rows))))
          (when (and data-json (> (length data-json) 0))
            (cauldron.json:decode data-json)))))))

(defun pg-session-save (store session-id session-data)
  "Save session data to PG. Creates or updates the session row."
  (ensure-session-table store)
  (let ((json (cauldron.json:encode session-data)))
    (cauldron.db:with-pool-connection (conn (pg-session-store-pool store))
      (cauldron.db:execute-sql conn
        (format nil "INSERT INTO ~A (session_id, data, updated_at)
                     VALUES ($1, $2, NOW())
                     ON CONFLICT (session_id)
                     DO UPDATE SET data = $2, updated_at = NOW()"
                (pg-session-store-table-name store))
        session-id json))))

(defun pg-session-delete (store session-id)
  "Delete a session from PG."
  (ensure-session-table store)
  (cauldron.db:with-pool-connection (conn (pg-session-store-pool store))
    (cauldron.db:execute-sql conn
      (format nil "DELETE FROM ~A WHERE session_id = $1"
              (pg-session-store-table-name store))
      session-id)))

(defun pg-session-cleanup (store &key (max-age-seconds 86400))
  "Delete sessions older than MAX-AGE-SECONDS."
  (ensure-session-table store)
  (cauldron.db:with-pool-connection (conn (pg-session-store-pool store))
    (cauldron.db:execute-sql conn
      (format nil "DELETE FROM ~A WHERE updated_at < NOW() - INTERVAL '~D seconds'"
              (pg-session-store-table-name store)
              max-age-seconds))))

;;; ============================================================
;;; PG Session Plug
;;; ============================================================

(defun plug-pg-session (conn)
  "Session plug using PostgreSQL storage instead of in-memory.
Requires *pg-session-store* to be set. Falls back to in-memory plug-session
if *pg-session-store* is NIL."
  (unless *pg-session-store*
    (return-from plug-pg-session (plug-session conn)))
  (let* ((cookies (conn-get-assign conn :cookies))
         (cookie-value (cdr (assoc *session-cookie-name* cookies :test #'string=)))
         (session-id (when cookie-value (verify-session-cookie cookie-value)))
         (session-data nil)
         (new-session-p nil))
    ;; Try to load existing session from PG
    (when session-id
      (setf session-data (pg-session-load *pg-session-store* session-id)))
    ;; Create new session if needed
    (unless session-data
      (setf session-id (cauldron.crypto:generate-token :length 32))
      (setf session-data (make-hash-table :test 'equal))
      (setf new-session-p t))
    ;; Store on conn
    (conn-put-assign conn :session-id session-id)
    (conn-put-assign conn :session session-data)
    ;; Mark session for PG save on response
    (conn-put-assign conn :pg-session-store *pg-session-store*)
    ;; Set cookie if new session
    (when new-session-p
      (let ((signed (sign-session-id session-id)))
        (plug-set-cookie conn *session-cookie-name* signed
                         :path "/" :http-only t
                         :same-site "Lax"
                         :max-age *session-max-age*)))
    conn))

(defun plug-pg-session-save (conn)
  "Post-handler plug that persists session data to PG.
Place this plug after the handler in the pipeline."
  (let ((store (conn-get-assign conn :pg-session-store))
        (session-id (conn-get-assign conn :session-id))
        (session-data (conn-get-assign conn :session)))
    (when (and store session-id session-data)
      (pg-session-save store session-id session-data)))
  conn)
