;;;; src/crucible/auth.lisp — Authentication plugs and helpers
;;;; Provides password hashing, session-based auth, and role-based access control.

(in-package :cauldron.crucible)

;;; ============================================================
;;; Password Hashing (delegates to cauldron.crypto:bcrypt)
;;; ============================================================

(defun hash-password (password &key (cost 12))
  "Hash a plaintext password using bcrypt. Returns the hash string."
  (cauldron.crypto:bcrypt-hash password :cost cost))

(defun verify-password (password hash)
  "Verify a plaintext password against a bcrypt hash. Returns T or NIL."
  (cauldron.crypto:bcrypt-verify password hash))

;;; ============================================================
;;; Login / Logout
;;; ============================================================

(defun login (conn user-data)
  "Store USER-DATA in the session to log the user in.
USER-DATA should be a hash-table or alist of user fields.
Returns conn."
  (session-put conn "_current_user" user-data)
  conn)

(defun logout (conn)
  "Remove the current user from the session. Returns conn."
  (session-delete conn "_current_user")
  conn)

(defun current-user (conn)
  "Get the current logged-in user data from the session, or NIL."
  (session-get conn "_current_user"))

;;; ============================================================
;;; Authentication Plugs
;;; ============================================================

(defun make-plug-authenticate (&key user-loader)
  "Create an authentication plug that loads the current user.
USER-LOADER is a function (user-data) → user-or-nil that takes the raw
session data and returns a full user object (or NIL if invalid/expired).
If no USER-LOADER is provided, the raw session data is used directly.
Sets :current-user assign on conn."
  (lambda (conn)
    (let ((user-data (session-get conn "_current_user")))
      (if user-data
          (let ((user (if user-loader
                          (funcall user-loader user-data)
                          user-data)))
            (conn-put-assign conn :current-user user))
          (conn-put-assign conn :current-user nil))
      conn)))

(defun plug-require-auth (conn)
  "Halt with 401 if no current user is authenticated.
Must run after make-plug-authenticate in the pipeline."
  (if (conn-get-assign conn :current-user)
      conn
      (halt-conn conn :status 401
                       :body "Unauthorized: authentication required")))

(defun make-plug-require-role (&rest required-roles)
  "Create a plug that halts with 403 unless the current user has one of
REQUIRED-ROLES. Expects :current-user assign to have a hash-table with
a \"role\" key, or an alist with a :role entry.
Must run after make-plug-authenticate."
  (lambda (conn)
    (let* ((user (conn-get-assign conn :current-user))
           (role (when user
                   (typecase user
                     (hash-table (gethash "role" user))
                     (list (cdr (assoc :role user)))
                     (t nil)))))
      (if (and role (member role required-roles :test #'equal))
          conn
          (halt-conn conn :status 403
                          :body "Forbidden: insufficient permissions")))))
