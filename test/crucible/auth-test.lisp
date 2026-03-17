;;;; test/crucible/auth-test.lisp — Tests for authentication plugs
(in-package :cauldron.test)

(defsuite :crucible-auth)

;;; ============================================================
;;; Password hashing
;;; ============================================================

(deftest test-hash-password-returns-string
  (let ((hash (cauldron.crucible:hash-password "secret" :cost 4)))
    (is-not-nil hash)
    (is (stringp hash))
    (is (> (length hash) 0))))

(deftest test-verify-password-correct
  (let ((hash (cauldron.crucible:hash-password "mypassword" :cost 4)))
    (is-true (cauldron.crucible:verify-password "mypassword" hash))))

(deftest test-verify-password-wrong
  (let ((hash (cauldron.crucible:hash-password "correct" :cost 4)))
    (is-false (cauldron.crucible:verify-password "wrong" hash))))

;;; ============================================================
;;; Login / Logout / current-user
;;; ============================================================

(deftest test-login-stores-user
  (let ((cauldron.crucible:*session-secret* "test-secret")
        (cauldron.crucible:*session-store* (make-hash-table :test 'equal)))
    (let* ((conn (cauldron.crucible:make-conn))
           (conn (cauldron.crucible:plug-parse-cookies conn))
           (conn (cauldron.crucible:plug-session conn))
           (user-data (make-hash-table :test 'equal)))
      (setf (gethash "id" user-data) 1)
      (setf (gethash "email" user-data) "alice@example.com")
      (cauldron.crucible:login conn user-data)
      (is-not-nil (cauldron.crucible:current-user conn))
      (is-equal 1 (gethash "id" (cauldron.crucible:current-user conn))))))

(deftest test-logout-clears-user
  (let ((cauldron.crucible:*session-secret* "test-secret")
        (cauldron.crucible:*session-store* (make-hash-table :test 'equal)))
    (let* ((conn (cauldron.crucible:make-conn))
           (conn (cauldron.crucible:plug-parse-cookies conn))
           (conn (cauldron.crucible:plug-session conn))
           (user-data (make-hash-table :test 'equal)))
      (setf (gethash "id" user-data) 1)
      (cauldron.crucible:login conn user-data)
      (cauldron.crucible:logout conn)
      (is-nil (cauldron.crucible:current-user conn)))))

(deftest test-current-user-nil-when-not-logged-in
  (let ((cauldron.crucible:*session-secret* "test-secret")
        (cauldron.crucible:*session-store* (make-hash-table :test 'equal)))
    (let* ((conn (cauldron.crucible:make-conn))
           (conn (cauldron.crucible:plug-parse-cookies conn))
           (conn (cauldron.crucible:plug-session conn)))
      (is-nil (cauldron.crucible:current-user conn)))))

;;; ============================================================
;;; make-plug-authenticate
;;; ============================================================

(deftest test-authenticate-sets-current-user-assign
  (let ((cauldron.crucible:*session-secret* "test-secret")
        (cauldron.crucible:*session-store* (make-hash-table :test 'equal)))
    (let* ((conn (cauldron.crucible:make-conn))
           (conn (cauldron.crucible:plug-parse-cookies conn))
           (conn (cauldron.crucible:plug-session conn))
           (user-data (make-hash-table :test 'equal)))
      (setf (gethash "id" user-data) 42)
      (cauldron.crucible:login conn user-data)
      ;; Run authenticate plug
      (let* ((plug (cauldron.crucible:make-plug-authenticate))
             (result (funcall plug conn)))
        (is-not-nil (cauldron.crucible:conn-get-assign result :current-user))
        (is-equal 42 (gethash "id" (cauldron.crucible:conn-get-assign result :current-user)))))))

(deftest test-authenticate-nil-when-not-logged-in
  (let ((cauldron.crucible:*session-secret* "test-secret")
        (cauldron.crucible:*session-store* (make-hash-table :test 'equal)))
    (let* ((conn (cauldron.crucible:make-conn))
           (conn (cauldron.crucible:plug-parse-cookies conn))
           (conn (cauldron.crucible:plug-session conn))
           (plug (cauldron.crucible:make-plug-authenticate))
           (result (funcall plug conn)))
      (is-nil (cauldron.crucible:conn-get-assign result :current-user)))))

(deftest test-authenticate-with-user-loader
  (let ((cauldron.crucible:*session-secret* "test-secret")
        (cauldron.crucible:*session-store* (make-hash-table :test 'equal)))
    (let* ((conn (cauldron.crucible:make-conn))
           (conn (cauldron.crucible:plug-parse-cookies conn))
           (conn (cauldron.crucible:plug-session conn))
           (user-data (make-hash-table :test 'equal)))
      (setf (gethash "id" user-data) 1)
      (cauldron.crucible:login conn user-data)
      ;; User loader transforms the data
      (let* ((plug (cauldron.crucible:make-plug-authenticate
                    :user-loader (lambda (data)
                                   (let ((enriched (make-hash-table :test 'equal)))
                                     (setf (gethash "id" enriched) (gethash "id" data))
                                     (setf (gethash "loaded" enriched) t)
                                     enriched))))
             (result (funcall plug conn)))
        (is-true (gethash "loaded" (cauldron.crucible:conn-get-assign result :current-user)))))))

(deftest test-authenticate-user-loader-returns-nil
  (let ((cauldron.crucible:*session-secret* "test-secret")
        (cauldron.crucible:*session-store* (make-hash-table :test 'equal)))
    (let* ((conn (cauldron.crucible:make-conn))
           (conn (cauldron.crucible:plug-parse-cookies conn))
           (conn (cauldron.crucible:plug-session conn)))
      ;; Store user in session
      (cauldron.crucible:session-put conn "_current_user" "stale-data")
      ;; User loader returns nil (user not found)
      (let* ((plug (cauldron.crucible:make-plug-authenticate
                    :user-loader (lambda (data) (declare (ignore data)) nil)))
             (result (funcall plug conn)))
        (is-nil (cauldron.crucible:conn-get-assign result :current-user))))))

;;; ============================================================
;;; plug-require-auth
;;; ============================================================

(deftest test-require-auth-passes-when-authenticated
  (let ((conn (cauldron.crucible:make-conn)))
    (cauldron.crucible:conn-put-assign conn :current-user '((:id . 1)))
    (let ((result (cauldron.crucible:plug-require-auth conn)))
      (is-false (cauldron.crucible:conn-halted-p result)))))

(deftest test-require-auth-halts-when-not-authenticated
  (let ((conn (cauldron.crucible:make-conn)))
    ;; No :current-user assign
    (let ((result (cauldron.crucible:plug-require-auth conn)))
      (is-true (cauldron.crucible:conn-halted-p result))
      (is-equal 401 (cauldron.crucible:conn-status result)))))

;;; ============================================================
;;; make-plug-require-role
;;; ============================================================

(deftest test-require-role-passes-matching-role-hash-table
  (let ((conn (cauldron.crucible:make-conn))
        (user (make-hash-table :test 'equal)))
    (setf (gethash "role" user) "admin")
    (cauldron.crucible:conn-put-assign conn :current-user user)
    (let* ((plug (cauldron.crucible:make-plug-require-role "admin" "superadmin"))
           (result (funcall plug conn)))
      (is-false (cauldron.crucible:conn-halted-p result)))))

(deftest test-require-role-halts-wrong-role
  (let ((conn (cauldron.crucible:make-conn))
        (user (make-hash-table :test 'equal)))
    (setf (gethash "role" user) "viewer")
    (cauldron.crucible:conn-put-assign conn :current-user user)
    (let* ((plug (cauldron.crucible:make-plug-require-role "admin"))
           (result (funcall plug conn)))
      (is-true (cauldron.crucible:conn-halted-p result))
      (is-equal 403 (cauldron.crucible:conn-status result)))))

(deftest test-require-role-halts-no-user
  (let ((conn (cauldron.crucible:make-conn)))
    (let* ((plug (cauldron.crucible:make-plug-require-role "admin"))
           (result (funcall plug conn)))
      (is-true (cauldron.crucible:conn-halted-p result))
      (is-equal 403 (cauldron.crucible:conn-status result)))))

(deftest test-require-role-alist-user
  (let ((conn (cauldron.crucible:make-conn)))
    (cauldron.crucible:conn-put-assign conn :current-user '((:role . "editor")))
    (let* ((plug (cauldron.crucible:make-plug-require-role "editor"))
           (result (funcall plug conn)))
      (is-false (cauldron.crucible:conn-halted-p result)))))
