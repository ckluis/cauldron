;;;; test/forge/audit-test.lisp — Forge audit system tests
(in-package :cauldron.test)

(defsuite :forge-audit)

;;; ============================================================
;;; setup-audit-table
;;; ============================================================

(deftest test-setup-audit-table-calls-count
  "setup-audit-table should call execute-fn 4 times (1 table + 3 indexes)."
  (let ((call-count 0))
    (cauldron.forge:setup-audit-table
     (lambda (sql) (declare (ignore sql)) (incf call-count)))
    (is-equal 4 call-count)))

(deftest test-setup-audit-table-first-is-create-table
  (let ((calls '()))
    (cauldron.forge:setup-audit-table
     (lambda (sql) (push sql calls)))
    (let ((first-call (car (last calls))))
      (is (search "CREATE TABLE" first-call)))))

(deftest test-setup-audit-table-indexes
  (let ((calls '()))
    (cauldron.forge:setup-audit-table
     (lambda (sql) (push sql calls)))
    ;; Last 3 calls (reversed, so first 3 in calls) are indexes
    (let ((index-calls (subseq calls 0 3)))
      (dolist (sql index-calls)
        (is (search "CREATE INDEX" sql))))))

(deftest test-setup-audit-table-ddl-contains-columns
  (let ((table-sql nil))
    (cauldron.forge:setup-audit-table
     (lambda (sql)
       (when (search "CREATE TABLE" sql)
         (setf table-sql sql))))
    (is-not-nil table-sql)
    (is (search "resource_type" table-sql))
    (is (search "resource_id" table-sql))
    (is (search "action" table-sql))
    (is (search "actor" table-sql))
    (is (search "changes" table-sql))
    (is (search "metadata" table-sql))
    (is (search "created_at" table-sql))))

;;; ============================================================
;;; record-audit-event
;;; ============================================================

(deftest test-record-audit-event-calls-execute
  (let ((called nil))
    (cauldron.forge:record-audit-event
     :execute-fn (lambda (sql &rest params)
                   (declare (ignore sql params))
                   (setf called t))
     :resource-type "user"
     :resource-id "123"
     :action "create"
     :actor "admin")
    (is-true called)))

(deftest test-record-audit-event-sql-is-insert
  (let ((captured-sql nil))
    (cauldron.forge:record-audit-event
     :execute-fn (lambda (sql &rest params)
                   (declare (ignore params))
                   (setf captured-sql sql))
     :resource-type "user"
     :resource-id "456"
     :action "update"
     :actor "admin")
    (is (search "INSERT INTO forge_audit_log" captured-sql))))

(deftest test-record-audit-event-params-order
  (let ((captured-params nil))
    (cauldron.forge:record-audit-event
     :execute-fn (lambda (sql &rest params)
                   (declare (ignore sql))
                   (setf captured-params params))
     :resource-type "post"
     :resource-id "789"
     :action "delete"
     :actor "moderator")
    ;; Params: resource-type, resource-id, action, actor, changes-json, metadata-json
    (is-equal "post" (first captured-params))
    (is-equal "789" (second captured-params))
    (is-equal "delete" (third captured-params))
    (is-equal "moderator" (fourth captured-params))))

(deftest test-record-audit-event-nil-changes-encodes-null
  (let ((captured-params nil))
    (cauldron.forge:record-audit-event
     :execute-fn (lambda (sql &rest params)
                   (declare (ignore sql))
                   (setf captured-params params))
     :resource-type "user"
     :resource-id "1"
     :action "view"
     :actor "admin"
     :changes nil
     :metadata nil)
    ;; 5th and 6th params should be "null"
    (is-equal "null" (fifth captured-params))
    (is-equal "null" (sixth captured-params))))

;;; ============================================================
;;; audit-log-view (HTML output)
;;; ============================================================

(deftest test-audit-log-view-returns-conn
  "audit-log-view should return a modified conn."
  (let* ((conn (cauldron.crucible:make-conn :method :get :path "/forge/audit"))
         (result (cauldron.forge:audit-log-view conn nil)))
    (is-not-nil result)))

(deftest test-audit-log-view-contains-heading
  (let* ((conn (cauldron.crucible:make-conn :method :get :path "/forge/audit"))
         (result (cauldron.forge:audit-log-view conn nil))
         (body (cauldron.crucible:conn-resp-body result)))
    (is (search "Audit Log" body))))

(deftest test-audit-log-view-contains-table-headers
  (let* ((conn (cauldron.crucible:make-conn :method :get :path "/forge/audit"))
         (result (cauldron.forge:audit-log-view conn nil))
         (body (cauldron.crucible:conn-resp-body result)))
    (is (search "Time" body))
    (is (search "Resource" body))
    (is (search "Action" body))
    (is (search "Actor" body))
    (is (search "Changes" body))))

;;; ============================================================
;;; system-info-view (HTML output)
;;; ============================================================

(deftest test-system-info-view-returns-conn
  (let* ((conn (cauldron.crucible:make-conn :method :get :path "/forge/system"))
         (result (cauldron.forge:system-info-view conn nil)))
    (is-not-nil result)))

(deftest test-system-info-view-contains-implementation
  (let* ((conn (cauldron.crucible:make-conn :method :get :path "/forge/system"))
         (result (cauldron.forge:system-info-view conn nil))
         (body (cauldron.crucible:conn-resp-body result)))
    (is (search (lisp-implementation-type) body))))

(deftest test-system-info-view-contains-version
  (let* ((conn (cauldron.crucible:make-conn :method :get :path "/forge/system"))
         (result (cauldron.forge:system-info-view conn nil))
         (body (cauldron.crucible:conn-resp-body result)))
    (is (search (lisp-implementation-version) body))))
