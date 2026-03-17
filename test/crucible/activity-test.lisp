;;;; test/crucible/activity-test.lisp — Phase 44: Activity feed tests
(in-package :cauldron.test)

(defsuite :activity-feed)

;;; ============================================================
;;; Activity Recording SQL
;;; ============================================================

(deftest test-record-activity-sql
  (multiple-value-bind (sql params)
      (cauldron.crucible:record-activity-sql "created" "contact" 42
                                              :actor-id 1 :company-id 5)
    (is (search "INSERT INTO activity_events" sql))
    (is (search "RETURNING" sql))
    (is-equal 7 (length params))
    (is-equal "user" (first params) "Default actor_type")
    (is-equal 1 (second params) "actor_id")
    (is-equal "created" (third params) "action")
    (is-equal "contact" (fourth params) "target_type")
    (is-equal 42 (fifth params) "target_id")))

(deftest test-record-activity-sql-with-context
  (multiple-value-bind (sql params)
      (cauldron.crucible:record-activity-sql "updated" "deal" 10
                                              :actor-type "agent"
                                              :actor-id 3
                                              :context "{\"field\":\"status\"}")
    (declare (ignore sql))
    (is-equal "agent" (first params) "Custom actor_type")
    (is-equal "{\"field\":\"status\"}" (sixth params) "Context JSON")))

(deftest test-record-activity-sql-default-context
  (multiple-value-bind (sql params)
      (cauldron.crucible:record-activity-sql "deleted" "note" 7)
    (declare (ignore sql))
    (is-equal "{}" (sixth params) "Default empty context")))

;;; ============================================================
;;; Activity Listing SQL
;;; ============================================================

(deftest test-list-activities-sql-no-filters
  (multiple-value-bind (sql params)
      (cauldron.crucible:list-activities-sql)
    (is (search "SELECT" sql))
    (is (search "ORDER BY created_at DESC" sql))
    (is (search "LIMIT 50" sql))
    (is-nil params "No params without filters")))

(deftest test-list-activities-sql-by-company
  (multiple-value-bind (sql params)
      (cauldron.crucible:list-activities-sql :company-id 3)
    (is (search "company_id = $1" sql))
    (is-equal 1 (length params))
    (is-equal 3 (first params))))

(deftest test-list-activities-sql-multiple-filters
  (multiple-value-bind (sql params)
      (cauldron.crucible:list-activities-sql :company-id 1 :action "created" :target-type "deal")
    (is (search "company_id" sql) "Company filter present")
    (is (search "action" sql) "Action filter present")
    (is (search "target_type" sql) "Target type filter present")
    (is-equal 3 (length params) "Three params")))

(deftest test-list-activities-sql-custom-pagination
  (multiple-value-bind (sql params)
      (cauldron.crucible:list-activities-sql :limit 10 :offset 20)
    (declare (ignore params))
    (is (search "LIMIT 10" sql))
    (is (search "OFFSET 20" sql))))

(deftest test-list-activities-sql-by-actor
  (multiple-value-bind (sql params)
      (cauldron.crucible:list-activities-sql :actor-type "agent" :actor-id 5)
    (is (search "actor_type" sql))
    (is (search "actor_id" sql))
    (is-equal 2 (length params))))

;;; ============================================================
;;; Count Activities
;;; ============================================================

(deftest test-count-activities-sql-no-filters
  (multiple-value-bind (sql params)
      (cauldron.crucible:count-activities-sql)
    (is (search "COUNT(*)" sql))
    (is-nil params)))

(deftest test-count-activities-sql-filtered
  (multiple-value-bind (sql params)
      (cauldron.crucible:count-activities-sql :company-id 2 :target-type "deal")
    (is (search "company_id" sql))
    (is (search "target_type" sql))
    (is-equal 2 (length params))))

;;; ============================================================
;;; Activity Feed
;;; ============================================================

(deftest test-activity-feed-sql
  (multiple-value-bind (sql params)
      (cauldron.crucible:activity-feed-sql 7)
    (is (search "company_id = $1" sql))
    (is (search "ORDER BY created_at DESC" sql))
    (is-equal 1 (length params))
    (is-equal 7 (first params))))

(deftest test-activity-feed-sql-pagination
  (multiple-value-bind (sql params)
      (cauldron.crucible:activity-feed-sql 1 :limit 10 :offset 5)
    (declare (ignore params))
    (is (search "LIMIT 10" sql))
    (is (search "OFFSET 5" sql))))

;;; ============================================================
;;; Activity Tracking Plug
;;; ============================================================

(deftest test-plug-activity-tracking-post
  (let ((plug (cauldron.crucible:make-plug-activity-tracking)))
    (let* ((conn (cauldron.crucible:make-conn :method :post :path "/api/contacts" :status 201))
           (result (funcall plug conn)))
      (is-equal "created" (cauldron.crucible:conn-get-assign result :activity-action))
      (is (cauldron.crucible:conn-get-assign result :activity-recorded)))))

(deftest test-plug-activity-tracking-put
  (let ((plug (cauldron.crucible:make-plug-activity-tracking)))
    (let* ((conn (cauldron.crucible:make-conn :method :put :path "/api/contacts/1" :status 200))
           (result (funcall plug conn)))
      (is-equal "updated" (cauldron.crucible:conn-get-assign result :activity-action)))))

(deftest test-plug-activity-tracking-delete
  (let ((plug (cauldron.crucible:make-plug-activity-tracking)))
    (let* ((conn (cauldron.crucible:make-conn :method :delete :path "/api/contacts/1" :status 200))
           (result (funcall plug conn)))
      (is-equal "deleted" (cauldron.crucible:conn-get-assign result :activity-action)))))

(deftest test-plug-activity-tracking-get-no-action
  (let ((plug (cauldron.crucible:make-plug-activity-tracking)))
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/api/contacts"))
           (result (funcall plug conn)))
      (is-nil (cauldron.crucible:conn-get-assign result :activity-action)))))

(deftest test-plug-activity-tracking-error-no-action
  (let ((plug (cauldron.crucible:make-plug-activity-tracking)))
    (let* ((conn (cauldron.crucible:make-conn :method :post :path "/api/contacts" :status 422))
           (result (funcall plug conn)))
      (is-nil (cauldron.crucible:conn-get-assign result :activity-action) "No action on error status"))))

(deftest test-plug-activity-tracking-custom-actor
  (let ((plug (cauldron.crucible:make-plug-activity-tracking :actor-type "agent")))
    (let* ((conn (cauldron.crucible:make-conn :method :post :path "/api/test" :status 201))
           (result (funcall plug conn)))
      (is-equal "agent" (cauldron.crucible:conn-get-assign result :activity-actor-type)))))

;;; ============================================================
;;; Ether Topic
;;; ============================================================

(deftest test-activity-ether-topic
  (is-equal "activity.42" (cauldron.crucible:activity-ether-topic 42))
  (is-equal "activity.1" (cauldron.crucible:activity-ether-topic 1)))
