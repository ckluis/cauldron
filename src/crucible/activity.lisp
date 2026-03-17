;;;; src/crucible/activity.lisp — Activity feed framework
;;;; Records system-wide activity events with actor/target tracking.
(in-package :cauldron.crucible)

;;; --- Activity Events DDL ---
;;; (In crucible module which loads before grimoire — use raw DDL)

(defvar *activity-events-ddl*
  "CREATE TABLE IF NOT EXISTS activity_events (
  id SERIAL PRIMARY KEY,
  actor_type TEXT NOT NULL DEFAULT 'user',
  actor_id INTEGER,
  action TEXT NOT NULL,
  target_type TEXT NOT NULL,
  target_id INTEGER,
  context TEXT DEFAULT '{}',
  company_id INTEGER,
  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW()
);
CREATE INDEX IF NOT EXISTS idx_activity_events_company_id ON activity_events(company_id);
CREATE INDEX IF NOT EXISTS idx_activity_events_actor ON activity_events(actor_type, actor_id);
CREATE INDEX IF NOT EXISTS idx_activity_events_target ON activity_events(target_type, target_id);
CREATE INDEX IF NOT EXISTS idx_activity_events_created_at ON activity_events(created_at);"
  "DDL for the activity_events table.")

;;; --- Activity Recording SQL ---

(defun record-activity-sql (action target-type target-id
                             &key (actor-type "user") actor-id company-id context)
  "Build SQL to insert an activity event.
ACTION is a string like 'created', 'updated', 'deleted', 'commented', 'linked', 'uploaded'.
Returns (values sql params)."
  (let ((context-json (if context
                          (if (stringp context)
                              context
                              (cauldron.json:encode context))
                          "{}")))
    (values
     "INSERT INTO activity_events (actor_type, actor_id, action, target_type, target_id, context, company_id, created_at, updated_at) VALUES ($1, $2, $3, $4, $5, $6, $7, NOW(), NOW()) RETURNING id"
     (list actor-type actor-id action target-type target-id context-json company-id))))

;;; --- Activity Query SQL ---

(defun list-activities-sql (&key company-id actor-type actor-id target-type action
                                 (limit 50) (offset 0))
  "Build SQL to list activities with optional filters.
Returns (values sql params)."
  (let ((conditions nil)
        (params nil)
        (param-idx 0))
    ;; Build WHERE clauses
    (when company-id
      (incf param-idx)
      (push (format nil "company_id = $~D" param-idx) conditions)
      (push company-id params))
    (when actor-type
      (incf param-idx)
      (push (format nil "actor_type = $~D" param-idx) conditions)
      (push actor-type params))
    (when actor-id
      (incf param-idx)
      (push (format nil "actor_id = $~D" param-idx) conditions)
      (push actor-id params))
    (when target-type
      (incf param-idx)
      (push (format nil "target_type = $~D" param-idx) conditions)
      (push target-type params))
    (when action
      (incf param-idx)
      (push (format nil "action = $~D" param-idx) conditions)
      (push action params))
    (let ((where (if conditions
                     (format nil " WHERE ~{~A~^ AND ~}" (nreverse conditions))
                     "")))
      (values
       (format nil "SELECT id, actor_type, actor_id, action, target_type, target_id, context, company_id, created_at FROM activity_events~A ORDER BY created_at DESC LIMIT ~D OFFSET ~D"
               where limit offset)
       (nreverse params)))))

(defun count-activities-sql (&key company-id target-type)
  "Build SQL to count activities with optional filters.
Returns (values sql params)."
  (let ((conditions nil)
        (params nil)
        (param-idx 0))
    (when company-id
      (incf param-idx)
      (push (format nil "company_id = $~D" param-idx) conditions)
      (push company-id params))
    (when target-type
      (incf param-idx)
      (push (format nil "target_type = $~D" param-idx) conditions)
      (push target-type params))
    (let ((where (if conditions
                     (format nil " WHERE ~{~A~^ AND ~}" (nreverse conditions))
                     "")))
      (values
       (format nil "SELECT COUNT(*) FROM activity_events~A" where)
       (nreverse params)))))

;;; --- Activity Feed Query (rich) ---

(defun activity-feed-sql (company-id &key (limit 50) (offset 0))
  "Build SQL for a rich activity feed with all fields.
Returns (values sql params)."
  (values
   (format nil "SELECT id, actor_type, actor_id, action, target_type, target_id, context, created_at FROM activity_events WHERE company_id = $1 ORDER BY created_at DESC LIMIT ~D OFFSET ~D"
           limit offset)
   (list company-id)))

;;; --- Activity Tracking Plug ---

(defun make-plug-activity-tracking (&key (actor-type "user"))
  "Create a plug that auto-records CRUD activities by inspecting responses.
Checks conn method and status to determine action type.
Sets :activity-recorded assign to T on the conn."
  (lambda (conn)
    (let* ((method (conn-method conn))
           (status (conn-status conn))
           (action (cond
                     ((and (eq method :post) (< status 400)) "created")
                     ((and (member method '(:put :patch)) (< status 400)) "updated")
                     ((and (eq method :delete) (< status 400)) "deleted")
                     (t nil))))
      (when action
        (conn-put-assign conn :activity-action action)
        (conn-put-assign conn :activity-actor-type actor-type)
        (conn-put-assign conn :activity-recorded t)))
    conn))

;;; --- Ether PubSub Integration ---

(defun activity-ether-topic (company-id)
  "Return the Ether PubSub topic for activity events in a company."
  (format nil "activity.~A" company-id))
