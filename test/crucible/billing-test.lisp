;;;; test/crucible/billing-test.lisp — Phase 41: Stripe billing tests
(in-package :cauldron.test)

(defsuite :billing)

;;; ============================================================
;;; DDL Strings
;;; ============================================================

(deftest test-billing-plans-ddl-exists
  (is (stringp cauldron.crucible:*billing-plans-ddl*) "DDL is a string")
  (is (search "billing_plans" cauldron.crucible:*billing-plans-ddl*)
      "Contains table name")
  (is (search "external_id" cauldron.crucible:*billing-plans-ddl*)
      "Contains external_id column")
  (is (search "amount_cents" cauldron.crucible:*billing-plans-ddl*)
      "Contains amount_cents column")
  (is (search "active_p" cauldron.crucible:*billing-plans-ddl*)
      "Contains active_p column"))

(deftest test-subscriptions-ddl-exists
  (is (stringp cauldron.crucible:*subscriptions-ddl*) "DDL is a string")
  (is (search "subscriptions" cauldron.crucible:*subscriptions-ddl*)
      "Contains table name")
  (is (search "company_id" cauldron.crucible:*subscriptions-ddl*)
      "Contains company_id column")
  (is (search "status" cauldron.crucible:*subscriptions-ddl*)
      "Contains status column")
  (is (search "cancel_at" cauldron.crucible:*subscriptions-ddl*)
      "Contains cancel_at column"))

(deftest test-invoices-ddl-exists
  (is (stringp cauldron.crucible:*invoices-ddl*) "DDL is a string")
  (is (search "invoices" cauldron.crucible:*invoices-ddl*)
      "Contains table name")
  (is (search "pdf_url" cauldron.crucible:*invoices-ddl*)
      "Contains pdf_url column")
  (is (search "paid_at" cauldron.crucible:*invoices-ddl*)
      "Contains paid_at column"))

;;; ============================================================
;;; Plan SQL
;;; ============================================================

(deftest test-create-plan-sql
  (multiple-value-bind (sql params)
      (cauldron.crucible:create-plan-sql "Pro" "price_abc123" 2999 "usd" "month")
    (is (search "INSERT INTO billing_plans" sql) "INSERT statement")
    (is (search "RETURNING" sql) "Has RETURNING clause")
    (is-equal 6 (length params))
    (is-equal "Pro" (first params))
    (is-equal "price_abc123" (second params))
    (is-equal 2999 (third params))
    (is-equal "usd" (fourth params))
    (is-equal "month" (fifth params))
    (is-equal "[]" (sixth params))))

(deftest test-create-plan-sql-with-features
  (multiple-value-bind (sql params)
      (cauldron.crucible:create-plan-sql "Enterprise" "price_xyz" 9999 "usd" "year"
                                          :features '("unlimited_users" "priority_support"))
    (is (search "INSERT" sql))
    (is-equal 6 (length params))
    ;; Features should be JSON-encoded
    (let ((features-json (sixth params)))
      (is (stringp features-json) "Features param is a string")
      (is (search "unlimited_users" features-json) "Contains feature"))))

(deftest test-list-plans-sql-all
  (multiple-value-bind (sql params)
      (cauldron.crucible:list-plans-sql)
    (is (search "SELECT" sql) "SELECT statement")
    (is (search "billing_plans" sql) "From billing_plans")
    (is (search "ORDER BY amount_cents" sql) "Ordered by amount")
    (is-nil params "No params for list all")))

(deftest test-list-plans-sql-active-only
  (multiple-value-bind (sql params)
      (cauldron.crucible:list-plans-sql :active-only t)
    (is (search "SELECT" sql) "SELECT statement")
    (is (search "active_p = TRUE" sql) "Filters active only")
    (is-nil params)))

(deftest test-list-plans-sql-includes-all-when-not-active-only
  (multiple-value-bind (sql params)
      (cauldron.crucible:list-plans-sql :active-only nil)
    (declare (ignore params))
    (is (not (search "active_p = TRUE" sql))
        "No active filter when active-only is nil")))

(deftest test-find-plan-sql
  (multiple-value-bind (sql params)
      (cauldron.crucible:find-plan-sql 42)
    (is (search "SELECT" sql) "SELECT statement")
    (is (search "WHERE id = $1" sql) "Filters by ID")
    (is-equal 1 (length params))
    (is-equal 42 (first params))))

;;; ============================================================
;;; Subscription SQL
;;; ============================================================

(deftest test-create-subscription-sql-default-status
  (multiple-value-bind (sql params)
      (cauldron.crucible:create-subscription-sql 1 10 "sub_abc123")
    (is (search "INSERT INTO subscriptions" sql) "INSERT statement")
    (is (search "RETURNING" sql) "Has RETURNING clause")
    (is-equal 5 (length params))
    (is-equal 1 (first params))
    (is-equal 10 (second params))
    (is-equal "sub_abc123" (third params))
    (is-equal "trialing" (fourth params) "Default status is trialing")))

(deftest test-create-subscription-sql-with-status
  (multiple-value-bind (sql params)
      (cauldron.crucible:create-subscription-sql 1 10 "sub_abc"
                                                  :status "active")
    (is (search "INSERT" sql))
    (is-equal "active" (fourth params))))

(deftest test-create-subscription-sql-with-trial-end
  (multiple-value-bind (sql params)
      (cauldron.crucible:create-subscription-sql 1 10 "sub_xyz"
                                                  :trial-end "2026-04-01")
    (is-equal "2026-04-01" (fifth params))))

(deftest test-update-subscription-status-sql
  (multiple-value-bind (sql params)
      (cauldron.crucible:update-subscription-status-sql 5 "active")
    (is (search "UPDATE subscriptions" sql) "UPDATE statement")
    (is (search "status = $1" sql) "Sets status")
    (is-equal 2 (length params))
    (is-equal "active" (first params))
    (is-equal 5 (second params))))

(deftest test-update-subscription-status-past-due
  (multiple-value-bind (sql params)
      (cauldron.crucible:update-subscription-status-sql 7 "past_due")
    (is (search "UPDATE" sql))
    (is-equal "past_due" (first params))))

(deftest test-cancel-subscription-sql-immediate
  (multiple-value-bind (sql params)
      (cauldron.crucible:cancel-subscription-sql 5)
    (is (search "UPDATE subscriptions" sql) "UPDATE statement")
    (is (search "canceled" sql) "Sets status to canceled")
    (is (search "cancel_at = NOW()" sql) "Sets cancel_at to NOW")
    (is-equal 1 (length params))
    (is-equal 5 (first params))))

(deftest test-cancel-subscription-sql-scheduled
  (multiple-value-bind (sql params)
      (cauldron.crucible:cancel-subscription-sql 5 :cancel-at "2026-04-15")
    (is (search "UPDATE subscriptions" sql))
    (is (search "canceled" sql))
    (is (search "cancel_at = $1" sql) "Uses param for cancel_at")
    (is-equal 2 (length params))
    (is-equal "2026-04-15" (first params))
    (is-equal 5 (second params))))

(deftest test-reactivate-subscription-sql
  (multiple-value-bind (sql params)
      (cauldron.crucible:reactivate-subscription-sql 5)
    (is (search "UPDATE subscriptions" sql) "UPDATE statement")
    (is (search "status = 'active'" sql) "Sets status to active")
    (is (search "cancel_at = NULL" sql) "Clears cancel_at")
    (is-equal 1 (length params))
    (is-equal 5 (first params))))

(deftest test-find-subscription-sql
  (multiple-value-bind (sql params)
      (cauldron.crucible:find-subscription-sql 3)
    (is (search "SELECT" sql) "SELECT statement")
    (is (search "company_id = $1" sql) "Filters by company")
    (is (search "ORDER BY created_at DESC" sql) "Orders by newest")
    (is (search "LIMIT 1" sql) "Returns only most recent")
    (is-equal 1 (length params))
    (is-equal 3 (first params))))

;;; ============================================================
;;; Subscription State Transitions
;;; ============================================================

(deftest test-subscription-transition-trialing-to-active
  (multiple-value-bind (sql params)
      (cauldron.crucible:update-subscription-status-sql 1 "active")
    (is (search "UPDATE" sql))
    (is-equal "active" (first params))))

(deftest test-subscription-transition-active-to-past-due
  (multiple-value-bind (sql params)
      (cauldron.crucible:update-subscription-status-sql 1 "past_due")
    (is-equal "past_due" (first params))))

(deftest test-subscription-transition-active-to-canceled
  (multiple-value-bind (sql params)
      (cauldron.crucible:cancel-subscription-sql 1)
    (is (search "canceled" sql))
    (is-equal 1 (first params))))

(deftest test-subscription-transition-canceled-to-active
  (multiple-value-bind (sql params)
      (cauldron.crucible:reactivate-subscription-sql 1)
    (is (search "'active'" sql))
    (is (search "cancel_at = NULL" sql))
    (is-equal 1 (first params))))

(deftest test-subscription-transition-to-incomplete
  (multiple-value-bind (sql params)
      (cauldron.crucible:update-subscription-status-sql 1 "incomplete")
    (is-equal "incomplete" (first params))))

;;; ============================================================
;;; Invoice SQL
;;; ============================================================

(deftest test-create-invoice-sql
  (multiple-value-bind (sql params)
      (cauldron.crucible:create-invoice-sql 1 10 "inv_abc" 2999 "paid")
    (is (search "INSERT INTO invoices" sql) "INSERT statement")
    (is (search "RETURNING" sql) "Has RETURNING clause")
    (is-equal 7 (length params))
    (is-equal 1 (first params))
    (is-equal 10 (second params))
    (is-equal "inv_abc" (third params))
    (is-equal 2999 (fourth params))
    (is-equal "paid" (fifth params))
    (is-nil (sixth params) "paid_at nil by default")
    (is-nil (seventh params) "pdf_url nil by default")))

(deftest test-create-invoice-sql-with-optional-fields
  (multiple-value-bind (sql params)
      (cauldron.crucible:create-invoice-sql 1 10 "inv_xyz" 4999 "paid"
                                             :paid-at "2026-03-15"
                                             :pdf-url "https://stripe.com/inv.pdf")
    (is (search "INSERT" sql))
    (is-equal "2026-03-15" (sixth params))
    (is-equal "https://stripe.com/inv.pdf" (seventh params))))

(deftest test-list-invoices-sql-no-limit
  (multiple-value-bind (sql params)
      (cauldron.crucible:list-invoices-sql 3)
    (is (search "SELECT" sql) "SELECT statement")
    (is (search "company_id = $1" sql) "Filters by company")
    (is (search "ORDER BY created_at DESC" sql) "Orders by newest")
    (is (not (search "LIMIT" sql)) "No LIMIT without limit param")
    (is-equal 1 (length params))
    (is-equal 3 (first params))))

(deftest test-list-invoices-sql-with-limit
  (multiple-value-bind (sql params)
      (cauldron.crucible:list-invoices-sql 3 :limit 10)
    (is (search "SELECT" sql))
    (is (search "LIMIT 10" sql) "Has LIMIT clause")
    (is-equal 1 (length params))
    (is-equal 3 (first params))))

(deftest test-list-invoices-sql-with-different-limit
  (multiple-value-bind (sql params)
      (cauldron.crucible:list-invoices-sql 5 :limit 25)
    (is (search "LIMIT 25" sql))
    (is-equal 5 (first params))))

;;; ============================================================
;;; make-plug-require-subscription — with active subscription
;;; ============================================================

(deftest test-plug-require-subscription-active
  (let ((plug (cauldron.crucible:make-plug-require-subscription)))
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/api/resource"))
           (conn (cauldron.crucible:conn-put-assign conn :subscription
                   (list (cons "status" "active")
                         (cons "id" 1))))
           (result (funcall plug conn)))
      (is (not (cauldron.crucible:conn-halted-p result)) "Not halted with active subscription")
      (is-equal t (cauldron.crucible:conn-get-assign result :subscription-active)
                "Subscription-active assign set"))))

(deftest test-plug-require-subscription-trialing
  (let ((plug (cauldron.crucible:make-plug-require-subscription)))
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/api/resource"))
           (conn (cauldron.crucible:conn-put-assign conn :subscription
                   (list (cons "status" "trialing"))))
           (result (funcall plug conn)))
      (is (not (cauldron.crucible:conn-halted-p result)) "Not halted during trial"))))

(deftest test-plug-require-subscription-no-subscription
  (let ((plug (cauldron.crucible:make-plug-require-subscription)))
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/api/resource"))
           (result (funcall plug conn)))
      (is-equal 402 (cauldron.crucible:conn-status result) "Returns 402")
      (is (cauldron.crucible:conn-halted-p result) "Halted without subscription"))))

(deftest test-plug-require-subscription-canceled
  (let ((plug (cauldron.crucible:make-plug-require-subscription)))
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/api/resource"))
           (conn (cauldron.crucible:conn-put-assign conn :subscription
                   (list (cons "status" "canceled"))))
           (result (funcall plug conn)))
      (is-equal 402 (cauldron.crucible:conn-status result) "Returns 402 for canceled")
      (is (cauldron.crucible:conn-halted-p result) "Halted for canceled"))))

(deftest test-plug-require-subscription-past-due
  (let ((plug (cauldron.crucible:make-plug-require-subscription)))
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/api/resource"))
           (conn (cauldron.crucible:conn-put-assign conn :subscription
                   (list (cons "status" "past_due"))))
           (result (funcall plug conn)))
      (is-equal 402 (cauldron.crucible:conn-status result) "Returns 402 for past_due")
      (is (cauldron.crucible:conn-halted-p result) "Halted for past_due"))))

;;; ============================================================
;;; make-plug-metered-usage — counter incrementing
;;; ============================================================

(deftest test-plug-metered-usage-first-call
  (let ((plug (cauldron.crucible:make-plug-metered-usage "api_calls")))
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/api/resource"))
           (result (funcall plug conn))
           (counters (cauldron.crucible:conn-get-assign result :usage-counters)))
      (is-not-nil counters "Counters set")
      (is-equal 1 (cdr (assoc "api_calls" counters :test #'string=))
                "First call is 1"))))

(deftest test-plug-metered-usage-increments
  (let ((plug (cauldron.crucible:make-plug-metered-usage "api_calls")))
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/api/resource"))
           (conn (funcall plug conn))
           (conn (funcall plug conn))
           (result (funcall plug conn))
           (counters (cauldron.crucible:conn-get-assign result :usage-counters)))
      (is-equal 3 (cdr (assoc "api_calls" counters :test #'string=))
                "Three calls counted"))))

(deftest test-plug-metered-usage-separate-counters
  (let ((api-plug (cauldron.crucible:make-plug-metered-usage "api_calls"))
        (search-plug (cauldron.crucible:make-plug-metered-usage "search_queries")))
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/api/search"))
           (conn (funcall api-plug conn))
           (conn (funcall api-plug conn))
           (result (funcall search-plug conn))
           (counters (cauldron.crucible:conn-get-assign result :usage-counters)))
      (is-equal 2 (cdr (assoc "api_calls" counters :test #'string=))
                "API calls counted separately")
      (is-equal 1 (cdr (assoc "search_queries" counters :test #'string=))
                "Search queries counted separately"))))

(deftest test-plug-metered-usage-does-not-halt
  (let ((plug (cauldron.crucible:make-plug-metered-usage "api_calls")))
    (let* ((conn (cauldron.crucible:make-conn :method :get :path "/api/resource"))
           (result (funcall plug conn)))
      (is (not (cauldron.crucible:conn-halted-p result))
          "Metered usage does not halt"))))
