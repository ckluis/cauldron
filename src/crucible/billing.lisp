;;;; src/crucible/billing.lisp — Stripe billing: plans, subscriptions, invoices, metered usage
;;;; Framework-level billing support with subscription gating and API metering.
(in-package :cauldron.crucible)

;;; --- Billing Tables DDL ---
;;; Table definitions as DDL strings (crucible loads before grimoire in core subsystem)

(defvar *billing-plans-ddl*
  "CREATE TABLE IF NOT EXISTS billing_plans (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  external_id TEXT NOT NULL,
  amount_cents INTEGER NOT NULL,
  currency TEXT NOT NULL DEFAULT 'usd',
  interval TEXT NOT NULL DEFAULT 'month',
  features TEXT DEFAULT '[]',
  active_p BOOLEAN NOT NULL DEFAULT TRUE,
  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW()
);
CREATE UNIQUE INDEX IF NOT EXISTS idx_billing_plans_external_id ON billing_plans(external_id);"
  "DDL for the billing_plans table.")

(defvar *subscriptions-ddl*
  "CREATE TABLE IF NOT EXISTS subscriptions (
  id SERIAL PRIMARY KEY,
  company_id INTEGER NOT NULL,
  plan_id INTEGER NOT NULL REFERENCES billing_plans(id),
  external_id TEXT NOT NULL,
  status TEXT NOT NULL DEFAULT 'trialing',
  current_period_start TIMESTAMP,
  current_period_end TIMESTAMP,
  cancel_at TIMESTAMP,
  trial_end TIMESTAMP,
  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW()
);
CREATE INDEX IF NOT EXISTS idx_subscriptions_company_id ON subscriptions(company_id);
CREATE INDEX IF NOT EXISTS idx_subscriptions_external_id ON subscriptions(external_id);
CREATE INDEX IF NOT EXISTS idx_subscriptions_status ON subscriptions(status);"
  "DDL for the subscriptions table.")

(defvar *invoices-ddl*
  "CREATE TABLE IF NOT EXISTS invoices (
  id SERIAL PRIMARY KEY,
  company_id INTEGER NOT NULL,
  subscription_id INTEGER REFERENCES subscriptions(id),
  external_id TEXT NOT NULL,
  amount_cents INTEGER NOT NULL,
  status TEXT NOT NULL,
  paid_at TIMESTAMP,
  pdf_url TEXT,
  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW()
);
CREATE INDEX IF NOT EXISTS idx_invoices_company_id ON invoices(company_id);
CREATE INDEX IF NOT EXISTS idx_invoices_subscription_id ON invoices(subscription_id);
CREATE INDEX IF NOT EXISTS idx_invoices_external_id ON invoices(external_id);"
  "DDL for the invoices table.")

;;; --- Plan SQL ---

(defun create-plan-sql (name external-id amount-cents currency interval &key features)
  "Build SQL to insert a new billing plan.
Returns (values sql params)."
  (let ((features-json (if features
                            (cauldron.json:encode (coerce features 'vector))
                            "[]")))
    (values
     "INSERT INTO billing_plans (name, external_id, amount_cents, currency, interval, features, created_at, updated_at) VALUES ($1, $2, $3, $4, $5, $6, NOW(), NOW()) RETURNING id, name, external_id"
     (list name external-id amount-cents currency interval features-json))))

(defun list-plans-sql (&key active-only)
  "Build SQL to list billing plans.
When ACTIVE-ONLY is true, only returns plans where active_p is true.
Returns (values sql params)."
  (if active-only
      (values
       "SELECT id, name, external_id, amount_cents, currency, interval, features, active_p FROM billing_plans WHERE active_p = TRUE ORDER BY amount_cents ASC"
       nil)
      (values
       "SELECT id, name, external_id, amount_cents, currency, interval, features, active_p FROM billing_plans ORDER BY amount_cents ASC"
       nil)))

(defun find-plan-sql (plan-id)
  "Build SQL to find a billing plan by ID.
Returns (values sql params)."
  (values
   "SELECT id, name, external_id, amount_cents, currency, interval, features, active_p FROM billing_plans WHERE id = $1"
   (list plan-id)))

;;; --- Subscription SQL ---

(defun create-subscription-sql (company-id plan-id external-id &key status trial-end)
  "Build SQL to insert a new subscription.
Returns (values sql params)."
  (values
   "INSERT INTO subscriptions (company_id, plan_id, external_id, status, trial_end, created_at, updated_at) VALUES ($1, $2, $3, $4, $5, NOW(), NOW()) RETURNING id, company_id, status"
   (list company-id plan-id external-id (or status "trialing") trial-end)))

(defun update-subscription-status-sql (subscription-id new-status)
  "Build SQL to update a subscription's status.
Returns (values sql params)."
  (values
   "UPDATE subscriptions SET status = $1, updated_at = NOW() WHERE id = $2"
   (list new-status subscription-id)))

(defun cancel-subscription-sql (subscription-id &key cancel-at)
  "Build SQL to cancel a subscription. Sets status to canceled and optional cancel_at.
Returns (values sql params)."
  (if cancel-at
      (values
       "UPDATE subscriptions SET status = 'canceled', cancel_at = $1, updated_at = NOW() WHERE id = $2"
       (list cancel-at subscription-id))
      (values
       "UPDATE subscriptions SET status = 'canceled', cancel_at = NOW(), updated_at = NOW() WHERE id = $1"
       (list subscription-id))))

(defun reactivate-subscription-sql (subscription-id)
  "Build SQL to reactivate a canceled subscription. Clears cancel_at and sets status to active.
Returns (values sql params)."
  (values
   "UPDATE subscriptions SET status = 'active', cancel_at = NULL, updated_at = NOW() WHERE id = $1"
   (list subscription-id)))

(defun find-subscription-sql (company-id)
  "Build SQL to find the current subscription for a company.
Returns the most recent subscription.
Returns (values sql params)."
  (values
   "SELECT id, company_id, plan_id, external_id, status, current_period_start, current_period_end, cancel_at, trial_end FROM subscriptions WHERE company_id = $1 ORDER BY created_at DESC LIMIT 1"
   (list company-id)))

;;; --- Invoice SQL ---

(defun create-invoice-sql (company-id subscription-id external-id amount-cents status &key paid-at pdf-url)
  "Build SQL to insert a new invoice.
Returns (values sql params)."
  (values
   "INSERT INTO invoices (company_id, subscription_id, external_id, amount_cents, status, paid_at, pdf_url, created_at, updated_at) VALUES ($1, $2, $3, $4, $5, $6, $7, NOW(), NOW()) RETURNING id, external_id, status"
   (list company-id subscription-id external-id amount-cents status paid-at pdf-url)))

(defun list-invoices-sql (company-id &key limit)
  "Build SQL to list invoices for a company, newest first.
Returns (values sql params)."
  (if limit
      (values
       (format nil "SELECT id, company_id, subscription_id, external_id, amount_cents, status, paid_at, pdf_url, created_at FROM invoices WHERE company_id = $1 ORDER BY created_at DESC LIMIT ~D" limit)
       (list company-id))
      (values
       "SELECT id, company_id, subscription_id, external_id, amount_cents, status, paid_at, pdf_url, created_at FROM invoices WHERE company_id = $1 ORDER BY created_at DESC"
       (list company-id))))

;;; --- Billing Plugs ---

(defun make-plug-require-subscription ()
  "Create a plug that requires an active subscription.
Checks for :subscription assign on conn (set by application layer).
Halts with 402 Payment Required if no active subscription found.
Sets :subscription-active assign to T on success."
  (lambda (conn)
    (let ((subscription (conn-get-assign conn :subscription)))
      (if (and subscription
               (member (cdr (assoc "status" subscription :test #'string=))
                       '("active" "trialing")
                       :test #'string=))
          (progn
            (conn-put-assign conn :subscription-active t)
            conn)
          (progn
            (json-error conn "Active subscription required" :status 402
                        :hint "Subscribe to a plan to access this resource")
            (halt-conn conn :status 402))))))

(defun make-plug-metered-usage (counter-name)
  "Create a plug that increments a usage counter for API call tracking.
COUNTER-NAME identifies the metric being tracked (e.g. \"api_calls\").
Stores counters in :usage-counters assign as an alist of (name . count)."
  (lambda (conn)
    (let* ((counters (or (conn-get-assign conn :usage-counters) '()))
           (entry (assoc counter-name counters :test #'string=))
           (new-count (if entry (1+ (cdr entry)) 1)))
      (if entry
          (setf (cdr entry) new-count)
          (setf counters (acons counter-name new-count counters)))
      (conn-put-assign conn :usage-counters counters)
      conn)))
