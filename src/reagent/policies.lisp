;;;; src/reagent/policies.lisp — Authorization policies for resources
(in-package :cauldron.reagent)

;;; -------------------------------------------------------
;;; Policy System
;;;
;;; Policies are rules that determine whether an actor can
;;; perform an action on a resource. Rules are evaluated in
;;; order; the first matching rule wins (allow or deny).
;;; -------------------------------------------------------

(defstruct policy-rule
  "A single authorization policy rule."
  (disposition :allow :type keyword)   ; :allow or :deny
  (action nil :type symbol)            ; action name, or :all for wildcard
  (condition nil)                       ; :always, or a function (actor resource) → boolean
  (resource nil :type (or null symbol)))

(defun make-policy (disposition action condition &optional resource)
  "Create a policy-rule. CONDITION can be :always, :never, or a function symbol/function."
  (make-policy-rule :disposition disposition
                    :action action
                    :condition condition
                    :resource resource))

(defun allow (action condition &optional resource)
  "Create an ALLOW policy rule for ACTION with CONDITION.
CONDITION is :always, :when followed by a predicate, or a function."
  (make-policy :allow action condition resource))

(defun deny (action condition &optional resource)
  "Create a DENY policy rule for ACTION with CONDITION."
  (make-policy :deny action condition resource))

;;; -------------------------------------------------------
;;; Policy Evaluation
;;; -------------------------------------------------------

(defun policy-matches-action-p (rule action)
  "Return T if the policy RULE applies to ACTION."
  (let ((rule-action (policy-rule-action rule)))
    (or (eq rule-action :all)
        (eq rule-action action))))

(defun evaluate-condition (condition actor resource)
  "Evaluate a policy CONDITION in the context of ACTOR and RESOURCE.
Returns T if the condition is satisfied."
  (cond
    ((eq condition :always) t)
    ((eq condition :never) nil)
    ((functionp condition) (funcall condition actor resource))
    ((and (symbolp condition) (fboundp condition))
     (funcall condition actor resource))
    (t nil)))

(defun check-policy (policies action actor &optional resource)
  "Evaluate POLICIES (list of policy-rule structs or plists) for ACTION by ACTOR.
Returns (values allowed-p matched-rule-or-nil).

Policies are checked in order. The first matching rule wins.
If no rule matches, access is denied by default."
  (dolist (rule policies (values nil nil))
    (let ((parsed-rule (if (policy-rule-p rule)
                           rule
                           ;; Handle plist-format policies from defresource
                           (make-policy-rule
                            :disposition (getf rule :disposition)
                            :action (getf rule :action)
                            :condition (getf rule :condition)))))
      (when (policy-matches-action-p parsed-rule action)
        (when (evaluate-condition (policy-rule-condition parsed-rule) actor resource)
          (return (values (eq (policy-rule-disposition parsed-rule) :allow)
                          parsed-rule)))))))

(defun check-resource-policy (resource-name action actor &optional resource)
  "Check the policies defined on resource RESOURCE-NAME for ACTION by ACTOR.
Returns (values allowed-p matched-rule-or-nil)."
  (let ((class (find-resource-class resource-name)))
    (if class
        (check-policy (resource-policies class) action actor resource)
        (values nil nil))))

;;; -------------------------------------------------------
;;; Convenience predicates for use in policy conditions
;;; -------------------------------------------------------

(defun authenticated-p (actor &optional resource)
  "Return T if ACTOR is non-nil (i.e., someone is logged in).
A common predicate for :when clauses."
  (declare (ignore resource))
  (not (null actor)))

(defun role-is (required-role)
  "Return a predicate function that checks if the actor has REQUIRED-ROLE.
Expects the actor to be a plist with a :role key."
  (lambda (actor &optional resource)
    (declare (ignore resource))
    (and actor
         (eq (getf actor :role) required-role))))

(defun owner-p (actor resource)
  "Return T if ACTOR owns RESOURCE.
Expects both to be plists with an :id key, and resource to have a :user-id key."
  (and actor resource
       (let ((actor-id (if (listp actor) (getf actor :id) nil))
             (resource-owner (if (listp resource) (getf resource :user-id) nil)))
         (and actor-id resource-owner
              (equal actor-id resource-owner)))))
