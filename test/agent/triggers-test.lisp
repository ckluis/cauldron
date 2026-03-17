;;;; test/agent/triggers-test.lisp — Trigger wiring verification
(in-package :cauldron.test)

(defsuite :agent-triggers)

;;; --- Format Trigger Input ---

(deftest test-format-trigger-input-pubsub
  (let ((input (cauldron.agent::%format-trigger-input :pubsub "events.new" "Hello")))
    (is (search "PUBSUB" input) "Contains trigger type")
    (is (search "events.new" input) "Contains source")
    (is (search "Hello" input) "Contains message")))

(deftest test-format-trigger-input-schedule
  (let ((input (cauldron.agent::%format-trigger-input :schedule "timer" "Check status")))
    (is (search "SCHEDULE" input))
    (is (search "Check status" input))))

(deftest test-format-trigger-input-non-string
  (let ((input (cauldron.agent::%format-trigger-input :webhook "stripe" '(:type "payment"))))
    (is (search "WEBHOOK" input))
    (is (stringp input) "Result is always a string")))

;;; --- PubSub Trigger Wiring ---

(deftest test-wire-pubsub-trigger
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq))
        (pubsub (cauldron.ether:make-pubsub)))
    (cauldron.agent:defagent pubsub-test-agent
      (:description "PubSub test"))
    (let ((sub-id (cauldron.agent::wire-pubsub-trigger
                   'pubsub-test-agent "test.topic" :pubsub pubsub)))
      (is (stringp sub-id) "Returns subscription ID")
      (is-equal 1 (cauldron.ether:pubsub-subscriber-count pubsub "test.topic")
                "Subscriber registered"))))

(deftest test-wire-pubsub-trigger-fires-on-broadcast
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq))
        (pubsub (cauldron.ether:make-pubsub)))
    (cauldron.agent:defagent fire-test-agent
      (:description "Fire test"))
    (cauldron.agent::wire-pubsub-trigger 'fire-test-agent "fire.test" :pubsub pubsub)
    ;; Broadcasting should not error
    (let ((count (cauldron.ether:broadcast pubsub "fire.test" "hello")))
      (is-equal 1 count "One subscriber notified"))))

(deftest test-wire-pubsub-disabled-agent-ignored
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq))
        (pubsub (cauldron.ether:make-pubsub)))
    (cauldron.agent:defagent disabled-trigger-agent
      (:description "Disabled test")
      (:enabled nil))
    (cauldron.agent::wire-pubsub-trigger 'disabled-trigger-agent "skip.test" :pubsub pubsub)
    ;; Should not error even though agent is disabled
    (let ((count (cauldron.ether:broadcast pubsub "skip.test" "ignored")))
      (is-equal 1 count "Subscriber called (checks enabled internally)"))))

;;; --- Schedule Trigger Wiring ---

(deftest test-wire-schedule-trigger-no-scheduler
  (let ((result (cauldron.agent::wire-schedule-trigger
                 'some-agent 60 "check" :scheduler nil)))
    (is-nil result "Returns NIL without scheduler")))

(deftest test-wire-schedule-trigger-with-scheduler
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq))
        (sched (cauldron.runtime:make-scheduler)))
    (cauldron.agent:defagent sched-test-agent
      (:description "Schedule test"))
    (unwind-protect
        (let ((timer-id (cauldron.agent::wire-schedule-trigger
                         'sched-test-agent 3600 "hourly-check"
                         :scheduler sched)))
          (is (integerp timer-id) "Returns timer ID")
          ;; Can cancel the timer
          (is (cauldron.runtime:cancel-timer sched timer-id) "Timer cancellable"))
      (cauldron.runtime:shutdown-scheduler sched))))

;;; --- HTTP Handler ---

(deftest test-agent-http-handler-returns-function
  (let ((handler (cauldron.agent:agent-http-handler 'test-agent)))
    (is (functionp handler) "Returns a function")))

;;; --- Webhook Trigger ---

(deftest test-wire-webhook-trigger
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq))
        (pubsub (cauldron.ether:make-pubsub)))
    (cauldron.agent:defagent webhook-test-agent
      (:description "Webhook test"))
    (let ((sub-id (cauldron.agent::wire-webhook-trigger
                   'webhook-test-agent 'stripe :payment-failed
                   :pubsub pubsub)))
      (is (stringp sub-id) "Returns subscription ID")
      ;; Topic should be "stripe.payment-failed"
      (is-equal 1 (cauldron.ether:pubsub-subscriber-count
                   pubsub "stripe.payment-failed")))))

;;; --- Wire All Triggers ---

(deftest test-wire-agent-triggers-pubsub-and-http
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq))
        (pubsub (cauldron.ether:make-pubsub)))
    (cauldron.agent:defagent multi-trigger-agent
      (:description "Multi trigger")
      (:triggers
        (:pubsub "events.new")
        (:http :post "/agents/multi")))
    (let ((wired (cauldron.agent:wire-agent-triggers
                  'multi-trigger-agent :pubsub pubsub)))
      (is-equal 2 (length wired))
      ;; First should be pubsub
      (is-equal :pubsub (car (first wired)))
      (is (stringp (cdr (first wired))) "PubSub returns sub ID")
      ;; Second should be http
      (is-equal :http (car (second wired)))
      (is (functionp (cdr (second wired))) "HTTP returns handler"))))

(deftest test-wire-agent-triggers-not-found
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq)))
    (signals-condition error
      (cauldron.agent:wire-agent-triggers 'nonexistent)
      "Missing agent signals error")))

(deftest test-wire-agent-triggers-with-schedule
  (let ((cauldron.agent:*agent-registry* (make-hash-table :test 'eq))
        (pubsub (cauldron.ether:make-pubsub))
        (sched (cauldron.runtime:make-scheduler)))
    (cauldron.agent:defagent schedule-trigger-agent
      (:description "Schedule trigger")
      (:triggers
        (:schedule 3600 :task :hourly)))
    (unwind-protect
        (let ((wired (cauldron.agent:wire-agent-triggers
                      'schedule-trigger-agent :scheduler sched :pubsub pubsub)))
          (is-equal 1 (length wired))
          (is-equal :schedule (car (first wired)))
          (is (integerp (cdr (first wired))) "Schedule returns timer ID"))
      (cauldron.runtime:shutdown-scheduler sched))))
