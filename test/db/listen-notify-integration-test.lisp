;;;; test/db/listen-notify-integration-test.lisp — LISTEN/NOTIFY integration tests

(in-package :cauldron.test)

(defsuite :db-listen-notify-integration)

(suite-setup :db-listen-notify-integration
  (lambda () (ensure-test-database)))

;;; --- Tests ---

(deftest test-listen-notify-with-payload
  (with-test-connection (listener)
    (with-test-connection (notifier)
      (cauldron.db:listen listener "test_channel")
      ;; Notify from a separate connection
      (let ((notifier-pid (cauldron.db::pg-connection-backend-pid notifier)))
        (cauldron.db:notify notifier "test_channel" "hello-from-test")
        ;; Read notification on the listener
        (multiple-value-bind (channel payload pid)
            (cauldron.db:read-notification listener :timeout 5)
          (is-equal "test_channel" channel "Channel should match")
          (is-equal "hello-from-test" payload "Payload should match")
          (is-equal notifier-pid pid "PID should match notifier's backend PID"))))))

(deftest test-listen-notify-without-payload
  (with-test-connection (listener)
    (with-test-connection (notifier)
      (cauldron.db:listen listener "test_no_payload")
      (cauldron.db:notify notifier "test_no_payload")
      (multiple-value-bind (channel payload pid)
          (cauldron.db:read-notification listener :timeout 5)
        (declare (ignore pid))
        (is-equal "test_no_payload" channel)
        (is-equal "" payload "Payload should be empty string without payload")))))

(deftest test-notify-pid-matches-sender
  (with-test-connection (listener)
    (with-test-connection (notifier)
      (let ((expected-pid (cauldron.db::pg-connection-backend-pid notifier)))
        (cauldron.db:listen listener "test_pid_check")
        (cauldron.db:notify notifier "test_pid_check" "pid-test")
        (multiple-value-bind (channel payload pid)
            (cauldron.db:read-notification listener :timeout 5)
          (declare (ignore channel payload))
          (is-equal expected-pid pid "Notification PID should match sender's backend PID"))))))
