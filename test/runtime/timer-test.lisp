;;;; test/runtime/timer-test.lisp — Timer and scheduler tests
(in-package :cauldron.test)

(defsuite :runtime-timer)

;;; --- %now ---

(deftest test-timer-now-positive
  (let ((now (cauldron.runtime::%now)))
    (is (typep now 'double-float) "%now returns double-float")
    (is (> now 0.0d0) "%now is positive")))

(deftest test-timer-now-monotonic
  (let ((t1 (cauldron.runtime::%now))
        (t2 (cauldron.runtime::%now)))
    (is (>= t2 t1) "%now is monotonic")))

;;; --- %next-timer-id ---

(deftest test-timer-id-increments
  (let ((id1 (cauldron.runtime::%next-timer-id))
        (id2 (cauldron.runtime::%next-timer-id)))
    (is (> id2 id1) "timer IDs increment monotonically")))

;;; --- timer-entry struct ---

(deftest test-timer-entry-construction
  (let ((entry (cauldron.runtime::make-timer-entry
                :id 1
                :fire-time 100.0d0
                :interval nil
                :callback (lambda () :fired))))
    (is-equal 1 (cauldron.runtime::timer-entry-id entry))
    (is-equal 100.0d0 (cauldron.runtime::timer-entry-fire-time entry))
    (is-nil (cauldron.runtime::timer-entry-interval entry) "one-shot has nil interval")
    (is-not-nil (cauldron.runtime::timer-entry-callback entry))))

(deftest test-timer-entry-recurring
  (let ((entry (cauldron.runtime::make-timer-entry
                :id 2
                :fire-time 50.0d0
                :interval 10
                :callback (lambda () nil))))
    (is-equal 10 (cauldron.runtime::timer-entry-interval entry) "recurring has interval")))

;;; --- %scheduler-tick ---

(deftest test-timer-tick-fires-past-due-oneshot
  "One-shot entry with past fire-time fires and is removed."
  (let* ((fired nil)
         (sched (cauldron.runtime::%make-scheduler
                 :entries (list (cauldron.runtime::make-timer-entry
                                :id 1
                                :fire-time 0.0d0  ; far in the past
                                :interval nil
                                :callback (lambda () (setf fired t))))
                 :running-p t)))
    (cauldron.runtime::%scheduler-tick sched)
    (is-true fired "callback fired for past-due entry")
    (is-equal 0 (length (cauldron.runtime::scheduler-entries sched))
              "one-shot entry removed after firing")))

(deftest test-timer-tick-recurring-reschedules
  "Recurring entry fires and is rescheduled at now + interval."
  (let* ((fire-count 0)
         (sched (cauldron.runtime::%make-scheduler
                 :entries (list (cauldron.runtime::make-timer-entry
                                :id 1
                                :fire-time 0.0d0
                                :interval 5
                                :callback (lambda () (incf fire-count))))
                 :running-p t)))
    (cauldron.runtime::%scheduler-tick sched)
    (is-equal 1 fire-count "callback fired once")
    ;; Should still have one entry (rescheduled)
    (is-equal 1 (length (cauldron.runtime::scheduler-entries sched))
              "recurring entry rescheduled")
    ;; Rescheduled entry should have future fire-time
    (let ((entry (first (cauldron.runtime::scheduler-entries sched))))
      (is (> (cauldron.runtime::timer-entry-fire-time entry) (cauldron.runtime::%now))
          "rescheduled fire-time is in the future")
      (is-equal 5 (cauldron.runtime::timer-entry-interval entry)
                "interval preserved"))))

(deftest test-timer-tick-future-entry-not-fired
  "Entry with future fire-time is not fired."
  (let* ((fired nil)
         (future-time (+ (cauldron.runtime::%now) 9999.0d0))
         (sched (cauldron.runtime::%make-scheduler
                 :entries (list (cauldron.runtime::make-timer-entry
                                :id 1
                                :fire-time future-time
                                :interval nil
                                :callback (lambda () (setf fired t))))
                 :running-p t)))
    (cauldron.runtime::%scheduler-tick sched)
    (is-nil fired "future entry not fired")
    (is-equal 1 (length (cauldron.runtime::scheduler-entries sched))
              "future entry remains")))

(deftest test-timer-tick-error-in-callback
  "Error in callback is caught; doesn't crash scheduler."
  (let* ((second-fired nil)
         (sched (cauldron.runtime::%make-scheduler
                 :entries (list (cauldron.runtime::make-timer-entry
                                :id 1
                                :fire-time 0.0d0
                                :interval nil
                                :callback (lambda () (error "boom")))
                               (cauldron.runtime::make-timer-entry
                                :id 2
                                :fire-time 0.0d0
                                :interval nil
                                :callback (lambda () (setf second-fired t))))
                 :running-p t)))
    (cauldron.runtime::%scheduler-tick sched)
    (is-true second-fired "second callback still fires after first errors")))

(deftest test-timer-tick-mixed-entries
  "Mix of past-due and future entries: only past-due fire."
  (let* ((past-fired nil)
         (future-fired nil)
         (sched (cauldron.runtime::%make-scheduler
                 :entries (list (cauldron.runtime::make-timer-entry
                                :id 1
                                :fire-time 0.0d0
                                :interval nil
                                :callback (lambda () (setf past-fired t)))
                               (cauldron.runtime::make-timer-entry
                                :id 2
                                :fire-time (+ (cauldron.runtime::%now) 9999.0d0)
                                :interval nil
                                :callback (lambda () (setf future-fired t))))
                 :running-p t)))
    (cauldron.runtime::%scheduler-tick sched)
    (is-true past-fired "past entry fired")
    (is-nil future-fired "future entry not fired")
    ;; Only the future entry remains
    (is-equal 1 (length (cauldron.runtime::scheduler-entries sched)))))

(deftest test-timer-tick-nconc-ordering
  "Verify nconc ordering: remaining (unfired) entries come before rescheduled recurring entries."
  (let* ((sched (cauldron.runtime::%make-scheduler
                 :entries (list
                           ;; Future entry (should remain)
                           (cauldron.runtime::make-timer-entry
                            :id 1
                            :fire-time (+ (cauldron.runtime::%now) 9999.0d0)
                            :interval nil
                            :callback (lambda () nil))
                           ;; Past-due recurring (should fire and reschedule)
                           (cauldron.runtime::make-timer-entry
                            :id 2
                            :fire-time 0.0d0
                            :interval 10
                            :callback (lambda () nil)))
                 :running-p t)))
    (cauldron.runtime::%scheduler-tick sched)
    (let ((entries (cauldron.runtime::scheduler-entries sched)))
      (is-equal 2 (length entries) "both entries present after tick")
      ;; With nconc, remaining comes first, then to-reschedule
      (let ((first-id (cauldron.runtime::timer-entry-id (first entries)))
            (second-id (cauldron.runtime::timer-entry-id (second entries))))
        (is-equal 1 first-id "unfired entry is first (remaining)")
        (is-equal 2 second-id "rescheduled entry is second (to-reschedule)")))))

;;; --- cancel-timer ---

(deftest test-timer-cancel-existing
  (let ((sched (cauldron.runtime::%make-scheduler
                :entries (list (cauldron.runtime::make-timer-entry
                               :id 42
                               :fire-time (+ (cauldron.runtime::%now) 9999.0d0)
                               :interval nil
                               :callback (lambda () nil)))
                :running-p t)))
    (is-true (cauldron.runtime:cancel-timer sched 42) "cancel returns T for existing")
    (is-equal 0 (length (cauldron.runtime::scheduler-entries sched)) "entry removed")))

(deftest test-timer-cancel-missing
  (let ((sched (cauldron.runtime::%make-scheduler :entries nil :running-p t)))
    (is-false (cauldron.runtime:cancel-timer sched 999) "cancel returns NIL for missing")))

;;; --- schedule-once / schedule-recurring ---

(deftest test-timer-schedule-once-adds-entry
  (let ((sched (cauldron.runtime::%make-scheduler :entries nil :running-p t)))
    (let ((id (cauldron.runtime:schedule-once sched 60 (lambda () nil))))
      (is (integerp id) "returns an integer ID")
      (is-equal 1 (length (cauldron.runtime::scheduler-entries sched)) "entry added")
      (let ((entry (first (cauldron.runtime::scheduler-entries sched))))
        (is-nil (cauldron.runtime::timer-entry-interval entry) "one-shot has nil interval")))))

(deftest test-timer-schedule-recurring-adds-entry
  (let ((sched (cauldron.runtime::%make-scheduler :entries nil :running-p t)))
    (let ((id (cauldron.runtime:schedule-recurring sched 5 (lambda () nil))))
      (is (integerp id) "returns an integer ID")
      (let ((entry (first (cauldron.runtime::scheduler-entries sched))))
        (is-equal 5 (cauldron.runtime::timer-entry-interval entry)
                  "recurring has correct interval")))))
