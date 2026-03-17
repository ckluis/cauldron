;;;; test/runtime/queue-test.lisp — Thread-safe concurrent queue tests
(in-package :cauldron.test)

(defsuite :runtime-queue)

;;; --- make-queue ---

(deftest test-queue-make-fresh
  (let ((q (cauldron.runtime:make-queue)))
    (is-not-nil q "make-queue returns a queue")
    (is-true (cauldron.runtime:queue-empty-p q) "new queue is empty")
    (is-equal 0 (cauldron.runtime:queue-length q) "new queue has length 0")))

(deftest test-queue-make-named
  (let ((q (cauldron.runtime:make-queue "test-q")))
    (is-not-nil q "named queue created")))

;;; --- enqueue / dequeue ---

(deftest test-queue-enqueue-dequeue-single
  (let ((q (cauldron.runtime:make-queue)))
    (cauldron.runtime:enqueue q :item-a)
    (is-false (cauldron.runtime:queue-empty-p q) "not empty after enqueue")
    (multiple-value-bind (val found) (cauldron.runtime:dequeue q :timeout 1)
      (is-equal :item-a val "dequeued correct item")
      (is-true found "dequeue returns T as second value"))))

(deftest test-queue-enqueue-returns-item
  (let ((q (cauldron.runtime:make-queue)))
    (let ((result (cauldron.runtime:enqueue q 42)))
      (is-equal 42 result "enqueue returns the item"))))

(deftest test-queue-fifo-ordering
  (let ((q (cauldron.runtime:make-queue)))
    (cauldron.runtime:enqueue q :first)
    (cauldron.runtime:enqueue q :second)
    (cauldron.runtime:enqueue q :third)
    (is-equal 3 (cauldron.runtime:queue-length q) "length is 3")
    (multiple-value-bind (v1 _) (cauldron.runtime:dequeue q :timeout 1)
      (declare (ignore _))
      (is-equal :first v1 "first out"))
    (multiple-value-bind (v2 _) (cauldron.runtime:dequeue q :timeout 1)
      (declare (ignore _))
      (is-equal :second v2 "second out"))
    (multiple-value-bind (v3 _) (cauldron.runtime:dequeue q :timeout 1)
      (declare (ignore _))
      (is-equal :third v3 "third out"))))

;;; --- queue-empty-p ---

(deftest test-queue-empty-p-lifecycle
  (let ((q (cauldron.runtime:make-queue)))
    (is-true (cauldron.runtime:queue-empty-p q) "empty initially")
    (cauldron.runtime:enqueue q :x)
    (is-false (cauldron.runtime:queue-empty-p q) "not empty after enqueue")
    (cauldron.runtime:dequeue q :timeout 1)
    (is-true (cauldron.runtime:queue-empty-p q) "empty after dequeue")))

;;; --- queue-length ---

(deftest test-queue-length-tracks
  (let ((q (cauldron.runtime:make-queue)))
    (is-equal 0 (cauldron.runtime:queue-length q) "starts at 0")
    (cauldron.runtime:enqueue q :a)
    (is-equal 1 (cauldron.runtime:queue-length q) "1 after enqueue")
    (cauldron.runtime:enqueue q :b)
    (is-equal 2 (cauldron.runtime:queue-length q) "2 after second enqueue")
    (cauldron.runtime:dequeue q :timeout 1)
    (is-equal 1 (cauldron.runtime:queue-length q) "1 after dequeue")))

;;; --- dequeue with timeout ---

(deftest test-queue-dequeue-timeout
  (let ((q (cauldron.runtime:make-queue)))
    (multiple-value-bind (val found) (cauldron.runtime:dequeue q :timeout 0.01)
      (is-nil val "timeout returns NIL value")
      (is-nil found "timeout returns NIL as second value"))))

;;; --- multiple item types ---

(deftest test-queue-mixed-types
  (let ((q (cauldron.runtime:make-queue)))
    (cauldron.runtime:enqueue q "string")
    (cauldron.runtime:enqueue q 42)
    (cauldron.runtime:enqueue q '(:a :b))
    (multiple-value-bind (v _) (cauldron.runtime:dequeue q :timeout 1)
      (declare (ignore _))
      (is-equal "string" v))
    (multiple-value-bind (v _) (cauldron.runtime:dequeue q :timeout 1)
      (declare (ignore _))
      (is-equal 42 v))
    (multiple-value-bind (v _) (cauldron.runtime:dequeue q :timeout 1)
      (declare (ignore _))
      (is-equal '(:a :b) v))))
