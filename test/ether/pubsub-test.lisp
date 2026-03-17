;;;; test/ether/pubsub-test.lisp — In-process PubSub tests
(in-package :cauldron.test)

(defsuite :ether-pubsub)

;;; --- make-pubsub ---

(deftest test-make-pubsub-fresh
  (let ((ps (cauldron.ether:make-pubsub)))
    (is-not-nil ps)
    (is-equal 0 (length (cauldron.ether:pubsub-topics ps)))))

;;; --- generate-subscription-id ---

(deftest test-generate-sub-id-increments
  (let ((ps (cauldron.ether:make-pubsub)))
    (let ((id1 (cauldron.ether::generate-subscription-id ps))
          (id2 (cauldron.ether::generate-subscription-id ps)))
      (is-equal "sub-1" id1)
      (is-equal "sub-2" id2))))

;;; --- subscribe ---

(deftest test-subscribe-returns-id
  (let ((ps (cauldron.ether:make-pubsub)))
    (let ((id (cauldron.ether:subscribe ps "topic-a" (lambda (msg) (declare (ignore msg))))))
      (is (stringp id))
      (is-equal 1 (cauldron.ether:pubsub-subscriber-count ps "topic-a")))))

(deftest test-subscribe-custom-id
  (let ((ps (cauldron.ether:make-pubsub)))
    (let ((id (cauldron.ether:subscribe ps "t" (lambda (m) (declare (ignore m))) :id "my-id")))
      (is-equal "my-id" id))))

(deftest test-subscribe-multiple-same-topic
  (let ((ps (cauldron.ether:make-pubsub)))
    (cauldron.ether:subscribe ps "t" (lambda (m) (declare (ignore m))))
    (cauldron.ether:subscribe ps "t" (lambda (m) (declare (ignore m))))
    (is-equal 2 (cauldron.ether:pubsub-subscriber-count ps "t"))))

(deftest test-subscribe-different-topics
  (let ((ps (cauldron.ether:make-pubsub)))
    (cauldron.ether:subscribe ps "a" (lambda (m) (declare (ignore m))))
    (cauldron.ether:subscribe ps "b" (lambda (m) (declare (ignore m))))
    (is-equal 2 (length (cauldron.ether:pubsub-topics ps)))))

;;; --- unsubscribe ---

(deftest test-unsubscribe-returns-t
  (let ((ps (cauldron.ether:make-pubsub)))
    (let ((id (cauldron.ether:subscribe ps "t" (lambda (m) (declare (ignore m))))))
      (is-true (cauldron.ether:unsubscribe ps id)))))

(deftest test-unsubscribe-missing-returns-nil
  (let ((ps (cauldron.ether:make-pubsub)))
    (is-false (cauldron.ether:unsubscribe ps "nonexistent"))))

(deftest test-unsubscribe-cleans-empty-topic
  (let ((ps (cauldron.ether:make-pubsub)))
    (let ((id (cauldron.ether:subscribe ps "t" (lambda (m) (declare (ignore m))))))
      (cauldron.ether:unsubscribe ps id)
      (is-equal 0 (length (cauldron.ether:pubsub-topics ps))))))

(deftest test-unsubscribe-partial
  "Unsubscribing one doesn't remove other subscribers."
  (let ((ps (cauldron.ether:make-pubsub)))
    (let ((id1 (cauldron.ether:subscribe ps "t" (lambda (m) (declare (ignore m))))))
      (cauldron.ether:subscribe ps "t" (lambda (m) (declare (ignore m))))
      (cauldron.ether:unsubscribe ps id1)
      (is-equal 1 (cauldron.ether:pubsub-subscriber-count ps "t")))))

;;; --- broadcast ---

(deftest test-broadcast-delivers-to-all
  (let ((ps (cauldron.ether:make-pubsub))
        (received '()))
    (cauldron.ether:subscribe ps "t" (lambda (m) (push (list :a m) received)))
    (cauldron.ether:subscribe ps "t" (lambda (m) (push (list :b m) received)))
    (let ((count (cauldron.ether:broadcast ps "t" "hello")))
      (is-equal 2 count)
      (is-equal 2 (length received)))))

(deftest test-broadcast-error-isolation
  "A failing subscriber doesn't block others."
  (let ((ps (cauldron.ether:make-pubsub))
        (received nil))
    (cauldron.ether:subscribe ps "t" (lambda (m) (declare (ignore m)) (error "boom")))
    (cauldron.ether:subscribe ps "t" (lambda (m) (setf received m)))
    (let ((count (cauldron.ether:broadcast ps "t" "test")))
      ;; One succeeded, one failed
      (is-equal 1 count)
      (is-equal "test" received))))

(deftest test-broadcast-no-subscribers
  (let ((ps (cauldron.ether:make-pubsub)))
    (is-equal 0 (cauldron.ether:broadcast ps "empty" "msg"))))

(deftest test-broadcast-different-topic
  "Broadcasting to topic A doesn't reach topic B subscribers."
  (let ((ps (cauldron.ether:make-pubsub))
        (received nil))
    (cauldron.ether:subscribe ps "a" (lambda (m) (setf received m)))
    (cauldron.ether:broadcast ps "b" "wrong")
    (is-nil received)))

;;; --- pubsub-topics ---

(deftest test-pubsub-topics-lists-active
  (let ((ps (cauldron.ether:make-pubsub)))
    (cauldron.ether:subscribe ps "x" (lambda (m) (declare (ignore m))))
    (cauldron.ether:subscribe ps "y" (lambda (m) (declare (ignore m))))
    (let ((topics (cauldron.ether:pubsub-topics ps)))
      (is-equal 2 (length topics))
      (is (member "x" topics :test #'string=))
      (is (member "y" topics :test #'string=)))))

(deftest test-pubsub-topics-empty-after-unsub
  (let ((ps (cauldron.ether:make-pubsub)))
    (let ((id (cauldron.ether:subscribe ps "t" (lambda (m) (declare (ignore m))))))
      (cauldron.ether:unsubscribe ps id)
      (is-equal 0 (length (cauldron.ether:pubsub-topics ps))))))

;;; --- pubsub-subscriber-count ---

(deftest test-pubsub-subscriber-count-zero
  (let ((ps (cauldron.ether:make-pubsub)))
    (is-equal 0 (cauldron.ether:pubsub-subscriber-count ps "nonexistent"))))

(deftest test-pubsub-subscriber-count-tracks
  (let ((ps (cauldron.ether:make-pubsub)))
    (cauldron.ether:subscribe ps "t" (lambda (m) (declare (ignore m))))
    (is-equal 1 (cauldron.ether:pubsub-subscriber-count ps "t"))
    (cauldron.ether:subscribe ps "t" (lambda (m) (declare (ignore m))))
    (is-equal 2 (cauldron.ether:pubsub-subscriber-count ps "t"))))
