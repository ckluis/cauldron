;;;; test/ether/pubsub-extended-test.lisp — Extended PubSub tests
(in-package :cauldron.test)

(defsuite :ether-pubsub-extended)

;;; --- Subscribe to same topic multiple times ---

(deftest test-pubsub-same-topic-three-subs
  "Three subscribers on the same topic all receive the broadcast."
  (let ((ps (cauldron.ether:make-pubsub))
        (received '()))
    (cauldron.ether:subscribe ps "chat" (lambda (m) (push (list :a m) received)))
    (cauldron.ether:subscribe ps "chat" (lambda (m) (push (list :b m) received)))
    (cauldron.ether:subscribe ps "chat" (lambda (m) (push (list :c m) received)))
    (is-equal 3 (cauldron.ether:pubsub-subscriber-count ps "chat"))
    (let ((count (cauldron.ether:broadcast ps "chat" "hello")))
      (is-equal 3 count)
      (is-equal 3 (length received)))))

(deftest test-pubsub-same-callback-twice
  "Same callback subscribed twice receives message twice."
  (let ((ps (cauldron.ether:make-pubsub))
        (call-count 0))
    (let ((cb (lambda (m) (declare (ignore m)) (incf call-count))))
      (cauldron.ether:subscribe ps "t" cb)
      (cauldron.ether:subscribe ps "t" cb))
    (cauldron.ether:broadcast ps "t" "ping")
    (is-equal 2 call-count)))

(deftest test-pubsub-subscribe-returns-unique-ids
  "Each subscribe call returns a distinct ID."
  (let ((ps (cauldron.ether:make-pubsub)))
    (let ((id1 (cauldron.ether:subscribe ps "t" (lambda (m) (declare (ignore m)))))
          (id2 (cauldron.ether:subscribe ps "t" (lambda (m) (declare (ignore m)))))
          (id3 (cauldron.ether:subscribe ps "t" (lambda (m) (declare (ignore m))))))
      (is (not (string= id1 id2)))
      (is (not (string= id2 id3)))
      (is (not (string= id1 id3))))))

;;; --- Broadcasting with no subscribers ---

(deftest test-broadcast-nonexistent-topic-returns-zero
  "Broadcast to a topic that was never subscribed returns 0."
  (let ((ps (cauldron.ether:make-pubsub)))
    (is-equal 0 (cauldron.ether:broadcast ps "ghost" "msg"))))

(deftest test-broadcast-after-all-unsubscribed
  "Broadcast after all subscribers removed returns 0."
  (let ((ps (cauldron.ether:make-pubsub)))
    (let ((id1 (cauldron.ether:subscribe ps "t" (lambda (m) (declare (ignore m)))))
          (id2 (cauldron.ether:subscribe ps "t" (lambda (m) (declare (ignore m))))))
      (cauldron.ether:unsubscribe ps id1)
      (cauldron.ether:unsubscribe ps id2)
      (is-equal 0 (cauldron.ether:broadcast ps "t" "echo")))))

(deftest test-broadcast-nil-message
  "Broadcasting nil as message delivers nil to subscribers."
  (let ((ps (cauldron.ether:make-pubsub))
        (received :sentinel))
    (cauldron.ether:subscribe ps "t" (lambda (m) (setf received m)))
    (cauldron.ether:broadcast ps "t" nil)
    (is-nil received)))

;;; --- Error isolation ---

(deftest test-error-isolation-first-subscriber-fails
  "First subscriber errors but second still receives."
  (let ((ps (cauldron.ether:make-pubsub))
        (received nil))
    (cauldron.ether:subscribe ps "t" (lambda (m) (declare (ignore m)) (error "fail-1")))
    (cauldron.ether:subscribe ps "t" (lambda (m) (setf received m)))
    (let ((count (cauldron.ether:broadcast ps "t" "data")))
      (is-equal 1 count "only one succeeded")
      (is-equal "data" received))))

(deftest test-error-isolation-middle-subscriber-fails
  "Middle subscriber errors but first and last still receive."
  (let ((ps (cauldron.ether:make-pubsub))
        (results '()))
    (cauldron.ether:subscribe ps "t" (lambda (m) (push (list :first m) results)))
    (cauldron.ether:subscribe ps "t" (lambda (m) (declare (ignore m)) (error "middle-boom")))
    (cauldron.ether:subscribe ps "t" (lambda (m) (push (list :last m) results)))
    (let ((count (cauldron.ether:broadcast ps "t" "test")))
      (is-equal 2 count)
      (is-equal 2 (length results)))))

(deftest test-error-isolation-all-fail
  "All subscribers fail; broadcast returns 0."
  (let ((ps (cauldron.ether:make-pubsub)))
    (cauldron.ether:subscribe ps "t" (lambda (m) (declare (ignore m)) (error "a")))
    (cauldron.ether:subscribe ps "t" (lambda (m) (declare (ignore m)) (error "b")))
    (let ((count (cauldron.ether:broadcast ps "t" "msg")))
      (is-equal 0 count))))

;;; --- Unsubscribe edge cases ---

(deftest test-unsubscribe-idempotent
  "Unsubscribing the same ID twice: first returns T, second returns NIL."
  (let ((ps (cauldron.ether:make-pubsub)))
    (let ((id (cauldron.ether:subscribe ps "t" (lambda (m) (declare (ignore m))))))
      (is-true (cauldron.ether:unsubscribe ps id))
      (is-false (cauldron.ether:unsubscribe ps id)))))

(deftest test-unsubscribe-one-of-many
  "Unsubscribing one of three leaves two active."
  (let ((ps (cauldron.ether:make-pubsub))
        (received '()))
    (let ((id1 (cauldron.ether:subscribe ps "t" (lambda (m) (push :a received)))))
      (cauldron.ether:subscribe ps "t" (lambda (m) (push :b received)))
      (cauldron.ether:subscribe ps "t" (lambda (m) (push :c received)))
      (cauldron.ether:unsubscribe ps id1)
      (is-equal 2 (cauldron.ether:pubsub-subscriber-count ps "t"))
      (cauldron.ether:broadcast ps "t" "x")
      (is-equal 2 (length received))
      ;; :a should NOT be in results
      (is-false (member :a received)))))

(deftest test-unsubscribe-with-custom-id
  "Custom ID can be used to unsubscribe."
  (let ((ps (cauldron.ether:make-pubsub)))
    (cauldron.ether:subscribe ps "t" (lambda (m) (declare (ignore m))) :id "my-sub")
    (is-equal 1 (cauldron.ether:pubsub-subscriber-count ps "t"))
    (is-true (cauldron.ether:unsubscribe ps "my-sub"))
    (is-equal 0 (cauldron.ether:pubsub-subscriber-count ps "t"))))

;;; --- Cross-topic isolation ---

(deftest test-topic-isolation-subscribe-and-broadcast
  "Subscribing to topic A and broadcasting to topic B leaves A subscriber untouched."
  (let ((ps (cauldron.ether:make-pubsub))
        (a-received nil)
        (b-received nil))
    (cauldron.ether:subscribe ps "topic-a" (lambda (m) (setf a-received m)))
    (cauldron.ether:subscribe ps "topic-b" (lambda (m) (setf b-received m)))
    (cauldron.ether:broadcast ps "topic-b" "only-b")
    (is-nil a-received)
    (is-equal "only-b" b-received)))

;;; --- pubsub-topics after mutations ---

(deftest test-pubsub-topics-after-partial-unsub
  "Topics list only includes topics with remaining subscribers."
  (let ((ps (cauldron.ether:make-pubsub)))
    (let ((id-a (cauldron.ether:subscribe ps "a" (lambda (m) (declare (ignore m))))))
      (cauldron.ether:subscribe ps "b" (lambda (m) (declare (ignore m))))
      ;; Remove all from topic a
      (cauldron.ether:unsubscribe ps id-a)
      (let ((topics (cauldron.ether:pubsub-topics ps)))
        (is-equal 1 (length topics))
        (is (member "b" topics :test #'string=))))))
