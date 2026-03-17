;;;; test/runtime/threads-test.lisp — Threading primitives tests
(in-package :cauldron.test)

(defsuite :runtime-threads)

;;; --- spawn + join-thread ---

(deftest test-threads-spawn-and-join
  (let ((thread (cauldron.runtime:spawn "test-worker"
                  (lambda () (+ 1 2)))))
    (is-not-nil thread "spawn returns a thread")
    (let ((result (cauldron.runtime:join-thread thread)))
      (is-equal 3 result "join returns thread's return value"))))

;;; --- current-thread ---

(deftest test-threads-current-thread
  (let ((ct (cauldron.runtime:current-thread)))
    (is-not-nil ct "current-thread returns a value")
    (is-true (cauldron.runtime:thread-alive-p ct) "current thread is alive")))

;;; --- thread-alive-p ---

(deftest test-threads-alive-p-lifecycle
  (let* ((flag nil)
         (thread (cauldron.runtime:spawn "alive-test"
                   (lambda ()
                     (loop while (not flag) do (sleep 0.01))
                     :done))))
    (is-true (cauldron.runtime:thread-alive-p thread) "alive while running")
    (setf flag t)
    (cauldron.runtime:join-thread thread)
    (is-false (cauldron.runtime:thread-alive-p thread) "dead after join")))

;;; --- thread-name ---

(deftest test-threads-name
  (let ((thread (cauldron.runtime:spawn "my-thread-name"
                  (lambda () 42))))
    (is-equal "my-thread-name" (cauldron.runtime:thread-name thread))
    (cauldron.runtime:join-thread thread)))

;;; --- make-lock + with-lock ---

(deftest test-threads-lock-basic
  (let ((lock (cauldron.runtime:make-lock "test-lock"))
        (counter 0))
    (is-not-nil lock "make-lock returns a lock")
    (cauldron.runtime:with-lock (lock)
      (incf counter))
    (is-equal 1 counter "body executes under lock")))

(deftest test-threads-lock-mutual-exclusion
  "Two threads incrementing a shared counter with lock protection."
  (let ((lock (cauldron.runtime:make-lock "mutex-test"))
        (counter 0)
        (iterations 1000))
    (let ((t1 (cauldron.runtime:spawn "inc-1"
                (lambda ()
                  (dotimes (i iterations)
                    (cauldron.runtime:with-lock (lock)
                      (incf counter))))))
          (t2 (cauldron.runtime:spawn "inc-2"
                (lambda ()
                  (dotimes (i iterations)
                    (cauldron.runtime:with-lock (lock)
                      (incf counter)))))))
      (cauldron.runtime:join-thread t1)
      (cauldron.runtime:join-thread t2)
      (is-equal (* 2 iterations) counter "no lost updates with lock"))))

;;; --- semaphore ---

(deftest test-threads-semaphore-basic
  (let ((sem (cauldron.runtime:make-semaphore :count 0 :name "test-sem")))
    (is-not-nil sem "make-semaphore returns a semaphore")
    ;; Signal then wait should succeed
    (cauldron.runtime:signal-semaphore sem)
    (let ((result (cauldron.runtime:wait-semaphore sem :timeout 1)))
      (is-not-nil result "wait returns non-NIL after signal"))))

(deftest test-threads-semaphore-timeout
  (let ((sem (cauldron.runtime:make-semaphore :count 0)))
    (let ((result (cauldron.runtime:wait-semaphore sem :timeout 0.01)))
      (is-nil result "wait returns NIL on timeout"))))

(deftest test-threads-semaphore-initial-count
  (let ((sem (cauldron.runtime:make-semaphore :count 2)))
    ;; Should be able to wait twice without blocking
    (is-not-nil (cauldron.runtime:wait-semaphore sem :timeout 0.1) "first wait ok")
    (is-not-nil (cauldron.runtime:wait-semaphore sem :timeout 0.1) "second wait ok")
    ;; Third should timeout
    (is-nil (cauldron.runtime:wait-semaphore sem :timeout 0.01) "third wait times out")))

;;; --- spawn error handling ---

(deftest test-threads-spawn-error-captured
  "Errors in spawned threads are captured, not propagated."
  (let ((thread (cauldron.runtime:spawn "error-thread"
                  (lambda () (error "deliberate error")))))
    ;; join-thread should return the values from the handler-case
    (multiple-value-bind (val err) (cauldron.runtime:join-thread thread)
      (is-nil val "first value is NIL on error")
      (is-not-nil err "second value is the condition"))))
