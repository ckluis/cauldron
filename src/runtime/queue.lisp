;;;; src/runtime/queue.lisp — Thread-safe concurrent queue via sb-concurrency mailbox
(in-package :cauldron.runtime)

;;; Uses sb-concurrency:mailbox which provides a lock-free, thread-safe FIFO
;;; queue with blocking receive support — exactly what we need for task dispatch.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-concurrency))

(defstruct (concurrent-queue (:constructor %make-queue))
  "A thread-safe concurrent queue backed by sb-concurrency:mailbox."
  (mailbox (sb-concurrency:make-mailbox) :type sb-concurrency:mailbox))

(defun make-queue (&optional name)
  "Create a new thread-safe queue. NAME is an optional identifier."
  (%make-queue :mailbox (sb-concurrency:make-mailbox :name (or name "cauldron-queue"))))

(defun enqueue (queue item)
  "Add ITEM to the back of QUEUE. Thread-safe and non-blocking."
  (sb-concurrency:send-message (concurrent-queue-mailbox queue) item)
  item)

(defun dequeue (queue &key timeout)
  "Remove and return the front item of QUEUE.
If TIMEOUT (in seconds) is given, returns (VALUES NIL NIL) on timeout.
Without TIMEOUT, blocks indefinitely until an item is available.
Returns (VALUES item T) on success."
  (if timeout
      (let ((result (sb-concurrency:receive-message
                     (concurrent-queue-mailbox queue)
                     :timeout timeout)))
        (if result
            (values result t)
            (values nil nil)))
      (values (sb-concurrency:receive-message
               (concurrent-queue-mailbox queue))
              t)))

(defun queue-empty-p (queue)
  "Return T if QUEUE currently has no items."
  (sb-concurrency:mailbox-empty-p (concurrent-queue-mailbox queue)))

(defun queue-length (queue)
  "Return the number of items currently in QUEUE."
  (sb-concurrency:mailbox-count (concurrent-queue-mailbox queue)))
