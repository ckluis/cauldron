;;;; src/ether/pubsub.lisp — In-process thread-safe PubSub
(in-package :cauldron.ether)

;;; -------------------------------------------------------
;;; PubSub System
;;;
;;; Thread-safe in-process pub/sub using SBCL mutexes.
;;; Each pubsub instance has a topic registry (hash-table)
;;; where each topic maps to a list of subscriptions.
;;; -------------------------------------------------------

(defstruct subscription
  "A single subscription to a topic."
  (id nil :type string)
  (topic nil :type string)
  (callback nil :type function))

(defstruct (pubsub (:constructor %make-pubsub))
  "An in-process pub/sub message broker."
  (topic-table (make-hash-table :test 'equal) :type hash-table)
  (lock (sb-thread:make-mutex :name "cauldron-pubsub-lock"))
  (id-counter 0 :type integer))

(defun make-pubsub ()
  "Create a new pubsub instance with an empty topic registry."
  (%make-pubsub))

(defvar *pubsub* (make-pubsub)
  "Default global pubsub instance.")

(defun generate-subscription-id (pubsub)
  "Generate a unique subscription ID. Must be called under lock."
  (format nil "sub-~D" (incf (pubsub-id-counter pubsub))))

(defun subscribe (pubsub topic callback &key id)
  "Subscribe CALLBACK to TOPIC on PUBSUB. Returns the subscription ID.

CALLBACK is a function of one argument (the message).
ID is an optional subscription ID; one is generated if not provided.
Thread-safe."
  (sb-thread:with-mutex ((pubsub-lock pubsub))
    (let* ((sub-id (or id (generate-subscription-id pubsub)))
           (sub (make-subscription :id sub-id :topic topic :callback callback))
           (subs (gethash topic (pubsub-topic-table pubsub))))
      (setf (gethash topic (pubsub-topic-table pubsub))
            (append subs (list sub)))
      sub-id)))

(defun unsubscribe (pubsub subscription-id)
  "Remove the subscription with SUBSCRIPTION-ID from PUBSUB.
If the last subscriber on a topic is removed, the topic is cleaned up.
Thread-safe. Returns T if a subscription was removed, NIL otherwise."
  (sb-thread:with-mutex ((pubsub-lock pubsub))
    (let ((found nil)
          (to-remove '()))
      ;; First pass: find and update (collect topics to remove)
      (maphash (lambda (topic subs)
                 (let ((remaining (remove subscription-id subs
                                         :key #'subscription-id
                                         :test #'string=)))
                   (when (/= (length remaining) (length subs))
                     (setf found t)
                     (if remaining
                         (setf (gethash topic (pubsub-topic-table pubsub)) remaining)
                         ;; Mark for removal (can't remhash during maphash)
                         (push topic to-remove)))))
               (pubsub-topic-table pubsub))
      ;; Second pass: remove empty topics
      (dolist (topic to-remove)
        (remhash topic (pubsub-topic-table pubsub)))
      found)))

(defun broadcast (pubsub topic message)
  "Broadcast MESSAGE to all subscribers of TOPIC on PUBSUB.
Each subscriber callback is wrapped in handler-case for error isolation:
a failing subscriber does not prevent delivery to others.
Thread-safe. Returns the number of subscribers notified."
  ;; Snapshot subscribers under lock, then deliver outside lock
  ;; to avoid holding the lock during potentially long callbacks.
  (let ((subs (sb-thread:with-mutex ((pubsub-lock pubsub))
                (copy-list (gethash topic (pubsub-topic-table pubsub))))))
    (let ((count 0))
      (dolist (sub subs count)
        (handler-case
            (progn
              (funcall (subscription-callback sub) message)
              (incf count))
          (error (e)
            (format *error-output*
                    "~&[cauldron.ether] Subscriber ~A on topic ~S error: ~A~%"
                    (subscription-id sub) topic e)))))))

(defun pubsub-topics (pubsub)
  "Return a list of active topic name strings on PUBSUB. Thread-safe."
  (sb-thread:with-mutex ((pubsub-lock pubsub))
    (let ((topics '()))
      (maphash (lambda (topic subs)
                 (declare (ignore subs))
                 (push topic topics))
               (pubsub-topic-table pubsub))
      (nreverse topics))))

(defun pubsub-subscriber-count (pubsub topic)
  "Return the number of subscribers for TOPIC on PUBSUB. Thread-safe."
  (sb-thread:with-mutex ((pubsub-lock pubsub))
    (length (gethash topic (pubsub-topic-table pubsub)))))
