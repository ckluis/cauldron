;;;; src/ether/pg-pubsub.lisp — PostgreSQL LISTEN/NOTIFY PubSub bridge
(in-package :cauldron.ether)

;;; -------------------------------------------------------
;;; PG PubSub Bridge
;;;
;;; Bridges PostgreSQL LISTEN/NOTIFY to the in-process pubsub.
;;; A dedicated background thread holds a PG connection, issues
;;; LISTEN commands, and polls for notifications. When a
;;; notification arrives, it broadcasts to the in-process pubsub.
;;;
;;; Depends on cauldron.db for pg-connection, listen, read-notification.
;;; -------------------------------------------------------

(defstruct pg-pubsub
  "A PostgreSQL LISTEN/NOTIFY → in-process pubsub bridge."
  (connection nil)               ; pg-connection from cauldron.db
  (channels nil :type list)      ; list of channel name strings
  (pubsub nil :type (or null pubsub))  ; target in-process pubsub
  (thread nil)                   ; background listener thread
  (running nil :type boolean)    ; control flag for the listener loop
  (poll-interval 0.1 :type float)) ; seconds between polls

(defun start-pg-pubsub (connection &key channels (pubsub *pubsub*)
                                        (poll-interval 0.1))
  "Start a PG LISTEN/NOTIFY bridge.

CONNECTION    — a cauldron.db:pg-connection (must be dedicated, not pooled)
CHANNELS      — list of channel name strings to LISTEN on
PUBSUB        — target in-process pubsub (defaults to *pubsub*)
POLL-INTERVAL — seconds between notification polls (default 0.1)

Returns a pg-pubsub struct. The background thread will:
  1. LISTEN on each channel
  2. Poll for notifications in a loop
  3. Broadcast received notifications to the in-process pubsub

The notification payload is broadcast as the message. The channel
name is used as the topic."
  (let ((pg-ps (make-pg-pubsub
                :connection connection
                :channels channels
                :pubsub pubsub
                :running t
                :poll-interval poll-interval)))
    ;; Issue LISTEN for each channel
    (dolist (channel channels)
      (cauldron.db:listen connection channel))
    ;; Start background polling thread
    (setf (pg-pubsub-thread pg-ps)
          (cauldron.runtime:spawn
           "cauldron-pg-pubsub-listener"
           (lambda ()
             (pg-pubsub-loop pg-ps))))
    pg-ps))

(defun pg-pubsub-loop (pg-ps)
  "Background loop that polls for PG notifications and broadcasts them.
Runs until (pg-pubsub-running pg-ps) is set to NIL."
  (loop while (pg-pubsub-running pg-ps)
        do (handler-case
               (let ((notification (cauldron.db:read-notification
                                    (pg-pubsub-connection pg-ps))))
                 (when notification
                   ;; notification is expected to be (channel payload)
                   ;; or a struct with channel and payload accessors
                   (let ((channel (if (consp notification)
                                      (first notification)
                                      notification))
                         (payload (if (consp notification)
                                      (second notification)
                                      nil)))
                     (broadcast (pg-pubsub-pubsub pg-ps) channel payload))))
             (error (e)
               (format *error-output*
                       "~&[cauldron.ether] PG pubsub listener error: ~A~%" e)
               ;; Brief pause on error to avoid tight error loops
               (sleep 1)))
           (sleep (pg-pubsub-poll-interval pg-ps))))

(defun stop-pg-pubsub (pg-ps)
  "Cleanly shut down the PG pubsub bridge.

1. Sets the running flag to NIL (loop will exit on next iteration)
2. Waits for the background thread to finish
3. UNLISTENs all channels on the connection

Returns T on successful shutdown."
  (setf (pg-pubsub-running pg-ps) nil)
  ;; Wait for the thread to complete
  (when (pg-pubsub-thread pg-ps)
    (handler-case
        (cauldron.runtime:join-thread (pg-pubsub-thread pg-ps))
      (error (e)
        (format *error-output*
                "~&[cauldron.ether] Error joining PG pubsub thread: ~A~%" e))))
  ;; UNLISTEN all channels
  (handler-case
      (dolist (channel (pg-pubsub-channels pg-ps))
        (cauldron.db:execute-sql (pg-pubsub-connection pg-ps)
                                 (format nil "UNLISTEN ~A" channel)))
    (error (e)
      (format *error-output*
              "~&[cauldron.ether] Error during UNLISTEN cleanup: ~A~%" e)))
  t)
