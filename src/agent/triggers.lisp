;;;; src/agent/triggers.lisp — Event trigger wiring for agents
(in-package :cauldron.agent)

;;; --- Trigger Wiring ---

(defun %format-trigger-input (trigger-type source message)
  "Format a trigger event into a user message for the agent."
  (format nil "[~A trigger from ~A]~%~A"
          (string-upcase (symbol-name trigger-type))
          source
          (if (stringp message) message (format nil "~S" message))))

(defun wire-pubsub-trigger (agent-name topic &key (pubsub cauldron.ether:*pubsub*))
  "Subscribe to a PubSub topic that triggers the agent.
Returns the subscription ID."
  (cauldron.ether:subscribe pubsub topic
    (lambda (message)
      (let ((spec (find-agent agent-name)))
        (when (and spec (agent-spec-enabled-p spec))
          (handler-case
              (let ((input (%format-trigger-input :pubsub topic message)))
                (declare (ignore input))
                ;; In full implementation, would call invoke-agent
                ;; with a DB connection from the pool
                (format *error-output*
                        "~&[cauldron.agent] PubSub trigger ~A for agent ~A~%"
                        topic (string-downcase (symbol-name agent-name))))
            (error (e)
              (format *error-output*
                      "~&[cauldron.agent] Trigger error for ~A: ~A~%"
                      (string-downcase (symbol-name agent-name)) e))))))))

(defun wire-schedule-trigger (agent-name interval-seconds task-description
                               &key (scheduler nil))
  "Wire a recurring schedule trigger for the agent.
Returns the timer ID, or NIL if no scheduler provided."
  (when scheduler
    (cauldron.runtime:schedule-recurring scheduler interval-seconds
      (lambda ()
        (let ((spec (find-agent agent-name)))
          (when (and spec (agent-spec-enabled-p spec))
            (handler-case
                (format *error-output*
                        "~&[cauldron.agent] Schedule trigger for ~A: ~A~%"
                        (string-downcase (symbol-name agent-name))
                        task-description)
              (error (e)
                (format *error-output*
                        "~&[cauldron.agent] Trigger error for ~A: ~A~%"
                        (string-downcase (symbol-name agent-name)) e)))))))))

(defun agent-http-handler (agent-name)
  "Create an HTTP handler function for an agent.
Returns a function suitable for use as a Crucible route handler."
  (lambda (conn)
    (let ((spec (find-agent agent-name)))
      (cond
        ((null spec)
         (cauldron.crucible:json-error conn "Agent not found" :status 404))
        ((not (agent-spec-enabled-p spec))
         (cauldron.crucible:json-error conn "Agent is disabled" :status 503))
        (t
         (let* ((raw-body (cauldron.crucible:conn-body conn))
                (input (cond
                         ((and raw-body (hash-table-p raw-body))
                          (or (gethash "message" raw-body)
                              (gethash "input" raw-body)
                              (format nil "~S" raw-body)))
                         ((stringp raw-body) raw-body)
                         (t ""))))
           ;; In full implementation, would call invoke-agent with conn
           (cauldron.crucible:json-response conn
             (cauldron.runtime:ht
              "agent" (string-downcase (symbol-name agent-name))
              "status" "received"
              "message" (format nil "Agent ~A received your message"
                                (string-downcase (symbol-name agent-name)))
              "input_length" (length input)))))))))

(defun wire-webhook-trigger (agent-name integration-name event-type
                              &key (pubsub cauldron.ether:*pubsub*))
  "Subscribe to an integration webhook topic to trigger the agent.
The integration's defintegration webhook already broadcasts to Ether.
Returns the subscription ID."
  (let ((topic (format nil "~A.~A"
                       (string-downcase (symbol-name integration-name))
                       (if event-type
                           (string-downcase (symbol-name event-type))
                           "*"))))
    (wire-pubsub-trigger agent-name topic :pubsub pubsub)))

;;; --- SSE Streaming Handler ---

(defun agent-sse-handler (agent-name)
  "Create an HTTP handler that invokes an agent and streams the response via SSE.
Returns a function suitable for use as a Crucible route handler.
The response uses text/event-stream content type for Server-Sent Events."
  (lambda (conn)
    (let ((spec (find-agent agent-name)))
      (cond
        ((null spec)
         (cauldron.crucible:json-error conn "Agent not found" :status 404))
        ((not (agent-spec-enabled-p spec))
         (cauldron.crucible:json-error conn "Agent is disabled" :status 503))
        (t
         (let* ((raw-body (cauldron.crucible:conn-body conn))
                (input (cond
                         ((and raw-body (hash-table-p raw-body))
                          (or (gethash "message" raw-body)
                              (gethash "input" raw-body)
                              (format nil "~S" raw-body)))
                         ((stringp raw-body) raw-body)
                         (t "")))
                (name-str (string-downcase (symbol-name agent-name))))
           ;; Build SSE response body
           ;; In a full streaming implementation, this would use chunked transfer
           ;; For now, assemble SSE events as a complete body
           (let ((events (list
                          (format nil "event: start~%data: {\"agent\":\"~A\",\"status\":\"processing\"}~%~%"
                                  name-str)
                          (format nil "event: message~%data: {\"agent\":\"~A\",\"content\":\"Agent ~A received: ~A\"}~%~%"
                                  name-str name-str
                                  (cauldron.alembic:escape-html
                                   (if (> (length input) 200)
                                       (subseq input 0 200)
                                       input)))
                          (format nil "event: done~%data: {\"agent\":\"~A\",\"status\":\"complete\"}~%~%"
                                  name-str))))
             (cauldron.crucible:conn-put-resp-body
              (cauldron.crucible:conn-put-resp-header
               (cauldron.crucible:conn-put-resp-header
                (cauldron.crucible:conn-put-status conn 200)
                "Content-Type" "text/event-stream")
               "Cache-Control" "no-cache")
              (format nil "~{~A~}" events)))))))))

;;; --- Wire All Triggers ---

(defun wire-agent-triggers (agent-name &key scheduler (pubsub cauldron.ether:*pubsub*))
  "Wire all triggers defined in the agent spec.
Returns a list of (trigger-type . id-or-handler) pairs."
  (let ((spec (find-agent agent-name))
        (wired nil))
    (unless spec
      (error "Agent ~S not found" agent-name))
    (dolist (trigger (agent-spec-trigger-specs spec))
      (let ((type (first trigger)))
        (case type
          (:pubsub
           (let ((topic (second trigger)))
             (let ((sub-id (wire-pubsub-trigger agent-name topic :pubsub pubsub)))
               (push (cons :pubsub sub-id) wired))))
          (:schedule
           (let ((interval (second trigger))
                 (task (getf (cddr trigger) :task)))
             (let ((timer-id (wire-schedule-trigger agent-name interval
                                                     (or task "scheduled-task")
                                                     :scheduler scheduler)))
               (when timer-id
                 (push (cons :schedule timer-id) wired)))))
          (:http
           (let ((handler (agent-http-handler agent-name)))
             (push (cons :http handler) wired)))
          (:webhook
           (let ((integration (second trigger))
                 (event (third trigger)))
             (let ((sub-id (wire-webhook-trigger agent-name integration event
                                                  :pubsub pubsub)))
               (push (cons :webhook sub-id) wired))))
          (otherwise
           (error "Unknown trigger type: ~S" type)))))
    (nreverse wired)))
