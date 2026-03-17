;;;; src/scry/transport.lisp — WebSocket transport for LiveView
(in-package :cauldron.scry)

;;; -------------------------------------------------------
;;; Scry WebSocket Transport
;;;
;;; Wire format (JSON arrays):
;;;   Client→Server: ["event", event-name, payload-object]
;;;   Server→Client: ["patch", diff-array]
;;;                  ["redirect", url]
;;;
;;; Uses cauldron.json for encoding/decoding.
;;; Uses cauldron.ws for WebSocket send/receive.
;;; -------------------------------------------------------

;;; --- Wire format helpers ---

(defun encode-patch-for-wire (patch)
  "Convert a single patch to a JSON-serializable list.
Patch: (type path data...) → [type-string, path-array, data...]"
  (let ((type (first patch))
        (path (second patch))
        (rest (cddr patch)))
    (append (list (string-downcase (symbol-name type))
                  (or path '()))
            rest)))

(defun encode-patches (patches)
  "Convert a list of patches to the wire format JSON string.
Returns a JSON string: [\"patch\", [[type, path, ...], ...]]"
  (let ((wire-patches (mapcar #'encode-patch-for-wire patches)))
    (cauldron.json:encode (list "patch" wire-patches))))

(defun encode-redirect (url)
  "Encode a redirect command as JSON string."
  (cauldron.json:encode (list "redirect" url)))

(defun decode-client-message (json-string)
  "Decode a client WebSocket message from JSON.
Returns (values message-type event-name payload).

Expected format: [\"event\", \"event-name\", {payload}]"
  (let ((msg (cauldron.json:decode json-string)))
    (when (and (typep msg 'sequence) (>= (length msg) 2))
      (let ((msg-type (elt msg 0))
            (event-name (elt msg 1))
            (payload (if (>= (length msg) 3) (elt msg 2) nil)))
        (values msg-type event-name payload)))))

;;; --- Session-Socket mapping ---

(defvar *socket-sessions* (make-hash-table :test 'eq)
  "Maps WebSocket connection objects to session IDs.")

(defvar *socket-sessions-lock*
  (sb-thread:make-mutex :name "cauldron-scry-socket-sessions-lock"))

(defun register-socket-session (socket session-id)
  "Associate SOCKET with SESSION-ID."
  (sb-thread:with-mutex (*socket-sessions-lock*)
    (setf (gethash socket *socket-sessions*) session-id)))

(defun find-session-for-socket (socket)
  "Look up the session associated with SOCKET."
  (let ((session-id (sb-thread:with-mutex (*socket-sessions-lock*)
                      (gethash socket *socket-sessions*))))
    (when session-id
      (find-session session-id))))

(defun unregister-socket-session (socket)
  "Remove the socket-session association and clean up the session."
  (let ((session-id (sb-thread:with-mutex (*socket-sessions-lock*)
                      (prog1 (gethash socket *socket-sessions*)
                        (remhash socket *socket-sessions*)))))
    (when session-id
      (remove-session session-id))))

;;; --- WebSocket message handler ---

(defun scry-ws-handler (socket message)
  "Handle a WebSocket MESSAGE from SOCKET for a Scry LiveView session.

This is the main entry point called by the WebSocket layer when a
text message arrives on a Scry connection.

Flow:
  1. Look up session for socket
  2. Decode the client message
  3. Dispatch to the appropriate handler
  4. Encode response patches
  5. Send back over WebSocket

Returns T on success, NIL if session not found."
  (let ((session (find-session-for-socket socket)))
    (unless session
      (format *error-output*
              "~&[cauldron.scry] No session for socket, ignoring message~%")
      (return-from scry-ws-handler nil))
    (multiple-value-bind (msg-type event-name payload)
        (decode-client-message message)
      (cond
        ;; Event message from client
        ((equal msg-type "event")
         (handler-case
             (multiple-value-bind (patches html)
                 (scry-handle-event session event-name payload)
               (declare (ignore html))
               (when patches
                 (let ((response (encode-patches patches)))
                   (cauldron.ws:ws-send socket response)))
               t)
           (error (e)
             (format *error-output*
                     "~&[cauldron.scry] Error handling event ~S: ~A~%"
                     event-name e)
             nil)))

        ;; Heartbeat / ping — just acknowledge
        ((equal msg-type "heartbeat")
         (cauldron.ws:ws-send socket
                              (cauldron.json:encode (list "heartbeat" "ok")))
         t)

        ;; Unknown message type
        (t
         (format *error-output*
                 "~&[cauldron.scry] Unknown message type: ~S~%" msg-type)
         nil)))))

;;; --- Connection lifecycle ---

(defun scry-ws-connect (socket view-name params &key session)
  "Handle a new WebSocket connection for a Scry LiveView.

Called when the client upgrades to WebSocket for a LiveView page.
Creates the session, mounts the view, and sends initial state.

Returns the scry-session."
  (multiple-value-bind (scry-sess html)
      (scry-mount view-name params :session session :socket socket)
    ;; Register socket → session mapping
    (register-socket-session socket (scry-session-id scry-sess))
    ;; Send initial rendered HTML as a full replace patch
    (let ((initial-patch (cauldron.json:encode
                          (list "patch"
                                (list (list "replace" '() html))))))
      (cauldron.ws:ws-send socket initial-patch))
    scry-sess))

(defun scry-ws-disconnect (socket)
  "Handle WebSocket disconnect for a Scry LiveView.
Cleans up the session and any subscriptions."
  (let ((session (find-session-for-socket socket)))
    (when session
      ;; Unsubscribe from any pubsub subscriptions
      (dolist (sub-id (scry-session-subscriptions session))
        (handler-case
            (cauldron.ether:unsubscribe cauldron.ether:*pubsub* sub-id)
          (error () nil)))
      ;; Remove session
      (unregister-socket-session socket)))
  t)
