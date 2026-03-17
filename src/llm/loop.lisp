;;;; src/llm/loop.lisp — Message format conversion + core agent loop
(in-package :cauldron.llm)

;;; --- Message Format Conversion ---

(defun messages-to-claude-format (messages)
  "Convert stored messages (list of hash-tables) to Claude API message format.
Returns a list of hash-tables ready for JSON encoding."
  (let ((result nil)
        (pending-tool-results nil))
    (dolist (msg messages)
      (let ((role (gethash "role" msg))
            (content (gethash "content" msg))
            (tool-calls-json (gethash "tool_calls" msg))
            (tool-call-id (gethash "tool_call_id" msg)))
        (cond
          ;; User message
          ((string= role "user")
           (when pending-tool-results
             (push (cauldron.runtime:ht "role" "user"
                        "content" (coerce (nreverse pending-tool-results) 'vector))
                   result)
             (setf pending-tool-results nil))
           (push (cauldron.runtime:ht "role" "user" "content" content) result))
          ;; Assistant message with tool calls
          ((and (string= role "assistant") tool-calls-json)
           (when pending-tool-results
             (push (cauldron.runtime:ht "role" "user"
                        "content" (coerce (nreverse pending-tool-results) 'vector))
                   result)
             (setf pending-tool-results nil))
           (let* ((tool-calls (if (stringp tool-calls-json)
                                  (cauldron.json:decode tool-calls-json)
                                  tool-calls-json))
                  (content-blocks nil))
             ;; Add text block if present
             (when (and content (> (length content) 0))
               (push (cauldron.runtime:ht "type" "text" "text" content) content-blocks))
             ;; Add tool_use blocks (tool-calls may be a list or vector)
             (let ((tc-list (if (vectorp tool-calls)
                                (coerce tool-calls 'list)
                                tool-calls)))
               (when (listp tc-list)
                 (dolist (tc tc-list)
                   (let ((tc-id (cauldron.runtime:ht-get tc "id"))
                         (tc-name (cauldron.runtime:ht-get tc "name"))
                         (tc-input (or (cauldron.runtime:ht-get tc "arguments")
                                       (cauldron.runtime:ht-get tc "input"))))
                     (push (cauldron.runtime:ht "type" "tool_use"
                                "id" (or tc-id "")
                                "name" (or tc-name "")
                                "input" (or tc-input (cauldron.runtime:ht)))
                           content-blocks)))))
             (push (cauldron.runtime:ht "role" "assistant"
                        "content" (coerce (nreverse content-blocks) 'vector))
                   result)))
          ;; Assistant text-only message
          ((string= role "assistant")
           (when pending-tool-results
             (push (cauldron.runtime:ht "role" "user"
                        "content" (coerce (nreverse pending-tool-results) 'vector))
                   result)
             (setf pending-tool-results nil))
           (push (cauldron.runtime:ht "role" "assistant" "content" content) result))
          ;; Tool result — accumulate for batching
          ((string= role "tool_result")
           (push (cauldron.runtime:ht "type" "tool_result"
                      "tool_use_id" (or tool-call-id "")
                      "content" (or content ""))
                 pending-tool-results)))))
    ;; Flush any remaining tool results
    (when pending-tool-results
      (push (cauldron.runtime:ht "role" "user"
                  "content" (coerce (nreverse pending-tool-results) 'vector))
            result))
    (nreverse result)))

;;; --- Core Agent Loop ---

(defun agent-turn (conn provider user-message &key conversation-id tools system-prompt (max-rounds 10))
  "Execute one agent turn: send user message, handle tool calls, return final text.
Returns (values response-string conversation-id)."
  ;; Create conversation if needed
  (unless conversation-id
    (let ((conv (create-conversation conn
                  :title (if (> (length user-message) 60)
                             (subseq user-message 0 60)
                             user-message)
                  :model (llm-provider-name provider))))
      (setf conversation-id (gethash "id" conv))))
  ;; Add user message
  (add-message conn conversation-id "user" user-message)
  ;; Build claude tools format
  (let ((claude-tools (when tools (tools-to-claude-format tools))))
    ;; Agent loop
    (dotimes (round max-rounds)
      (let* ((history (get-conversation-messages conn conversation-id))
             (messages (messages-to-claude-format history))
             (response (funcall (llm-provider-call-fn provider)
                                messages claude-tools system-prompt)))
        ;; Track tokens
        (let ((total (+ (llm-response-input-tokens response)
                        (llm-response-output-tokens response))))
          (when (> total 0)
            (update-conversation-tokens conn conversation-id total)))
        (cond
          ;; Tool calls: save assistant message, execute tools, continue
          ((llm-response-tool-calls response)
           (let ((tc-data (mapcar (lambda (tc)
                                    (list (cons "id" (tool-call-id tc))
                                          (cons "name" (tool-call-name tc))
                                          (cons "arguments"
                                                (when (tool-call-arguments tc)
                                                  (let ((ht (tool-call-arguments tc)))
                                                    (if (hash-table-p ht)
                                                        (let ((alist nil))
                                                          (maphash (lambda (k v) (push (cons k v) alist)) ht)
                                                          alist)
                                                        ht))))))
                                  (llm-response-tool-calls response))))
             ;; Save assistant message with tool calls
             (add-message conn conversation-id "assistant"
                          (or (llm-response-content response) "")
                          :tool-calls tc-data)
             ;; Execute each tool call
             (dolist (tc (llm-response-tool-calls response))
               (multiple-value-bind (result success-p)
                   (execute-tool conn (tool-call-name tc) (tool-call-arguments tc) tools)
                 (declare (ignore success-p))
                 (add-message conn conversation-id "tool_result" result
                              :tool-call-id (tool-call-id tc)
                              :tool-name (tool-call-name tc))))))
          ;; Text response: save and return
          (t
           (let ((content (or (llm-response-content response) "")))
             (add-message conn conversation-id "assistant" content)
             (return-from agent-turn (values content conversation-id)))))))
    ;; Max rounds exceeded
    (values (format nil "~A~%~%[max tool rounds reached]"
                    "I've used the maximum number of tool calls for this turn.")
            conversation-id)))
