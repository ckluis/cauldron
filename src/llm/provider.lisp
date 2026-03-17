;;;; src/llm/provider.lisp — LLM provider protocol: Claude, Ollama, Mock
(in-package :cauldron.llm)

;;; --- Data Structures ---

(defstruct llm-provider
  "An LLM provider that can be called with messages and tools."
  (name "" :type string)
  (call-fn nil :type function))     ; (lambda (messages tools system-prompt) → llm-response)

(defstruct llm-response
  "Response from an LLM provider."
  (content nil :type (or null string))
  (tool-calls nil :type list)       ; list of tool-call structs
  (stop-reason nil :type (or null string))
  (input-tokens 0 :type integer)
  (output-tokens 0 :type integer))

(defstruct tool-call
  "A tool invocation from the LLM."
  (id "" :type string)
  (name "" :type string)
  (arguments nil))                  ; hash-table of arg-name → value

;;; --- Claude Provider ---

(defun parse-claude-response (json-string)
  "Parse a Claude API response JSON string into an llm-response struct."
  (let* ((data (cauldron.json:decode json-string))
         (content-blocks (cauldron.runtime:ht-get data "content"))
         (stop-reason (cauldron.runtime:ht-get data "stop_reason"))
         (usage (cauldron.runtime:ht-get data "usage"))
         (input-tokens (if usage (or (cauldron.runtime:ht-get usage "input_tokens") 0) 0))
         (output-tokens (if usage (or (cauldron.runtime:ht-get usage "output_tokens") 0) 0))
         (text-parts nil)
         (tool-calls nil))
    ;; Process content blocks (may be a vector or list from JSON decode)
    (let ((blocks-list (cond ((vectorp content-blocks) (coerce content-blocks 'list))
                              ((listp content-blocks) content-blocks)
                              (t nil))))
      (dolist (block blocks-list)
        (let ((block-type (cauldron.runtime:ht-get block "type")))
          (cond
            ((string= block-type "text")
             (push (cauldron.runtime:ht-get block "text") text-parts))
            ((string= block-type "tool_use")
             (push (make-tool-call
                    :id (or (cauldron.runtime:ht-get block "id") "")
                    :name (or (cauldron.runtime:ht-get block "name") "")
                    :arguments (let ((input (cauldron.runtime:ht-get block "input")))
                                 (if (hash-table-p input)
                                     input
                                     (when (and input (listp input))
                                       (let ((ht (make-hash-table :test 'equal)))
                                         (dolist (pair input)
                                           (setf (gethash (car pair) ht) (cdr pair)))
                                         ht)))))
                   tool-calls))))))
    (make-llm-response
     :content (let ((texts (nreverse text-parts)))
                (when texts (format nil "~{~A~}" texts)))
     :tool-calls (nreverse tool-calls)
     :stop-reason stop-reason
     :input-tokens input-tokens
     :output-tokens output-tokens)))

(defun claude-api-call (messages tools system-prompt &key api-key model max-tokens)
  "Call Claude Messages API. Returns llm-response struct."
  (let* ((api-key (or api-key (sb-ext:posix-getenv "ANTHROPIC_API_KEY")))
         (model (or model "claude-sonnet-4-20250514"))
         (max-tokens (or max-tokens 4096))
         (body (cauldron.runtime:ht "model" model
                     "max_tokens" max-tokens
                     "messages" (coerce messages 'vector))))
    (when system-prompt
      (setf (gethash "system" body) system-prompt))
    (when tools
      (setf (gethash "tools" body) (coerce tools 'vector)))
    (let ((body-json (cauldron.json:encode body))
          (headers (list (cons "Content-Type" "application/json")
                         (cons "x-api-key" api-key)
                         (cons "anthropic-version" "2023-06-01"))))
      (multiple-value-bind (response-body status)
          (cauldron.http-client:http-post-json "https://api.anthropic.com/v1/messages" headers body-json)
        (if (and (>= status 200) (< status 300))
            (parse-claude-response response-body)
            (make-llm-response
             :content (format nil "API error (~A): ~A" status response-body)
             :stop-reason "error"))))))

(defun make-claude-provider (&key api-key model max-tokens)
  "Create a Claude API provider."
  (make-llm-provider
   :name "claude"
   :call-fn (lambda (messages tools system-prompt)
              (claude-api-call messages tools system-prompt
                                :api-key api-key :model model :max-tokens max-tokens))))

;;; --- Ollama Provider ---

(defun ollama-api-call (messages tools system-prompt &key host port model)
  "Call Ollama API via curl. Returns llm-response."
  (let* ((host (or host "localhost"))
         (port (or port 11434))
         (model (or model "llama3.1"))
         (url (format nil "http://~A:~A/api/chat" host port))
         (all-messages (if system-prompt
                          (coerce (cons (cauldron.runtime:ht "role" "system" "content" system-prompt)
                                        (coerce messages 'list))
                                  'vector)
                          (coerce messages 'vector)))
         (body (cauldron.runtime:ht "model" model
                     "messages" all-messages
                     "stream" :false)))
    (when tools
      (setf (gethash "tools" body) (coerce tools 'vector)))
    (let ((body-json (cauldron.json:encode body))
          (headers (list (cons "Content-Type" "application/json"))))
      (multiple-value-bind (response-body status)
          (cauldron.http-client:http-post-json url headers body-json)
        (if (and (>= status 200) (< status 300))
            (let* ((data (cauldron.json:decode response-body))
                   (message (cauldron.runtime:ht-get data "message"))
                   (content (when message (cauldron.runtime:ht-get message "content")))
                   (tool-calls-raw (when message (cauldron.runtime:ht-get message "tool_calls"))))
              (make-llm-response
               :content content
               :tool-calls (when tool-calls-raw
                             (mapcar (lambda (tc)
                                       (let ((fn (cauldron.runtime:ht-get tc "function")))
                                         (make-tool-call
                                          :id (format nil "ollama_~A" (random 100000))
                                          :name (cauldron.runtime:ht-get fn "name")
                                          :arguments (let ((args-raw (cauldron.runtime:ht-get fn "arguments")))
                                                       (if (hash-table-p args-raw)
                                                           args-raw
                                                           (when (and args-raw (listp args-raw))
                                                             (let ((ht (make-hash-table :test 'equal)))
                                                               (dolist (pair args-raw)
                                                                 (setf (gethash (car pair) ht) (cdr pair)))
                                                               ht)))))))
                                     tool-calls-raw))
               :stop-reason "end_turn"))
            (make-llm-response
             :content (format nil "Ollama error (~A): ~A" status response-body)
             :stop-reason "error"))))))

(defun make-ollama-provider (&key host port model)
  "Create an Ollama provider for local LLMs."
  (make-llm-provider
   :name "ollama"
   :call-fn (lambda (messages tools system-prompt)
              (ollama-api-call messages tools system-prompt
                                :host host :port port :model model))))

;;; --- Mock Provider (for testing) ---

(defun make-mock-provider (responses)
  "Create a mock provider that returns pre-built llm-response structs in order.
RESPONSES is a list of llm-response structs."
  (let ((remaining (copy-list responses)))
    (make-llm-provider
     :name "mock"
     :call-fn (lambda (messages tools system-prompt)
                (declare (ignore messages tools system-prompt))
                (if remaining
                    (pop remaining)
                    (make-llm-response
                     :content "Mock provider exhausted: no more responses"
                     :stop-reason "end_turn"))))))
