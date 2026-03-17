;;;; test/llm/provider-test.lisp — Provider + mock tests
(in-package :cauldron.test)

(defsuite :llm-provider)

(deftest test-llm-provider-struct-creation
  (let ((p (cauldron.llm:make-llm-provider
            :name "test"
            :call-fn (lambda (msgs tools sys) (declare (ignore msgs tools sys)) nil))))
    (is-equal "test" (cauldron.llm:llm-provider-name p))
    (is-not-nil (cauldron.llm:llm-provider-call-fn p))))

(deftest test-llm-response-struct-creation
  (let ((resp (cauldron.llm:make-llm-response
               :content "Hello world"
               :stop-reason "end_turn"
               :input-tokens 100
               :output-tokens 50)))
    (is-equal "Hello world" (cauldron.llm:llm-response-content resp))
    (is-equal "end_turn" (cauldron.llm:llm-response-stop-reason resp))
    (is-equal 100 (cauldron.llm:llm-response-input-tokens resp))
    (is-equal 50 (cauldron.llm:llm-response-output-tokens resp))))

(deftest test-tool-call-struct-creation
  (let ((tc (cauldron.llm:make-tool-call
             :id "toolu_123"
             :name "list_contacts")))
    (is-equal "toolu_123" (cauldron.llm:tool-call-id tc))
    (is-equal "list_contacts" (cauldron.llm:tool-call-name tc))))

(deftest test-mock-provider-returns-responses-in-order
  (let* ((r1 (cauldron.llm:make-llm-response :content "first"))
         (r2 (cauldron.llm:make-llm-response :content "second"))
         (provider (cauldron.llm:make-mock-provider (list r1 r2))))
    (let ((resp1 (funcall (cauldron.llm:llm-provider-call-fn provider) nil nil nil))
          (resp2 (funcall (cauldron.llm:llm-provider-call-fn provider) nil nil nil))
          (resp3 (funcall (cauldron.llm:llm-provider-call-fn provider) nil nil nil)))
      (is-equal "first" (cauldron.llm:llm-response-content resp1))
      (is-equal "second" (cauldron.llm:llm-response-content resp2))
      (is (search "exhausted" (cauldron.llm:llm-response-content resp3))))))

(deftest test-parse-claude-response-text-only
  (let* ((json "{\"content\":[{\"type\":\"text\",\"text\":\"Hello!\"}],\"stop_reason\":\"end_turn\",\"usage\":{\"input_tokens\":10,\"output_tokens\":5}}")
         (resp (cauldron.llm:parse-claude-response json)))
    (is-equal "Hello!" (cauldron.llm:llm-response-content resp))
    (is-equal "end_turn" (cauldron.llm:llm-response-stop-reason resp))
    (is (null (cauldron.llm:llm-response-tool-calls resp)))
    (is-equal 10 (cauldron.llm:llm-response-input-tokens resp))
    (is-equal 5 (cauldron.llm:llm-response-output-tokens resp))))

(deftest test-parse-claude-response-tool-use
  (let* ((json "{\"content\":[{\"type\":\"text\",\"text\":\"Let me check.\"},{\"type\":\"tool_use\",\"id\":\"toolu_abc\",\"name\":\"list_contacts\",\"input\":{}}],\"stop_reason\":\"tool_use\",\"usage\":{\"input_tokens\":50,\"output_tokens\":20}}")
         (resp (cauldron.llm:parse-claude-response json)))
    (is-equal "Let me check." (cauldron.llm:llm-response-content resp))
    (is-equal "tool_use" (cauldron.llm:llm-response-stop-reason resp))
    (is-equal 1 (length (cauldron.llm:llm-response-tool-calls resp)))
    (let ((tc (first (cauldron.llm:llm-response-tool-calls resp))))
      (is-equal "toolu_abc" (cauldron.llm:tool-call-id tc))
      (is-equal "list_contacts" (cauldron.llm:tool-call-name tc)))))

(deftest test-make-claude-provider-exists
  (is (fboundp 'cauldron.llm:make-claude-provider)))

(deftest test-make-ollama-provider-exists
  (is (fboundp 'cauldron.llm:make-ollama-provider)))
