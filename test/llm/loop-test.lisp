;;;; test/llm/loop-test.lisp — Message format + agent loop tests
(in-package :cauldron.test)

(defsuite :llm-loop)

(deftest test-messages-to-claude-format-user-message
  (let ((msg (make-hash-table :test 'equal)))
    (setf (gethash "role" msg) "user"
          (gethash "content" msg) "Hello")
    (let* ((result (cauldron.llm:messages-to-claude-format (list msg)))
           (first-msg (first result)))
      (is-equal "user" (gethash "role" first-msg))
      (is-equal "Hello" (gethash "content" first-msg)))))

(deftest test-messages-to-claude-format-assistant-text
  (let ((msg (make-hash-table :test 'equal)))
    (setf (gethash "role" msg) "assistant"
          (gethash "content" msg) "Hi there")
    (let* ((result (cauldron.llm:messages-to-claude-format (list msg)))
           (first-msg (first result)))
      (is-equal "assistant" (gethash "role" first-msg))
      (is-equal "Hi there" (gethash "content" first-msg)))))

(deftest test-messages-to-claude-format-assistant-with-tool-calls
  (let ((msg (make-hash-table :test 'equal))
        (tc-ht (make-hash-table :test 'equal)))
    (setf (gethash "id" tc-ht) "toolu_1"
          (gethash "name" tc-ht) "list_contacts")
    (setf (gethash "role" msg) "assistant"
          (gethash "content" msg) "Let me check."
          (gethash "tool_calls" msg)
          (cauldron.json:encode (vector tc-ht)))
    (let* ((result (cauldron.llm:messages-to-claude-format (list msg)))
           (first-msg (first result))
           (content-blocks (gethash "content" first-msg)))
      (is-equal "assistant" (gethash "role" first-msg))
      (is (vectorp content-blocks))
      (is (>= (length content-blocks) 2)))))

(deftest test-messages-to-claude-format-tool-result
  (let ((assistant-msg (make-hash-table :test 'equal))
        (tool-msg (make-hash-table :test 'equal))
        (tc-ht (make-hash-table :test 'equal)))
    (setf (gethash "id" tc-ht) "toolu_1"
          (gethash "name" tc-ht) "list_contacts")
    (setf (gethash "role" assistant-msg) "assistant"
          (gethash "content" assistant-msg) ""
          (gethash "tool_calls" assistant-msg)
          (cauldron.json:encode (vector tc-ht)))
    (setf (gethash "role" tool-msg) "tool_result"
          (gethash "content" tool-msg) "[{\"name\":\"John\"}]"
          (gethash "tool_call_id" tool-msg) "toolu_1"
          (gethash "tool_name" tool-msg) "list_contacts")
    (let* ((result (cauldron.llm:messages-to-claude-format (list assistant-msg tool-msg)))
           (tool-result-msg (second result))
           (content (gethash "content" tool-result-msg)))
      (is-equal "user" (gethash "role" tool-result-msg))
      (is (vectorp content))
      (let ((tr-block (aref content 0)))
        (is-equal "tool_result" (gethash "type" tr-block))
        (is-equal "toolu_1" (gethash "tool_use_id" tr-block))))))

(deftest test-agent-turn-exists
  (is (fboundp 'cauldron.llm:agent-turn)))
