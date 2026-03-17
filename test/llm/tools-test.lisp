;;;; test/llm/tools-test.lisp — Tool struct + format tests
(in-package :cauldron.test)

(defsuite :llm-tools)

(deftest test-agent-tool-struct-creation
  (let ((tool (cauldron.llm:make-agent-tool
               :name "test_tool"
               :description "A test tool"
               :handler (lambda (conn args) (declare (ignore conn args)) "ok")
               :destructive-p nil)))
    (is-equal "test_tool" (cauldron.llm:agent-tool-name tool))
    (is-equal "A test tool" (cauldron.llm:agent-tool-description tool))
    (is-not-nil (cauldron.llm:agent-tool-handler tool))
    (is (not (cauldron.llm:agent-tool-destructive-p tool)))))

(deftest test-tool-parameter-struct-creation
  (let ((param (cauldron.llm:make-tool-parameter
                :name "query"
                :type "string"
                :description "Search query"
                :required-p t)))
    (is-equal "query" (cauldron.llm:tool-parameter-name param))
    (is-equal "string" (cauldron.llm:tool-parameter-type param))
    (is (cauldron.llm:tool-parameter-required-p param))))

(deftest test-tools-to-claude-format-structure
  (let* ((tool (cauldron.llm:make-agent-tool
                :name "test_tool"
                :description "Test"
                :parameters (list (cauldron.llm:make-tool-parameter
                                   :name "q" :type "string"
                                   :description "Query" :required-p t))
                :handler (lambda (c a) (declare (ignore c a)) nil)))
         (formatted (cauldron.llm:tools-to-claude-format (list tool)))
         (first-tool (first formatted)))
    (is-equal "test_tool" (gethash "name" first-tool))
    (is-equal "Test" (gethash "description" first-tool))
    (let ((schema (gethash "input_schema" first-tool)))
      (is-equal "object" (gethash "type" schema))
      (is-not-nil (gethash "properties" schema))
      (is-not-nil (gethash "required" schema)))))

(deftest test-execute-tool-dispatches-correctly
  (let* ((tool (cauldron.llm:make-agent-tool
                :name "hello"
                :description "Say hello"
                :handler (lambda (c a) (declare (ignore c a)) "hello world")))
         (tools (list tool)))
    (multiple-value-bind (result success-p)
        (cauldron.llm:execute-tool nil "hello" nil tools)
      (is success-p)
      (is-equal "hello world" result))))

(deftest test-execute-tool-unknown-tool-error
  (multiple-value-bind (result success-p)
      (cauldron.llm:execute-tool nil "nonexistent" nil nil)
    (is (not success-p))
    (is (search "unknown tool" result))))
