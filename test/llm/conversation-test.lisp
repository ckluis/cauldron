;;;; test/llm/conversation-test.lisp — Conversation CRUD fboundp tests
(in-package :cauldron.test)

(defsuite :llm-conversation)

(deftest test-create-conversation-exists
  (is (fboundp 'cauldron.llm:create-conversation)))

(deftest test-add-message-exists
  (is (fboundp 'cauldron.llm:add-message)))

(deftest test-get-conversation-messages-exists
  (is (fboundp 'cauldron.llm:get-conversation-messages)))

(deftest test-list-conversations-exists
  (is (fboundp 'cauldron.llm:list-conversations)))

(deftest test-update-conversation-tokens-exists
  (is (fboundp 'cauldron.llm:update-conversation-tokens)))

;;; --- Deftable registration tests ---

(deftest test-llm-agent-conversations-deftable-registered
  (is-not-nil (cauldron.grimoire:find-table-spec "agent_conversations")))

(deftest test-llm-agent-messages-deftable-registered
  (is-not-nil (cauldron.grimoire:find-table-spec "agent_messages")))

(deftest test-llm-agent-messages-has-conversation-fk
  (let* ((spec (cauldron.grimoire:find-table-spec "agent_messages"))
         (ddl-list (cauldron.grimoire:generate-table-ddl spec))
         (create-sql (first ddl-list)))
    (is (search "REFERENCES" (string-upcase create-sql)))))
