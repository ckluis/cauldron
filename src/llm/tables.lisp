;;;; src/llm/tables.lisp — Conversation storage tables
(in-package :cauldron.llm)

(cauldron.grimoire:deftable agent_conversations
  (:column title :type text)
  (:column model :type text)
  (:column agent_name :type text)
  (:column total_tokens :type integer :default "0")
  (:index created_at))

(cauldron.grimoire:deftable agent_messages
  (:column conversation_id :type integer :required t :references agent_conversations :on-delete "CASCADE")
  (:column role :type text :required t)
  (:column content :type textarea)
  (:column tool_calls :type json)
  (:column tool_call_id :type text)
  (:column tool_name :type text)
  (:index conversation_id))
