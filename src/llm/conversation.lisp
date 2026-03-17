;;;; src/llm/conversation.lisp — Conversation CRUD operations
(in-package :cauldron.llm)

(defun create-conversation (conn &key title agent-name model)
  "Create a new agent conversation. Returns hash-table."
  (first (cauldron.db:query conn
    "INSERT INTO agent_conversations (title, agent_name, model)
     VALUES ($1, $2, $3) RETURNING *"
    (or title "New conversation")
    (or agent-name "default")
    (or model "claude-sonnet-4-20250514"))))

(defun add-message (conn conversation-id role content &key tool-calls tool-call-id tool-name)
  "Add a message to a conversation. Returns hash-table."
  (first (cauldron.db:query conn
    "INSERT INTO agent_messages (conversation_id, role, content, tool_calls, tool_call_id, tool_name)
     VALUES ($1, $2, $3, $4, $5, $6) RETURNING *"
    conversation-id
    role
    content
    (when tool-calls (cauldron.json:encode tool-calls))
    tool-call-id
    tool-name)))

(defun get-conversation-messages (conn conversation-id &key (limit 50))
  "Get messages for a conversation, ordered by creation time."
  (cauldron.db:query conn
    "SELECT * FROM agent_messages WHERE conversation_id = $1
     ORDER BY id ASC LIMIT $2"
    conversation-id limit))

(defun list-conversations (conn &key (limit 20))
  "List recent conversations."
  (cauldron.db:query conn
    "SELECT * FROM agent_conversations ORDER BY created_at DESC LIMIT $1"
    limit))

(defun update-conversation-tokens (conn conversation-id tokens)
  "Increment the total_tokens counter for a conversation."
  (cauldron.db:query conn
    "UPDATE agent_conversations SET total_tokens = total_tokens + $1
     WHERE id = $2"
    tokens conversation-id))
