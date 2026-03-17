;;;; cauldron.asd — Zero-dependency Common Lisp web framework
;;;; Built on SBCL. No external dependencies. One image, one binary.

(defsystem "cauldron"
  :description "Zero-dependency Common Lisp web framework built on SBCL"
  :version "1.0.0"
  :license "MIT"
  :depends-on ()
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    (;; Layer 0: Runtime — threading, sockets, atomics, utilities
     (:module "runtime"
      :serial t
      :components ((:file "packages")
                   (:file "utilities")
                   (:file "threads")
                   (:file "queue")
                   (:file "thread-pool")
                   (:file "sockets")
                   (:file "timer")
                   (:file "config")
                   (:file "shutdown")
                   (:file "request-id")))

     ;; Layer 1a: Crypto — SHA, HMAC, bcrypt, base64, CSPRNG
     (:module "crypto"
      :serial t
      :components ((:file "packages")
                   (:file "base64")
                   (:file "sha1")
                   (:file "sha256")
                   (:file "hmac")
                   (:file "pbkdf2")
                   (:file "bcrypt")
                   (:file "random")
                   (:file "aes")))

     ;; Layer 1b: JSON — encoder/decoder
     (:module "json"
      :serial t
      :components ((:file "packages")
                   (:file "encoder")
                   (:file "decoder")))

     ;; Layer 1c: HTTP Client — outbound HTTP via curl
     (:module "http-client"
      :serial t
      :components ((:file "packages")
                   (:file "client")))

     ;; Layer 1d: Logging — canonical structured log lines
     (:module "logging"
      :serial t
      :components ((:file "packages")
                   (:file "stream")
                   (:file "format")
                   (:file "context")))

     ;; Layer 2a: Oracle — condition system
     (:module "oracle"
      :serial t
      :components ((:file "packages")
                   (:file "conditions")
                   (:file "hooks")
                   (:file "recovery")))

     ;; Layer 2b: HTTP/1.1 server
     (:module "http"
      :serial t
      :components ((:file "packages")
                   (:file "request")
                   (:file "response")
                   (:file "multipart")
                   (:file "compress")
                   (:file "server")))

     ;; Layer 2c: WebSocket
     (:module "ws"
      :serial t
      :components ((:file "packages")
                   (:file "frame")
                   (:file "connection")))

     ;; Layer 2d: PostgreSQL wire protocol
     (:module "db"
      :serial t
      :components ((:file "packages")
                   (:file "protocol")
                   (:file "types")
                   (:file "auth")
                   (:file "connection")
                   (:file "pool")
                   (:file "schema")))

     ;; Layer 3a: Grimoire — query DSL + migrations + declarative DDL + search
     (:module "grimoire"
      :serial t
      :components ((:file "packages")
                   (:file "query")
                   (:file "conditions")
                   (:file "changeset")
                   (:file "migration")
                   (:file "deftable")
                   (:file "search")))

     ;; Layer 3b: Alembic — HTML template engine + SEO
     (:module "alembic"
      :serial t
      :components ((:file "packages")
                   (:file "escape")
                   (:file "html")
                   (:file "forms")
                   (:file "seo")))

     ;; Layer 3c: Crucible — router, bridge, plugs, auth, pg-session, api helpers, api-keys, uploads, billing
     (:module "crucible"
      :serial t
      :components ((:file "packages")
                   (:file "trie")
                   (:file "pipeline")
                   (:file "router")
                   (:file "bridge")
                   (:file "logging")
                   (:file "cors")
                   (:file "rate-limit")
                   (:file "compress")
                   (:file "plugs")
                   (:file "auth")
                   (:file "pg-session")
                   (:file "api")
                   (:file "api-keys")
                   (:file "uploads")
                   (:file "billing")
                   (:file "activity")
                   (:file "security")))

     ;; Layer 3d: LLM — provider protocol, tools, agent loop, conversation
     (:module "llm"
      :serial t
      :components ((:file "packages")
                   (:file "provider")
                   (:file "tools")
                   (:file "tables")
                   (:file "conversation")
                   (:file "loop")))

     ;; Layer 4: Reagent — MOP resource system
     (:module "reagent"
      :serial t
      :components ((:file "packages")
                   (:file "metaclass")
                   (:file "attributes")
                   (:file "actions")
                   (:file "policies")
                   (:file "relationships")
                   (:file "registry")))

     ;; Layer 5a: Ether — PubSub
     (:module "ether"
      :serial t
      :components ((:file "packages")
                   (:file "pubsub")
                   (:file "pg-pubsub")))

     ;; Layer 5c: Integration Protocol — declarative external integrations + delivery
     (:module "integration"
      :serial t
      :components ((:file "packages")
                   (:file "spec")
                   (:file "registry")
                   (:file "credentials")
                   (:file "retry")
                   (:file "health")
                   (:file "client")
                   (:file "webhook")
                   (:file "delivery")))

     ;; Layer 5d: Agent — declarative autonomous agents
     (:module "agent"
      :serial t
      :components ((:file "packages")
                   (:file "spec")
                   (:file "memory")
                   (:file "tools")
                   (:file "triggers")
                   (:file "executor")))

     ;; Layer 5b: Scry — LiveView engine
     (:module "scry"
      :serial t
      :components ((:file "packages")
                   (:file "diff")
                   (:file "lifecycle")
                   (:file "transport")))

     ;; Layer 6: Forge — admin interface
     (:module "forge"
      :serial t
      :components ((:file "packages")
                   (:file "config")
                   (:file "router")
                   (:file "dashboard")
                   (:file "crud")
                   (:file "sql-console")
                   (:file "schema-browser")
                   (:file "audit")
                   (:file "integrations")
                   (:file "agents")))

     ;; Layer 7: CLI infrastructure
     (:module "cli"
      :serial t
      :components ((:file "packages")
                   (:file "cli")
                   (:file "output")
                   (:file "db")
                   (:file "db-commands")
                   (:file "agent-commands")))))))

;;; Core subsystem: pure-function modules that can compile independently.
;;; Used by the test system to avoid loading full HTTP/WS/DB server layers.
(defsystem "cauldron/core"
  :description "Cauldron core: runtime, crypto, JSON, oracle, alembic, crucible, grimoire, plus pure modules from http, db, scry, reagent, forge"
  :depends-on ()
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:module "runtime"
      :serial t
      :components ((:file "packages")
                   (:file "utilities")
                   (:file "threads")
                   (:file "queue")
                   (:file "thread-pool")
                   (:file "sockets")
                   (:file "timer")
                   (:file "config")
                   (:file "shutdown")
                   (:file "request-id")))

     (:module "crypto"
      :serial t
      :components ((:file "packages")
                   (:file "base64")
                   (:file "sha1")
                   (:file "sha256")
                   (:file "hmac")
                   (:file "pbkdf2")
                   (:file "bcrypt")
                   (:file "random")
                   (:file "aes")))

     (:module "json"
      :serial t
      :components ((:file "packages")
                   (:file "encoder")
                   (:file "decoder")))

     (:module "http-client"
      :serial t
      :components ((:file "packages")
                   (:file "client")))

     (:module "logging"
      :serial t
      :components ((:file "packages")
                   (:file "stream")
                   (:file "format")
                   (:file "context")))

     (:module "oracle"
      :serial t
      :components ((:file "packages")
                   (:file "conditions")
                   (:file "hooks")
                   (:file "recovery")))

     (:module "alembic"
      :serial t
      :components ((:file "packages")
                   (:file "escape")
                   (:file "html")
                   (:file "forms")
                   (:file "seo")))

     ;; HTTP request/response parsing (pure, no server)
     (:module "http"
      :serial t
      :components ((:file "packages")
                   (:file "request")
                   (:file "response")
                   (:file "multipart")
                   (:file "compress")))

     ;; DB types, protocol, auth, connection parsing, and pool state (no live connections)
     (:module "db"
      :serial t
      :components ((:file "packages")
                   (:file "protocol")
                   (:file "types")
                   (:file "auth")
                   (:file "connection")
                   (:file "pool")
                   (:file "schema")))

     (:module "crucible"
      :serial t
      :components ((:file "packages")
                   (:file "trie")
                   (:file "pipeline")
                   (:file "router")
                   (:file "bridge")
                   (:file "logging")
                   (:file "cors")
                   (:file "rate-limit")
                   (:file "compress")
                   (:file "plugs")
                   (:file "auth")
                   (:file "pg-session")
                   (:file "api")
                   (:file "api-keys")
                   (:file "uploads")
                   (:file "billing")
                   (:file "activity")
                   (:file "security")))

     (:module "grimoire"
      :serial t
      :components ((:file "packages")
                   (:file "conditions")
                   (:file "query")
                   (:file "changeset")
                   (:file "migration")
                   (:file "deftable")
                   (:file "search")))

     ;; LLM — provider protocol, tools, agent loop, conversation
     (:module "llm"
      :serial t
      :components ((:file "packages")
                   (:file "provider")
                   (:file "tools")
                   (:file "tables")
                   (:file "conversation")
                   (:file "loop")))

     ;; WebSocket frame encoding + connection (pure byte manipulation + handshake)
     (:module "ws"
      :serial t
      :components ((:file "packages")
                   (:file "frame")
                   (:file "connection")))

     ;; Reagent resource system (pure metadata + policies + relationships + actions)
     (:module "reagent"
      :serial t
      :components ((:file "packages")
                   (:file "metaclass")
                   (:file "attributes")
                   (:file "actions")
                   (:file "policies")
                   (:file "relationships")
                   (:file "registry")))

     ;; Ether PubSub (in-process, thread-safe)
     (:module "ether"
      :serial t
      :components ((:file "packages")
                   (:file "pubsub")))

     ;; Integration Protocol (declarative external integrations + delivery)
     (:module "integration"
      :serial t
      :components ((:file "packages")
                   (:file "spec")
                   (:file "registry")
                   (:file "credentials")
                   (:file "retry")
                   (:file "health")
                   (:file "client")
                   (:file "webhook")
                   (:file "delivery")))

     ;; Agent — declarative autonomous agents (pure spec + tools + triggers)
     (:module "agent"
      :serial t
      :components ((:file "packages")
                   (:file "spec")
                   (:file "memory")
                   (:file "tools")
                   (:file "triggers")
                   (:file "executor")))

     ;; Scry diff, lifecycle, transport (pure functional + wire format)
     (:module "scry"
      :serial t
      :components ((:file "packages")
                   (:file "diff")
                   (:file "lifecycle")
                   (:file "transport")))

     ;; Forge config, router, dashboard, CRUD, sql-console, schema-browser, audit
     (:module "forge"
      :serial t
      :components ((:file "packages")
                   (:file "config")
                   (:file "dashboard")
                   (:file "crud")
                   (:file "sql-console")
                   (:file "schema-browser")
                   (:file "audit")
                   (:file "integrations")
                   (:file "agents")
                   (:file "router")))

     ;; CLI infrastructure (pure arg parsing + output formatting)
     (:module "cli"
      :serial t
      :components ((:file "packages")
                   (:file "cli")
                   (:file "output")
                   (:file "db")
                   (:file "db-commands")
                   (:file "agent-commands")))))))

(defsystem "cauldron/test"
  :description "Cauldron test suite"
  :depends-on ("cauldron")
  :serial t
  :components
  ((:module "test"
    :serial t
    :components
    ((:file "runner")
     (:file "tags")
     (:file "runtime/utilities-test")
     (:file "runtime/config-test")
     (:file "http-client/client-test")
     (:file "llm/provider-test")
     (:file "llm/tools-test")
     (:file "llm/loop-test")
     (:file "llm/conversation-test")
     (:file "logging/format-test")
     (:file "logging/context-test")
     (:file "http-client/enhanced-client-test")
     (:file "crucible/api-test")
     (:file "crucible/generic-plugs-test")
     (:file "crypto/base64-test")
     (:file "crypto/sha1-test")
     (:file "crypto/sha256-test")
     (:file "crypto/hmac-test")
     (:file "crypto/pbkdf2-test")
     (:file "crypto/random-test")
     (:file "json/encoder-test")
     (:file "json/decoder-test")
     (:file "oracle/conditions-test")
     (:file "oracle/hooks-test")
     (:file "alembic/escape-test")
     (:file "alembic/html-test")
     (:file "crucible/trie-test")
     (:file "crucible/pipeline-test")
     (:file "grimoire/query-test")
     (:file "grimoire/changeset-test")
     ;; New test suites
     (:file "scry/diff-test")
     (:file "db/types-test")
     (:file "db/protocol-test")
     (:file "http/timeout-test")
     (:file "http/compress-test")
     (:file "http/request-test")
     (:file "http/response-test")
     (:file "reagent/attributes-test")
     (:file "forge/drift-test")
     ;; Phase 3 test suites
     (:file "crypto/bcrypt-test")
     (:file "ws/frame-test")
     (:file "alembic/forms-test")
     (:file "reagent/policies-test")
     (:file "reagent/relationships-test")
     (:file "grimoire/migration-test")
     (:file "reagent/actions-test")
     (:file "crucible/router-test")
     (:file "crucible/bridge-test")
     (:file "crucible/logging-test")
     (:file "crucible/cors-test")
     (:file "crucible/rate-limit-test")
     (:file "crucible/compress-test")
     (:file "crucible/plugs-test")
     ;; Phase 4 test suites
     (:file "db/auth-test")
     (:file "forge/config-test")
     (:file "scry/transport-test")
     (:file "ether/pubsub-test")
     (:file "oracle/recovery-test")
     (:file "forge/crud-test")
     (:file "reagent/metaclass-test")
     ;; Phase 5 test suites
     (:file "runtime/queue-test")
     (:file "runtime/threads-test")
     (:file "runtime/timer-test")
     (:file "scry/lifecycle-test")
     (:file "ws/connection-test")
     (:file "reagent/registry-test")
     ;; Phase 6 test suites
     (:file "db/connection-test")
     (:file "db/pool-test")
     (:file "forge/router-test")
     (:file "forge/audit-test")
     (:file "forge/schema-browser-test")
     (:file "runtime/sockets-test")
     ;; Phase 7 test suites
     (:file "forge/dashboard-test")
     ;; Phase 8 integration test suites
     (:file "http/server-test")
     ;; Phase 10 DB integration test suites
     (:file "db/integration-helpers")
     (:file "db/connection-integration-test")
     (:file "db/types-integration-test")
     (:file "db/pool-integration-test")
     (:file "grimoire/migration-integration-test")
     (:file "grimoire/query-integration-test")
     (:file "db/listen-notify-integration-test")
     ;; Phase 12: Multi-schema support
     (:file "db/schema-test")
     (:file "db/schema-integration-test")
     ;; Phase 13: Auth, Multipart
     (:file "crucible/auth-test")
     (:file "http/multipart-test")
     ;; Phase 14: SEO
     (:file "alembic/seo-test")
     ;; Phase 15: CLI
     (:file "cli/cli-test")
     ;; Phase 16A: CLI enhancements
     (:file "cli/output-test")
     ;; Phase 21: CLI DB admin commands
     (:file "cli/db-commands-test")
     ;; Phase 21A: Declarative table definitions
     (:file "grimoire/deftable-test")
     ;; (Crucible Works app tests moved to crucible-works repo)
     ;; Phase 38: Integration Protocol
     (:file "crypto/aes-test")
     (:file "integration/spec-test")
     (:file "integration/credential-test")
     (:file "integration/client-test")
     (:file "integration/webhook-test")
     (:file "integration/retry-test")
     ;; (User Lifecycle app tests moved to crucible-works repo)
     ;; Phase 39: Declarative Agents
     (:file "agent/spec-test")
     (:file "agent/memory-test")
     (:file "agent/tools-test")
     (:file "agent/triggers-test")
     (:file "agent/executor-test")
     ;; Phase 39B: Agent Hardening
     (:file "agent/hardening-test")
     ;; Phase 40: API Keys + Webhook Delivery
     (:file "crucible/api-keys-test")
     (:file "integration/delivery-test")
     ;; Phase 41: Stripe Billing
     (:file "crucible/billing-test")
     ;; Phase 42: File Uploads
     (:file "crucible/uploads-test")
     ;; Phase 43: Full-Text Search
     (:file "grimoire/search-test")
     ;; Phase 44: Activity Feed
     (:file "crucible/activity-test")
     ;; Phase 45: Security Hardening
     (:file "crucible/security-test")
     ;; T1: Comprehensive Testing Pass
     (:file "scry/lifecycle-extended-test")
     (:file "ether/pubsub-extended-test")
     (:file "crypto/property-test")
     (:file "http/edge-cases-test")
     (:file "crucible/rate-limit-extended-test")
     (:file "forge/crud-extended-test")
     (:file "db/pool-extended-test")
     ;; T2: Production Hardening
     (:file "runtime/shutdown-test")
     (:file "runtime/request-id-test")))))
