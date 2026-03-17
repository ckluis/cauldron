# Cauldron Architecture

## The Onion: Layer Dependency Rules

Each layer depends ONLY on layers below it. No upward or lateral dependencies.

```
Layer 7: CLI                    ← ALL below
Layer 6: Forge (admin)          ← ALL below
Layer 5: Scry, Ether, Integration, Agent ← WS, Alembic, Crucible, Reagent, Ether
Layer 4: Reagent (resources)    ← Grimoire, Oracle
Layer 3: Grimoire, Alembic, Crucible, LLM ← HTTP, DB, Oracle
Layer 2: HTTP, WebSocket, DB    ← Runtime, Crypto
Layer 1: Crypto, JSON, HTTP Client, Logging ← Runtime
Layer 0: Runtime                ← SBCL only
```

## Dependency Rules

1. **No upward dependencies.** Runtime MUST NOT import HTTP. Crypto MUST NOT import DB.
2. **No lateral dependencies within a layer** unless explicitly documented (e.g., WS depends on HTTP for upgrade).
3. **SBCL is the only external dependency.** No Quicklisp packages. No FFI to C libraries.
4. **Package naming:** `cauldron.MODULE` (e.g., `cauldron.runtime`, `cauldron.crypto`).
5. **One package per module.** Internal symbols stay unexported.

## Module Map

| Module | Package | Layer | Purpose |
|--------|---------|-------|---------|
| runtime | cauldron.runtime | 0 | Threads, sockets, queues, timer, config |
| crypto | cauldron.crypto | 1 | SHA-1/256, HMAC, bcrypt, PBKDF2, AES-256, base64, CSPRNG |
| json | cauldron.json | 1 | Streaming JSON encoder and decoder |
| http-client | cauldron.http-client | 1 | Outbound HTTP via curl subprocess |
| logging | cauldron.logging | 1 | Stripe-style canonical structured log lines |
| oracle | cauldron.oracle | 2 | Condition system, hooks, recovery strategies |
| http | cauldron.http | 2 | HTTP/1.1 server with thread-pool, multipart, compression |
| ws | cauldron.ws | 2 | WebSocket RFC 6455 frames, upgrade, connection |
| db | cauldron.db | 2 | PostgreSQL wire protocol v3, SCRAM-SHA-256, connection pooling |
| grimoire | cauldron.grimoire | 3 | Query DSL, changesets, migrations, deftable DDL, full-text search |
| alembic | cauldron.alembic | 3 | S-expression HTML template engine, SEO |
| crucible | cauldron.crucible | 3 | Trie router, pipelines, plugs, auth, sessions, API keys, billing, uploads, activity, security |
| llm | cauldron.llm | 3 | Provider protocol (Claude/Ollama/Mock), tool system, agent loop, conversation |
| reagent | cauldron.reagent | 4 | MOP-based resource metaclass with attributes, actions, policies, relationships |
| ether | cauldron.ether | 5 | PubSub (in-process + PG LISTEN/NOTIFY bridge) |
| integration | cauldron.integration | 5 | Declarative external integrations, encrypted credentials, webhook delivery |
| agent | cauldron.agent | 5 | Declarative autonomous agents (defagent), A2A, tool caching, SSE |
| scry | cauldron.scry | 5 | LiveView engine with virtual DOM diff + WebSocket transport |
| forge | cauldron.forge | 6 | Auto-generated admin: CRUD, SQL console, schema browser, audit log, agent dashboard |
| cli | cauldron.cli | 7 | Command registration, argument parsing, output formatting, DB/agent commands |

## Key Design Patterns

### Declarative Specs Over Imperative Code
- `deftable` — declarative table DDL with auto id/timestamps
- `defagent` — declarative agent with tools, triggers, memory, model
- `defintegration` — declarative external integration endpoints + webhooks
- `defmigration` — versioned schema migrations with up/down
- `defcommand` — CLI command with options and dispatch

### SQL Generation, Not Execution
Framework functions return `(values sql params)` — the application layer executes. This keeps the framework pure and testable without a database.

### Thread-Safe Registries
All global registries (`*agent-registry*`, `*integration-registry*`, `*table-registry*`, `*commands*`) are protected by locks or use thread-safe patterns.

### Plug-Based Middleware
Plugs are functions `(conn) → conn` that compose into pipelines. Factory functions (`make-plug-*`) create configurable plugs.

### Zero Dependencies
Every byte of functionality is implemented in Common Lisp on SBCL. No Quicklisp, no FFI, no C libraries. Cryptographic primitives, HTTP parsing, WebSocket frames, PostgreSQL wire protocol — all from scratch.

## Build Target

- **Primary:** SBCL on Linux x86-64
- **Secondary:** SBCL on macOS ARM64
- **Deployment:** `save-lisp-and-die :executable t` → single binary

## Reference Implementation

**[Crucible Works](https://github.com/ckluis/crucible-works)** is a multi-tenant business platform (CRM, CMS, knowledge base, billing, agents) built entirely on Cauldron. It demonstrates how the framework's modules compose into a real application — 69 source files, 540 tests, using 16 of 21 framework modules.

## Statistics (v1.0)

- ~17,000 lines of framework code
- 1,554 unit tests + integration tests
- 21 modules across 8 layers
- Zero external dependencies
