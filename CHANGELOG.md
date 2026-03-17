# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0] — 2026-03-17

### Added

**Pre-v1.0 Track**
- T1: Comprehensive testing pass — extended test coverage for Scry lifecycle, Ether PubSub, crypto properties, HTTP edge cases, rate limiting, Forge CRUD, DB pool
- T2: Production hardening — graceful shutdown hooks, request ID generation/propagation
- T3: Performance benchmarks — `bench/` directory with JSON, crypto, HTTP, query builder benchmarks
- T4: Documentation — README.md quick start, ARCHITECTURE.md with full module map
- T5: v1.0 release — version bump, comprehensive CHANGELOG, marketing site update

**Phase 45: Security Hardening**
- `make-plug-content-security-policy` — configurable CSP directives
- `make-plug-hsts` — Strict-Transport-Security with includeSubDomains + preload
- `plug-secure-headers-enhanced` — full header set (COOP, CORP, Permissions-Policy)
- `make-plug-ip-allowlist` — restrict routes to IP ranges
- `make-plug-request-size-limit` — reject oversized bodies (1MB JSON, 10MB multipart)
- `secure-random-string` with configurable alphabet
- `validate-sql-identifier` — prevents SQL injection via dynamic identifiers
- `regenerate-session-id` — session fixation protection

**Phase 44: Activity Feed**
- `activity_events` table: actor/action/target tracking with JSON context
- Dynamic query builder with parameterized filters (company, actor, target, action)
- `make-plug-activity-tracking` — auto-records CRUD from HTTP method + status
- Ether PubSub integration for real-time activity feeds

**Phase 43: Full-Text Search**
- PostgreSQL FTS: `add-search-column-ddl` generates tsvector + GIN index + auto-update trigger
- `build-search-query` with ts_rank ordering
- `highlight-match` via ts_headline with custom markers
- `cross-table-search-sql` — UNION ALL across multiple object types
- `generate-search-ddl` from table name + column list

**Phase 42: File Uploads**
- Upload storage with UUID-based paths (year/month/uuid.ext)
- MIME type allowlist validation (png, jpg, gif, webp, svg, pdf, csv, txt, zip)
- Polymorphic attachments: attachable_type + attachable_id linking
- `make-plug-upload-limit` — Content-Length check with 413 rejection
- Serve headers with Content-Type, Cache-Control, X-Content-Type-Options

**Phase 41: Stripe Billing**
- Billing plans, subscriptions, invoices with DDL
- Subscription state machine: trialing/active/past_due/canceled/incomplete
- `make-plug-require-subscription` — 402 gating for unpaid accounts
- `make-plug-metered-usage` — API call counting per billing period

**Phase 40: API Keys + Webhook Delivery**
- API key generation: `cld_` prefix, SHA-256 hashing, 40-char hex tokens
- Key lifecycle: generate, revoke, rotate, touch (last_used_at)
- `make-plug-api-key-auth` — Bearer token extraction with hash lookup
- `make-plug-require-scope` — per-endpoint scope authorization
- Webhook endpoints with event type filtering and HMAC secrets
- Webhook delivery with status tracking (pending/delivered/failed/exhausted)
- HMAC-SHA256 payload signing with X-Webhook-Signature header
- Exponential backoff: 10s → 60s → 5m → 30m → 2h (max 5 attempts)

**Phase 39B: Agent Hardening**
- Token budgets: `:token-budget` clause, `agent-budget-exceeded` condition, budget-checked provider
- Agent-to-agent: `from-agent` tool clause, `generate-agent-tool`, cycle detection (max depth 5)
- Tool caching: `*tool-cache*` with TTL (300s), `cached-assemble-agent-tools`, `invalidate-agent-tools`
- SSE streaming: `agent-sse-handler` for text/event-stream responses
- Forge dashboard: `/forge/agents` list + `/forge/agents/:name` detail views
- CLI: `agent list|invoke|tools|history` commands
- Enhanced templates: `{{date}}`, `{{time}}`, `{{config:KEY}}` interpolation

## [0.5.0] — 2026-03-15

### Refactored

**Phase 34: Core Framework Refactoring**
- Moved 17 generic utilities from app layer to framework
- New `cauldron.http-client` module
- New `cauldron.llm` module: provider protocol, tool system, conversation CRUD, agent-turn loop
- `cauldron.runtime:ht` and `ht-get` — hash-table builder + polymorphic getter

**Phase 35: Framework Hardening**
- Centralized config: `get-env`, `defconfig` macro, `validate-config`
- Request logging: `plug-request-log` with method/path/status/duration
- CORS: `make-plug-cors` with preflight handling, origin matching
- Rate limiting: `make-plug-rate-limit` per-IP with sliding window
- Response compression: pure-CL DEFLATE/gzip (RFC 1951/1952)

**Phase 36: User Lifecycle**
- Password reset flow with secure tokens
- Email verification with soft enforcement
- User settings: profile editing, password change
- Team invitations: invite → accept flow

**Phase 37: Canonical Logging + HTTP Client**
- `cauldron.logging` module: Stripe-style canonical log lines
- Enhanced HTTP client with GET/POST/PUT/PATCH/DELETE

**Phase 38: Integration Protocol**
- `defintegration` macro for declarative external integrations
- AES-256-CBC encryption with PKCS7 padding
- Encrypted credential store with cascading scope resolution
- Integration client with retry, rate limiting, health tracking
- Webhook verification with HMAC-SHA256

**Phase 38H: Integration Hardening**
- Constant-time PKCS7 unpadding
- SQL identifier validation on aggregates
- Thread-safe integration registry

**Phase 39: Declarative Autonomous Agents**
- `defagent` macro: model, tools, triggers, memory, role in one form
- Auto-tool derivation from Reagent resources and integration specs
- Persistent agent memory with scope isolation
- Multi-trigger system: PubSub, schedule, HTTP, webhook
- Provider resolution: Claude Sonnet/Opus/Haiku, Ollama, Mock

### Added

**Phase 21: Declarative DDL Engine**
- `deftable` macro generating CREATE TABLE DDL from Lisp declarations

**Phase 22: Permissions & Audit**
- RBAC: owner > admin > member > viewer hierarchy
- Audit logging with filterable web viewer
- Input validation for email, URL, number, date, boolean

**Phase 23: Dynamic UI**
- Metadata-driven forms and tables — no hand-coded views per object

**Phase 24: CMS Template**
- 7 declarative tables, Markdown-to-HTML renderer

**Phase 25: Knowledge Base + LLM**
- KB articles, Claude API integration, answer generation

**Phase 26: Scry LiveView**
- Real-time components with auto-generated JavaScript

**Phase 27: Integration Tests**
- 21 cross-cutting tests across all platform modules

**Phase 28: Additional Templates**
- Project management, newsletter with subscriber management

**Phase 29: Hypermedia API**
- Self-documenting JSON API with _links, _actions, type-checked validation

**Phase 30: Static Site Gen + Islands**
- Zero-JS static sites with interactive islands pattern

**Phase 31: Schema-Level Relations**
- Cross-object entity graph with link/unlink

**Phase 32: Dual Agent Runtime**
- Agent tools, loop, CLI, web endpoints in application layer

**Phase 33: Customer Acquisition Pipeline**
- Custom domains, SEO integration, email, notifications, newsletters

## [0.4.0] — 2026-03-15

### Added

**Phase 19: Web Routes & Views**
- Application pipelines: browser, auth, company with full plug chains
- Auth views: login, signup with bcrypt verification
- Contacts + Deals CRUD with pagination
- Dark theme CSS (Linear-inspired)

## [0.3.0] — 2026-03-15

### Added

**Phase 16: CLI Enhancement**
- Progressive help, fuzzy matching, required option validation
- `emit-table` with overflow mode

**Phase 16B-C: Crucible Works App Scaffold**
- Platform tables: users, companies, company_members
- CLI: user/company create and list

**Phase 17: Custom Objects & CRM Template**
- `deftemplate` macro, metadata-driven tables
- CRM: contacts, companies, pipelines, stages, deals, activities

**Phase 18: Record CRUD & Queries**
- Generic record operations with filter/sort parsing
- CLI: record CRUD, link/unlink commands

**Phase 21: DB Admin CLI**
- `cauldron db migrate`, `rollback`, `status`
- Schema inspection: `schemas`, `tables`, `columns`

## [0.2.0] — 2026-03-14

### Added

**Phases 11-13: Multi-Schema & Auth**
- Per-tenant schema isolation with `SET search_path`
- Password hashing, session management, CSRF protection

**Phase 14: SEO**
- `<meta>` tag generation, sitemap XML, robots.txt

**Phase 15: CLI Infrastructure**
- `defcommand` macro, argument parsing, JSON/table output

## [0.1.0] — 2026-03-14

### Added

- Layer 0: Runtime (threads, sockets, queues, timer)
- Layer 1: Crypto (SHA, HMAC, bcrypt, PBKDF2, CSPRNG, base64)
- Layer 1: JSON (encoder/decoder)
- Layer 2: Oracle (conditions, hooks, recovery)
- Layer 2: HTTP/1.1 server with thread pool
- Layer 2: WebSocket RFC 6455
- Layer 2: PostgreSQL wire protocol v3
- Layer 3: Grimoire (query DSL, changesets, migrations)
- Layer 3: Alembic (HTML templates)
- Layer 3: Crucible (router, pipelines, plugs)
- Layer 4: Reagent (MOP resource system)
- Layer 5: Ether (PubSub)
- Layer 5: Scry (LiveView)
- Layer 6: Forge (admin interface)
- 817 tests, ~10,000 LOC

### Fixed
- diff-attrs reversed plist ordering
- audit.lisp package lock violation
- Recovery use-value not returning value
- Scry transport type dispatch
- Forge router let/let* binding
