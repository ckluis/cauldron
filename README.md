# Cauldron

> Build web applications with zero operational debt.

A full-stack Common Lisp framework that implements everything from scratch — HTTP server, PostgreSQL driver, cryptography, LiveView, admin interface — in pure SBCL with zero external dependencies. One binary. No build tools. No supply chain.

## The Pitch

Cauldron is what happens when you take the "no dependencies" philosophy seriously. Every byte of functionality — from SHA-256 hashing to WebSocket frame encoding to PostgreSQL wire protocol — is implemented in Common Lisp. The entire framework is auditable by one person, deployable as a single binary via `save-lisp-and-die`, and immune to the supply chain attacks, transitive dependency breakage, and operational complexity that plague modern web stacks.

This isn't a protest framework. It's a full-featured platform: declarative schema definitions generate your tables, a query DSL builds your SQL, a trie router dispatches your requests, a LiveView engine pushes real-time UI over WebSocket, and an admin interface materializes from your resource definitions. You get the developer experience of a mature framework without the hidden cost of understanding someone else's dependency tree.

Cauldron is for teams that value understanding over convenience — engineers who want to read the source of every function they call, deploy without Docker or Kubernetes, and ship applications that will compile unchanged in ten years.

## Quick Start

```bash
# Prerequisites: SBCL (Steel Bank Common Lisp)

# Clone and run tests
git clone https://github.com/ckluis/cauldron.git
cd cauldron
sbcl --script run-tests.lisp --unit

# Generate the marketing site (dogfoods Alembic)
sbcl --script site/generate.lisp
```

## What You Get

### Declare Your Schema, Get Everything

Define your tables in Lisp. Get DDL, migrations, metadata, and query integration — no hand-written SQL.

```lisp
(deftable contacts
  (:columns
    (name    :type :text :not-null t)
    (email   :type :text :not-null t :unique t)
    (company :type :text)
    (stage   :type :text :default "'lead'"))
  (:indexes
    (email-idx :columns (email) :unique t)))

;; Query with the Grimoire DSL
(grimoire:select :contacts
  (grimoire:where :like :name "%smith%")
  (grimoire:order-by :created_at :desc)
  (grimoire:limit 20))
;; → (values "SELECT ... WHERE name LIKE $1 ORDER BY ..." ("'%smith%"))
```

### Real-Time UI Without JavaScript

Scry is Cauldron's LiveView engine. Server-rendered HTML, diffed and patched over WebSocket. Write your UI in Lisp — the client JavaScript is auto-generated.

```lisp
(defscry counter (:count 0)
  ;; handle-event — server-side state update
  (:on "increment" (state)
    (list :count (1+ (getf state :count))))

  ;; render — produces HTML from state
  (:render (state)
    (html (:div
      (:h1 (format nil "Count: ~D" (getf state :count)))
      (:button :scry-click "increment" "Add one")))))
```

### Autonomous Agents That Understand Your App

Declarative agents with tool calling, memory, triggers, and budget controls. Agents share your RBAC — they can't exceed their declared role.

```lisp
(defagent support-bot
  (:model :claude-sonnet)
  (:role "Answer customer questions using the knowledge base")
  (:tools (from-resources :kb-articles)  ; auto-generated CRUD tools
          (from-integration :slack))      ; from integration spec
  (:triggers (:http "/api/chat")
             (:pubsub :new-ticket))
  (:memory :persistent)
  (:token-budget 4000))
```

### Admin Interface Writes Itself

Define your resources with Reagent. Forge auto-generates a complete admin interface — dashboard, CRUD, SQL console, schema browser, audit log, agent management. Zero configuration.

```lisp
(define-resource contact
  (:attributes name email company stage)
  (:policies (:allow :admin :all)
             (:allow :member :read :update)
             (:deny :viewer :delete)))

;; Forge serves: /forge/dashboard, /forge/contacts, /forge/sql, ...
```

### Industrial Cryptography, Zero Dependencies

Every algorithm implemented from the RFC. No FFI, no OpenSSL, no C libraries.

```lisp
(crypto:sha256 "hello")                    ; SHA-256 hash
(crypto:hmac-sha256 key message)           ; HMAC authentication
(crypto:hash-password "secret")            ; bcrypt (configurable cost)
(crypto:verify-password "secret" hash)     ; constant-time comparison
(crypto:aes-encrypt key plaintext)         ; AES-256-CBC with random IV
(crypto:generate-token)                    ; 64-char hex from /dev/urandom
```

### External APIs Without Boilerplate

Declarative integrations with encrypted credential storage, retry logic, health tracking, and webhook verification.

```lisp
(defintegration stripe
  (:base-url "https://api.stripe.com/v1")
  (:auth :bearer :key "stripe-api-key")
  (:endpoints
    (:create-charge :post "/charges"
      :params (amount currency source)))
  (:webhooks
    (:payment-received :events ("charge.succeeded"))))
```

## Architecture

8 layers, 21 modules, strict dependency ordering. Each layer depends only on layers below it.

```
Layer 7  CLI                          Command dispatch, fuzzy matching
Layer 6  Forge                        Auto-generated admin interface
Layer 5  Scry · Ether · Integration · Agent   LiveView, PubSub, external APIs, agents
Layer 4  Reagent                      MOP-based resource metaclass
Layer 3  Grimoire · Alembic · Crucible · LLM  Query DSL, HTML, router, AI
Layer 2  HTTP · WebSocket · DB        Server, RFC 6455, PostgreSQL wire protocol
Layer 1  Crypto · JSON · HTTP Client · Logging  SHA, HMAC, bcrypt, AES, base64
Layer 0  Runtime                      Threads, sockets, queues, timer
```

See [ARCHITECTURE.md](ARCHITECTURE.md) for the full module map and design patterns.

## Built With Cauldron

**[Crucible Works](https://github.com/ckluis/crucible-works)** — A multi-tenant business platform (CRM, CMS, knowledge base, billing, agents) built entirely on Cauldron. 69 source files, 540 tests. If you want to see what a real application looks like on this framework, start there.

## Stats

| Metric | Value |
|--------|-------|
| Lines of code | ~17,000 (framework) |
| Tests | 1,554 (unit) + integration |
| Modules | 21 across 8 layers |
| Dependencies | 0 |
| External C libraries | 0 |
| Build tools required | 0 |

## Why Not Rails / Django / Phoenix?

Those are excellent frameworks. Use them if you want a large ecosystem, abundant tutorials, and hiring-pool familiarity.

Choose Cauldron if you value:

- **Auditability** — Read every line of your stack. When something breaks at 3am, there's no mystery dependency six layers deep.
- **Stability** — No upstream breakage. No Dependabot alerts. No breaking changes in transitive dependencies you didn't know existed.
- **Deployment** — `save-lisp-and-die` produces a single binary. Copy it to a server. Run it. No containers, no orchestration, no runtime.
- **Learning** — Understand HTTP, PostgreSQL, WebSocket, and cryptography by reading working implementations, not documentation.

## License

MIT — see [LICENSE](LICENSE).
