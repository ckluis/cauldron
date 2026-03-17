;;;; site/generate.lisp — Generate Cauldron marketing landing page + changelog
;;;; Dogfoods Cauldron's own Alembic HTML engine.
;;;; Usage: sbcl --script site/generate.lisp

;;; --- Bootstrap: load Cauldron/core via ASDF ---

(require :asdf)

(defvar cl-user::*site-script-dir*
  (make-pathname :directory (pathname-directory *load-truename*)))

(push (truename (merge-pathnames "../" cl-user::*site-script-dir*))
      asdf:*central-registry*)

(asdf:load-system "cauldron/core")

;;; --- Own package to avoid CL symbol conflicts ---

(defpackage :cauldron.site
  (:use :cl))

(in-package :cauldron.site)

(defvar *script-dir* cl-user::*site-script-dir*)

;;; --- Module data ---

(defvar *site-modules*
  '((:name "Runtime"     :layer 0 :desc "Threads, sockets, queues, thread pool, timer, config, graceful shutdown")
    (:name "Crypto"      :layer 1 :desc "SHA-1/256, HMAC, bcrypt, PBKDF2, AES-256-CBC, CSPRNG, base64")
    (:name "JSON"        :layer 1 :desc "Streaming encoder and decoder")
    (:name "HTTP Client"  :layer 1 :desc "Outbound HTTP/HTTPS via curl subprocess")
    (:name "Logging"     :layer 1 :desc "Stripe-style canonical structured log lines")
    (:name "Oracle"      :layer 2 :desc "Condition system, hooks, recovery strategies")
    (:name "HTTP"        :layer 2 :desc "HTTP/1.1 server with thread-pool, multipart, gzip")
    (:name "WebSocket"   :layer 2 :desc "RFC 6455 frames, upgrade, fragmentation")
    (:name "DB"          :layer 2 :desc "PostgreSQL wire protocol v3, SCRAM-SHA-256, connection pooling")
    (:name "Grimoire"    :layer 3 :desc "Query DSL, changesets, migrations, deftable DDL, full-text search")
    (:name "Alembic"     :layer 3 :desc "S-expression HTML template engine with SEO helpers")
    (:name "Crucible"    :layer 3 :desc "Trie router, pipelines, auth, sessions, API keys, billing, uploads, security")
    (:name "LLM"         :layer 3 :desc "Provider protocol (Claude/Ollama/Mock), tool system, agent loop")
    (:name "Reagent"     :layer 4 :desc "MOP-based resource metaclass with policies and relationships")
    (:name "Ether"       :layer 5 :desc "PubSub with PostgreSQL LISTEN/NOTIFY bridge")
    (:name "Integration" :layer 5 :desc "Declarative external integrations, encrypted credentials, webhook delivery")
    (:name "Agent"       :layer 5 :desc "Declarative autonomous agents (defagent), A2A, tool caching, SSE")
    (:name "Scry"        :layer 5 :desc "LiveView engine with virtual DOM diff over WebSocket")
    (:name "Forge"       :layer 6 :desc "Auto-generated admin: CRUD, SQL console, schema browser, audit, agents")
    (:name "CLI"         :layer 7 :desc "Command dispatch, progressive help, fuzzy matching, DB admin")))

(defun mod-get (mod key)
  (getf mod key))

;;; ============================================================
;;; CSS — Complete redesign
;;; ============================================================

(defvar *css* "
/* Reset */
*,*::before,*::after{box-sizing:border-box;margin:0;padding:0}

/* Base */
body{
  background:#0a0a0f;color:#e8e6e0;
  font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Oxygen,sans-serif;
  line-height:1.7;font-size:16px;
}
a{color:#c4841d;text-decoration:none;transition:color 0.15s}
a:hover{color:#d9a044;text-decoration:underline}
code{
  font-family:'SF Mono',SFMono-Regular,Consolas,'Liberation Mono',Menlo,monospace;
  font-size:0.85em;background:#13131f;padding:2px 6px;border-radius:3px;color:#e8cc65;
}
.container{max-width:960px;margin:0 auto;padding:0 24px}

/* ---- Hero ---- */
.hero{text-align:center;padding:96px 0 48px}
.hero h1{
  font-size:clamp(2.8rem,7vw,5rem);font-weight:800;
  color:#c4841d;letter-spacing:-0.03em;margin-bottom:12px;
  line-height:1.1;
}
.hero .tagline{font-size:1.35rem;color:#b0ada6;margin-bottom:8px;font-weight:400}
.hero .subtitle{font-size:1rem;color:#706d66;max-width:520px;margin:0 auto}

/* CTAs */
.cta-row{display:flex;gap:16px;justify-content:center;margin-top:32px;flex-wrap:wrap}
.cta-primary{
  display:inline-flex;align-items:center;gap:8px;
  background:#c4841d;color:#0a0a0f;
  padding:12px 28px;border-radius:6px;font-weight:600;font-size:1rem;
  transition:background 0.15s,transform 0.1s;text-decoration:none;
}
.cta-primary:hover{background:#d9a044;transform:translateY(-1px);text-decoration:none;color:#0a0a0f}
.cta-ghost{
  display:inline-flex;align-items:center;gap:8px;
  border:1px solid #333;color:#b0ada6;
  padding:12px 28px;border-radius:6px;font-weight:500;font-size:1rem;
  transition:border-color 0.15s,color 0.15s;text-decoration:none;
}
.cta-ghost:hover{border-color:#c4841d;color:#e8e6e0;text-decoration:none}

/* ---- Stats bar ---- */
.stats{
  display:flex;justify-content:center;gap:48px;
  padding:32px 0;border-top:1px solid #1a1a2e;border-bottom:1px solid #1a1a2e;
  margin-bottom:80px;
}
.stat{text-align:center}
.stat .number{font-size:2rem;font-weight:700;color:#c4841d;display:block;line-height:1.2}
.stat .label{font-size:0.7rem;text-transform:uppercase;letter-spacing:0.12em;color:#706d66}

/* ---- Sections ---- */
.section{margin-bottom:80px}
.section h2{
  font-size:1.75rem;color:#e8e6e0;margin-bottom:8px;font-weight:700;
  letter-spacing:-0.01em;
}
.section .section-sub{font-size:1rem;color:#706d66;margin-bottom:32px;max-width:640px}

/* ---- Pitch ---- */
.pitch{max-width:720px;margin:0 auto 80px;text-align:center}
.pitch h2{font-size:1.75rem;color:#e8e6e0;margin-bottom:24px}
.pitch p{color:#b0ada6;margin-bottom:16px;font-size:1.05rem;text-align:left}
.pitch p:last-child{margin-bottom:0}

/* ---- Feature sections (alternating) ---- */
.feature{
  display:grid;grid-template-columns:1fr 1fr;gap:48px;
  align-items:start;margin-bottom:72px;
}
.feature:nth-child(even){direction:rtl}
.feature:nth-child(even) > *{direction:ltr}
.feature-prose h3{font-size:1.35rem;color:#e8e6e0;margin-bottom:12px;font-weight:600}
.feature-prose p{color:#b0ada6;font-size:0.95rem;margin-bottom:12px}
.feature-prose .benefit{
  color:#c4841d;font-size:0.9rem;font-weight:500;
  border-left:2px solid #c4841d;padding-left:12px;margin-top:16px;
}
.feature-code{
  background:#0c0c18;border:1px solid #1a1a2e;border-radius:6px;
  padding:20px 24px;overflow-x:auto;
}
.feature-code pre{margin:0}
.feature-code code{
  font-size:0.82rem;line-height:1.65;background:none;padding:0;
  color:#e8e6e0;display:block;white-space:pre;
}

/* Syntax highlighting (static classes) */
.kw{color:#c4841d}     /* keywords, macros */
.str{color:#6a9955}    /* strings */
.sym{color:#d4c87a}    /* symbols */
.cmt{color:#555;font-style:italic} /* comments */
.fn{color:#dcdcaa}     /* function names */
.type{color:#4ec9b0}   /* types */

/* ---- Architecture stack ---- */
.arch-stack{display:flex;flex-direction:column;gap:2px;max-width:720px;margin:0 auto}
.arch-layer{
  display:flex;align-items:center;padding:14px 20px;
  border-radius:4px;position:relative;
  transition:transform 0.15s ease;
}
.arch-layer:hover{transform:translateX(4px)}
.arch-layer-num{
  font-size:0.65rem;font-weight:700;text-transform:uppercase;letter-spacing:0.1em;
  opacity:0.5;width:56px;flex-shrink:0;
}
.arch-layer-name{font-size:0.95rem;font-weight:600;flex:1}
.arch-layer-modules{font-size:0.8rem;opacity:0.7;text-align:right}
.arch-l0{background:#12100a;color:#8a7240;border-left:3px solid #8a7240}
.arch-l1{background:#13110b;color:#9a8545;border-left:3px solid #9a8545}
.arch-l2{background:#14120c;color:#aa924a;border-left:3px solid #aa924a}
.arch-l3{background:#15130d;color:#b89e50;border-left:3px solid #b89e50}
.arch-l4{background:#16140e;color:#c4a855;border-left:3px solid #c4a855}
.arch-l5{background:#17150f;color:#d0b45a;border-left:3px solid #d0b45a}
.arch-l6{background:#181610;color:#dcc060;border-left:3px solid #dcc060}
.arch-l7{background:#191711;color:#e8cc65;border-left:3px solid #e8cc65}

/* ---- Philosophy ---- */
.philosophy{display:grid;grid-template-columns:1fr 1fr;gap:32px}
.phil-item h3{font-size:1.1rem;color:#e8e6e0;margin-bottom:8px}
.phil-item p{font-size:0.9rem;color:#b0ada6}

/* ---- Quick start ---- */
.quickstart{
  background:#0c0c18;border:1px solid #1a1a2e;border-radius:6px;
  padding:28px 32px;max-width:640px;margin:0 auto;
}
.quickstart code{
  font-size:0.85rem;line-height:1.8;display:block;white-space:pre;
  background:none;padding:0;color:#e8e6e0;
}

/* ---- Built-with showcase ---- */
.showcase{
  background:#0c0c18;border:1px solid #1a1a2e;border-radius:6px;
  padding:32px;display:flex;gap:32px;align-items:center;
}
.showcase-text h3{font-size:1.25rem;color:#e8e6e0;margin-bottom:8px}
.showcase-text p{font-size:0.9rem;color:#b0ada6;margin-bottom:12px}
.showcase-text .showcase-stats{font-size:0.8rem;color:#706d66}
.showcase-text a{font-weight:500}

/* ---- Module grid ---- */
.module-grid{
  display:grid;grid-template-columns:repeat(auto-fill,minmax(280px,1fr));gap:12px;
}
.module-card{
  background:#0c0c18;border:1px solid #1a1a2e;border-radius:4px;padding:16px 20px;
}
.module-card h3{font-size:0.95rem;color:#e8e6e0;margin-bottom:4px}
.module-card p{font-size:0.82rem;color:#8a8880}
.layer-badge{
  display:inline-block;font-size:0.65rem;padding:2px 8px;border-radius:3px;
  margin-bottom:6px;font-weight:600;
}
.layer-0{background:#1a1510;color:#8a7240}
.layer-1{background:#1a1812;color:#9a8545}
.layer-2{background:#1c1a12;color:#aa924a}
.layer-3{background:#1e1c12;color:#b89e50}
.layer-4{background:#201e12;color:#c4a855}
.layer-5{background:#222012;color:#d0b45a}
.layer-6{background:#242212;color:#dcc060}
.layer-7{background:#262412;color:#e8cc65}

/* ---- Benchmarks ---- */
.bench-table{width:100%;border-collapse:collapse}
.bench-table th{
  text-align:left;font-size:0.75rem;text-transform:uppercase;letter-spacing:0.08em;
  color:#706d66;padding:8px 12px;border-bottom:1px solid #1a1a2e;
}
.bench-table td{
  padding:10px 12px;font-size:0.9rem;color:#b0ada6;border-bottom:1px solid #0f0f1a;
}
.bench-table td:first-child{color:#e8e6e0;font-weight:500}
.bench-table td code{font-size:0.8rem}

/* ---- Footer ---- */
.footer{
  text-align:center;padding:48px 0;
  border-top:1px solid #1a1a2e;color:#706d66;font-size:0.85rem;
}
.footer p{margin-bottom:8px}
.footer a{color:#c4841d}

/* ---- Responsive ---- */
@media(max-width:768px){
  .hero{padding:64px 0 32px}
  .stats{flex-direction:column;gap:16px;align-items:center}
  .feature{grid-template-columns:1fr;gap:24px}
  .feature:nth-child(even){direction:ltr}
  .philosophy{grid-template-columns:1fr}
  .showcase{flex-direction:column}
  .module-grid{grid-template-columns:1fr}
  .arch-layer{flex-direction:column;align-items:flex-start;gap:2px;padding:10px 16px}
  .arch-layer-modules{text-align:left}
}
")

;;; ============================================================
;;; Architecture layer data
;;; ============================================================

(defvar *architecture-layers*
  '((:num 7 :name "CLI"      :modules "Command dispatch, fuzzy matching, DB admin")
    (:num 6 :name "Forge"    :modules "Auto-generated admin interface + agent dashboard")
    (:num 5 :name "Scry + Ether + Integration + Agent"  :modules "LiveView, PubSub, external APIs, autonomous agents")
    (:num 4 :name "Reagent"  :modules "MOP resource metaclass, policies, relationships")
    (:num 3 :name "Grimoire + Alembic + Crucible + LLM"  :modules "Query DSL, HTML templates, trie router, AI provider protocol")
    (:num 2 :name "HTTP + WebSocket + DB"  :modules "Server, RFC 6455, PostgreSQL wire protocol v3")
    (:num 1 :name "Crypto + JSON + HTTP Client + Logging"  :modules "SHA, HMAC, bcrypt, AES-256, base64, JSON, outbound HTTP, canonical logs")
    (:num 0 :name "Runtime"  :modules "Threads, sockets, queues, thread pool, timer, config, shutdown")))

;;; ============================================================
;;; Generate landing page
;;; ============================================================

(defvar *output*
  (cauldron.alembic:html
    (cauldron.alembic:raw "<!DOCTYPE html>")
    (:html :lang "en"
      (:head
        (:meta :charset "utf-8")
        (:meta :name "viewport" :content "width=device-width, initial-scale=1")
        (:title "Cauldron — Zero-dependency Common Lisp Web Framework")
        (:meta :name "description" :content "A full-stack Common Lisp framework. One binary. No dependencies. No build tools. HTTP server, PostgreSQL driver, cryptography, LiveView, and admin interface — all from scratch.")
        (:style (cauldron.alembic:raw *css*)))
      (:body
        (:div :class "container"

          ;; ===== HERO =====
          (:div :class "hero"
            (:h1 "Build web applications with zero operational debt")
            (:p :class "tagline" "A full-stack Common Lisp framework. One binary. No dependencies. No build tools.")
            (:p :class "subtitle" "HTTP server, PostgreSQL driver, cryptography, LiveView, admin interface — everything from scratch in pure SBCL.")
            (:div :class "cta-row"
              (:a :class "cta-primary" :href "#quickstart" "Get Started")
              (:a :class "cta-ghost" :href "https://github.com/ckluis/cauldron" "View on GitHub")))

          ;; ===== STATS BAR =====
          (:div :class "stats"
            (:div :class "stat"
              (:span :class "number" "0")
              (:span :class "label" "Dependencies"))
            (:div :class "stat"
              (:span :class "number" "~17,000")
              (:span :class "label" "Lines of Code"))
            (:div :class "stat"
              (:span :class "number" "21")
              (:span :class "label" "Modules"))
            (:div :class "stat"
              (:span :class "number" "1,554")
              (:span :class "label" "Tests"))
            (:div :class "stat"
              (:span :class "number" "1")
              (:span :class "label" "Binary")))

          ;; ===== THE PITCH =====
          (:div :class "pitch"
            (:h2 "What Is Cauldron?")
            (:p "Cauldron is a full-stack web framework that implements everything from scratch in Common Lisp — HTTP server, PostgreSQL wire protocol, WebSocket, cryptography, query DSL, template engine, LiveView, and admin interface. Every function you call, you can read the source of.")
            (:p "This matters because modern web stacks are built on towers of dependencies you've never audited. One compromised package, one breaking change in a transitive dependency, one abandoned library — and your production system is at risk. Cauldron eliminates this entire class of problem. There is no supply chain.")
            (:p "Cauldron is for teams that value understanding over convenience — engineers who want to deploy a single binary with " (:code "save-lisp-and-die") ", audit every line of their stack, and ship applications that will compile unchanged in ten years."))

          ;; ===== FEATURE DEEP-DIVES =====
          (:div :class "section"
            (:h2 "What You Get")
            (:p :class "section-sub" "Six frameworks worth of functionality. One dependency: SBCL.")

            ;; Feature 1: Grimoire + deftable
            (:div :class "feature"
              (:div :class "feature-prose"
                (:h3 "Declare Your Schema, Get Everything")
                (:p "Define tables in Lisp with " (:code "deftable") ". Get CREATE TABLE DDL, metadata registration, index generation, and seed data — no hand-written SQL anywhere in the stack.")
                (:p "Query with Grimoire's immutable builder DSL. Compose clauses, get parameterized SQL back. Type-safe, injection-proof, readable.")
                (:div :class "benefit" "One declaration drives DDL, migrations, admin UI, API validation, and agent tools."))
              (:div :class "feature-code"
                (:pre (:code
                  (cauldron.alembic:raw
"<span class=kw>(deftable</span> <span class=fn>contacts</span>
  (<span class=sym>:columns</span>
    (name    <span class=sym>:type</span> <span class=type>:text</span> <span class=sym>:not-null</span> t)
    (email   <span class=sym>:type</span> <span class=type>:text</span> <span class=sym>:not-null</span> t <span class=sym>:unique</span> t)
    (company <span class=sym>:type</span> <span class=type>:text</span>)
    (stage   <span class=sym>:type</span> <span class=type>:text</span> <span class=sym>:default</span> <span class=str>\"'lead'\"</span>))
  (<span class=sym>:indexes</span>
    (email-idx <span class=sym>:columns</span> (email) <span class=sym>:unique</span> t)))

<span class=cmt>;; Query with the Grimoire DSL</span>
(<span class=kw>grimoire:select</span> <span class=sym>:contacts</span>
  (grimoire:where <span class=sym>:like</span> <span class=sym>:name</span> <span class=str>\"%smith%\"</span>)
  (grimoire:order-by <span class=sym>:created_at</span> <span class=sym>:desc</span>)
  (grimoire:limit 20))")))))

            ;; Feature 2: Scry LiveView
            (:div :class "feature"
              (:div :class "feature-prose"
                (:h3 "Real-Time UI Without JavaScript")
                (:p "Scry is Cauldron's LiveView engine. Write your UI as server-side Lisp components. State changes are diffed on the server and patched on the client over WebSocket.")
                (:p "No React. No build step. No JavaScript framework. The ~180-line client runtime is auto-generated. You write Lisp; your users get real-time interactivity.")
                (:div :class "benefit" "Interactive UIs with server-side state. Debug in one language. Ship in one binary."))
              (:div :class "feature-code"
                (:pre (:code
                  (cauldron.alembic:raw
"<span class=kw>(defscry</span> <span class=fn>counter</span> (<span class=sym>:count</span> 0)
  <span class=cmt>;; Server-side state update</span>
  (<span class=sym>:on</span> <span class=str>\"increment\"</span> (state)
    (list <span class=sym>:count</span> (1+ (getf state <span class=sym>:count</span>))))

  <span class=cmt>;; Renders HTML from state</span>
  (<span class=sym>:render</span> (state)
    (<span class=kw>html</span>
      (<span class=sym>:div</span>
        (<span class=sym>:h1</span> (format nil <span class=str>\"Count: ~D\"</span>
                   (getf state <span class=sym>:count</span>)))
        (<span class=sym>:button</span> <span class=sym>:scry-click</span> <span class=str>\"increment\"</span>
          <span class=str>\"Add one\"</span>)))))")))))

            ;; Feature 3: Agent
            (:div :class "feature"
              (:div :class "feature-prose"
                (:h3 "Autonomous Agents That Understand Your App")
                (:p (:code "defagent") " declares an agent in one form: model, tools, triggers, memory, role, and token budget. Tools are auto-derived from your Reagent resources and integration specs.")
                (:p "Agents share your RBAC pipeline — they can't exceed their declared role. Memory is persistent and scope-isolated. A2A delegation has cycle detection at depth 5.")
                (:div :class "benefit" "LLM agents that respect your authorization model and survive restarts."))
              (:div :class "feature-code"
                (:pre (:code
                  (cauldron.alembic:raw
"<span class=kw>(defagent</span> <span class=fn>support-bot</span>
  (<span class=sym>:model</span> <span class=type>:claude-sonnet</span>)
  (<span class=sym>:role</span> <span class=str>\"Answer questions using the KB\"</span>)
  (<span class=sym>:tools</span>
    (<span class=kw>from-resources</span> <span class=sym>:kb-articles</span>)
    (<span class=kw>from-integration</span> <span class=sym>:slack</span>))
  (<span class=sym>:triggers</span>
    (<span class=sym>:http</span> <span class=str>\"/api/chat\"</span>)
    (<span class=sym>:pubsub</span> <span class=sym>:new-ticket</span>))
  (<span class=sym>:memory</span> <span class=sym>:persistent</span>)
  (<span class=sym>:token-budget</span> 4000))")))))

            ;; Feature 4: Forge
            (:div :class "feature"
              (:div :class "feature-prose"
                (:h3 "Admin Interface Writes Itself")
                (:p "Define resources with Reagent's MOP metaclass. Forge auto-generates a complete admin interface — dashboard, paginated CRUD, SQL console, schema browser, audit log, integration health, and agent management.")
                (:p "No templates to write. No admin routes to configure. Add a resource, get an admin panel.")
                (:div :class "benefit" "Ship an admin interface on day one. Focus on your actual product."))
              (:div :class "feature-code"
                (:pre (:code
                  (cauldron.alembic:raw
"<span class=kw>(define-resource</span> <span class=fn>contact</span>
  (<span class=sym>:attributes</span> name email company stage)
  (<span class=sym>:policies</span>
    (<span class=sym>:allow</span> <span class=type>:admin</span>  <span class=sym>:all</span>)
    (<span class=sym>:allow</span> <span class=type>:member</span> <span class=sym>:read</span> <span class=sym>:update</span>)
    (<span class=sym>:deny</span>  <span class=type>:viewer</span> <span class=sym>:delete</span>)))

<span class=cmt>;; Forge auto-generates:</span>
<span class=cmt>;;   /forge/dashboard</span>
<span class=cmt>;;   /forge/contacts (list/show/new/edit)</span>
<span class=cmt>;;   /forge/sql</span>
<span class=cmt>;;   /forge/schema</span>
<span class=cmt>;;   /forge/agents</span>")))))

            ;; Feature 5: Crypto
            (:div :class "feature"
              (:div :class "feature-prose"
                (:h3 "Industrial Cryptography, Zero Dependencies")
                (:p "Every algorithm implemented from the RFC in pure Common Lisp. SHA-1, SHA-256, HMAC, bcrypt with configurable cost, PBKDF2 key derivation, AES-256-CBC with PKCS7 padding, and CSPRNG from " (:code "/dev/urandom") ".")
                (:p "Constant-time comparison prevents timing attacks. AES key expansion is hoisted for performance. NIST known-vector validated.")
                (:div :class "benefit" "No OpenSSL. No FFI. Audit the crypto you ship."))
              (:div :class "feature-code"
                (:pre (:code
                  (cauldron.alembic:raw
"<span class=cmt>;; Hash</span>
(crypto:<span class=fn>sha256</span> <span class=str>\"hello\"</span>)
(crypto:<span class=fn>hmac-sha256</span> key message)

<span class=cmt>;; Passwords</span>
(crypto:<span class=fn>hash-password</span> <span class=str>\"secret\"</span>)    <span class=cmt>; bcrypt</span>
(crypto:<span class=fn>verify-password</span> <span class=str>\"secret\"</span> hash)

<span class=cmt>;; Encryption</span>
(crypto:<span class=fn>aes-encrypt</span> key plaintext)  <span class=cmt>; AES-256-CBC</span>
(crypto:<span class=fn>aes-decrypt</span> key ciphertext)

<span class=cmt>;; Tokens</span>
(crypto:<span class=fn>generate-token</span>)  <span class=cmt>; 64-char hex</span>")))))

            ;; Feature 6: Integration
            (:div :class "feature"
              (:div :class "feature-prose"
                (:h3 "External APIs Without Boilerplate")
                (:p (:code "defintegration") " declares external API endpoints, auth patterns, and webhook handlers in one form. Credentials are AES-256 encrypted with PBKDF2 key derivation and cascading scope resolution.")
                (:p "Built-in retry with exponential backoff, per-integration rate limiting, health tracking, and HMAC-SHA256 webhook verification.")
                (:div :class "benefit" "First-class external integrations. Encrypted credentials. Observable health."))
              (:div :class "feature-code"
                (:pre (:code
                  (cauldron.alembic:raw
"<span class=kw>(defintegration</span> <span class=fn>stripe</span>
  (<span class=sym>:base-url</span> <span class=str>\"https://api.stripe.com/v1\"</span>)
  (<span class=sym>:auth</span> <span class=sym>:bearer</span> <span class=sym>:key</span> <span class=str>\"stripe-api-key\"</span>)
  (<span class=sym>:endpoints</span>
    (<span class=sym>:create-charge</span> <span class=sym>:post</span> <span class=str>\"/charges\"</span>
      <span class=sym>:params</span> (amount currency source)))
  (<span class=sym>:webhooks</span>
    (<span class=sym>:payment-received</span>
      <span class=sym>:events</span> (<span class=str>\"charge.succeeded\"</span>))))")))))) ;; closes feature 6 + section

          ;; ===== ARCHITECTURE =====
          (:div :class "section"
            (:h2 "Architecture")
            (:p :class "section-sub" "Eight layers. Each depends only on layers below it. No upward or lateral dependencies.")
            (:div :class "arch-stack"
              (cauldron.alembic:for-each (layer *architecture-layers*)
                (let ((n (mod-get layer :num)))
                  `(:div :class ,(format nil "arch-layer arch-l~D" n)
                     (:span :class "arch-layer-num" ,(format nil "Layer ~D" n))
                     (:span :class "arch-layer-name" ,(mod-get layer :name))
                     (:span :class "arch-layer-modules" ,(mod-get layer :modules)))))))

          ;; ===== WHY CAULDRON? =====
          (:div :class "section"
            (:h2 "Why Cauldron?")
            (:p :class "section-sub" "Philosophy, not feature comparison.")
            (:div :class "philosophy"
              (:div :class "phil-item"
                (:h3 "Auditability")
                (:p "Read every line of your stack. When something breaks at 3am, there's no mystery dependency six layers deep. You wrote it, or you can read what did."))
              (:div :class "phil-item"
                (:h3 "Stability")
                (:p "No upstream breakage. No Dependabot alerts. No breaking changes in transitive dependencies you didn't know existed. Your code compiles the same in ten years."))
              (:div :class "phil-item"
                (:h3 "Deployment")
                (:p (:code "save-lisp-and-die") " produces a single binary. Copy it to a server. Run it. No containers, no orchestration, no runtime to install."))
              (:div :class "phil-item"
                (:h3 "Learning")
                (:p "Understand HTTP, PostgreSQL, WebSocket, and cryptography by reading working implementations — not documentation about abstractions over abstractions."))))

          ;; ===== QUICK START =====
          (:div :class "section" :id "quickstart"
            (:h2 "Quick Start")
            (:div :class "quickstart"
              (:code
                (cauldron.alembic:raw
"<span class=cmt># Prerequisites: SBCL (Steel Bank Common Lisp)</span>

git clone https://github.com/ckluis/cauldron.git
cd cauldron
sbcl --script run-tests.lisp --unit    <span class=cmt># 1,554 tests</span>
sbcl --script site/generate.lisp       <span class=cmt># dogfoods Alembic</span>"))))

          ;; ===== BUILT WITH CAULDRON =====
          (:div :class "section"
            (:h2 "Built With Cauldron")
            (:div :class "showcase"
              (:div :class "showcase-text"
                (:h3 (:a :href "https://github.com/ckluis/crucible-works" "Crucible Works"))
                (:p "A multi-tenant business platform — CRM, CMS, knowledge base, billing, email, newsletters, autonomous agents — built entirely on Cauldron. The reference implementation that proves the framework works at application scale.")
                (:p :class "showcase-stats" "69 source files · 540 tests · Multi-tenant with per-company schemas")
                (:p (:a :href "https://github.com/ckluis/crucible-works" "View repository")))))

          ;; ===== MODULE REFERENCE =====
          (:div :class "section"
            (:h2 "All 21 Modules")
            (:div :class "module-grid"
              (cauldron.alembic:for-each (mod *site-modules*)
                (let ((layer (mod-get mod :layer)))
                  `(:div :class "module-card"
                     (:span :class ,(format nil "layer-badge layer-~D" layer)
                       ,(format nil "Layer ~D" layer))
                     (:h3 ,(mod-get mod :name))
                     (:p ,(mod-get mod :desc)))))))

          ;; ===== FOOTER =====
          (:div :class "footer"
            (:p (:a :href "https://github.com/ckluis/cauldron" "GitHub")
                " · "
                (:a :href "/changelog/" "Changelog")
                " · MIT License")
            (:p "This site was generated by Cauldron's own Alembic engine — the proof is in the source.")))))))

;;; --- Write landing page ---

(let ((output-path (merge-pathnames "index.html" *script-dir*)))
  (with-open-file (stream output-path
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create
                   :external-format :utf-8)
    (write-string *output* stream))
  (format t "~&Generated ~A (~D bytes)~%" output-path (length *output*)))

;;; ============================================================
;;; Changelog: data-driven multi-page generation
;;; ============================================================
;;;
;;; Each release is a plist with :version, :date, :slug, :summary, and :content.
;;; :content is the Alembic HTML for the detail page body.
;;; URL structure: changelog/index.html, changelog/{slug}/index.html
;;;

;;; --- Changelog page CSS ---

(defvar *changelog-css* "
*,*::before,*::after{box-sizing:border-box;margin:0;padding:0}
body{
  background:#0a0a0f;color:#e8e6e0;
  font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Oxygen,sans-serif;
  line-height:1.6;
}
a{color:#c4841d;text-decoration:none}
a:hover{text-decoration:underline}
.container{max-width:960px;margin:0 auto;padding:0 24px}

/* Header */
.changelog-header{text-align:center;padding:64px 0 48px;border-bottom:1px solid #1a1a2e;margin-bottom:48px}
.changelog-header h1{font-size:2.5rem;font-weight:700;color:#c4841d;letter-spacing:-0.02em;margin-bottom:8px}
.changelog-header .subtitle{font-size:1rem;color:#706d66}
.changelog-header .back{display:inline-block;margin-bottom:24px;font-size:0.9rem;color:#706d66}
.changelog-header .back:hover{color:#c4841d}

/* Release list (index) */
.release-list{list-style:none;padding:0}
.release-item{
  display:flex;align-items:baseline;gap:16px;
  padding:20px 0;border-bottom:1px solid #1a1a2e;
}
.release-item:first-child{padding-top:0}
.release-item .version-link{
  font-size:1.25rem;font-weight:700;color:#e8e6e0;
  min-width:80px;flex-shrink:0;
}
.release-item .version-link:hover{color:#c4841d}
.release-item .summary{font-size:0.95rem;color:#b0ada6;flex:1}
.release-item .date{font-size:0.85rem;color:#706d66;flex-shrink:0}

/* Version block (detail pages) */
.version{margin-bottom:56px}
.version-heading{
  display:flex;align-items:baseline;gap:16px;
  margin-bottom:24px;padding-bottom:12px;border-bottom:1px solid #1a1a2e;
}
.version-heading h2{font-size:1.75rem;font-weight:700;color:#e8e6e0}
.version-heading .date{font-size:0.9rem;color:#706d66}

/* Category */
.category{margin-bottom:32px}
.category h3{
  font-size:1.1rem;font-weight:600;color:#c4841d;
  margin-bottom:16px;text-transform:uppercase;letter-spacing:0.05em;
}

/* Layer group */
.layer-group{margin-bottom:20px}
.layer-group h4{font-size:0.95rem;font-weight:600;color:#b0ada6;margin-bottom:8px}
.layer-group ul{list-style:none;padding-left:0}
.layer-group li{
  position:relative;padding-left:20px;margin-bottom:6px;
  font-size:0.9rem;color:#b0ada6;
}
.layer-group li::before{
  content:'';position:absolute;left:0;top:10px;
  width:6px;height:6px;border-radius:50%;background:#c4841d;
}
.layer-group li code{
  font-family:'SF Mono',SFMono-Regular,Consolas,'Liberation Mono',Menlo,monospace;
  font-size:0.8rem;background:#1a1a2e;padding:1px 6px;border-radius:3px;color:#e8e6e0;
}

/* Phase badge */
.phase{
  display:inline-block;font-size:0.65rem;padding:1px 6px;border-radius:3px;
  background:#1a1510;color:#8a7240;margin-left:8px;font-weight:600;vertical-align:middle;
}

/* Fix items */
.fix-group{margin-bottom:20px}
.fix-group h4{font-size:0.95rem;font-weight:600;color:#b0ada6;margin-bottom:8px}
.fix-group ul{list-style:none;padding-left:0}
.fix-group li{
  position:relative;padding-left:20px;margin-bottom:6px;
  font-size:0.9rem;color:#b0ada6;
}
.fix-group li::before{
  content:'';position:absolute;left:0;top:10px;
  width:6px;height:6px;border-radius:50%;background:#706d66;
}
.fix-group li code{
  font-family:'SF Mono',SFMono-Regular,Consolas,'Liberation Mono',Menlo,monospace;
  font-size:0.8rem;background:#1a1a2e;padding:1px 6px;border-radius:3px;color:#e8e6e0;
}

/* Nav between releases */
.release-nav{
  display:flex;justify-content:space-between;
  padding:24px 0;margin-top:32px;border-top:1px solid #1a1a2e;
}
.release-nav a{font-size:0.9rem;color:#706d66}
.release-nav a:hover{color:#c4841d}

/* Footer */
.footer{
  text-align:center;padding:48px 0;
  border-top:1px solid #1a1a2e;color:#706d66;font-size:0.85rem;
}
.footer p{margin-bottom:8px}
.footer a{color:#c4841d}

@media(max-width:640px){
  .changelog-header{padding:40px 0 32px}
  .changelog-header h1{font-size:2rem}
  .version-heading{flex-direction:column;gap:4px}
  .release-item{flex-direction:column;gap:4px}
}
")

;;; --- Release registry ---
;;; Each release: (:version V :date D :slug S :summary TEXT :content (alembic-body))
;;; Content is the inner body (categories/layer-groups) for the detail page.

(defvar *releases*
  (list
    ;; Newest first
    (list :version "0.5.0" :date "2026-03-15"
          :slug "0-5-0-multi-business"
          :summary "Multi-business platform — declarative DDL, RBAC, dynamic UI, CMS, knowledge base + LLM API"
          :content
          (cauldron.alembic:html
            (:div :class "category"
              (:h3 "Refactored")
              (:div :class "layer-group"
                (:h4 "Core Framework Refactoring" (:span :class "phase" "Phase 34"))
                (:ul
                  (:li "Moved 17 generic utilities from app layer to framework: things every Cauldron app needs now live in core")
                  (:li "New " (:code "cauldron.http-client") " module: " (:code "http-post-json") " for outbound HTTP via curl subprocess")
                  (:li "New " (:code "cauldron.llm") " module: provider protocol (Claude, Ollama, Mock), tool system, conversation CRUD, " (:code "agent-turn") " loop")
                  (:li (:code "cauldron.runtime:ht") " and " (:code "ht-get") " — hash-table builder + polymorphic getter (works on hash-tables and alists)")
                  (:li "API response helpers moved to " (:code "cauldron.crucible") ": " (:code "json-response") ", " (:code "json-error") ", " (:code "wrap-api-response") ", " (:code "role-allows-p"))
                  (:li "Generic plugs moved to " (:code "cauldron.crucible") ": " (:code "plug-json-content-type") ", " (:code "plug-html-content-type") ", " (:code "redirect-with-flash"))
                  (:li "Module count: 15 → 17 | Test count: 1,446 → 1,487 (41 new tests)")))
              (:div :class "layer-group"
                (:h4 "Framework Hardening" (:span :class "phase" "Phase 35"))
                (:ul
                  (:li "Centralized config: " (:code "get-env") ", " (:code "defconfig") " macro, " (:code "validate-config") ", " (:code "config-summary") " with secret masking")
                  (:li "Request logging: " (:code "plug-request-log") " + " (:code "log-request-completion") " with method/path/status/duration")
                  (:li "CORS middleware: " (:code "make-plug-cors") " with preflight handling, origin matching, credentials, expose-headers")
                  (:li "Rate limiting: " (:code "make-plug-rate-limit") " per-IP tracking with sliding window, 429 + Retry-After")
                  (:li "Response compression: pure-CL DEFLATE/gzip (RFC 1951/1952), " (:code "compress-response") " plug with content-type awareness")
                  (:li "Test count: 1,487 → 1,531 (44 new tests) | LOC: ~20,700 → ~21,200")))
              (:div :class "layer-group"
                (:h4 "User Lifecycle" (:span :class "phase" "Phase 36"))
                (:ul
                  (:li "Password reset flow: forgot-password → email with token → reset form → password update")
                  (:li "Email verification: verification on signup, resend link, soft enforcement via dashboard banner")
                  (:li "User settings page: profile editing, password change")
                  (:li "Team invitations: admin invite → email → accept/signup flow")
                  (:li "Test count: 1,531 → 1,584 (53 new tests) | LOC: ~21,200 → ~22,200")))
              (:div :class "layer-group"
                (:h4 "Canonical Logging + HTTP Client" (:span :class "phase" "Phase 37"))
                (:ul
                  (:li "New " (:code "cauldron.logging") " module: Stripe-style canonical log lines")
                  (:li "Enhanced HTTP client: GET/POST/PUT/PATCH/DELETE with timeout")
                  (:li "Bridge integration: auto request_id generation, canonical log emission per request")
                  (:li "Test count: 1,584 → 1,633 (49 new tests) | LOC: ~22,200 → ~22,600")))
              (:div :class "layer-group"
                (:h4 "Integration Protocol" (:span :class "phase" "Phase 38"))
                (:ul
                  (:li "New " (:code "cauldron.integration") " module: declarative " (:code "defintegration") " macro")
                  (:li "Full AES-256-CBC encryption with NIST known-vector validation")
                  (:li "Encrypted credential store with cascading scope resolution")
                  (:li "Integration client with retry, rate limiting, health tracking")
                  (:li "Webhook verification with HMAC-SHA256")
                  (:li "Test count: 1,633 → 1,668 (35 new tests) | Modules: 18 → 19")))
              (:div :class "layer-group"
                (:h4 "Declarative Autonomous Agents" (:span :class "phase" "Phase 39"))
                (:ul
                  (:li (:code "defagent") " macro: model, tools, triggers, memory, role in one form")
                  (:li "Auto-tool derivation from Reagent resources and integration specs")
                  (:li "Persistent agent memory with scope isolation")
                  (:li "Multi-trigger system: PubSub, schedule, HTTP, webhook")
                  (:li "Test count: 1,689 → 1,757 (68 new tests) | Modules: 19 → 20")))
              (:div :class "layer-group"
                (:h4 "Agent Hardening" (:span :class "phase" "Phase 39B"))
                (:ul
                  (:li "Token budgets, A2A delegation with cycle detection, tool caching with TTL")
                  (:li "SSE streaming, Forge agent dashboard, CLI agent commands")
                  (:li "Test count: 1,757 → 1,799 (42 new tests) | Modules: 20 → 21")))
              (:div :class "layer-group"
                (:h4 "API Keys + Webhook Delivery + Billing + Uploads + Search + Activity + Security" (:span :class "phase" "Phases 40–45"))
                (:ul
                  (:li "API key generation with SHA-256 hashing, scope-based access control")
                  (:li "Webhook delivery with HMAC signing and exponential backoff")
                  (:li "Subscription billing: state machine, metered usage, 402 gating")
                  (:li "File uploads with UUID paths, MIME validation, polymorphic attachments")
                  (:li "PostgreSQL full-text search: tsvector, GIN indexes, cross-table search")
                  (:li "Activity feed: actor/action/target tracking with PubSub integration")
                  (:li "Security: CSP, HSTS, IP allowlist, request size limits, session fixation protection")
                  (:li "Test count: 1,799 → 1,970 (171 new tests)"))))
            (:div :class "category"
              (:h3 "Added")
              (:div :class "layer-group"
                (:h4 "Application Platform" (:span :class "phase" "Phases 21–33"))
                (:ul
                  (:li (:code "deftable") " macro: declarative table definitions generating CREATE TABLE DDL")
                  (:li "RBAC: owner > admin > member > viewer hierarchy")
                  (:li "Metadata-driven forms and tables — no hand-coded views per object")
                  (:li "CMS template with Markdown rendering, CRM template, ecommerce, project management")
                  (:li "Knowledge base + LLM integration with context building")
                  (:li "Scry LiveView: client JS, real-time components, PubSub wiring")
                  (:li "Self-documenting hypermedia API with _links and _actions")
                  (:li "Static site generation with islands architecture")
                  (:li "Schema-level object relations with cascading deletes")
                  (:li "Dual agent runtime: platform (CLI) and tenant (web, RBAC-enforced)")
                  (:li "Customer acquisition: domains, SEO, email, notifications, newsletters"))))))

    (list :version "0.4.0" :date "2026-03-15"
          :slug "0-4-0-web-ui"
          :summary "Web UI — routes, pipelines, layouts, auth views, CRM CRUD, company context, inline CSS"
          :content
          (cauldron.alembic:html
            (:div :class "category"
              (:h3 "Added")
              (:div :class "layer-group"
                (:h4 "Web Routes & Views" (:span :class "phase" "Phase 19"))
                (:ul
                  (:li "Application pipelines: " (:code ":browser") ", " (:code ":auth") ", " (:code ":company") " with full plug chains")
                  (:li "Auth views: login, signup with bcrypt verification")
                  (:li "Contacts + Deals CRUD with pagination")
                  (:li "Dark theme CSS (Linear-inspired)")
                  (:li "Test count: 952 → 1,005 (53 new tests)"))))))

    (list :version "0.3.0" :date "2026-03-15"
          :slug "0-3-0-crucible-works"
          :summary "Crucible Works application — CLI, app scaffold, CRM template, record CRUD, DB admin"
          :content
          (cauldron.alembic:html
            (:div :class "category"
              (:h3 "Added")
              (:div :class "layer-group"
                (:h4 "CLI + App Scaffold + CRM" (:span :class "phase" "Phases 16–18, 21"))
                (:ul
                  (:li "Progressive help, fuzzy matching, required option validation")
                  (:li "ASDF app system with platform tables: users, companies, company_members")
                  (:li (:code "deftemplate") " macro and CRM template: contacts, companies, pipelines, stages, deals")
                  (:li "Generic record CRUD with filter/sort parsing")
                  (:li "DB admin CLI: migrate, rollback, status, schema inspection")
                  (:li "Test count: 868 → 952 (84 new tests)"))))))

    (list :version "0.2.0" :date "2026-03-14"
          :slug "0-2-0-framework-completion"
          :summary "Framework completion — multi-schema, auth, sessions, multipart, SEO, CLI infrastructure"
          :content
          (cauldron.alembic:html
            (:div :class "category"
              (:h3 "Added")
              (:div :class "layer-group"
                (:h4 "Multi-Schema, Auth, SEO, CLI" (:span :class "phase" "Phases 11–15"))
                (:ul
                  (:li "Per-tenant schema isolation with " (:code "SET search_path"))
                  (:li "Password hashing, session management, CSRF protection")
                  (:li "SEO helpers: meta-tags, og-tags, sitemap XML, robots.txt")
                  (:li "CLI framework: " (:code "defcommand") " macro, argument parsing")
                  (:li "Test count: 817 → 868 (51 new tests)"))))))

    (list :version "0.1.0" :date "2026-03-13"
          :slug "0-1-0-initial-framework"
          :summary "Initial framework — 14 modules across 7 layers, 817 tests, zero dependencies"
          :content
          (cauldron.alembic:html
            (:div :class "category"
              (:h3 "Added")
              (:div :class "layer-group"
                (:h4 "Full Framework" (:span :class "phase" "Phases 1–10"))
                (:ul
                  (:li "Layer 0: Runtime — threads, sockets, queues, timer")
                  (:li "Layer 1: Crypto — SHA-1/256, HMAC, bcrypt, PBKDF2, CSPRNG, base64")
                  (:li "Layer 1: JSON — streaming encoder/decoder")
                  (:li "Layer 2: Oracle — conditions, hooks, recovery strategies")
                  (:li "Layer 2: HTTP/1.1 server with thread pool")
                  (:li "Layer 2: WebSocket RFC 6455")
                  (:li "Layer 2: PostgreSQL wire protocol v3 with SCRAM-SHA-256")
                  (:li "Layer 3: Grimoire — query DSL, changesets, migrations")
                  (:li "Layer 3: Alembic — S-expression HTML templates")
                  (:li "Layer 3: Crucible — trie router, pipelines, plugs")
                  (:li "Layer 4: Reagent — MOP resource metaclass")
                  (:li "Layer 5: Ether — PubSub with PG LISTEN/NOTIFY")
                  (:li "Layer 5: Scry — LiveView with virtual DOM diff")
                  (:li "Layer 6: Forge — auto-generated admin interface")
                  (:li "Custom test runner, CLI orchestrator, 817 tests")))
            (:div :class "category"
              (:h3 "Fixed")
              (:div :class "fix-group"
                (:h4 "Phases 1–8")
                (:ul
                  (:li (:code "diff-attrs") " reversed plist ordering")
                  (:li (:code "audit.lisp") " package lock violation on SBCL")
                  (:li "Recovery " (:code "use-value") " not returning the supplied value")
                  (:li "Scry transport type dispatch")
                  (:li "Forge router " (:code "let") "/" (:code "let*") " binding")))))))))

;;; --- Changelog page helpers ---

(defun changelog-header (back-href back-text title &optional subtitle)
  "Generate the standard changelog page header."
  (cauldron.alembic:html
    (:div :class "changelog-header"
      (:a :class "back" :href back-href back-text)
      (:h1 title)
      (when subtitle
        `(:p :class "subtitle" ,subtitle)))))

(defun changelog-footer ()
  "Generate the standard changelog page footer."
  (cauldron.alembic:html
    (:div :class "footer"
      (:p "MIT License · "
          (:a :href "/changelog/" "All Releases")
          " · "
          (:a :href "/" "Cauldron")))))

(defun changelog-page (title body-html)
  "Wrap body HTML in a full changelog page shell."
  (cauldron.alembic:html
    (cauldron.alembic:raw "<!DOCTYPE html>")
    (:html :lang "en"
      (:head
        (:meta :charset "utf-8")
        (:meta :name "viewport" :content "width=device-width, initial-scale=1")
        (:title title)
        (:style (cauldron.alembic:raw *changelog-css*)))
      (:body
        (:div :class "container"
          (cauldron.alembic:raw body-html))))))

(defun write-page (dir filename content)
  "Write CONTENT string to DIR/FILENAME."
  (let ((path (merge-pathnames filename dir)))
    (with-open-file (stream path
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create
                     :external-format :utf-8)
      (write-string content stream))
    (format t "~&Generated ~A (~D bytes)~%" path (length content))))

;;; --- Generate changelog index ---

(let* ((changelog-dir (merge-pathnames "changelog/" *script-dir*))
       (index-body
         (concatenate 'string
           (changelog-header "/" "← Back to Cauldron" "Changelog"
                             "All notable changes to Cauldron.")
           (cauldron.alembic:html
             (:ul :class "release-list"
               (cauldron.alembic:for-each (rel *releases*)
                 `(:li :class "release-item"
                    (:a :class "version-link"
                        :href ,(concatenate 'string "/changelog/" (getf rel :slug))
                        ,(getf rel :version))
                    (:span :class "summary" ,(getf rel :summary))
                    (:span :class "date" ,(getf rel :date))))))
           (changelog-footer))))
  (write-page changelog-dir "index.html"
    (changelog-page "Changelog — Cauldron" index-body)))

;;; --- Generate individual release pages ---

(let ((changelog-dir (merge-pathnames "changelog/" *script-dir*)))
  (loop for (rel . rest) on *releases*
        for i from 0
        do
    (let* ((next-rel (when (> i 0) (nth (1- i) *releases*)))
           (prev-rel (nth (1+ i) *releases*))
           (body
             (concatenate 'string
               (changelog-header "/changelog/" "← All Releases"
                                 (concatenate 'string "Cauldron " (getf rel :version))
                                 (getf rel :summary))
               (cauldron.alembic:html
                 (:div :class "version"
                   (:div :class "version-heading"
                     (:h2 (cauldron.alembic:raw (getf rel :version)))
                     (:span :class "date" (cauldron.alembic:raw (getf rel :date))))
                   (cauldron.alembic:raw (getf rel :content))))
               ;; Nav between releases
               (cauldron.alembic:html
                 (:div :class "release-nav"
                   (if prev-rel
                       `(:a :href ,(concatenate 'string "/changelog/" (getf prev-rel :slug))
                            ,(concatenate 'string "← " (getf prev-rel :version)))
                       '(:span ""))
                   (if next-rel
                       `(:a :href ,(concatenate 'string "/changelog/" (getf next-rel :slug))
                            ,(concatenate 'string (getf next-rel :version) " →"))
                       '(:span ""))))
               (changelog-footer))))
      ;; Generate as slug/index.html for clean URLs
      (let ((slug-dir (merge-pathnames
                        (concatenate 'string (getf rel :slug) "/")
                        changelog-dir)))
        (ensure-directories-exist slug-dir)
        (write-page slug-dir "index.html"
          (changelog-page
            (concatenate 'string (getf rel :version) " — Cauldron")
            body))))))

;;; --- Write changelog.html redirect for backwards compat ---

(write-page *script-dir* "changelog.html"
  (cauldron.alembic:html
    (cauldron.alembic:raw "<!DOCTYPE html>")
    (:html :lang "en"
      (:head
        (:meta :charset "utf-8")
        (:meta :http-equiv "refresh" :content "0;url=/changelog/")
        (:title "Redirecting..."))
      (:body
        (:p "Redirecting to "
            (:a :href "/changelog/" "changelog")
            "...")))))
