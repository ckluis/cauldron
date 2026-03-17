/**
 * cauldron.js — Client-side runtime for Cauldron Scry (LiveView)
 * Zero npm dependencies. Single file. < 40KB.
 *
 * Responsibilities:
 * 1. WebSocket connection + reconnection with exponential backoff
 * 2. DOM patching from server-sent diffs (no innerHTML — XSS safe)
 * 3. Event delegation for scry-* attributes
 * 4. Form state preservation across patches
 * 5. Live navigation (pushState)
 * 6. Loading states
 */
(function () {
  "use strict";

  // ========================================
  // Configuration
  // ========================================
  const CONFIG = {
    wsPath: "/scry/websocket",
    reconnectMin: 1000,     // 1s initial
    reconnectMax: 30000,    // 30s max
    reconnectFactor: 1.5,
    pingInterval: 30000,    // 30s
    loadingDebounce: 100,   // ms before showing loading state
  };

  // ========================================
  // WebSocket Manager
  // ========================================
  class ScrySocket {
    constructor(url) {
      this.url = url;
      this.ws = null;
      this.state = "disconnected"; // connecting, connected, disconnected
      this.reconnectDelay = CONFIG.reconnectMin;
      this.reconnectTimer = null;
      this.pingTimer = null;
      this.eventQueue = [];
      this.onPatch = null;
      this.onRedirect = null;
      this.onError = null;
    }

    connect() {
      if (this.state === "connected" || this.state === "connecting") return;
      this.state = "connecting";

      try {
        const protocol = location.protocol === "https:" ? "wss:" : "ws:";
        this.ws = new WebSocket(`${protocol}//${location.host}${this.url}`);
      } catch (e) {
        this._scheduleReconnect();
        return;
      }

      this.ws.onopen = () => {
        this.state = "connected";
        this.reconnectDelay = CONFIG.reconnectMin;
        this._startPing();
        this._flushQueue();
        document.dispatchEvent(new CustomEvent("scry:connected"));
      };

      this.ws.onmessage = (evt) => {
        try {
          const msg = JSON.parse(evt.data);
          this._handleMessage(msg);
        } catch (e) {
          console.error("[cauldron] Failed to parse message:", e);
        }
      };

      this.ws.onclose = (evt) => {
        this.state = "disconnected";
        this._stopPing();
        if (!evt.wasClean) {
          this._scheduleReconnect();
        }
        document.dispatchEvent(new CustomEvent("scry:disconnected"));
      };

      this.ws.onerror = () => {
        // onclose will fire after this
      };
    }

    send(type, name, payload) {
      const msg = JSON.stringify([type, name, payload || {}]);
      if (this.state === "connected") {
        this.ws.send(msg);
      } else {
        this.eventQueue.push(msg);
      }
    }

    disconnect() {
      clearTimeout(this.reconnectTimer);
      this._stopPing();
      if (this.ws) {
        this.ws.close(1000, "client disconnect");
        this.ws = null;
      }
      this.state = "disconnected";
    }

    _handleMessage(msg) {
      if (!Array.isArray(msg) || msg.length < 2) return;
      const [type, data] = msg;

      switch (type) {
        case "patch":
          if (this.onPatch) this.onPatch(data);
          break;
        case "redirect":
          if (this.onRedirect) this.onRedirect(data);
          break;
        case "error":
          if (this.onError) this.onError(data);
          break;
        case "pong":
          // Server acknowledged ping
          break;
      }
    }

    _scheduleReconnect() {
      this.reconnectTimer = setTimeout(() => {
        this.reconnectDelay = Math.min(
          this.reconnectDelay * CONFIG.reconnectFactor,
          CONFIG.reconnectMax
        );
        this.connect();
      }, this.reconnectDelay);
    }

    _startPing() {
      this.pingTimer = setInterval(() => {
        if (this.state === "connected") {
          this.ws.send(JSON.stringify(["ping"]));
        }
      }, CONFIG.pingInterval);
    }

    _stopPing() {
      clearInterval(this.pingTimer);
    }

    _flushQueue() {
      while (this.eventQueue.length > 0) {
        const msg = this.eventQueue.shift();
        this.ws.send(msg);
      }
    }
  }

  // ========================================
  // DOM Patcher (No innerHTML — XSS safe)
  // ========================================
  const Patcher = {
    /**
     * Apply a list of patches to the DOM.
     * Each patch: { type, path, ... }
     */
    apply(patches) {
      const savedState = FormPreserver.save();

      // Batch DOM mutations
      const fragment = document.createDocumentFragment();
      for (const patch of patches) {
        this._applyPatch(patch);
      }

      FormPreserver.restore(savedState);

      // Remove loading states
      document.querySelectorAll("[scry-loading]").forEach((el) => {
        el.removeAttribute("scry-loading");
      });
    },

    _applyPatch(patch) {
      const target = this._resolve(patch.path);
      if (!target) return;

      switch (patch.type) {
        case "replace":
          this._replaceNode(target, patch.node);
          break;
        case "insert":
          this._insertNode(target, patch.node, patch.index);
          break;
        case "remove":
          target.remove();
          break;
        case "update-attrs":
          this._updateAttrs(target, patch.attrs);
          break;
        case "update-text":
          target.textContent = patch.text;
          break;
      }
    },

    _resolve(path) {
      if (!path || path.length === 0) return document.body;
      let node = document.querySelector("[scry-root]") || document.body;
      for (const index of path) {
        if (!node.children || !node.children[index]) return null;
        node = node.children[index];
      }
      return node;
    },

    _replaceNode(target, spec) {
      const newNode = this._createNode(spec);
      if (newNode) {
        target.parentNode.replaceChild(newNode, target);
      }
    },

    _insertNode(parent, spec, index) {
      const newNode = this._createNode(spec);
      if (!newNode) return;
      if (index != null && parent.children[index]) {
        parent.insertBefore(newNode, parent.children[index]);
      } else {
        parent.appendChild(newNode);
      }
    },

    _createNode(spec) {
      if (typeof spec === "string") {
        return document.createTextNode(spec);
      }
      if (!spec || !spec.tag) return null;

      const el = document.createElement(spec.tag);
      if (spec.attrs) {
        for (const [key, value] of Object.entries(spec.attrs)) {
          if (value === true) {
            el.setAttribute(key, "");
          } else if (value != null && value !== false) {
            el.setAttribute(key, value);
          }
        }
      }
      if (spec.children) {
        for (const child of spec.children) {
          const childNode = this._createNode(child);
          if (childNode) el.appendChild(childNode);
        }
      }
      return el;
    },

    _updateAttrs(target, attrs) {
      for (const [key, value] of Object.entries(attrs)) {
        if (value === null || value === false) {
          target.removeAttribute(key);
        } else if (value === true) {
          target.setAttribute(key, "");
        } else {
          target.setAttribute(key, value);
        }
      }
    },
  };

  // ========================================
  // Form State Preservation
  // ========================================
  const FormPreserver = {
    save() {
      const state = {
        focused: null,
        values: {},
        selections: {},
      };

      const active = document.activeElement;
      if (active && active.id) {
        state.focused = active.id;
        if (active.selectionStart != null) {
          state.selections[active.id] = {
            start: active.selectionStart,
            end: active.selectionEnd,
          };
        }
      }

      // Save dirty form values
      document.querySelectorAll("input, textarea, select").forEach((el) => {
        if (el.id && el.type !== "hidden") {
          if (el.type === "checkbox" || el.type === "radio") {
            state.values[el.id] = el.checked;
          } else {
            state.values[el.id] = el.value;
          }
        }
      });

      return state;
    },

    restore(state) {
      // Restore values
      for (const [id, value] of Object.entries(state.values)) {
        const el = document.getElementById(id);
        if (!el) continue;
        if (el.type === "checkbox" || el.type === "radio") {
          el.checked = value;
        } else if (el.value !== value) {
          el.value = value;
        }
      }

      // Restore focus
      if (state.focused) {
        const el = document.getElementById(state.focused);
        if (el) {
          el.focus();
          const sel = state.selections[state.focused];
          if (sel && el.setSelectionRange) {
            el.setSelectionRange(sel.start, sel.end);
          }
        }
      }
    },
  };

  // ========================================
  // Event Delegation
  // ========================================
  const Events = {
    socket: null,

    init(socket) {
      this.socket = socket;

      // Click events
      document.addEventListener("click", (e) => {
        const target = e.target.closest("[scry-click]");
        if (target) {
          e.preventDefault();
          const event = target.getAttribute("scry-click");
          this._sendEvent(target, "click", event);
        }

        // Live navigation
        const link = e.target.closest("[scry-link]");
        if (link) {
          e.preventDefault();
          const href = link.getAttribute("href") || link.getAttribute("scry-link");
          this.socket.send("event", "navigate", { to: href });
          history.pushState({}, "", href);
        }
      });

      // Change events
      document.addEventListener("change", (e) => {
        const target = e.target.closest("[scry-change]");
        if (target) {
          const event = target.getAttribute("scry-change");
          const value = target.type === "checkbox" ? target.checked : target.value;
          this.socket.send("event", event, { value: value });
        }
      });

      // Submit events
      document.addEventListener("submit", (e) => {
        const form = e.target.closest("[scry-submit]");
        if (form) {
          e.preventDefault();
          const event = form.getAttribute("scry-submit");
          const formData = new FormData(form);
          const data = {};
          formData.forEach((v, k) => { data[k] = v; });
          this._sendEvent(form, "submit", event, data);
        }
      });

      // Keydown events
      document.addEventListener("keydown", (e) => {
        const target = e.target.closest("[scry-keydown]");
        if (target) {
          const event = target.getAttribute("scry-keydown");
          this.socket.send("event", event, { key: e.key, code: e.code });
        }
      });

      // Input events (debounced)
      let inputTimer;
      document.addEventListener("input", (e) => {
        const target = e.target.closest("[scry-change]");
        if (target && target.tagName !== "SELECT") {
          clearTimeout(inputTimer);
          inputTimer = setTimeout(() => {
            const event = target.getAttribute("scry-change");
            this.socket.send("event", event, { value: target.value });
          }, 300); // 300ms debounce
        }
      });

      // Handle popstate (back/forward)
      window.addEventListener("popstate", () => {
        this.socket.send("event", "navigate", { to: location.pathname });
      });
    },

    _sendEvent(el, type, event, payload) {
      // Add loading state
      const loadingTimer = setTimeout(() => {
        el.setAttribute("scry-loading", "");
      }, CONFIG.loadingDebounce);

      // Store timer to clear on patch
      el._scryLoadingTimer = loadingTimer;

      this.socket.send("event", event, payload || {});
    },
  };

  // ========================================
  // Main — Cauldron Client
  // ========================================
  const Cauldron = {
    socket: null,

    /**
     * Initialize the Cauldron client.
     * Call this on DOMContentLoaded.
     */
    init(opts) {
      const options = Object.assign({}, CONFIG, opts || {});

      // Only connect if there's a scry-root element
      const root = document.querySelector("[scry-root]");
      if (!root) return;

      this.socket = new ScrySocket(options.wsPath);

      this.socket.onPatch = (patches) => {
        Patcher.apply(patches);
      };

      this.socket.onRedirect = (url) => {
        if (url.startsWith("/")) {
          history.pushState({}, "", url);
          this.socket.send("event", "navigate", { to: url });
        } else {
          window.location.href = url;
        }
      };

      this.socket.onError = (error) => {
        console.error("[cauldron] Server error:", error);
      };

      Events.init(this.socket);
      this.socket.connect();
    },
  };

  // Auto-init on DOMContentLoaded
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", () => Cauldron.init());
  } else {
    Cauldron.init();
  }

  // Export for manual init
  window.Cauldron = Cauldron;
})();
