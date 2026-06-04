/**
 * formula-input.js — interactive R-formula builder (blockr.stats).
 *
 * Single source of truth lives in R (terms()-based parser). This widget is the
 * view/editor: chip edits produce already-canonical terms; the text field is
 * the round-trip path (raw text -> R parse_formula() -> normalized state back).
 *
 * Requires Blockr.Select / Blockr.Input / Blockr.icons (blockr.dplyr deps).
 */
(function () {
  "use strict";

  var needsBacktick = function (name) {
    return !/^[A-Za-z.][A-Za-z0-9._]*$/.test(name);
  };
  var lab = function (name) {
    return needsBacktick(name) ? "`" + name + "`" : name;
  };
  var responseText = function (resp) {
    if (resp == null) return null;
    if (typeof resp === "object" && resp.fn === "cbind") {
      return "cbind(" + (resp.args || []).join(", ") + ")";
    }
    return resp;
  };

  function FormulaInput(el) {
    this.el = el;
    this._callback = null;
    this._submitted = false;
    this._debounce = null;

    this.columns = [];
    this.colMeta = {};
    this.response = null;
    this.intercept = true;
    this.terms = [];
    this.bars = [];
    this.offset = null;
    this.weights = null;

    this._build();
  }

  FormulaInput.prototype._build = function () {
    var self = this;
    this.el.innerHTML = "";
    this.el.classList.add("formula-input");

    // Equation pill: [response] ~ [predictors] inside the shared .blockr-row
    // pill (bordered/rounded/gray) — same chrome as the filter block. The
    // selects inside are borderless; the row provides the border, with a
    // divider between response and predictors.
    var row = document.createElement("div");
    row.className = "blockr-row formula-row-pill";

    var lhsWrap = document.createElement("div");
    lhsWrap.className = "formula-lhs-wrap";
    var respHost = document.createElement("div");
    respHost.className = "formula-control";
    lhsWrap.appendChild(respHost);

    var rhsWrap = document.createElement("div");
    rhsWrap.className = "blockr-row-content";
    // Intercept as a click-through toggle button at the left of the RHS.
    // (The v-bar divider already separates LHS from RHS — no tilde needed.)
    this._iceptEl = document.createElement("button");
    this._iceptEl.type = "button";
    this._iceptEl.className = "formula-icept";
    this._iceptEl.title = "Click to include or drop the intercept";
    this._iceptEl.addEventListener("click", function (e) {
      e.preventDefault();
      self.intercept = !self.intercept;
      self._renderIntercept();
      self._sync();
    });
    var predHost = document.createElement("div");
    predHost.className = "formula-control";
    rhsWrap.appendChild(this._iceptEl);
    rhsWrap.appendChild(predHost);

    row.appendChild(lhsWrap);
    row.appendChild(rhsWrap);
    this.el.appendChild(row);

    this._respSelect = Blockr.Select.single(respHost, {
      placeholder: "response…",
      onChange: function (v) {
        self.response = v || null;
        self._sync();
      }
    });

    this._predSelect = Blockr.Select.multi(predHost, {
      placeholder: "add predictors…",
      onChange: function (sel) {
        self._onPredChange(sel || []);
      }
    });

    // Derived/advanced terms (interactions, transforms, splines, opaque,
    // random effects) render as colored chips INSIDE the predictors select
    // (see renderChips), alongside the column chips.

    // Visible affordance to open the build menu (interactions / transforms /
    // splines). Styled like the filter block's "+ Add condition" link.
    var addRow = document.createElement("div");
    addRow.className = "blockr-add-row formula-add-row";
    this._addLink = document.createElement("span");
    this._addLink.className = "blockr-add-link formula-add-link";
    this._addLink.innerHTML =
      '<span class="blockr-add-icon">' +
      Blockr.icons.plus +
      "</span> interaction / transform";
    this._addLink.addEventListener("click", function (e) {
      e.stopPropagation();
      self._openMenu(self._addLink);
    });
    addRow.appendChild(this._addLink);
    this.el.appendChild(addRow);

    // Right-click anywhere on the predictors area opens the same menu.
    rhsWrap.addEventListener("contextmenu", function (e) {
      e.preventDefault();
      self._openMenu(null, { x: e.clientX, y: e.clientY });
    });

    // (intercept is the inline 1/0 toggle at the left of the RHS, above)

    // Advanced: raw formula text (round-trips through R)
    var advRow = mkRow("Formula");
    advRow.classList.add("formula-advanced");
    var textHost = document.createElement("div");
    textHost.className = "formula-control";
    advRow.appendChild(textHost);
    this.el.appendChild(advRow);
    this._text = Blockr.Input.create(textHost, {
      placeholder: "y ~ x1 + x2 + x1:x2 …",
      onConfirm: function (t) {
        if (t && t.indexOf("~") !== -1) {
          Shiny.setInputValue(self.el.id + "_parse_request", t, {
            priority: "event"
          });
        }
      }
    });

    this._renderIntercept();
    this.renderChips();
  };

  FormulaInput.prototype._colOptions = function () {
    return this.columns.map(function (c) {
      // label kept distinct from value, and blank when it would just repeat
      // the column name (Blockr.Select renders value + label).
      var l = c.label && c.label !== c.name ? c.label : "";
      return { value: c.name, label: l };
    });
  };

  // Response select auto-shows its first option; read the live value so state
  // never diverges from what the user sees.
  FormulaInput.prototype._responseValue = function () {
    if (typeof this.response === "object" && this.response) return this.response;
    if (this._respSelect) return this._respSelect.getValue() || null;
    return this.response || null;
  };

  FormulaInput.prototype._renderIntercept = function () {
    if (!this._iceptEl) return;
    this._iceptEl.textContent = "intercept";
    this._iceptEl.classList.toggle("formula-icept--off", !this.intercept);
  };

  FormulaInput.prototype._isColTerm = function (t) {
    return t.kind === "main" || t.kind === "factor";
  };

  // Column main-effect/factor vars currently in the model (for the multi-select)
  FormulaInput.prototype._colVars = function () {
    var self = this;
    return this.terms
      .filter(function (t) { return self._isColTerm(t); })
      .map(function (t) { return t.var; });
  };

  // Multi-select changed: rebuild column terms from the selection, preserving
  // any derived terms (interactions/transforms/...) added via the formula field.
  FormulaInput.prototype._onPredChange = function (selected) {
    var self = this;
    var extra = this.terms.filter(function (t) { return !self._isColTerm(t); });
    var cols = selected.map(function (name) {
      var meta = self.colMeta[name];
      var isFac =
        meta &&
        ["factor", "ordered", "character", "logical"].indexOf(meta.type) !== -1;
      return { kind: isFac ? "factor" : "main", label: lab(name), var: name };
    });
    this.terms = cols.concat(extra);
    this.renderChips();
    this._sync();
  };

  // Renders ONLY the derived/advanced terms (non-column) + random effects.
  // Plain column predictors live inside the multi-select, not here.
  FormulaInput.prototype.renderChips = function () {
    var self = this;
    var tags =
      this._predSelect &&
      this._predSelect.el &&
      this._predSelect.el.querySelector(".blockr-select__tags");
    if (!tags) return;

    // Clear previously injected derived chips (the select owns the column ones)
    var old = tags.querySelectorAll(".formula-tag--derived");
    for (var i = 0; i < old.length; i++) {
      old[i].parentNode.removeChild(old[i]);
    }

    var derived = this.terms
      .filter(function (t) { return !self._isColTerm(t); })
      .map(function (t) { return { kind: t.kind, label: t.label, t: t }; })
      .concat(
        this.bars.map(function (b) {
          return { kind: "bar", label: "(" + b.raw + ")", b: b };
        })
      );

    derived.forEach(function (item) {
      var chip = document.createElement("span");
      chip.className =
        "blockr-select__tag formula-tag--derived formula-tag--" + item.kind;
      var lbl = document.createElement("span");
      lbl.className = "blockr-select__tag-label";
      lbl.textContent = item.label;
      var rm = document.createElement("button");
      rm.type = "button";
      rm.className = "blockr-select__tag-remove";
      rm.setAttribute("tabindex", "-1");
      rm.innerHTML = Blockr.icons.remove;
      rm.addEventListener("click", function (e) {
        e.stopPropagation();
        if (item.b) {
          self.bars = self.bars.filter(function (b) { return b !== item.b; });
        } else {
          self.terms = self.terms.filter(function (t) { return t !== item.t; });
        }
        self.renderChips();
        self._sync();
      });
      chip.appendChild(lbl);
      chip.appendChild(rm);
      // place right after the column chips, before the search input
      var search = tags.querySelector(".blockr-select__search");
      if (search) tags.insertBefore(chip, search);
      else tags.appendChild(chip);
    });
  };

  // -- Build menu (interactions / transforms / splines) ----------------------

  // Close any open menu and detach the outside-click / escape listeners.
  FormulaInput.prototype._closeMenu = function () {
    if (this._menu && this._menu.parentNode) {
      this._menu.parentNode.removeChild(this._menu);
    }
    this._menu = null;
    if (this._menuDismiss) {
      document.removeEventListener("mousedown", this._menuDismiss, true);
      document.removeEventListener("keydown", this._menuDismiss, true);
      this._menuDismiss = null;
    }
  };

  // Open the build menu. `anchor` (the link) positions it below the link;
  // `at` ({x,y}) positions it at the cursor (right-click path).
  FormulaInput.prototype._openMenu = function (anchor, at) {
    var self = this;
    this._closeMenu();

    var menu = document.createElement("div");
    menu.className = "formula-menu";
    this._menu = menu;

    this._renderMenuRoot(menu);

    document.body.appendChild(menu);

    // Position: anchored under the link, or at the cursor.
    var top, left;
    if (at) {
      left = at.x;
      top = at.y;
    } else if (anchor) {
      var r = anchor.getBoundingClientRect();
      left = r.left;
      top = r.bottom + 4;
    } else {
      left = 40;
      top = 40;
    }
    // Keep within viewport.
    var mw = menu.offsetWidth || 220;
    var mh = menu.offsetHeight || 160;
    if (left + mw > window.innerWidth - 8) left = window.innerWidth - mw - 8;
    if (top + mh > window.innerHeight - 8) top = window.innerHeight - mh - 8;
    menu.style.left = Math.max(8, left) + "px";
    menu.style.top = Math.max(8, top) + "px";

    // Dismiss on outside click or Escape.
    this._menuDismiss = function (e) {
      if (e.type === "keydown") {
        if (e.key === "Escape") self._closeMenu();
        return;
      }
      if (self._menu && !self._menu.contains(e.target)) self._closeMenu();
    };
    document.addEventListener("mousedown", this._menuDismiss, true);
    document.addEventListener("keydown", this._menuDismiss, true);
  };

  // Top-level item list.
  FormulaInput.prototype._renderMenuRoot = function (menu) {
    var self = this;
    menu.innerHTML = "";
    var items = [
      { label: "Interaction…", fn: function () { self._panelInteraction(menu); } },
      { label: "All 2-way", fn: function () { self._addAll2Way(); self._closeMenu(); } },
      { label: "Transform…", fn: function () { self._panelTransform(menu); } },
      { label: "Spline…", fn: function () { self._panelSpline(menu); } }
    ];
    items.forEach(function (it) {
      var row = document.createElement("div");
      row.className = "formula-menu__item";
      row.textContent = it.label;
      row.addEventListener("click", function (e) {
        e.stopPropagation();
        it.fn();
      });
      menu.appendChild(row);
    });
  };

  // Sub-panel chrome: a back header + a confirm button row.
  FormulaInput.prototype._menuPanel = function (menu, title) {
    var self = this;
    menu.innerHTML = "";
    var head = document.createElement("div");
    head.className = "formula-menu__head";
    var back = document.createElement("span");
    back.className = "formula-menu__back";
    back.innerHTML = Blockr.icons.chevron;
    back.addEventListener("click", function (e) {
      e.stopPropagation();
      self._renderMenuRoot(menu);
    });
    var ttl = document.createElement("span");
    ttl.className = "formula-menu__title";
    ttl.textContent = title;
    head.appendChild(back);
    head.appendChild(ttl);
    menu.appendChild(head);

    var body = document.createElement("div");
    body.className = "formula-menu__body";
    menu.appendChild(body);
    return body;
  };

  FormulaInput.prototype._menuConfirm = function (menu, label, onConfirm) {
    var self = this;
    var foot = document.createElement("div");
    foot.className = "formula-menu__foot";
    var btn = document.createElement("button");
    btn.type = "button";
    btn.className = "blockr-pill formula-menu__confirm";
    btn.textContent = label || "Add";
    btn.addEventListener("click", function (e) {
      e.stopPropagation();
      if (onConfirm() !== false) self._closeMenu();
    });
    foot.appendChild(btn);
    menu.appendChild(foot);
    return btn;
  };

  // True when an interaction over exactly `vars` already exists.
  FormulaInput.prototype._hasInteraction = function (vars) {
    var key = vars.slice().sort().join(" ");
    return this.terms.some(function (t) {
      return (
        t.kind === "interaction" &&
        (t.vars || []).slice().sort().join(" ") === key
      );
    });
  };

  // All unordered pairs of current main/factor predictors.
  FormulaInput.prototype._addAll2Way = function () {
    var cols = this._colVars();
    for (var i = 0; i < cols.length; i++) {
      for (var j = i + 1; j < cols.length; j++) {
        var vars = [cols[i], cols[j]];
        if (!this._hasInteraction(vars)) {
          this.terms.push({
            kind: "interaction",
            label: vars.join(":"),
            vars: vars
          });
        }
      }
    }
    this.renderChips();
    this._sync();
  };

  // Interaction sub-panel: checkboxes over current predictors.
  FormulaInput.prototype._panelInteraction = function (menu) {
    var self = this;
    var body = this._menuPanel(menu, "Interaction");
    var cols = this._colVars();
    if (cols.length < 2) {
      var note = document.createElement("div");
      note.className = "formula-menu__note";
      note.textContent = "Add at least two predictors first.";
      body.appendChild(note);
      return;
    }
    var checks = [];
    cols.forEach(function (name) {
      var row = document.createElement("label");
      row.className = "formula-menu__check";
      var cb = document.createElement("input");
      cb.type = "checkbox";
      cb.value = name;
      row.appendChild(cb);
      row.appendChild(document.createTextNode(" " + name));
      body.appendChild(row);
      checks.push(cb);
    });
    this._menuConfirm(menu, "Add interaction", function () {
      var vars = checks
        .filter(function (c) { return c.checked; })
        .map(function (c) { return c.value; });
      if (vars.length < 2) return false;
      if (!self._hasInteraction(vars)) {
        self.terms.push({
          kind: "interaction",
          label: vars.join(":"),
          vars: vars
        });
        self.renderChips();
        self._sync();
      }
      return true;
    });
  };

  // Small native <select> helper for the panels.
  FormulaInput.prototype._mkSelect = function (options) {
    var sel = document.createElement("select");
    sel.className = "formula-menu__select";
    options.forEach(function (o) {
      var opt = document.createElement("option");
      opt.value = o.value;
      opt.textContent = o.label;
      sel.appendChild(opt);
    });
    return sel;
  };

  // Transform sub-panel: column + function (log/sqrt/poly2/poly3).
  FormulaInput.prototype._panelTransform = function (menu) {
    var self = this;
    var body = this._menuPanel(menu, "Transform");
    if (!this.columns.length) {
      var note = document.createElement("div");
      note.className = "formula-menu__note";
      note.textContent = "No columns available.";
      body.appendChild(note);
      return;
    }
    var colSel = this._mkSelect(
      this.columns.map(function (c) {
        return { value: c.name, label: c.name };
      })
    );
    var fnSel = this._mkSelect([
      { value: "log", label: "log" },
      { value: "sqrt", label: "sqrt" },
      { value: "poly2", label: "poly(2)" },
      { value: "poly3", label: "poly(3)" }
    ]);
    body.appendChild(colSel);
    body.appendChild(fnSel);
    this._menuConfirm(menu, "Add transform", function () {
      var v = colSel.value;
      var f = fnSel.value;
      if (!v) return false;
      if (f === "poly2" || f === "poly3") {
        var degree = f === "poly2" ? 2 : 3;
        self.terms.push({
          kind: "poly",
          var: v,
          degree: degree,
          label: "poly(" + v + ", " + degree + ")"
        });
      } else {
        self.terms.push({
          kind: "transform",
          fn: f,
          var: v,
          raw: f + "(" + v + ")",
          label: f + "(" + v + ")"
        });
      }
      self.renderChips();
      self._sync();
      return true;
    });
  };

  // Spline sub-panel: column + ns|bs + df (3/4/5).
  FormulaInput.prototype._panelSpline = function (menu) {
    var self = this;
    var body = this._menuPanel(menu, "Spline");
    if (!this.columns.length) {
      var note = document.createElement("div");
      note.className = "formula-menu__note";
      note.textContent = "No columns available.";
      body.appendChild(note);
      return;
    }
    var colSel = this._mkSelect(
      this.columns.map(function (c) {
        return { value: c.name, label: c.name };
      })
    );
    var fnSel = this._mkSelect([
      { value: "ns", label: "ns (natural)" },
      { value: "bs", label: "bs (B-spline)" }
    ]);
    var dfSel = this._mkSelect([
      { value: "3", label: "df 3" },
      { value: "4", label: "df 4" },
      { value: "5", label: "df 5" }
    ]);
    body.appendChild(colSel);
    body.appendChild(fnSel);
    body.appendChild(dfSel);
    this._menuConfirm(menu, "Add spline", function () {
      var v = colSel.value;
      var f = fnSel.value;
      var df = parseInt(dfSel.value, 10);
      if (!v) return false;
      self.terms.push({
        kind: "spline",
        fn: f,
        var: v,
        df: df,
        label: f + "(" + v + ", " + df + ")"
      });
      self.renderChips();
      self._sync();
      return true;
    });
  };

  FormulaInput.prototype._currentFormulaText = function () {
    var lhs = responseText(this._responseValue());
    if (lhs == null) return "";
    var parts = this.terms
      .map(function (t) {
        return t.label;
      })
      .concat(
        this.bars.map(function (b) {
          return "(" + b.raw + ")";
        })
      );
    if (!parts.length) {
      return lhs + " ~ " + (this.intercept ? "1" : "0");
    }
    return lhs + " ~ " + (this.intercept ? "" : "0 + ") + parts.join(" + ");
  };

  FormulaInput.prototype._compose = function () {
    return {
      response: this._responseValue(),
      intercept: this.intercept,
      terms: this.terms,
      bars: this.bars,
      offset: this.offset,
      weights: this.weights
    };
  };

  FormulaInput.prototype._sync = function () {
    if (this._text) this._text.setValue(this._currentFormulaText());
    this._autoSubmit();
  };

  FormulaInput.prototype._autoSubmit = function () {
    var self = this;
    clearTimeout(this._debounce);
    this._debounce = setTimeout(function () {
      self._submitted = true;
      if (self._callback) self._callback(true);
    }, 300);
  };

  FormulaInput.prototype.getValue = function () {
    return this._submitted ? this._compose() : null;
  };

  FormulaInput.prototype.setState = function (state, silent) {
    state = state || {};
    this.response = state.response != null ? state.response : null;
    this.intercept = state.intercept !== false;
    this.terms = (state.terms || []).slice();
    this.bars = (state.bars || []).slice();
    this.offset = state.offset || null;
    this.weights = state.weights || null;

    var rv = typeof this.response === "string" ? this.response : null;
    this._respSelect.setOptions(this._colOptions(), rv);
    this._predSelect.setOptions(this._colOptions(), this._colVars());
    this._renderIntercept();
    this.renderChips();
    if (this._text) this._text.setValue(this._currentFormulaText());
    // silent: do not fire callback
  };

  FormulaInput.prototype.updateColumns = function (meta) {
    var self = this;
    this.columns = meta || [];
    this.colMeta = {};
    this.columns.forEach(function (c) {
      self.colMeta[c.name] = c;
    });
    var opts = this._colOptions();
    var rv = typeof this.response === "string" ? this.response : null;
    this._respSelect.setOptions(opts, rv);
    this._predSelect.setOptions(opts, this._colVars());
    this.renderChips();
    if (this._text)
      this._text.setColumns(
        this.columns.map(function (c) {
          return c.name;
        })
      );
  };

  // -- Shiny input binding ---------------------------------------------------
  var binding = new Shiny.InputBinding();
  Object.assign(binding, {
    find: function (scope) {
      return $(scope).find(".formula-input-container");
    },
    getId: function (el) {
      return el.id || null;
    },
    getValue: function (el) {
      return el._fi ? el._fi.getValue() : null;
    },
    setValue: function (el, value) {
      if (el._fi) el._fi.setState(value, true);
    },
    subscribe: function (el, callback) {
      if (el._fi) el._fi._callback = function () {
        callback(true);
      };
    },
    unsubscribe: function (el) {
      if (el._fi) el._fi._callback = null;
    },
    initialize: function (el) {
      el._fi = new FormulaInput(el);
      if (el._pendingColumns) {
        el._fi.updateColumns(el._pendingColumns);
        delete el._pendingColumns;
      }
      if (el._pendingState) {
        el._fi.setState(el._pendingState, true);
        delete el._pendingState;
      }
    }
  });
  Shiny.inputBindings.register(binding, "blockr.stats.formula");

  Shiny.addCustomMessageHandler("formula-columns", function (msg) {
    var el = document.getElementById(msg.id);
    if (el && el._fi) el._fi.updateColumns(msg.columns);
    else if (el) el._pendingColumns = msg.columns;
  });
  Shiny.addCustomMessageHandler("formula-update", function (msg) {
    var el = document.getElementById(msg.id);
    if (el && el._fi) el._fi.setState(msg.state, true);
    else if (el) el._pendingState = msg.state;
  });

  function mkRow(labelText) {
    var row = document.createElement("div");
    row.className = "formula-row";
    var l = document.createElement("label");
    l.className = "formula-row__label";
    l.textContent = labelText;
    row.appendChild(l);
    return row;
  }
})();
