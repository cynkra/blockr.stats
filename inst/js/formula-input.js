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

    var tilde = document.createElement("span");
    tilde.className = "formula-tilde";
    tilde.textContent = "~";

    var rhsWrap = document.createElement("div");
    rhsWrap.className = "blockr-row-content";
    var predHost = document.createElement("div");
    predHost.className = "formula-control";
    rhsWrap.appendChild(predHost);

    row.appendChild(lhsWrap);
    row.appendChild(tilde);
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

    // Secondary strip: derived/advanced terms that are not plain columns
    // (interactions, transforms, splines, opaque, random effects). Comes from
    // the formula field; hidden when empty.
    this._chips = document.createElement("div");
    this._chips.className = "formula-chips";
    this.el.appendChild(this._chips);

    // Intercept
    var intRow = document.createElement("label");
    intRow.className = "formula-intercept";
    this._intBox = document.createElement("input");
    this._intBox.type = "checkbox";
    this._intBox.checked = true;
    this._intBox.addEventListener("change", function () {
      self.intercept = self._intBox.checked;
      self._sync();
    });
    intRow.appendChild(this._intBox);
    intRow.appendChild(document.createTextNode(" Include intercept"));
    this.el.appendChild(intRow);

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
    this._chips.innerHTML = "";
    var all = this.terms
      .filter(function (t) { return !self._isColTerm(t); })
      .map(function (t) {
        return { kind: "term", t: t, label: t.label, k: t.kind };
      })
      .concat(
        this.bars.map(function (b) {
          return { kind: "bar", b: b, label: "(" + b.raw + ")", k: "bar" };
        })
      );

    if (all.length === 0) {
      this._chips.style.display = "none";
      return;
    }
    this._chips.style.display = "";

    all.forEach(function (item) {
      var chip = document.createElement("span");
      chip.className = "formula-chip formula-chip--" + item.k;
      var txt = document.createElement("span");
      txt.className = "formula-chip__label";
      txt.textContent = item.label;
      chip.appendChild(txt);
      var rm = document.createElement("span");
      rm.className = "formula-chip__remove";
      rm.innerHTML = Blockr.icons.x;
      rm.addEventListener("click", function () {
        if (item.kind === "bar") {
          self.bars = self.bars.filter(function (b) { return b !== item.b; });
        } else {
          self.terms = self.terms.filter(function (t) { return t !== item.t; });
        }
        self.renderChips();
        self._sync();
      });
      chip.appendChild(rm);
      self._chips.appendChild(chip);
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
    var rhs = parts.length ? parts.join(" + ") : "1";
    return lhs + " ~ " + (this.intercept ? "" : "0 + ") + rhs;
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
    this._intBox.checked = this.intercept;
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
