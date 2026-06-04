/**
 * formula-explorer.js — EXPERIMENTAL R-formula builder (blockr.stats).
 *
 * Fork of formula-input.js that uses the vendored BlockrX.multi (chip-selection
 * + right-click) for predictors. Right-clicking selected predictor chips pops a
 * small context menu offering 'Cross (interaction)' and 'Remove selected'.
 *
 * Single source of truth still lives in R (terms()-based parser). Chip edits
 * produce already-canonical terms; the text field is the round-trip path.
 *
 * Requires Blockr.Select / Blockr.Input / Blockr.icons (blockr.dplyr deps) and
 * BlockrX.multi (blockr-select-multi.js).
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

  function FormulaExplorer(el) {
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

    this._menu = null;

    this._build();
  }

  FormulaExplorer.prototype._build = function () {
    var self = this;
    this.el.innerHTML = "";
    this.el.classList.add("formula-input");

    // Equation pill: [response] ~ [predictors] inside the shared .blockr-row.
    var row = document.createElement("div");
    row.className = "blockr-row formula-row-pill";

    var lhsWrap = document.createElement("div");
    lhsWrap.className = "formula-lhs-wrap";
    var respHost = document.createElement("div");
    respHost.className = "formula-control";
    lhsWrap.appendChild(respHost);

    var tilde = document.createElement("span");
    tilde.className = "formula-tilde";
    tilde.innerHTML =
      '<svg width="20" height="20" viewBox="0 0 18 18" fill="none" ' +
      'stroke="currentColor" stroke-width="2" stroke-linecap="round" ' +
      'stroke-linejoin="round"><path d="M2 11c1.4-3 3.4-3 4.9 0s3.5 3 4.9 0"/></svg>';

    var rhsWrap = document.createElement("div");
    rhsWrap.className = "blockr-row-content";
    // Inline intercept toggle at the left of the RHS: the leading 1 (include)
    // or 0 (drop) of the right-hand side.
    this._iceptEl = document.createElement("span");
    this._iceptEl.className = "formula-icept";
    this._iceptEl.title = "Intercept: 1 = include, 0 = drop";
    this._iceptEl.addEventListener("click", function () {
      self.intercept = !self.intercept;
      self._renderIntercept();
      self._sync();
    });
    var predHost = document.createElement("div");
    predHost.className = "formula-control";
    rhsWrap.appendChild(this._iceptEl);
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

    // Predictors use the vendored fork with chip-selection + right-click.
    this._predSelect = BlockrX.multi(predHost, {
      placeholder: "add predictors…",
      onChange: function (sel) {
        self._onPredChange(sel || []);
      },
      onContextMenu: function (selectedValues, x, y) {
        self._showContextMenu(selectedValues, x, y);
      }
    });

    // Secondary strip: derived/advanced terms (interactions, transforms, …).
    this._chips = document.createElement("div");
    this._chips.className = "formula-chips";
    this.el.appendChild(this._chips);

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

  // --- Context menu (right-click on selected predictor chips) ---------------

  FormulaExplorer.prototype._closeMenu = function () {
    if (this._menu) {
      if (this._menu._onDocClick) {
        document.removeEventListener("click", this._menu._onDocClick, true);
      }
      if (this._menu.parentElement) this._menu.parentElement.removeChild(this._menu);
      this._menu = null;
    }
  };

  FormulaExplorer.prototype._showContextMenu = function (selected, x, y) {
    var self = this;
    this._closeMenu();
    if (!selected || selected.length === 0) return;

    var menu = document.createElement("div");
    menu.className = "formula-context-menu";

    var mkItem = function (text, enabled, handler) {
      var item = document.createElement("div");
      item.className = "formula-context-menu__item";
      if (!enabled) item.className += " formula-context-menu__item--disabled";
      item.textContent = text;
      if (enabled) {
        item.addEventListener("click", function (e) {
          e.stopPropagation();
          handler();
          self._closeMenu();
        });
      }
      return item;
    };

    // Cross (interaction): needs >= 2 distinct selected predictors.
    menu.appendChild(
      mkItem("Cross (interaction)", selected.length >= 2, function () {
        self._crossSelected(selected);
      })
    );
    menu.appendChild(
      mkItem("Remove selected", true, function () {
        self._removeSelectedCols(selected);
      })
    );

    menu.style.position = "fixed";
    menu.style.left = x + "px";
    menu.style.top = y + "px";
    document.body.appendChild(menu);
    this._menu = menu;

    // Reposition if it would overflow the viewport.
    var r = menu.getBoundingClientRect();
    if (r.right > window.innerWidth) menu.style.left = (x - r.width) + "px";
    if (r.bottom > window.innerHeight) menu.style.top = (y - r.height) + "px";

    var onDocClick = function (e) {
      if (!menu.contains(e.target)) self._closeMenu();
    };
    menu._onDocClick = onDocClick;
    // Defer so the contextmenu-originating click does not immediately close it.
    setTimeout(function () {
      document.addEventListener("click", onDocClick, true);
    }, 0);
  };

  // Add an interaction term from the selected predictor chips.
  FormulaExplorer.prototype._crossSelected = function (selected) {
    var vars = selected.map(function (v) { return lab(v); });
    var label = vars.join(":");
    // Avoid duplicate interaction terms with the same label.
    var exists = this.terms.some(function (t) {
      return t.kind === "interaction" && t.label === label;
    });
    if (!exists) {
      this.terms.push({ kind: "interaction", label: label, vars: vars });
    }
    if (this._predSelect.clearSelected) this._predSelect.clearSelected();
    this.renderChips();
    this._sync();
  };

  // Remove the selected predictor columns from the multi-select + terms.
  FormulaExplorer.prototype._removeSelectedCols = function (selected) {
    var keep = this._predSelect.getValue().filter(function (v) {
      return selected.indexOf(v) < 0;
    });
    if (this._predSelect.clearSelected) this._predSelect.clearSelected();
    // setOptions with the trimmed selection rebuilds tags; then sync terms.
    this._predSelect.setOptions(this._colOptions(), keep);
    this._onPredChange(keep);
  };

  FormulaExplorer.prototype._colOptions = function () {
    return this.columns.map(function (c) {
      var l = c.label && c.label !== c.name ? c.label : "";
      return { value: c.name, label: l };
    });
  };

  FormulaExplorer.prototype._responseValue = function () {
    if (typeof this.response === "object" && this.response) return this.response;
    if (this._respSelect) return this._respSelect.getValue() || null;
    return this.response || null;
  };

  FormulaExplorer.prototype._renderIntercept = function () {
    if (!this._iceptEl) return;
    this._iceptEl.textContent = this.intercept ? "1" : "0";
    this._iceptEl.classList.toggle("formula-icept--off", !this.intercept);
  };

  FormulaExplorer.prototype._isColTerm = function (t) {
    return t.kind === "main" || t.kind === "factor";
  };

  FormulaExplorer.prototype._colVars = function () {
    var self = this;
    return this.terms
      .filter(function (t) { return self._isColTerm(t); })
      .map(function (t) { return t.var; });
  };

  FormulaExplorer.prototype._onPredChange = function (selected) {
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

  FormulaExplorer.prototype.renderChips = function () {
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

  FormulaExplorer.prototype._currentFormulaText = function () {
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

  FormulaExplorer.prototype._compose = function () {
    return {
      response: this._responseValue(),
      intercept: this.intercept,
      terms: this.terms,
      bars: this.bars,
      offset: this.offset,
      weights: this.weights
    };
  };

  FormulaExplorer.prototype._sync = function () {
    if (this._text) this._text.setValue(this._currentFormulaText());
    this._autoSubmit();
  };

  FormulaExplorer.prototype._autoSubmit = function () {
    var self = this;
    clearTimeout(this._debounce);
    this._debounce = setTimeout(function () {
      self._submitted = true;
      if (self._callback) self._callback(true);
    }, 300);
  };

  FormulaExplorer.prototype.getValue = function () {
    return this._submitted ? this._compose() : null;
  };

  FormulaExplorer.prototype.setState = function (state, silent) {
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

  FormulaExplorer.prototype.updateColumns = function (meta) {
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
      return $(scope).find(".formula-explorer-container");
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
      el._fi = new FormulaExplorer(el);
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
  Shiny.inputBindings.register(binding, "blockr.stats.formulaExplorer");

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
