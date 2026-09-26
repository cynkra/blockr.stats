// @ts-check
/**
 * stats-controls.js: one Shiny input binding for the blockr.ui controls that
 * blockr.stats' R-rendered blocks use (R/controls.R builds the markup).
 *
 *   <div class="blockr-stats-control" id="ns-x" data-kind="select"
 *        data-config='{"options":[...],"selected":"a"}'></div>
 *
 * Kinds, each the control the design system names for the job:
 *   select     Blockr.Select.single, bordered (42px field)
 *   multi      Blockr.Select.multi with tags, bordered
 *   segmented  Blockr.segmented, for a fixed set of two or three values
 *   checkbox   Blockr.checkbox
 *   number     a 42px field that commits on Enter or blur (Blockr.textCommit)
 *   text       the same, for text
 *
 * The value reaches R like any Shiny input. A server push
 * (session$sendInputMessage, see update_control()) changes the control
 * without reporting the change back: the server already holds the value it
 * pushed, and an echo would come back as a user gesture.
 *
 * Also here: Blockr.statsGear(), which wires a gear button to its tray with
 * blockr.ui's Blockr.gearTray.
 *
 * Depends on: blockr-ui.js and blockr-select.js (blockr.ui::controls_dep()).
 */
(function () {
  'use strict';

  /**
   * @typedef {{ value: string, label?: string }} Opt
   * @typedef {{
   *   options?: Opt[], selected?: any, placeholder?: string,
   *   allowEmpty?: boolean, labelOnly?: boolean, label?: string,
   *   min?: number, max?: number, step?: number
   * }} Config
   * @typedef {{
   *   kind: string,
   *   get: () => any,
   *   set: (value: any) => void,
   *   setOptions?: (opts: Opt[], selected: any) => void,
   *   callback: ((allowDeferred: boolean) => void) | null
   * }} Control
   */

  /** @param {HTMLElement} el @returns {Config} */
  function readConfig(el) {
    var raw = el.getAttribute('data-config');
    if (!raw) return {};
    try {
      return JSON.parse(raw);
    } catch (e) {
      return {};
    }
  }

  /** jsonlite sends a length-1 vector as a bare value. @param {any} x */
  function asArray(x) {
    if (x == null) return [];
    return Array.isArray(x) ? x.slice() : [x];
  }

  /**
   * Blockr.Select shows an option's value, then its label as meta text. A
   * fixed vocabulary whose values are internal tokens ("two.sided") asks
   * for the label alone, so with `labelOnly` the widget is given the labels
   * as its values and this binding translates them back.
   * @param {Opt[]} opts @param {boolean} labelOnly
   */
  function makeMap(opts, labelOnly) {
    var toShown = {};
    var toValue = {};
    var shown = opts.map(function (o) {
      var s = labelOnly && o.label ? o.label : o.value;
      toShown[o.value] = s;
      toValue[s] = o.value;
      return labelOnly ? s : o;
    });
    return {
      shown: shown,
      /** @param {any} v */
      show: function (v) {
        return v == null || v === '' ? v : (toShown[v] != null ? toShown[v] : v);
      },
      /** @param {any} s */
      value: function (s) {
        return s == null || s === '' ? s : (toValue[s] != null ? toValue[s] : s);
      }
    };
  }

  /** @param {HTMLElement} el @param {Config} cfg @param {boolean} multi @returns {Control} */
  function buildSelect(el, cfg, multi) {
    var labelOnly = !!cfg.labelOnly;
    var map = makeMap(cfg.options || [], labelOnly);
    /** @type {Control} */
    var ctl = { kind: multi ? 'multi' : 'select', get: function () { return null; },
                set: function () {}, callback: null };
    var Select = /** @type {BlockrSelectStatic} */ (Blockr.Select);
    var common = {
      bordered: true,
      placeholder: cfg.placeholder || '',
      options: map.shown
    };
    if (multi) {
      var hm = Select.multi(el, Object.assign(common, {
        selected: asArray(cfg.selected).map(map.show),
        onChange: function () { if (ctl.callback) ctl.callback(false); }
      }));
      ctl.get = function () { return hm.getValue().map(map.value); };
      ctl.set = function (v) { hm.setValue(asArray(v).map(map.show)); };
      ctl.setOptions = function (opts, sel) {
        map = makeMap(opts, labelOnly);
        hm.updateOptions(map.shown, asArray(sel).map(map.show));
      };
    } else {
      var sel0 = cfg.selected == null ? '' : String(cfg.selected);
      var hs = Select.single(el, Object.assign(common, {
        selected: sel0 === '' && !cfg.allowEmpty ? null : map.show(sel0),
        allowEmpty: !!cfg.allowEmpty,
        onChange: function () { if (ctl.callback) ctl.callback(false); }
      }));
      ctl.get = function () { return map.value(hs.getValue()); };
      ctl.set = function (v) { hs.setValue(v == null ? '' : map.show(String(v))); };
      ctl.setOptions = function (opts, sel) {
        map = makeMap(opts, labelOnly);
        var s = sel == null ? '' : map.show(String(asArray(sel)[0] || ''));
        hs.updateOptions(map.shown, s);
      };
    }
    return ctl;
  }

  /** @param {HTMLElement} el @param {Config} cfg @returns {Control} */
  function buildSegmented(el, cfg) {
    /** @type {Control} */
    var ctl = { kind: 'segmented', get: function () { return null; },
                set: function () {}, callback: null };
    var opts = (cfg.options || []).map(function (o) {
      return { value: o.value, label: o.label || o.value };
    });
    var seg = Blockr.segmented(opts, String(cfg.selected), function () {
      if (ctl.callback) ctl.callback(false);
    }, { label: cfg.label });
    el.appendChild(seg.el);
    ctl.get = function () { return seg.get(); };
    ctl.set = function (v) { seg.set(String(v)); };
    return ctl;
  }

  /** @param {HTMLElement} el @param {Config} cfg @returns {Control} */
  function buildCheckbox(el, cfg) {
    /** @type {Control} */
    var ctl = { kind: 'checkbox', get: function () { return null; },
                set: function () {}, callback: null };
    var box = Blockr.checkbox(cfg.label || '', !!cfg.selected, function () {
      if (ctl.callback) ctl.callback(false);
    });
    el.appendChild(box.el);
    ctl.get = function () { return box.get(); };
    ctl.set = function (v) { box.set(!!v); };
    return ctl;
  }

  /** @param {HTMLElement} el @param {Config} cfg @param {boolean} numeric @returns {Control} */
  function buildField(el, cfg, numeric) {
    /** @type {Control} */
    var ctl = { kind: numeric ? 'number' : 'text', get: function () { return null; },
                set: function () {}, callback: null };
    var wrap = document.createElement('div');
    wrap.className = 'blockr-commit-field';
    var input = document.createElement('input');
    input.type = numeric ? 'number' : 'text';
    input.className = 'blockr-text-input';
    if (numeric) {
      if (cfg.min != null) input.min = String(cfg.min);
      if (cfg.max != null) input.max = String(cfg.max);
      if (cfg.step != null) input.step = String(cfg.step);
    }
    if (cfg.placeholder) input.placeholder = cfg.placeholder;
    input.value = cfg.selected == null ? '' : String(cfg.selected);
    wrap.appendChild(input);
    el.appendChild(wrap);
    var commit = Blockr.textCommit(input, {
      onCommit: function () { if (ctl.callback) ctl.callback(false); }
    });
    ctl.get = function () {
      if (!numeric) return input.value;
      var n = parseFloat(input.value);
      return isFinite(n) ? n : null;
    };
    ctl.set = function (v) { commit.sync(v == null ? '' : String(v)); };
    return ctl;
  }

  /** @param {HTMLElement} el @returns {Control} */
  function build(el) {
    var cfg = readConfig(el);
    switch (el.getAttribute('data-kind')) {
      case 'multi': return buildSelect(el, cfg, true);
      case 'segmented': return buildSegmented(el, cfg);
      case 'checkbox': return buildCheckbox(el, cfg);
      case 'number': return buildField(el, cfg, true);
      case 'text': return buildField(el, cfg, false);
      default: return buildSelect(el, cfg, false);
    }
  }

  /** @param {HTMLElement} el @returns {Control} */
  function control(el) {
    var anyEl = /** @type {any} */ (el);
    if (!anyEl._statsControl) anyEl._statsControl = build(el);
    return anyEl._statsControl;
  }

  var ShinyNs = /** @type {any} */ (window).Shiny;
  if (ShinyNs && ShinyNs.InputBinding) {
    var binding = new ShinyNs.InputBinding();
    Object.assign(binding, {
      /** @param {HTMLElement} scope */
      find: function (scope) {
        return /** @type {any} */ (window).$(scope).find('.blockr-stats-control');
      },
      /** @param {HTMLElement} el */
      getId: function (el) { return el.id || null; },
      getType: function () { return 'blockr.stats.control'; },
      /** @param {HTMLElement} el */
      getValue: function (el) { return control(el).get(); },
      /** @param {HTMLElement} el @param {any} value */
      setValue: function (el, value) { control(el).set(value); },
      /** @param {HTMLElement} el @param {(allowDeferred: boolean) => void} callback */
      subscribe: function (el, callback) { control(el).callback = callback; },
      /** @param {HTMLElement} el */
      unsubscribe: function (el) { control(el).callback = null; },
      /**
       * A server push. Applied without reporting back; Shiny forgets the
       * last value it sent, so a later pick of the value the server moved
       * away from still reaches R.
       * @param {HTMLElement} el @param {{ options?: Opt[], selected?: any }} msg
       */
      receiveMessage: function (el, msg) {
        var ctl = control(el);
        if (msg.options && ctl.setOptions) {
          ctl.setOptions(msg.options, msg.selected);
        } else if ('selected' in msg) {
          ctl.set(msg.selected);
        }
        if (ShinyNs.forgetLastInputValue) ShinyNs.forgetLastInputValue(el.id);
      },
      /** @param {HTMLElement} el */
      initialize: function (el) { control(el); }
    });
    ShinyNs.inputBindings.register(binding, 'blockr.stats.control');
  }

  /**
   * Wire a gear to its tray. Block UI arrives after this file, and a
   * re-render runs the call again, so it waits for the elements and wires
   * each pair once.
   * @param {string} gearId @param {string} bandId @param {string} [label]
   */
  function statsGear(gearId, bandId, label) {
    var gear = /** @type {HTMLButtonElement | null} */ (document.getElementById(gearId));
    var band = document.getElementById(bandId);
    if (!gear || !band || !Blockr.gearTray || !Blockr.icons) {
      setTimeout(function () { statsGear(gearId, bandId, label); }, 50);
      return;
    }
    if (gear.getAttribute('data-stats-gear') === '1') return;
    gear.setAttribute('data-stats-gear', '1');
    gear.innerHTML = Blockr.icons.gear;
    Blockr.gearTray(band, gear, { label: label || 'Settings' });
  }

  var ns = /** @type {any} */ (window).Blockr || (/** @type {any} */ (window).Blockr = {});
  ns.statsGear = statsGear;
})();
