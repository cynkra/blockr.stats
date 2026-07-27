// @ts-check
/**
 * model-summary-gear.js — mounts the model summary block's gear controls with
 * the design-system components instead of Bootstrap/selectize widgets.
 *
 * Every option in this block is a display choice, so the whole surface lives
 * in the settings band: three `Blockr.Select.single` pickers and three
 * `Blockr.checkbox` options. Per the design system's boolean-controls record,
 * gear options are CHECKBOXES, not switches -- a column of switches
 * over-promises for what are quiet display options.
 *
 * R renders empty, declarative containers (see ms_gear_ui() in
 * R/model-summary-block.R) and this file fills them, so the option
 * vocabulary stays defined in one place, in R:
 *
 *   <div data-ms-select="..." data-ms-input="ns-id" data-ms-selected="ci95"
 *        data-ms-options='[{"value":"ci95","label":"95% confidence interval"}]'>
 *   <div data-ms-checks='[{"input":"ns-id","label":"...","checked":true}]'>
 *
 * Changes go straight back with Shiny.setInputValue against the namespaced id,
 * which the block server already observes. Nothing is sent on mount: the
 * constructor arguments are the initial state, and announcing them here would
 * invalidate the block on every render (and fight state restore).
 *
 * Depends on: blockr-core.js, blockr-select.js (blockr.dplyr),
 * settings-band.js (Blockr.checkbox).
 */
(function () {
  'use strict';

  /** @param {Element} el @param {string} attr */
  function parseJson(el, attr) {
    var raw = el.getAttribute(attr);
    if (!raw) return null;
    try {
      return JSON.parse(raw);
    } catch (e) {
      return null;
    }
  }

  /** @param {string | null} id @param {unknown} value */
  function setInput(id, value) {
    if (id && window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue(id, value);
    }
  }

  /**
   * Fill one band's containers. Idempotent: a re-render of the block UI runs
   * the mount script again, and the flag keeps a second component from being
   * stacked into the same container.
   * @param {HTMLElement} band
   */
  function mount(band) {
    if (band.getAttribute('data-ms-mounted') === '1') return true;
    if (!window.Blockr || !Blockr.Select || !Blockr.checkbox) return false;

    var selects = band.querySelectorAll('[data-ms-select]');
    for (var i = 0; i < selects.length; i++) {
      var host = /** @type {HTMLElement} */ (selects[i]);
      var options = parseJson(host, 'data-ms-options') || [];
      var inputId = host.getAttribute('data-ms-input');
      var selected = host.getAttribute('data-ms-selected');
      var handle = Blockr.Select.single(host, {
        options: options,
        selected: selected,
        onChange: (function (id) {
          return function (value) { setInput(id, value); };
        })(inputId)
      });
      handle.el.classList.add('blockr-select--bordered');
    }

    var checkHosts = band.querySelectorAll('[data-ms-checks]');
    for (var j = 0; j < checkHosts.length; j++) {
      var chost = /** @type {HTMLElement} */ (checkHosts[j]);
      var checks = parseJson(chost, 'data-ms-checks') || [];
      for (var k = 0; k < checks.length; k++) {
        var spec = checks[k];
        var box = Blockr.checkbox(
          spec.label,
          !!spec.checked,
          (function (id) {
            return function (checked) { setInput(id, checked); };
          })(spec.input)
        );
        chost.appendChild(box.el);
      }
    }

    band.setAttribute('data-ms-mounted', '1');
    return true;
  }

  var ns = /** @type {BlockrNamespace} */ (
    (typeof Blockr !== 'undefined') ? Blockr
      : (window.Blockr = window.Blockr || /** @type {BlockrNamespace} */ ({})));
  ns.msGear = { mount: mount };
})();
