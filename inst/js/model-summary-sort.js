// @ts-check
/**
 * model-summary-sort.js — click-to-sort for the model summary card.
 *
 * Deliberately browser-only. Sorting a coefficient table is a reading aid for
 * a model with many predictors ("show me the biggest effects", "the weakest
 * evidence", "find the term alphabetically"), not a property of the analysis:
 * nothing here is sent to R, so it never enters block state, the saved board
 * or the exported code, and the block's value keeps model order.
 *
 * Because the card is re-rendered by R on every update (a new model upstream,
 * a gear change), the sort would be lost on each redraw. So the chosen order
 * is remembered per output element and re-applied when the new table appears.
 * It lives as long as the tab does, and not one moment longer.
 *
 * Sort keys come from data attributes on each row, not from the rendered text,
 * which carries a unicode minus, "<0.001" and en-dash ranges. The intercept
 * carries data-ms-pin="last" and stays at the bottom in every order: it is the
 * baseline, not a term competing with the others.
 */
(function () {
  'use strict';

  /**
   * Chosen order per output id. Never persisted.
   * @type {Record<string, { key: string, dir: string }>}
   */
  var state = {};

  /** @param {Element} el @returns {Element} */
  function hostOf(el) {
    var out = el.closest ? el.closest('.shiny-html-output') : null;
    return out || document.body;
  }

  /** @param {Element} host @returns {string} */
  function keyFor(host) {
    return host.id || '__anon__';
  }

  /** @param {Element} tr @param {string} key @returns {string | number | null} */
  function rowValue(tr, key) {
    var raw = tr.getAttribute('data-ms-' + key);
    if (raw === null) return null;
    if (key === 'term') return raw;
    var num = parseFloat(raw);
    return isNaN(num) ? null : num;
  }

  /**
   * Reorder tbody rows. Pinned rows keep the bottom, missing values sink
   * below the sorted ones so an empty p-value never wins a sort.
   * @param {Element} table @param {string} key @param {string} dir
   */
  function applySort(table, key, dir) {
    var tbody = table.querySelector('tbody');
    if (!tbody) return;
    var rows = Array.prototype.slice.call(tbody.querySelectorAll('tr'));
    var sign = dir === 'desc' ? -1 : 1;

    rows.sort(function (a, b) {
      var pinA = a.getAttribute('data-ms-pin') === 'last';
      var pinB = b.getAttribute('data-ms-pin') === 'last';
      if (pinA !== pinB) return pinA ? 1 : -1;
      if (pinA && pinB) return 0;

      var va = rowValue(a, key);
      var vb = rowValue(b, key);
      if (va === null && vb === null) return 0;
      if (va === null) return 1;
      if (vb === null) return -1;
      if (typeof va === 'string' || typeof vb === 'string') {
        return sign * String(va).localeCompare(String(vb));
      }
      return sign * (va - vb);
    });

    for (var k = 0; k < rows.length; k++) tbody.appendChild(rows[k]);

    table.querySelectorAll('th[data-ms-sort]').forEach(function (th) {
      var icon = th.querySelector('.blockr-sort-icon');
      if (!icon) return;
      var on = th.getAttribute('data-ms-sort') === key;
      icon.className = 'blockr-sort-icon' +
        (on ? ' blockr-sort-icon-' + dir : '');
      th.setAttribute('aria-sort',
        on ? (dir === 'asc' ? 'ascending' : 'descending') : 'none');
    });
  }

  /** Restore model order: the rows carry their original index. @param {Element} table */
  function clearSort(table) {
    var tbody = table.querySelector('tbody');
    if (!tbody) return;
    var rows = Array.prototype.slice.call(tbody.querySelectorAll('tr'));
    rows.sort(function (a, b) {
      return (+a.getAttribute('data-ms-row')) - (+b.getAttribute('data-ms-row'));
    });
    for (var k = 0; k < rows.length; k++) tbody.appendChild(rows[k]);
    table.querySelectorAll('.blockr-sort-icon').forEach(function (icon) {
      icon.className = 'blockr-sort-icon';
    });
    table.querySelectorAll('th[data-ms-sort]').forEach(function (th) {
      th.setAttribute('aria-sort', 'none');
    });
  }

  /**
   * Third click on the same column returns to model order, so the reader can
   * always get back to the table as the model wrote it.
   * @param {Element} th
   */
  function onHeaderClick(th) {
    var table = th.closest('table');
    if (!table) return;
    var host = hostOf(table);
    var id = keyFor(host);
    var key = th.getAttribute('data-ms-sort');
    if (!key) return;
    var cur = state[id];

    var dir = 'asc';
    if (cur && cur.key === key) {
      if (cur.dir === 'asc') {
        dir = 'desc';
      } else {
        delete state[id];
        clearSort(table);
        return;
      }
    }
    state[id] = { key: key, dir: dir };
    applySort(table, key, dir);
  }

  /** Re-apply the remembered order to a freshly rendered table. @param {Element} table */
  function restore(table) {
    var st = state[keyFor(hostOf(table))];
    if (st) applySort(table, st.key, st.dir);
  }

  document.addEventListener('click', function (e) {
    // Guard the target: clicks land on text nodes and on document too.
    var target = e.target instanceof Element ? e.target : null;
    var th = target ? target.closest('th[data-ms-sort]') : null;
    if (th) onHeaderClick(th);
  });

  // Every R redraw replaces the table, so watch for new ones and put the
  // reader's chosen order back.
  var pending = false;
  new MutationObserver(function () {
    if (pending) return;
    pending = true;
    requestAnimationFrame(function () {
      pending = false;
      document.querySelectorAll('table.msc-ct').forEach(restore);
    });
  }).observe(document.body, { childList: true, subtree: true });
})();
