/**
 * BlockrX.multi — vendored, modified COPY of blockr.dplyr's Blockr.Select multi.
 *
 * EXPERIMENTAL fork for the formula-explorer block. Exposes a DIFFERENT global
 * (window.BlockrX) so it never clashes with the real window.Blockr.Select.
 *
 * Additions over the upstream multi select:
 *   - Clicking a chip BODY (not its × remove) toggles an 'is-selected'
 *     highlight class and tracks the selected values.
 *   - getSelected() returns the currently chip-selected values.
 *   - config.onContextMenu(selectedValues, x, y) fires on a `contextmenu`
 *     event over the control when >= 1 chip is selected (native menu suppressed).
 *
 * Depends on blockr-core.js (Blockr namespace, icons, utilities) — same as the
 * upstream component.
 */
(() => {
  'use strict';

  const optValue = (o) => typeof o === 'object' && o !== null ? o.value : o;
  const optLabel = (o) => typeof o === 'object' && o !== null ? (o.label || '') : '';
  const findOpt = (opts, val) => opts.find(o => optValue(o) === val);
  const fillOptContent = (el, o) => {
    el.textContent = '';
    const val = optValue(o);
    const lbl = optLabel(o);
    el.appendChild(document.createTextNode(val));
    if (lbl) {
      const span = document.createElement('span');
      span.className = 'blockr-select__opt-label';
      span.textContent = lbl;
      span.setAttribute('title', lbl);
      el.appendChild(span);
    }
  };

  const createMulti = (container, config) => {
    const mode = 'multi';
    const id = Blockr.uid('bselx');
    const dropdownId = `${id}-lb`;

    // State
    let options = config.options || [];
    let selected = (config.selected || []).slice();
    const placeholder = config.placeholder || '';
    const reorderable = config.reorderable !== false;
    const onChange = config.onChange || null;
    const onContextMenu = config.onContextMenu || null;
    let isOpen = false;
    let searchQuery = '';
    let highlightIdx = -1;
    let destroyed = false;

    // Chip-selection state (NEW): values whose chip is highlighted/selected.
    let chipSelected = [];

    // Drag state
    let dragValue = null;
    let dragOverTag = null;
    let dragSide = null;

    // DOM
    const root = document.createElement('div');
    root.className = `blockr-select blockr-select--${mode}`;
    root.setAttribute('role', 'combobox');
    root.setAttribute('aria-expanded', 'false');
    root.setAttribute('aria-haspopup', 'listbox');
    root.setAttribute('aria-owns', dropdownId);

    const control = document.createElement('div');
    control.className = 'blockr-select__control';

    const tagsEl = document.createElement('div');
    tagsEl.className = 'blockr-select__tags';
    control.appendChild(tagsEl);

    const searchInput = document.createElement('input');
    searchInput.type = 'text';
    searchInput.className = 'blockr-select__search';
    searchInput.setAttribute('aria-autocomplete', 'list');
    searchInput.setAttribute('aria-controls', dropdownId);
    searchInput.setAttribute('autocomplete', 'off');
    searchInput.setAttribute('autocorrect', 'off');
    searchInput.setAttribute('autocapitalize', 'off');
    searchInput.setAttribute('spellcheck', 'false');
    searchInput.setAttribute('placeholder', placeholder);
    tagsEl.appendChild(searchInput);

    const dropdown = document.createElement('div');
    dropdown.className = 'blockr-select__dropdown';
    dropdown.id = dropdownId;
    dropdown.setAttribute('role', 'listbox');

    root.appendChild(control);
    container.appendChild(root);

    const computePosition = () => {
      const r = root.getBoundingClientRect();
      const dropH = dropdown.offsetHeight || 240;
      const spaceBelow = window.innerHeight - r.bottom - 8;
      const flipAbove = spaceBelow < dropH && r.top > dropH;

      dropdown.style.position = 'fixed';
      dropdown.style.width    = r.width + 'px';
      dropdown.style.left     = r.left + 'px';
      dropdown.style.bottom   = 'auto';

      if (flipAbove) {
        dropdown.style.top = (r.top - dropH - 4) + 'px';
        root.classList.add('blockr-select--above');
      } else {
        dropdown.style.top = (r.bottom + 4) + 'px';
        root.classList.remove('blockr-select--above');
      }
    };

    const onScrollOrResize = () => { if (isOpen) computePosition(); };

    // --- Rendering ---

    const getFiltered = () => {
      const q = searchQuery.toLowerCase();
      const result = [];
      for (let i = 0; i < options.length; i++) {
        const opt = options[i];
        const val = optValue(opt);
        if (selected.indexOf(val) >= 0) continue;
        if (q) {
          const matchVal = val.toLowerCase().indexOf(q) >= 0;
          const matchLabel = optLabel(opt).toLowerCase().indexOf(q) >= 0;
          if (!matchVal && !matchLabel) continue;
        }
        result.push(opt);
      }
      return result;
    };

    const renderDropdown = () => {
      const filtered = getFiltered();
      dropdown.innerHTML = '';

      if (filtered.length === 0) {
        const empty = document.createElement('div');
        empty.className = 'blockr-select__empty';
        empty.textContent = searchQuery ? 'No matches' : 'All selected';
        dropdown.appendChild(empty);
        highlightIdx = -1;
        return;
      }

      if (highlightIdx >= filtered.length) highlightIdx = filtered.length - 1;
      if (highlightIdx < 0 && filtered.length > 0) highlightIdx = 0;

      for (let i = 0; i < filtered.length; i++) {
        const opt = filtered[i];
        const val = optValue(opt);
        const div = document.createElement('div');
        div.className = 'blockr-select__option';
        if (i === highlightIdx) div.className += ' blockr-select__option--highlighted';
        div.setAttribute('role', 'option');
        div.setAttribute('id', `${id}-opt-${i}`);
        div.setAttribute('aria-selected', 'false');
        div.setAttribute('data-value', val);
        fillOptContent(div, opt);
        dropdown.appendChild(div);
      }

      if (highlightIdx >= 0) {
        searchInput.setAttribute('aria-activedescendant', `${id}-opt-${highlightIdx}`);
      }
    };

    const renderTags = () => {
      // Drop chip-selection for values that are no longer present.
      chipSelected = chipSelected.filter(v => selected.indexOf(v) >= 0);
      // Remove only tag elements, preserve the search input
      tagsEl.querySelectorAll('.blockr-select__tag').forEach(t => t.remove());
      for (let i = 0; i < selected.length; i++) {
        const val = selected[i];
        const tag = document.createElement('span');
        tag.className = 'blockr-select__tag';
        if (chipSelected.indexOf(val) >= 0) tag.classList.add('is-selected');
        tag.setAttribute('data-value', val);
        if (reorderable) tag.setAttribute('draggable', 'true');

        const label = document.createElement('span');
        label.className = 'blockr-select__tag-label';
        label.textContent = val;
        tag.appendChild(label);

        const removeBtn = document.createElement('button');
        removeBtn.type = 'button';
        removeBtn.className = 'blockr-select__tag-remove';
        removeBtn.setAttribute('aria-label', `Remove ${val}`);
        removeBtn.setAttribute('tabindex', '-1');
        removeBtn.innerHTML = Blockr.icons.remove;
        tag.appendChild(removeBtn);

        tagsEl.insertBefore(tag, searchInput);
      }
      searchInput.setAttribute('placeholder', selected.length === 0 ? placeholder : '');
    };

    const render = () => {
      renderTags();
      if (isOpen) renderDropdown();
    };

    // --- Open / close ---

    const open = () => {
      if (isOpen || destroyed) return;
      isOpen = true;
      searchQuery = '';
      searchInput.value = '';
      highlightIdx = 0;

      if (dropdown.parentElement !== document.body) {
        document.body.appendChild(dropdown);
      }
      dropdown.style.display = 'block';

      root.classList.add('blockr-select--open');
      root.setAttribute('aria-expanded', 'true');

      renderDropdown();
      computePosition();
      window.addEventListener('scroll', onScrollOrResize, { capture: true, passive: true });
      window.addEventListener('resize', onScrollOrResize, { passive: true });
      searchInput.focus();
    };

    const close = () => {
      if (!isOpen) return;
      isOpen = false;
      searchQuery = '';
      searchInput.value = '';

      window.removeEventListener('scroll', onScrollOrResize, { capture: true });
      window.removeEventListener('resize', onScrollOrResize);

      dropdown.style.display = '';

      root.classList.remove('blockr-select--open', 'blockr-select--above');
      root.setAttribute('aria-expanded', 'false');
      searchInput.removeAttribute('aria-activedescendant');

      dropdown.innerHTML = '';
    };

    // --- Selection ---

    const selectOption = (value) => {
      if (selected.indexOf(value) < 0) {
        selected.push(value);
        searchQuery = '';
        searchInput.value = '';
        highlightIdx = 0;
        render();
        renderDropdown();
        if (onChange) onChange(selected.slice());
      }
    };

    const removeTag = (value) => {
      const idx = selected.indexOf(value);
      if (idx >= 0) {
        selected.splice(idx, 1);
        const cidx = chipSelected.indexOf(value);
        if (cidx >= 0) chipSelected.splice(cidx, 1);
        render();
        if (isOpen) renderDropdown();
        if (onChange) onChange(selected.slice());
      }
    };

    // NEW: toggle the chip-selection highlight for a value.
    const toggleChipSelected = (value) => {
      const idx = chipSelected.indexOf(value);
      if (idx >= 0) chipSelected.splice(idx, 1);
      else chipSelected.push(value);
      renderTags();
    };

    // --- Event handlers ---

    const onControlClick = (e) => {
      if (e.target.closest('.blockr-select__tag-remove')) return;
      // NEW: clicking a chip body toggles its selection rather than opening.
      const tag = e.target.closest('.blockr-select__tag');
      if (tag) {
        const val = tag.getAttribute('data-value');
        if (val != null) {
          toggleChipSelected(val);
          e.stopPropagation();
          return;
        }
      }
      if (!isOpen) open();
      searchInput.focus();
    };

    // NEW: right-click over the control with >=1 chip selected -> callback.
    const onControlContextMenu = (e) => {
      if (chipSelected.length >= 1 && onContextMenu) {
        e.preventDefault();
        onContextMenu(chipSelected.slice(), e.clientX, e.clientY);
      }
    };

    const onDropdownClick = (e) => {
      const optEl = e.target.closest('.blockr-select__option');
      if (optEl) {
        const val = optEl.getAttribute('data-value');
        if (val != null) selectOption(val);
      }
    };

    const onTagRemoveClick = (e) => {
      const btn = e.target.closest('.blockr-select__tag-remove');
      if (!btn) return;
      const tag = btn.closest('.blockr-select__tag');
      if (tag) {
        const val = tag.getAttribute('data-value');
        if (val != null) removeTag(val);
      }
      e.stopPropagation();
    };

    const onSearchInput = () => {
      searchQuery = searchInput.value;
      highlightIdx = 0;
      if (!isOpen) open();
      else renderDropdown();
    };

    const onSearchKeydown = (e) => {
      const filtered = getFiltered();
      switch (e.key) {
        case 'ArrowDown':
          e.preventDefault();
          if (!isOpen) { open(); return; }
          highlightIdx = (highlightIdx + 1) % (filtered.length || 1);
          renderDropdown();
          scrollHighlightIntoView();
          break;
        case 'ArrowUp':
          e.preventDefault();
          if (!isOpen) { open(); return; }
          highlightIdx = (highlightIdx - 1 + (filtered.length || 1)) % (filtered.length || 1);
          renderDropdown();
          scrollHighlightIntoView();
          break;
        case 'Enter':
          e.preventDefault();
          if (!isOpen) { open(); return; }
          if (highlightIdx >= 0 && highlightIdx < filtered.length) {
            selectOption(optValue(filtered[highlightIdx]));
          }
          break;
        case 'Escape':
          e.preventDefault();
          close();
          root.focus();
          break;
        case 'Backspace':
          if (searchInput.value === '' && selected.length > 0) {
            removeTag(selected[selected.length - 1]);
          }
          break;
        case 'Tab':
          close();
          break;
      }
    };

    const scrollHighlightIntoView = () => {
      dropdown.querySelector('.blockr-select__option--highlighted')
        ?.scrollIntoView({ block: 'nearest' });
    };

    const onDocumentClick = (e) => {
      if (root.contains(e.target) || dropdown.contains(e.target)) return;
      close();
    };

    const onRootKeydown = (e) => {
      if (e.target === root && !isOpen) {
        if (e.key === 'Enter' || e.key === ' ' || e.key === 'ArrowDown' || e.key === 'ArrowUp') {
          e.preventDefault();
          open();
        }
      }
    };

    // --- Drag and drop ---

    const clearDropIndicators = () => {
      tagsEl.querySelectorAll('.blockr-select__tag--drop-before, .blockr-select__tag--drop-after')
        .forEach(el => el.classList.remove('blockr-select__tag--drop-before', 'blockr-select__tag--drop-after'));
    };

    const onDragStart = (e) => {
      const tag = e.target.closest('.blockr-select__tag');
      if (!tag) return;
      dragValue = tag.getAttribute('data-value');
      e.dataTransfer.effectAllowed = 'move';
      e.dataTransfer.setData('text/plain', dragValue);
      tag.classList.add('blockr-select__tag--dragging');
    };

    const onDragOver = (e) => {
      if (dragValue == null) return;
      e.preventDefault();
      e.dataTransfer.dropEffect = 'move';

      const tag = e.target.closest('.blockr-select__tag');
      if (!tag || tag.getAttribute('data-value') === dragValue) {
        clearDropIndicators();
        return;
      }

      const rect = tag.getBoundingClientRect();
      const mid = rect.left + rect.width / 2;
      const side = e.clientX < mid ? 'before' : 'after';

      if (tag !== dragOverTag || side !== dragSide) {
        clearDropIndicators();
        dragOverTag = tag;
        dragSide = side;
        tag.classList.add(`blockr-select__tag--drop-${side}`);
      }
    };

    const onDragEnd = () => {
      clearDropIndicators();
      tagsEl.querySelectorAll('.blockr-select__tag--dragging')
        .forEach(el => el.classList.remove('blockr-select__tag--dragging'));
      dragValue = null;
      dragOverTag = null;
      dragSide = null;
    };

    const onDrop = (e) => {
      e.preventDefault();
      if (dragValue == null || !dragOverTag) { onDragEnd(); return; }

      const targetVal = dragOverTag.getAttribute('data-value');
      const fromIdx = selected.indexOf(dragValue);
      let toIdx = selected.indexOf(targetVal);
      if (fromIdx < 0 || toIdx < 0 || fromIdx === toIdx) { onDragEnd(); return; }

      selected.splice(fromIdx, 1);
      toIdx = selected.indexOf(targetVal);
      const insertIdx = dragSide === 'after' ? toIdx + 1 : toIdx;
      selected.splice(insertIdx, 0, dragValue);

      onDragEnd();
      render();
      if (onChange) onChange(selected.slice());
    };

    // --- Bind events ---

    control.addEventListener('click', onControlClick);
    control.addEventListener('contextmenu', onControlContextMenu);
    dropdown.addEventListener('click', onDropdownClick);
    searchInput.addEventListener('input', onSearchInput);
    searchInput.addEventListener('keydown', onSearchKeydown);
    document.addEventListener('click', onDocumentClick, true);
    root.addEventListener('keydown', onRootKeydown);

    control.addEventListener('click', onTagRemoveClick);
    if (reorderable) {
      tagsEl.addEventListener('dragstart', onDragStart);
      tagsEl.addEventListener('dragover', onDragOver);
      tagsEl.addEventListener('dragend', onDragEnd);
      tagsEl.addEventListener('drop', onDrop);
    }

    render();

    // --- Public API ---

    return {
      el: root,

      setOptions(opts, sel) {
        options = Array.isArray(opts) ? opts : (opts != null ? [opts] : []);
        const vals = options.map(optValue);
        if (sel != null) {
          selected = sel.filter(v => vals.indexOf(v) >= 0);
        } else {
          selected = selected.filter(v => vals.indexOf(v) >= 0);
        }
        chipSelected = chipSelected.filter(v => selected.indexOf(v) >= 0);
        render();
        if (isOpen) renderDropdown();
      },

      getValue() {
        return selected.slice();
      },

      // NEW: chip-selected (highlighted) values.
      getSelected() {
        return chipSelected.slice();
      },

      // NEW: clear the chip-selection highlight.
      clearSelected() {
        chipSelected = [];
        renderTags();
      },

      destroy() {
        if (destroyed) return;
        destroyed = true;
        close();

        if (dropdown.parentElement === document.body) {
          dropdown.remove();
        }

        control.removeEventListener('click', onControlClick);
        control.removeEventListener('contextmenu', onControlContextMenu);
        dropdown.removeEventListener('click', onDropdownClick);
        searchInput.removeEventListener('input', onSearchInput);
        searchInput.removeEventListener('keydown', onSearchKeydown);
        document.removeEventListener('click', onDocumentClick, true);
        root.removeEventListener('keydown', onRootKeydown);

        control.removeEventListener('click', onTagRemoveClick);
        if (reorderable) {
          tagsEl.removeEventListener('dragstart', onDragStart);
          tagsEl.removeEventListener('dragover', onDragOver);
          tagsEl.removeEventListener('dragend', onDragEnd);
          tagsEl.removeEventListener('drop', onDrop);
        }

        Blockr.removeNode(root);
      }
    };
  };

  window.BlockrX = {
    multi: (container, config) => createMulti(container, config)
  };
})();
