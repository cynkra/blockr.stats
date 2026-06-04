#' CSS Utilities for blockr.stats Blocks
#'
#' Provides centralized CSS functions for consistent block styling.
#' Based on blockr.dplyr patterns.
#'
#' @noRd
NULL

#' Responsive grid layout CSS for blocks
#'
#' Creates CSS for responsive grid layout with consistent styling.
#' This is the foundation CSS that **must** be loaded by all blocks.
#'
#' @return HTML style tag with responsive grid CSS
#' @noRd
css_responsive_grid <- function() {
  tags$style(HTML(
    "
    .block-container {
      width: 100%;
      padding-bottom: 10px;
    }

    /* One shared grid across the whole form */
    .block-form-grid {
      display: grid;
      gap: 15px;
      grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
    }

    /* Flatten wrappers so all controls share the same tracks */
    .block-section,
    .block-section-grid {
      display: contents;
    }

    /* Headings/help span full width in grid */
    .block-section h4,
    .block-help-text {
      grid-column: 1 / -1;
    }

    .block-section:not(:first-child) {
      margin-top: 20px;
    }

    .block-input-wrapper {
      width: 100%;
    }

    .block-input-wrapper .form-group {
      margin-bottom: 10px;
    }

    /* Help text styling */
    .block-help-text {
      color: #6c757d;
      font-size: 0.875rem;
      margin-bottom: 10px;
    }
    "
  ))
}

#' Model summary card CSS
#'
#' Styles the generic HTML model card ([model_summary_html()]). Uses
#' `--blockr-*` design tokens with literal fallbacks so the card also
#' renders standalone (e.g. in the dev preview harness).
#'
#' @return HTML style tag.
#' @noRd
css_model_summary <- function() {
  tags$style(HTML(
    "
    .smb-card {
      font-size: var(--blockr-font-size-base, 0.875rem);
      color: var(--blockr-color-text-primary, #111827);
      background: #fff;
      padding: 4px 2px;
    }
    .smb-empty { padding: 16px; color: var(--blockr-color-text-muted, #9ca3af); }

    /* visual <-> R-text switch: discrete .blockr-pill toggle, no server round-trip */
    .smb-radio { position: absolute; width: 0; height: 0; opacity: 0; pointer-events: none; }
    /* segmented button group, muted (blockr-pill styling) */
    .smb-switch { display: flex; justify-content: flex-end; margin-bottom: 8px; }
    .smb-seg {
      display: inline-flex; align-items: center; justify-content: center;
      padding: 2px 10px; height: 24px; min-width: 30px;
      font-size: 11px; font-weight: var(--blockr-font-weight-medium, 500);
      color: var(--blockr-grey-500, #6b7280);
      background: var(--blockr-grey-50, #f9fafb);
      border: 1px solid var(--blockr-grey-200, #e5e7eb);
      cursor: pointer; user-select: none; white-space: nowrap;
      transition: background 0.15s ease, border-color 0.15s ease, color 0.15s ease;
    }
    /* connected segments: one control, rounded only on the outer edges */
    .smb-seg-v { border-radius: 4px 0 0 4px; }
    .smb-seg-r { border-radius: 0 4px 4px 0; border-left: none; }
    .smb-seg:hover {
      background: var(--blockr-grey-100, #f3f4f6);
      border-color: var(--blockr-grey-300, #d1d5db);
      color: var(--blockr-grey-600, #4b5563);
    }
    /* active segment: muted blue tint (blockr-popover-toggle-active), not a solid fill */
    .smb-radio-v:checked ~ .smb-switch .smb-seg-v,
    .smb-radio-r:checked ~ .smb-switch .smb-seg-r {
      color: var(--blockr-color-primary, #2563eb);
      background: rgba(37, 99, 235, 0.08);
      border-color: rgba(37, 99, 235, 0.2);
    }

    .smb-rtext {
      display: none; margin: 0; padding: 10px 12px;
      background: var(--blockr-grey-50, #f9fafb); border-radius: 6px;
      font-family: 'SF Mono', 'Fira Code', 'Consolas', 'Monaco', monospace;
      font-size: var(--blockr-font-size-xs, 0.75rem);
      color: var(--blockr-grey-700, #374151);
      overflow-x: auto; white-space: pre;
    }
    .smb-radio-r:checked ~ .smb-visual { display: none; }
    .smb-radio-r:checked ~ .smb-rtext { display: block; }
    /* R-only card (no usable visual, e.g. KM/CIF): always show the summary */
    .smb-rtext.smb-rtext-only { display: block; }

    /* headline */
    .smb-hl {
      display: flex; align-items: center; gap: 16px;
      padding-bottom: 8px;
    }
    .smb-hl-kind {
      min-width: 0; flex: 1 1 auto;
      font-size: var(--blockr-font-size-sm, 0.8125rem);
      color: var(--blockr-grey-600, #4b5563);
      white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
    }
    .smb-chip { flex: 0 0 auto; text-align: right; min-width: 64px; }
    .smb-chip-row { display: flex; align-items: baseline; gap: 5px; justify-content: flex-end; }
    .smb-chip-k { font-size: var(--blockr-font-size-xs, 0.75rem); color: var(--blockr-grey-500, #6b7280); }
    .smb-chip-v {
      font-size: var(--blockr-font-size-sm, 0.8125rem);
      font-weight: var(--blockr-font-weight-medium, 500);
      color: var(--blockr-grey-600, #4b5563);
      font-variant-numeric: tabular-nums;
    }
    .smb-chip-bar {
      height: 3px; border-radius: 2px; margin-top: 4px;
      background: var(--blockr-grey-200, #e5e7eb);
    }
    .smb-chip-fill { height: 100%; border-radius: 2px; background: var(--blockr-blue-600, #2563eb); }

    /* forest plot */
    .smb-forest { width: 100%; border-collapse: collapse; margin: 2px 0 4px; }
    .smb-forest td { padding: 3px 0; vertical-align: middle; }
    .smb-fterm {
      width: 1%; white-space: nowrap; padding-right: 12px !important;
      font-size: var(--blockr-font-size-sm, 0.8125rem);
      color: var(--blockr-grey-700, #374151);
    }
    .smb-fbar { width: 100%; }
    .smb-fval {
      width: 1%; white-space: nowrap; padding-left: 12px !important;
      text-align: right; font-variant-numeric: tabular-nums;
      font-size: var(--blockr-font-size-sm, 0.8125rem);
    }
    .smb-track { position: relative; height: 16px; }
    .smb-ref {
      position: absolute; top: 0; bottom: 0; width: 1px;
      background: var(--blockr-grey-300, #d1d5db);
    }
    .smb-whisk {
      position: absolute; top: 50%; height: 2px;
      transform: translateY(-50%); border-radius: 1px; min-width: 1px;
    }
    .smb-dot {
      position: absolute; top: 50%; width: 8px; height: 8px;
      border-radius: 50%; transform: translate(-50%, -50%);
      box-shadow: 0 0 0 2px #fff;
    }

    "
  ))
}

#' Force single-column layout for a block
#'
#' @param block_name Character string, name of the block
#' @return HTML style tag with single-column grid CSS
#' @noRd
css_single_column <- function(block_name) {
  tags$style(HTML(sprintf(
    "
    .%s-block-container .block-form-grid {
      grid-template-columns: 1fr !important;
    }
    ",
    block_name
  )))
}
