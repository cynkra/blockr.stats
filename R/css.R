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
      border: 1px solid var(--blockr-color-border, #e5e7eb);
      border-radius: 6px;
      overflow: hidden;
    }
    .smb-empty {
      padding: 12px;
      color: var(--blockr-color-text-muted, #9ca3af);
    }

    .smb-head {
      display: flex;
      flex-wrap: wrap;
      align-items: baseline;
      gap: 4px 10px;
      padding: 8px 12px;
      border-bottom: 1px solid var(--blockr-color-border, #e5e7eb);
      background: var(--blockr-grey-50, #f9fafb);
    }
    .smb-kind { font-weight: var(--blockr-font-weight-semibold, 600); }
    .smb-formula {
      font-family: 'SF Mono', 'Fira Code', 'Consolas', 'Monaco', monospace;
      font-size: var(--blockr-font-size-xs, 0.75rem);
      color: var(--blockr-grey-600, #4b5563);
      background: transparent;
      padding: 0;
    }
    .smb-n {
      margin-left: auto;
      font-size: var(--blockr-font-size-xs, 0.75rem);
      color: var(--blockr-grey-500, #6b7280);
    }

    .smb-coef {
      width: 100%;
      border-collapse: collapse;
      display: block;
      overflow-x: auto;
    }
    .smb-coef th, .smb-coef td {
      padding: 5px 12px;
      text-align: right;
      white-space: nowrap;
      border-bottom: 1px solid var(--blockr-grey-100, #f3f4f6);
    }
    .smb-coef th {
      font-size: var(--blockr-font-size-xs, 0.75rem);
      font-weight: var(--blockr-font-weight-medium, 500);
      color: var(--blockr-grey-500, #6b7280);
    }
    .smb-coef td.smb-term, .smb-coef th:first-child { text-align: left; }
    .smb-coef td.smb-est { font-weight: var(--blockr-font-weight-semibold, 600); }
    .smb-coef td.smb-p   { color: var(--blockr-grey-500, #6b7280); }
    .smb-coef tbody tr:last-child td { border-bottom: none; }

    .smb-fit {
      display: flex;
      flex-wrap: wrap;
      gap: 4px 14px;
      padding: 8px 12px;
      border-top: 1px solid var(--blockr-color-border, #e5e7eb);
      background: var(--blockr-grey-50, #f9fafb);
      font-size: var(--blockr-font-size-xs, 0.75rem);
    }
    .smb-pill-k { color: var(--blockr-grey-500, #6b7280); margin-right: 4px; }
    .smb-pill-v { color: var(--blockr-grey-700, #374151); font-weight: var(--blockr-font-weight-medium, 500); }
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
