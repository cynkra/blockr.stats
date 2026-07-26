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
#' Styles the model / survival block's R-print preview
#' (`model_summary_html()`). The card's own styles live in
#' `css_summary_card()`.
#'
#' @return HTML style tag.
#' @noRd
css_model_summary <- function() {
  tags$style(HTML(
    "
    .smb-card {
      font-size: var(--blockr-font-size-base, 0.875rem);
      color: var(--blockr-color-text-primary, #111827);
      padding: 4px 2px;
    }
    .smb-empty { padding: 16px; color: var(--blockr-color-text-muted, #6b7280); }

    .smb-rtext {
      margin: 0; padding: 10px 12px;
      background: var(--blockr-color-bg-subtle, #f9fafb);
      border-radius: var(--blockr-radius-md, 6px);
      font-family: var(--blockr-font-mono, 'SF Mono', 'Fira Code', 'Consolas', 'Monaco', monospace);
      font-size: var(--blockr-font-size-xs, 0.75rem);
      color: var(--blockr-color-text-secondary, #374151);
      overflow-x: auto; white-space: pre;
    }
    "
  ))
}

#' Model summary block card CSS
#'
#' Styles `model_summary_card()`: the facts stripe, the coefficient table and
#' the inline forest column (track, reference line, whisker, dot, axis).
#' `--blockr-*` tokens with literal fallbacks, so the card also renders
#' standalone in a dev harness or a knitted report.
#'
#' @return HTML style tag.
#' @noRd
css_summary_card <- function() {
  tags$style(HTML(
    "
    .msc-card {
      font-size: var(--blockr-font-size-base, 0.875rem);
      color: var(--blockr-color-text-primary, #111827);
      padding: 2px 2px 6px;
    }
    .msc-empty, .msc-note {
      padding: 14px 2px;
      color: var(--blockr-color-text-muted, #6b7280);
      font-size: var(--blockr-font-size-sm, 0.8125rem);
    }

    /* Don't blink on an update. Shiny fades a recalculating output to 30%
       (`.recalculating { opacity: var(--_shiny-fade-opacity) }`, 250ms after
       a 500ms delay), so a refit that takes longer than half a second reads
       as the card vanishing and coming back. It has not vanished: its numbers
       are half a second stale, which is worth nothing to signal and a lot to
       flicker over. Measured: the DOM swap itself is atomic and the output
       slot is never empty. Same suppression blockr.ui applies to its table
       preview. */
    .shiny-html-output.recalculating:has(.msc-card) {
      --_shiny-fade-opacity: 1;
    }

    /* facts line (S2): what the model IS on the left, how well it FITS on
       the right, where the numbers line up with the table's numeric columns */
    .msc-facts {
      display: flex; flex-wrap: wrap; align-items: baseline;
      gap: 6px 16px; padding: 2px 0 10px;
      font-size: var(--blockr-font-size-sm, 0.8125rem);
      color: var(--blockr-color-text-muted, #6b7280);
    }
    .msc-id {
      font-weight: var(--blockr-font-weight-medium, 500);
      color: var(--blockr-color-text-secondary, #374151);
    }
    .msc-n { font-weight: var(--blockr-font-weight-normal, 400); color: var(--blockr-color-text-muted, #6b7280); }
    .msc-fit { margin-left: auto; display: flex; gap: 14px; align-items: baseline; flex-wrap: wrap; }
    .msc-pair { font-variant-numeric: tabular-nums; white-space: nowrap; }
    .msc-pair b {
      color: var(--blockr-color-text-secondary, #374151);
      font-weight: var(--blockr-font-weight-medium, 500);
    }
    .msc-sep { color: var(--blockr-grey-300, #d1d5db); }

    /* coefficient table. The header is blockr's column header, the same
       recipe as blockr.ui's table preview and blockr.viz's table block: 14px
       medium in primary ink, numeric columns right-aligned off the column
       type. No sub-label tier -- the interval belongs in the name it
       qualifies. */
    .msc-ct { width: 100%; border-collapse: collapse; }
    .msc-ct thead tr { border-bottom: 1px solid var(--blockr-color-border, #e5e7eb); }
    .msc-ct th {
      padding: 0 0 7px; text-align: left; vertical-align: bottom;
      font-weight: var(--blockr-font-weight-medium, 500);
      color: var(--blockr-color-text-primary, #111827);
      white-space: nowrap;
    }
    .msc-ct .blockr-col-name {
      display: inline-block; max-width: 100%;
      font-size: 14px; font-weight: var(--blockr-font-weight-medium, 500);
      color: var(--blockr-color-text-primary, #111827);
      overflow: hidden; text-overflow: ellipsis;
    }
    .msc-ct th.dt-col-num { text-align: right; }
    .msc-ct th.dt-col-num .dt-th-namerow { justify-content: flex-end; }
    .msc-ct .dt-th-namerow { display: flex; align-items: center; gap: 4px; }

    /* Sorting is a browser-side reading aid (see inst/js/model-summary-sort.js):
       useful with many predictors, never stored, never in the exported code.
       The icon box is ALWAYS 12px wide, exactly as the html preview does it:
       the arrow appears inside space that was already reserved, so setting or
       clearing a sort never changes a column width. No hover hint -- the
       preview has none, and a hint that grows the box shifts the whole table
       under the pointer. Hover feedback is the row tint, which costs no
       layout. */
    .msc-ct th.blockr-sortable { cursor: pointer; user-select: none; transition: background-color 0.15s ease; }
    .msc-ct th.blockr-sortable:hover { background-color: var(--blockr-color-bg-subtle, #f9fafb); }
    .msc-ct .blockr-sort-icon {
      display: inline-block; width: 12px; height: 12px;
      font-size: 10px; line-height: 12px; text-align: center;
    }
    .msc-ct .blockr-sort-icon-asc::after { content: '\\2191'; color: var(--blockr-grey-700, #374151); }
    .msc-ct .blockr-sort-icon-desc::after { content: '\\2193'; color: var(--blockr-grey-700, #374151); }
    .msc-ct td {
      padding: 7px 0; vertical-align: middle;
      border-bottom: 1px solid var(--blockr-grey-100, #f3f4f6);
    }
    .msc-ct tbody tr:last-child td { border-bottom: none; }
    .msc-term {
      white-space: nowrap; padding-right: 16px !important;
      color: var(--blockr-color-text-secondary, #374151);
    }
    /* ONE grey for quiet text, everywhere: the factor level after its
       variable, the intercept row, the axis ticks and the facts-line labels.
       It is text-muted rather than text-subtle because some of that text is
       DATA (the intercept's estimate and interval) and text-subtle does not
       carry enough contrast to read numbers off. The separate, lighter grey in
       this card belongs to the MARK, not to text: a dot or whisker greys when
       the term is not distinguishable from zero, which is a data colour beside
       the blue and the red. So grey text reads as quieter, a grey mark reads
       as no effect, and the two can no longer be confused. */
    .msc-lvl { color: var(--blockr-color-text-muted, #6b7280); }
    .msc-num {
      text-align: right; white-space: nowrap;
      padding-left: 16px !important;
      font-variant-numeric: tabular-nums;
    }
    .msc-sig {
      text-align: right; white-space: nowrap;
      padding-left: 12px !important; width: 1%;
      font-variant-numeric: tabular-nums;
    }
    /* the intercept is a nuisance term: present, and quiet in the same grey
       as every other quiet thing */
    .msc-int td { color: var(--blockr-color-text-muted, #6b7280); }

    /* Significance chips are the house two-tone badge: the same recipe
       blockr.dock uses for the package name in Block details
       (.badge-two-tone) -- 2px 8px, 10px, 4px radius, tinted fill inside a
       1px border. Only the tint changes with the level. A solid fill was
       shouting: three chips per row, on every row.

       The ladder: 5% / 1% / 0.1% are all COLOURED, deepening one step at a
       time, because 5% is the line most applied fields actually read; a grey
       chip there would dismiss the very terms the reader is looking for. The
       10% level (the dot in R's stars) does get a chip, in the neutral grey
       badge: worth marking as borderline, not worth reading as a result.
       Above 10%, no chip at all.

       The three blue steps are one alpha ramp on the primary rather than
       three separate hues, so the increase reads as intensity. Alpha of the
       primary is established house practice (--blockr-focus-ring,
       --blockr-color-primary-subtle in viz). */
    /* One box for every level. The labels differ in width (four characters
       for 0.1%, two for 1%) and a chip that shrinks with its label makes the
       weaker levels read as physically smaller findings. Fixed min-width and
       centred text, so only colour varies down the column.

       min-width is the widest label's OWN width and not a round number:
       measured at 40.8px for the 0.1% label with this font and padding, so
       4.1em at 10px. Any more and the chip carries surplus air that the
       package badge in Block details does not, which is what made it read as
       a size bigger than the rest of the house. Height, font and padding
       already match that badge exactly. */
    .msc-chip {
      display: inline-block; box-sizing: border-box;
      min-width: 4.1em; padding: 2px 8px; text-align: center;
      font-size: 0.625rem; border-radius: var(--blockr-radius-sm, 4px);
      white-space: nowrap;
      background-color: rgba(37, 99, 235, 0.22);
      border: 1px solid rgba(37, 99, 235, 0.62);
      color: var(--blockr-blue-700, #1d4ed8);
    }
    .msc-chip--1 {
      background-color: rgba(37, 99, 235, 0.13);
      border-color: rgba(37, 99, 235, 0.38);
      color: var(--blockr-blue-700, #1d4ed8);
    }
    .msc-chip--5 {
      background-color: rgba(37, 99, 235, 0.06);
      border-color: rgba(37, 99, 235, 0.20);
      color: var(--blockr-blue-600, #2563eb);
    }
    .msc-chip--10 {
      background-color: var(--blockr-grey-100, #f3f4f6);
      border-color: var(--blockr-color-border, #e5e7eb);
      color: var(--blockr-color-text-muted, #6b7280);
    }

    /* inline forest column */
    .msc-eff { width: 42%; min-width: 140px; }
    .msc-track { position: relative; height: 16px; }
    .msc-ref {
      position: absolute; top: 0; bottom: 0; width: 1px;
      background: var(--blockr-grey-300, #d1d5db);
    }
    .msc-whisk {
      position: absolute; top: 50%; height: 2px; min-width: 2px;
      transform: translateY(-50%); border-radius: 1px;
    }
    .msc-dot {
      position: absolute; top: 50%; width: 8px; height: 8px;
      border-radius: 50%; transform: translate(-50%, -50%);
      box-shadow: 0 0 0 2px #fff;
    }
    /* off-scale marker: the term ran past the axis, the axis did not move */
    /* the off-scale arrow is a mark, so it takes the mark grey */
    .msc-off {
      position: absolute; top: 50%; transform: translateY(-50%);
      font-size: 9px; line-height: 1;
      color: var(--blockr-color-text-subtle, #9ca3af);
    }

    /* shared axis under the forest column */
    .msc-axis { position: relative; height: 15px; }
    .msc-axis span {
      position: absolute; top: 0; transform: translateX(-50%);
      font-size: var(--blockr-font-size-xs, 0.75rem);
      color: var(--blockr-color-text-muted, #6b7280);
      font-variant-numeric: tabular-nums; white-space: nowrap;
    }
    .msc-ct tfoot td { border-bottom: none; padding-top: 2px; }
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
