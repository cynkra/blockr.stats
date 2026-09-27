#' CSS for the blocks' outputs
#'
#' The R print of the model and survival blocks, and the model summary card.
#' Tokens only: blockr.ui's theme defines them ([blockr.ui::theme_dep()],
#' attached with each output).
#'
#' @noRd
NULL

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
      font-size: var(--blockr-font-size-base);
      color: var(--blockr-color-text-default);
      padding: 4px 2px;
    }

    .smb-rtext {
      margin: 0; padding: 10px 12px;
      background: var(--blockr-color-bg-subtle);
      border-radius: var(--blockr-radius-md);
      font-family: var(--blockr-font-mono);
      font-size: var(--blockr-font-size-sm);
      color: var(--blockr-color-text-default);
      overflow-x: auto; white-space: pre;
    }
    "
  ))
}

#' Model summary block card CSS
#'
#' Styles `model_summary_card()`: the facts stripe, the coefficient table and
#' the inline forest column (track, reference line, whisker, dot, axis). The
#' table takes the design system's table style: 13px semibold headers with a
#' rule under each, 30px rows, no rules in the body, the hover wash, sort
#' bars.
#'
#' @return HTML style tag.
#' @noRd
css_summary_card <- function() {
  tags$style(HTML(
    "
    .msc-card {
      /* The forest's mark colours: an effect above the reference, below it,
         and one not told apart from none. Local names for meaning tokens,
         so the three can move to blockr.theme's data colours together. */
      --blockr-stats-mark-positive: var(--blockr-color-border-accent);
      --blockr-stats-mark-negative: var(--blockr-color-border-danger);
      --blockr-stats-mark-null: var(--blockr-color-text-disabled);

      font-size: var(--blockr-font-size-base);
      color: var(--blockr-color-text-default);
      padding: 2px 0 6px;
    }
    .msc-note {
      padding: 14px 0;
      color: var(--blockr-color-text-muted);
      font-size: var(--blockr-font-size-sm);
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
      font-size: var(--blockr-font-size-sm);
      color: var(--blockr-color-text-muted);
    }
    .msc-id {
      font-weight: var(--blockr-font-weight-medium);
      color: var(--blockr-color-text-default);
    }
    .msc-n {
      font-weight: var(--blockr-font-weight-normal);
      color: var(--blockr-color-text-muted);
    }
    .msc-fit { margin-left: auto; display: flex; gap: 14px; align-items: baseline; flex-wrap: wrap; }
    .msc-pair { font-variant-numeric: tabular-nums; white-space: nowrap; }
    .msc-pair b {
      color: var(--blockr-color-text-default);
      font-weight: var(--blockr-font-weight-medium);
    }
    .msc-sep { color: var(--blockr-color-border-strong); }

    /* The coefficient table, in the design system's table style. */
    .msc-ct { width: 100%; border-collapse: collapse; }
    .msc-ct th {
      padding: 7px 0; text-align: left; vertical-align: bottom;
      font-size: var(--blockr-font-size-sm);
      font-weight: var(--blockr-font-weight-semibold);
      color: var(--blockr-color-text-default);
      white-space: nowrap;
      /* A rule under each column header, 12px short of the next column,
         in place of one rule across the header. None under the term
         column, which holds the row labels. */
      background-image: linear-gradient(var(--blockr-color-border-strong), var(--blockr-color-border-strong));
      background-repeat: no-repeat;
      background-size: calc(100% - 12px) 1px;
      background-position: left bottom;
    }
    .msc-ct th:first-child { background-image: none; }
    .msc-ct th.dt-col-num { text-align: right; background-position: right bottom; }
    .msc-ct .blockr-col-name {
      display: inline-block; max-width: 100%;
      overflow: hidden; text-overflow: ellipsis;
    }
    .msc-ct .dt-th-namerow { display: inline-flex; align-items: center; gap: 4px; }
    /* On a right-aligned column the cue goes before the name, so the name
       stays over its numbers. */
    .msc-ct th.dt-col-num .dt-th-namerow { flex-direction: row-reverse; }

    /* Sorting is a browser-side reading aid (see inst/js/model-summary-sort.js):
       never stored, never in the exported code. The cue is the design
       system's sort bars, drawn top to bottom short to long for ascending,
       long to short for descending, in the accent on the sorted column. An
       unsorted column shows the ascending bars, muted, on hover. The 12px box
       is always reserved, so a sort never moves a column. */
    .msc-ct th.blockr-sortable {
      cursor: pointer; user-select: none;
      transition: background-color var(--blockr-transition);
    }
    .msc-ct th.blockr-sortable:hover { background-color: var(--blockr-color-bg-hover); }
    .msc-ct .blockr-sort-icon {
      flex: none; width: 12px; height: 12px;
      visibility: hidden;
      color: var(--blockr-color-text-muted);
      background-color: currentColor;
      -webkit-mask: url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 12 12' fill='none' stroke='black' stroke-width='1.4' stroke-linecap='round'%3E%3Cpath d='M2 3h3M2 6h5.5M2 9h8'/%3E%3C/svg%3E\") no-repeat center / 12px 12px;
      mask: url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 12 12' fill='none' stroke='black' stroke-width='1.4' stroke-linecap='round'%3E%3Cpath d='M2 3h3M2 6h5.5M2 9h8'/%3E%3C/svg%3E\") no-repeat center / 12px 12px;
    }
    .msc-ct th.blockr-sortable:hover .blockr-sort-icon { visibility: visible; }
    .msc-ct .blockr-sort-icon-asc,
    .msc-ct .blockr-sort-icon-desc {
      visibility: visible;
      color: var(--blockr-color-text-accent);
    }
    .msc-ct .blockr-sort-icon-desc {
      -webkit-mask-image: url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 12 12' fill='none' stroke='black' stroke-width='1.4' stroke-linecap='round'%3E%3Cpath d='M2 3h8M2 6h5.5M2 9h3'/%3E%3C/svg%3E\");
      mask-image: url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 12 12' fill='none' stroke='black' stroke-width='1.4' stroke-linecap='round'%3E%3Cpath d='M2 3h8M2 6h5.5M2 9h3'/%3E%3C/svg%3E\");
    }

    /* 30px rows: 20px of text and 5px above and below. No rules in the
       body; the hover wash carries the eye along a row. */
    .msc-ct tbody tr { transition: background-color var(--blockr-transition); }
    .msc-ct tbody tr:hover { background-color: var(--blockr-color-bg-hover); }
    .msc-ct td {
      padding: 5px 0; line-height: 20px; vertical-align: middle;
    }
    /* Side padding: the card keeps its own, tighter than the preview's 12px
       per cell, so the table fits a half-width panel: 16px between columns,
       none at the card's edges. */
    .msc-ct .msc-term { padding-right: 16px; }
    .msc-ct td.msc-num { padding-left: 16px; }
    .msc-ct td.msc-sig { padding-left: 12px; }
    .msc-term { white-space: nowrap; color: var(--blockr-color-text-default); }
    /* ONE grey for quiet text, everywhere: the factor level after its
       variable, the intercept row, the axis ticks and the facts-line labels.
       The mark grey (--blockr-stats-mark-null) is a data colour and stays
       apart from it: grey text reads as quieter, a grey mark reads as no
       effect. */
    .msc-lvl { color: var(--blockr-color-text-muted); }
    /* Muted text never sits on the hover wash: on a hovered row it turns
       default. */
    .msc-ct tbody tr:hover .msc-lvl,
    .msc-ct tbody tr.msc-int:hover td { color: var(--blockr-color-text-default); }
    .msc-num {
      text-align: right; white-space: nowrap;
      font-variant-numeric: tabular-nums;
    }
    .msc-sig {
      text-align: right; white-space: nowrap; width: 1%;
      font-variant-numeric: tabular-nums;
    }
    /* the intercept is a nuisance term: present, and quiet in the same grey
       as every other quiet thing */
    .msc-int td { color: var(--blockr-color-text-muted); }

    /* Significance levels: badges on a ladder. 0.1%, 1% and 5% are accent
       tints deepening one step at a time, because 5% is the line most
       readers look for and a grey badge there would dismiss the very terms
       they want; 10% is neutral, borderline rather than a result; above
       10% there is no badge. This is an exception to the spec's neutral
       badge: the badge encodes the strength of the evidence, which is data.
       The steps are one ramp on the accent token, so they follow the theme
       and dark mode. One width for every level, so a shorter label does
       not read as a smaller finding. */
    .msc-chip {
      display: inline-flex; align-items: center; justify-content: center;
      box-sizing: border-box; height: 18px; min-width: 42px;
      padding: 0 7px;
      font-size: 11px; font-weight: var(--blockr-font-weight-medium);
      line-height: 1; white-space: nowrap;
      border-radius: var(--blockr-radius-pill);
      background: color-mix(in srgb, var(--blockr-color-border-accent) 22%, transparent);
      border: 1px solid color-mix(in srgb, var(--blockr-color-border-accent) 62%, transparent);
      color: var(--blockr-color-text-accent);
      font-variant-numeric: tabular-nums;
    }
    .msc-chip--1 {
      background: color-mix(in srgb, var(--blockr-color-border-accent) 13%, transparent);
      border-color: color-mix(in srgb, var(--blockr-color-border-accent) 38%, transparent);
    }
    .msc-chip--5 {
      background: color-mix(in srgb, var(--blockr-color-border-accent) 6%, transparent);
      border-color: color-mix(in srgb, var(--blockr-color-border-accent) 20%, transparent);
    }
    .msc-chip--10 {
      background: var(--blockr-color-bg-subtle);
      border-color: var(--blockr-color-border-default);
      color: var(--blockr-color-text-muted);
    }

    /* inline forest column */
    .msc-eff { width: 42%; min-width: 140px; }
    .msc-track { position: relative; height: 16px; }
    .msc-ref {
      position: absolute; top: 0; bottom: 0; width: 1px;
      background: var(--blockr-color-border-strong);
    }
    .msc-whisk {
      position: absolute; top: 50%; height: 2px; min-width: 2px;
      transform: translateY(-50%); border-radius: 1px;
    }
    .msc-dot {
      position: absolute; top: 50%; width: 8px; height: 8px;
      border-radius: 50%; transform: translate(-50%, -50%);
      box-shadow: 0 0 0 2px var(--blockr-color-bg-surface);
    }
    /* off-scale marker: the term ran past the axis, the axis did not move.
       It is a mark, so it takes the mark grey. */
    .msc-off {
      position: absolute; top: 50%; transform: translateY(-50%);
      font-size: 9px; line-height: 1;
      color: var(--blockr-stats-mark-null);
    }

    /* shared axis under the forest column */
    .msc-axis { position: relative; height: 15px; }
    .msc-axis span {
      position: absolute; top: 0; transform: translateX(-50%);
      font-size: var(--blockr-font-size-xs);
      color: var(--blockr-color-text-muted);
      font-variant-numeric: tabular-nums; white-space: nowrap;
    }
    .msc-ct tfoot td { padding-top: 2px; }
    .msc-ct tfoot tr:hover { background: none; }
    "
  ))
}
