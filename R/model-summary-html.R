#' Minimal R-print model preview
#'
#' The model / survival block's `block_output` preview: `summary(model)` as
#' text, and nothing else. The block's job is to fit and return the model
#' object; **the visual summary is a separate block downstream**
#' ([new_model_summary_block()]), so the fit and the way it is drawn stay
#' independent and the card is placeable, exportable and configurable without
#' touching the model.
#'
#' Until 2026-07-26 this preview carried a Visual / R toggle with a small
#' forest plot. That visual is now the summary block's card, at full size and
#' with its own options; showing it here as well would be the same numbers
#' rendered twice.
#'
#' @param model A fitted model object (or `NULL`).
#' @param ... Ignored, kept so existing callers do not break.
#' @return An [htmltools::tagList()] / tag.
#' @noRd
model_summary_html <- function(model, ...) {
  if (is.null(model)) {
    return(tags$div(class = "smb-card smb-empty", "Pick variables to fit a model."))
  }

  rtext <- tryCatch({
    out <- utils::capture.output(summary(model))
    if (length(out) > 200L) out <- c(out[seq_len(200L)], "...")
    paste(out, collapse = "\n")
  }, error = function(e) "summary() not available")

  tags$div(class = "smb-card", tags$pre(class = "smb-rtext", rtext))
}
