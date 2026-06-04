#' Build a model formula from the structured formula-input state
#'
#' Assembles a `response ~ rhs` formula from the structured model produced by
#' [parse_formula()] / the formula-input widget. Core terms and opaque/bar
#' terms are emitted by their stored labels; the intercept is handled via
#' [stats::reformulate()]'s `intercept` argument. Returns `NULL` when there is
#' no response (so the consuming block can pass through / emit `NULL`).
#'
#' Offset and weights are NOT part of the formula — they are role-box state
#' passed as arguments to the fitting call by the consuming model block.
#'
#' @param state Structured formula model (see [parse_formula()]).
#' @return A `formula`, or `NULL`.
#' @keywords internal
#' @noRd
make_model_formula <- function(state) {
  if (is.null(state) || is.null(state$response)) {
    return(NULL)
  }
  resp <- response_to_text(state$response)
  if (is.null(resp) || !nzchar(resp)) {
    return(NULL)
  }

  terms_list <- if (is.null(state$terms)) list() else state$terms
  bars_list  <- if (is.null(state$bars)) list() else state$bars

  labels <- vapply(terms_list, function(t) t$label, character(1))
  bars   <- vapply(bars_list, function(b) sprintf("(%s)", b$raw), character(1))
  rhs    <- c(labels, bars)

  intercept <- is.null(state$intercept) || isTRUE(state$intercept)

  if (!length(rhs)) {
    if (!intercept) {
      return(NULL)          # no terms and no intercept = empty model
    }
    rhs <- "1"
  }

  stats::reformulate(rhs, response = resp, intercept = intercept)
}

#' Build the bquoted fitting call for a model type
#'
#' Splices the formula `f` as a value and leaves `.(data)` for blockr.core to
#' resolve (`expr_type = "bquoted"`). Weights/offset wiring is deferred to the
#' role-box UI (the widget's JS layer).
#'
#' @param model_type One of `"lm"`, `"logistic"`, `"poisson"`, `"gamma"`.
#' @param f A `formula` (from [make_model_formula()]).
#' @keywords internal
#' @noRd
build_model_call <- function(model_type, f) {
  switch(
    model_type,
    logistic = blockr.core::bbquote(
      stats::glm(.(f), data = .(data), family = stats::binomial()), list(f = f)),
    poisson = blockr.core::bbquote(
      stats::glm(.(f), data = .(data), family = stats::poisson()), list(f = f)),
    gamma = blockr.core::bbquote(
      stats::glm(.(f), data = .(data), family = stats::Gamma()), list(f = f)),
    blockr.core::bbquote(stats::lm(.(f), data = .(data)), list(f = f))
  )
}
