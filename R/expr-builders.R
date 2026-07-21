#' Build a model formula from the structured formula-input state
#'
#' Assembles a `response ~ rhs` formula from the structured model produced by
#' `parse_formula()` / the formula-input widget. Core terms and opaque/bar
#' terms are emitted by their stored labels; the intercept is handled via
#' [stats::reformulate()]'s `intercept` argument. Returns `NULL` when there is
#' no response (so the consuming block can pass through / emit `NULL`).
#'
#' Offset and weights are NOT part of the formula — they are role-box state
#' passed as arguments to the fitting call by the consuming model block.
#'
#' @param state Structured formula model (see `parse_formula()`).
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

  labels <- vapply(terms_list, function(t) {
    lbl <- t$label
    # ns()/bs() live in the splines package (not attached): emit prefixed so the
    # fit can find them. Covers both menu- and text-added spline terms.
    if (identical(t$kind, "spline")) {
      lbl <- sub("^(ns|bs)\\(", "splines::\\1(", lbl)
    }
    lbl
  }, character(1))
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
#' resolve (`expr_type = "bquoted"`). `weights` / `offset`, when supplied, are
#' column names spliced in as bare symbols (resolved in the data frame by the
#' fitting function).
#'
#' @param model_type One of `"lm"`, `"logistic"`, `"poisson"`, `"gamma"`.
#' @param f A `formula` (from `make_model_formula()`).
#' @param weights,offset Optional column-name strings (or `NULL`).
#' @keywords internal
#' @noRd
build_model_call <- function(model_type, f, weights = NULL, offset = NULL) {
  call <- switch(
    model_type,
    logistic = blockr.core::bbquote(
      stats::glm(.(f), data = .(data), family = stats::binomial()), list(f = f)),
    poisson = blockr.core::bbquote(
      stats::glm(.(f), data = .(data), family = stats::poisson()), list(f = f)),
    gamma = blockr.core::bbquote(
      stats::glm(.(f), data = .(data), family = stats::Gamma()), list(f = f)),
    blockr.core::bbquote(stats::lm(.(f), data = .(data)), list(f = f))
  )
  if (!is.null(weights) && is.character(weights) && nzchar(weights)) {
    call[["weights"]] <- as.name(weights)
  }
  if (!is.null(offset) && is.character(offset) && nzchar(offset)) {
    call[["offset"]] <- as.name(offset)
  }
  call
}

#' Parse a formula string into the structured formula-input model, safely
#'
#' The model block authors its formula as a plain STRING (`"mpg ~ hp + wt"`) so
#' that humans and the AI assistant write it natively; the visual formula-input
#' widget keeps a structured AST internally. This seeds that AST from the string,
#' tolerating empty / invalid input by returning an empty (pass-through) model.
#' A list is passed through unchanged (defensive: an already-parsed AST).
#'
#' @param text A formula string, an empty string, or an AST list.
#' @return A structured formula model (see `parse_formula()`).
#' @keywords internal
#' @noRd
parse_formula_safe <- function(text) {
  empty <- list(
    response = NULL, intercept = TRUE,
    terms = list(), bars = list(), offset = NULL, weights = NULL
  )
  if (is.list(text)) {
    return(text)
  }
  if (is.null(text) || !is.character(text) || !nzchar(trimws(text[1L]))) {
    return(empty)
  }
  tryCatch(parse_formula(text), error = function(e) empty)
}

#' Project the structured formula-input model back to a formula string
#'
#' The inverse of [parse_formula_safe()]: turns the widget's AST into the plain
#' string that the block exposes as state. Returns `""` when there is no usable
#' formula (no response), so the state field stays an empty-string sentinel.
#'
#' @param state Structured formula model (see `parse_formula()`).
#' @return A length-1 character (possibly `""`).
#' @keywords internal
#' @noRd
formula_ast_to_text <- function(state) {
  f <- tryCatch(make_model_formula(state), error = function(e) NULL)
  if (is.null(f)) {
    return("")
  }
  paste(trimws(deparse(f)), collapse = " ")
}

#' Build the standard-R broom expression for the selected output
#'
#' Emits plain `broom::tidy()` / `glance()` / `augment()` with the broom
#' block's conveniences inlined (CI fallback, optional QQ columns, model-aware
#' column `label` attributes). No blockr.stats function appears in the result.
#'
#' @param output One of `"tidy"`, `"glance"`, `"augment"`.
#' @param conf_int,conf_level CI controls for `tidy`.
#' @param qq Add QQ columns to `augment`.
#' @return A language object using only `broom` / base R.
#' @keywords internal
#' @noRd
build_broom_call <- function(output, conf_int = TRUE, conf_level = 0.95,
                             qq = FALSE) {
  # The block's input must reach the exported code as the `.(data)`
  # placeholder that blockr.core substitutes with the upstream block's
  # name (this is a bquoted block). A bare `data` is bound at runtime by
  # the block server but is UNBOUND in the reproducible code the outline /
  # generate_code emit -- broom::glance(data) then resolves `data` to the
  # base function and errors. blockr.core::bbquote leaves `.(data)` intact
  # (it only substitutes vars named in the explicit `where` list), the way
  # the model block does.
  switch(
    output,
    glance = blockr.core::bbquote(
      as.data.frame(broom::glance(.(data))), list()
    ),
    augment = if (isTRUE(qq)) {
      blockr.core::bbquote({
        out <- as.data.frame(broom::augment(.(data)))
        if (".std.resid" %in% names(out)) {
          qn <- stats::qqnorm(out$.std.resid, plot.it = FALSE)
          out$.qq_theoretical <- qn$x
          out$.qq_sample <- qn$y
        }
        out
      }, list())
    } else {
      blockr.core::bbquote(as.data.frame(broom::augment(.(data))), list())
    },
    {
      tidy_call <- if (isTRUE(conf_int)) {
        blockr.core::bbquote(
          broom::tidy(.(data), conf.int = TRUE, conf.level = .(cl)),
          list(cl = conf_level)
        )
      } else {
        blockr.core::bbquote(broom::tidy(.(data)), list())
      }
      # Just the tidy call. broom::tidy methods take conf.int through
      # `...` and ignore it where it does not apply (checked: survfit,
      # coxph), so no fallback is needed -- if a model genuinely cannot be
      # tidied, let it error. (Earlier this branch also stamped pretty
      # column labels as attributes; nothing read them but the app header,
      # they never reached the rendered report, and they buried the code.)
      blockr.core::bbquote(as.data.frame(.(tc)), list(tc = tidy_call))
    }
  )
}
