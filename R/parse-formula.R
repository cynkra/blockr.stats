#' Parse an R model formula into the structured formula-input model
#'
#' The "super-terms" layer behind the formula-input widget. It delegates the
#' core decomposition to base [stats::terms()] (which keeps the normal form
#' honest — it equals what the model actually fits) and adds a thin
#' classification pass on top. Bar-terms (`(1 | g)`, mixed-model random effects)
#' and the multi-part `|` are isolated by `terms()` as single labels and routed
#' to the opaque `bars` tier; everything `terms()` does not recognise as a
#' known kind becomes an opaque term — never dropped.
#'
#' @param text A formula, as a string (e.g. `"mpg ~ hp * cyl"`).
#' @param col_types Named list/character mapping column name -> class
#'   (`"numeric"`, `"factor"`, ...), used to split bare names into main effects
#'   vs factors. Optional; missing types default to main effects.
#' @return A structured list: `response`, `intercept`, `offset`, `weights`,
#'   `terms` (list of classified term objects), `bars` (list of `list(raw=)`).
#' @keywords internal
#' @noRd
parse_formula <- function(text, col_types = list()) {
  f <- stats::as.formula(text)
  has_response <- length(f) == 3L
  tt <- stats::terms(f)
  labels <- attr(tt, "term.labels")
  intercept <- attr(tt, "intercept") == 1L

  is_bar <- vapply(labels, function(l) {
    e <- tryCatch(str2lang(l), error = function(...) NULL)
    is.call(e) && as.character(e[[1L]]) %in% c("|", "||")
  }, logical(1))

  core <- lapply(labels[!is_bar], classify_term, col_types = col_types)
  bars <- lapply(labels[is_bar], function(l) list(raw = l))

  list(
    response  = if (has_response) parse_response(f[[2L]]) else NULL,
    intercept = intercept,
    offset    = NULL,
    weights   = NULL,
    terms     = unname(core),
    bars      = unname(bars)
  )
}

#' Classify a single `terms()` term label into a chip kind
#' @keywords internal
#' @noRd
classify_term <- function(label, col_types = list()) {
  e <- tryCatch(str2lang(label), error = function(...) NULL)
  if (is.null(e)) {
    return(list(kind = "opaque", label = label, raw = label))
  }
  if (is.name(e)) {
    v <- as.character(e)
    ty <- col_types[[v]]
    is_fac <- !is.null(ty) &&
      any(ty %in% c("factor", "ordered", "character", "logical"))
    return(list(kind = if (is_fac) "factor" else "main", label = label, var = v))
  }
  if (is.call(e)) {
    op <- as.character(e[[1L]])
    if (identical(op, ":")) {
      return(list(kind = "interaction", label = label, vars = all.vars(e)))
    }
    if (identical(op, "poly")) {
      deg <- if (!is.null(e[["degree"]])) e[["degree"]]
        else if (length(e) >= 3L) e[[3L]] else NULL
      degree <- tryCatch(as.integer(eval(deg)), error = function(...) NA_integer_)
      return(list(kind = "poly", label = label,
                  var = all.vars(e)[1L], degree = degree))
    }
    if (op %in% c("ns", "bs")) {
      dfa <- if (!is.null(e[["df"]])) e[["df"]]
        else if (length(e) >= 3L) e[[3L]] else NULL
      df <- tryCatch(as.integer(eval(dfa)), error = function(...) NA_integer_)
      return(list(kind = "spline", label = label, fn = op,
                  var = all.vars(e)[1L], df = df))
    }
    if (length(all.vars(e)) >= 1L) {
      return(list(kind = "transform", label = label, raw = label,
                  fn = op, var = all.vars(e)[1L]))
    }
  }
  list(kind = "opaque", label = label, raw = label)
}

#' Parse the LHS of a formula into a response descriptor
#'
#' Bare name -> string; `cbind(a, b)` -> `list(fn = "cbind", args = ...)`;
#' anything else -> opaque deparsed text. (`Surv(...)` is intentionally not
#' special-cased here — survival has its own block.)
#' @keywords internal
#' @noRd
parse_response <- function(lhs) {
  if (is.name(lhs)) {
    return(as.character(lhs))
  }
  if (is.call(lhs) && identical(as.character(lhs[[1L]]), "cbind")) {
    return(list(
      fn = "cbind",
      args = vapply(as.list(lhs)[-1L], deparse1, character(1))
    ))
  }
  deparse1(lhs)
}

#' Render a response descriptor back to formula-LHS text
#' @keywords internal
#' @noRd
response_to_text <- function(response) {
  if (is.null(response)) {
    return(NULL)
  }
  if (is.list(response) && identical(response$fn, "cbind")) {
    return(paste0("cbind(", paste(response$args, collapse = ", "), ")"))
  }
  as.character(response)
}
