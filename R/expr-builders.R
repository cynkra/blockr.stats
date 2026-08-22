# Packages a formula term is allowed to reach into, in the order they are
# tried. `stats` first because that is where `poly()` lives and `poly()` is
# the term this exists for; `splines` next because `ns()`/`bs()` were the
# first casualties and are still the second most likely. Anything else that
# happens to be loaded is consulted after these, so a term naming a function
# from the user's own attached package still fits.
formula_fn_pkgs <- c("stats", "splines", "survival", "MASS", "mgcv")

#' Find the package that exports a function name
#'
#' @param nm Function name, unqualified.
#' @return Package name, or `NULL` when nothing loaded exports it.
#' @keywords internal
#' @noRd
formula_fn_pkg <- function(nm) {
  for (pkg in unique(c(formula_fn_pkgs, loadedNamespaces()))) {
    ns <- tryCatch(asNamespace(pkg), error = function(...) NULL)
    if (is.null(ns) || !nm %in% getNamespaceExports(ns)) {
      next
    }
    if (is.function(tryCatch(get(nm, envir = ns), error = function(...) NULL))) {
      return(pkg)
    }
  }
  NULL
}

#' Namespace-qualify the functions named in a formula term
#'
#' EVERY FUNCTION A FORMULA TERM NAMES HAS TO BE FINDABLE AT FIT TIME, and a
#' block does not fit in your console. `blockr.core::eval_env()` parents the
#' evaluation environment on `baseenv()` unless the board sets the
#' `attach_default_packages` option, so the search path is not there and
#' neither is `stats`. `poly(x, 2)` typed into the formula widget therefore
#' fits fine when you try it by hand and dies inside a board with
#' `could not find function "poly"`. So does anything else outside base.
#'
#' `ns()`/`bs()` hit this first and were patched by name with a `sub()` on the
#' label. This is that fix without the list: leave alone anything reachable
#' from `baseenv()` (which is `+`, `:`, `I()`, `log()`, `scale()`, `factor()`
#' and the rest of base), and qualify the rest with whichever loaded package
#' exports it.
#'
#' The qualified name is what reaches the fit AND the exported document, where
#' `stats::poly(...)` is the more honest reading anyway: it is a function the
#' reader can look up. The label the WIDGET shows is untouched -- it comes
#' from `formula_ast_to_text()`, not from here -- so the block's saved state
#' stays unqualified and restores as the user typed it.
#'
#' A label that will not parse, or one naming a function nothing exports, is
#' returned verbatim. That is the pre-existing behaviour and the error it
#' produces is the user's typo, which is theirs to see.
#'
#' @param label A single term label, e.g. `"poly(Date, 2)"`.
#' @return The label with non-base functions qualified.
#' @keywords internal
#' @noRd
qualify_term_fns <- function(label) {
  qualify <- function(e) {
    if (!is.call(e)) {
      return(e)
    }
    fn <- e[[1L]]
    # `::`/`:::` calls are already qualified; walking into one would try to
    # qualify the package name itself.
    already <- is.call(fn) &&
      as.character(fn[[1L]])[1L] %in% c("::", ":::")
    if (is.name(fn) && !as.character(fn) %in% c("::", ":::")) {
      nm <- as.character(fn)
      if (!exists(nm, envir = baseenv(), mode = "function")) {
        pkg <- formula_fn_pkg(nm)
        if (!is.null(pkg)) {
          e[[1L]] <- call("::", as.name(pkg), as.name(nm))
        }
      }
    } else if (already) {
      return(e)
    }
    for (i in seq_along(e)[-1L]) {
      arg <- tryCatch(e[[i]], error = function(...) NULL)
      # Empty arguments (`x[, 1]`) are not missing values you can assign back.
      if (!is.null(arg) && !identical(arg, quote(expr = ))) {
        e[[i]] <- qualify(arg)
      }
    }
    e
  }

  tryCatch(
    paste0(deparse(qualify(str2lang(label)), width.cutoff = 500L),
           collapse = ""),
    error = function(...) label
  )
}

# Packages a formula term is allowed to reach into, in the order they are
# tried. `stats` first because that is where `poly()` lives and `poly()` is
# the term this exists for; `splines` next because `ns()`/`bs()` were the
# first casualties and are still the second most likely. Anything else that
# happens to be loaded is consulted after these, so a term naming a function
# from the user's own attached package still fits.
formula_fn_pkgs <- c("stats", "splines", "survival", "MASS", "mgcv")

#' Find the package that exports a function name
#'
#' @param nm Function name, unqualified.
#' @return Package name, or `NULL` when nothing loaded exports it.
#' @keywords internal
#' @noRd
formula_fn_pkg <- function(nm) {
  for (pkg in unique(c(formula_fn_pkgs, loadedNamespaces()))) {
    ns <- tryCatch(asNamespace(pkg), error = function(...) NULL)
    if (is.null(ns) || !nm %in% getNamespaceExports(ns)) {
      next
    }
    if (is.function(tryCatch(get(nm, envir = ns), error = function(...) NULL))) {
      return(pkg)
    }
  }
  NULL
}

#' Namespace-qualify the functions named in a formula
#'
#' EVERY FUNCTION A FORMULA NAMES HAS TO BE FINDABLE AT FIT TIME, and a block
#' does not fit in your console. `blockr.core::eval_env()` parents the
#' evaluation environment on `baseenv()` unless the board sets the
#' `attach_default_packages` option, so the search path is not there and
#' neither is `stats`. `poly(x, 2)` typed into the formula widget therefore
#' fits fine when you try it by hand and dies inside a board with
#' `could not find function "poly"`. So does `ns()`, `bs()`, and `Surv()` on
#' the response side of the survival block.
#'
#' `ns()`/`bs()` hit this first and were patched by name with a `sub()` on the
#' term label. This is that fix without the list: leave alone anything
#' reachable from `baseenv()` (which is `~`, `+`, `:`, `I()`, `log()`,
#' `scale()`, `factor()` and the rest of base), and qualify the rest with
#' whichever loaded package exports it.
#'
#' WHY THIS RUNS ON THE FITTING CALL AND NOT IN `make_model_formula()`, which
#' is where the `sub()` used to live: that function is also what
#' `formula_ast_to_text()` deparses for the widget's text box and the block's
#' SAVED STATE. Qualifying there rewrites `poly(t, 2)` to `stats::poly(t, 2)`
#' under the user's cursor as they type it, and stores it that way -- and
#' `parse_term()` does not recognise a `::` call as a poly term, so the
#' restored widget shows an opaque chip instead of a degree spinner. Qualify
#' late: the widget and the state stay as the user wrote them, the fit and the
#' exported document get the name that resolves.
#'
#' A term that names a function nothing exports is left alone. The error that
#' produces is the user's typo, which is theirs to see.
#'
#' @param e A language object (a formula, a call, or a name).
#' @return `e` with non-base function calls qualified.
#' @keywords internal
#' @noRd
qualify_fn_calls <- function(e) {
  if (!is.call(e)) {
    return(e)
  }
  fn <- e[[1L]]
  # An already-qualified call: walk no further into it, or the package name
  # itself gets treated as a function to look up.
  if (is.call(fn) && as.character(fn[[1L]])[1L] %in% c("::", ":::")) {
    return(e)
  }
  if (is.name(fn) && !as.character(fn) %in% c("::", ":::")) {
    nm <- as.character(fn)
    if (!exists(nm, envir = baseenv(), mode = "function")) {
      pkg <- formula_fn_pkg(nm)
      if (!is.null(pkg)) {
        e[[1L]] <- call("::", as.name(pkg), as.name(nm))
      }
    }
  }
  for (i in seq_along(e)[-1L]) {
    arg <- tryCatch(e[[i]], error = function(...) NULL)
    # Empty arguments (`x[, 1]`) are not values you can assign back.
    if (!is.null(arg) && !identical(arg, quote(expr = ))) {
      e[[i]] <- qualify_fn_calls(arg)
    }
  }
  e
}

#' Qualify a formula in place, keeping its class and environment
#'
#' @param f A `formula`, or `NULL`.
#' @return The formula with non-base functions qualified.
#' @keywords internal
#' @noRd
qualify_model_formula <- function(f) {
  if (!inherits(f, "formula")) {
    return(f)
  }
  env <- environment(f)
  out <- tryCatch(qualify_fn_calls(f), error = function(...) f)
  environment(out) <- env
  out
}

#' Qualify the functions in a single term label
#'
#' String in, string out. Kept beside [qualify_model_formula()] so the two
#' share one walker; used by the tests and available for any caller holding a
#' label rather than a formula.
#'
#' @param label A single term label, e.g. `"poly(Date, 2)"`.
#' @return The label with non-base functions qualified.
#' @keywords internal
#' @noRd
qualify_term_fns <- function(label) {
  tryCatch(
    paste0(deparse(qualify_fn_calls(str2lang(label)), width.cutoff = 500L),
           collapse = ""),
    error = function(...) label
  )
}

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

  # Emitted verbatim. Qualification of non-base functions happens later, on
  # the fitting call, so that this function's output stays usable as the
  # widget's text and the block's saved state -- see qualify_model_formula().
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
  f <- qualify_model_formula(f)
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

#' Build the model summary expression
#'
#' One `blockr.stats::model_summary()` call over the upstream model. Options
#' left at their default are pruned from the call, so the common case exports
#' as a bare `model_summary(model)` and only the settings the user actually
#' changed appear in the code.
#'
#' Unlike the broom block this does name a blockr.stats function: the card's
#' options travel with the value (as attributes), which is what keeps the
#' rendered card and the returned frame from ever disagreeing.
#'
#' @inheritParams model_summary
#' @return A language object.
#' @keywords internal
#' @noRd
build_model_summary_call <- function(uncertainty, significance, scale,
                                     effect_column, facts, intercept) {
  vals <- list(
    uncertainty = uncertainty,
    significance = significance,
    scale = scale,
    effect_column = isTRUE(effect_column),
    facts = isTRUE(facts),
    intercept = isTRUE(intercept)
  )

  # Placeholders are named for the arguments they fill, so `vals` doubles as
  # the substitution list and every `.()` name resolves to a formal above
  # (a bare `.(u)` reads as an undefined global to R CMD check).
  call <- blockr.core::bbquote(
    blockr.stats::model_summary(
      .(data),
      uncertainty = .(uncertainty), significance = .(significance),
      scale = .(scale), effect_column = .(effect_column),
      facts = .(facts), intercept = .(intercept)
    ),
    vals
  )

  for (nm in names(vals)) {
    if (identical(vals[[nm]], ms_defaults[[nm]])) {
      call[[nm]] <- NULL
    }
  }
  call
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
      cl <- conf_level
      tidy_call <- if (isTRUE(conf_int)) {
        blockr.core::bbquote(
          broom::tidy(.(data), conf.int = TRUE, conf.level = .(cl)),
          list(cl = cl)
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
      tc <- tidy_call
      blockr.core::bbquote(as.data.frame(.(tc)), list(tc = tc))
    }
  )
}
