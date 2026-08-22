#' Formula-input widget: HTML dependency
#'
#' JS + CSS for the formula-input widget. Exported so other blockr packages can
#' embed the widget (mirrors `blockr.dplyr::filter_block_dep()`). Version is
#' pinned to the package version so editing `inst/js` busts the browser cache
#' after a Version bump + reinstall.
#'
#' @return An [htmltools::tagList] of [htmltools::htmlDependency] objects.
#' @export
formula_input_dep <- function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "formula-input-js",
      version = utils::packageVersion("blockr.stats"),
      src = system.file("js", package = "blockr.stats"),
      script = "formula-input.js"
    ),
    htmltools::htmlDependency(
      name = "formula-input-css",
      version = utils::packageVersion("blockr.stats"),
      src = system.file("css", package = "blockr.stats"),
      stylesheet = "formula-input.css"
    )
  )
}

#' Formula-input widget: UI container
#'
#' Pulls the shared blockr JS primitives (from blockr.dplyr) and the
#' formula-input dependency, then the namespaced container the JS binds to.
#'
#' @param id Module id (namespace).
#' @param response_mode `"single"` (one response column) or `"surv"`
#'   (a `Surv(time, event)` response, for the survival block).
#' @return A UI tagList.
#' @export
formula_input_ui <- function(id, response_mode = "single") {
  htmltools::tagList(
    blockr.dplyr::blockr_core_js_dep(),
    blockr.dplyr::blockr_blocks_css_dep(),
    blockr.dplyr::blockr_select_dep(),
    blockr.dplyr::blockr_input_dep(),
    formula_input_dep(),
    shiny::div(
      id = shiny::NS(id, "formula_input"),
      class = "formula-input-container",
      `data-response-mode` = response_mode
    )
  )
}

#' Wire the formula-input widget's server side
#'
#' Call inside a block's `moduleServer`. Sets up typed-column metadata push,
#' the text-parse round-trip, and JS<->R state sync, returning the widget state
#' reactiveVal. Shared by `new_model_block` and `new_survival_block`.
#'
#' @param input,output,session The block module's reactive context.
#' @param data Reactive data frame.
#' @param state Initial widget state.
#' @param response_mode `"single"` or `"surv"`.
#' @return A `reactiveVal` holding the widget state.
#' @keywords internal
#' @noRd
formula_input_server <- function(input, output, session, data, state,
                                 response_mode = "single") {
  ns <- session$ns
  r_state <- shiny::reactiveVal(state)
  self_write <- new.env(parent = emptyenv())
  self_write$active <- FALSE

  shiny::observeEvent(data(), {
    session$sendCustomMessage(
      "formula-columns",
      list(
        id = ns("formula_input"),
        columns = build_formula_columns(data()),
        responseMode = response_mode
      )
    )
  })

  shiny::observeEvent(input$formula_input_parse_request, {
    parsed <- parse_formula(
      input$formula_input_parse_request,
      formula_col_types(data())
    )
    parsed$offset <- r_state()$offset
    parsed$weights <- r_state()$weights
    self_write$active <- TRUE
    r_state(parsed)
    session$sendCustomMessage(
      "formula-update",
      list(id = ns("formula_input"), state = normalize_formula_for_js(parsed))
    )
  })

  shiny::observeEvent(input$formula_input, {
    self_write$active <- TRUE
    r_state(input$formula_input)
  })

  shiny::observeEvent(r_state(), {
    if (self_write$active) {
      self_write$active <- FALSE
    } else {
      session$sendCustomMessage(
        "formula-update",
        list(
          id = ns("formula_input"),
          state = normalize_formula_for_js(r_state())
        )
      )
    }
  })

  # ...except when the block sits on a dock panel that is not on the startup
  # view. The block's server still runs at boot -- blockr.core constructs it
  # whenever a visible block downstream needs it -- but the container div ships
  # with the panel, on first visit. Both pushes above therefore fire while the
  # element does not exist, and Shiny drops a custom message whose target it
  # cannot find. The widget's own `_pendingColumns` / `_pendingState` parking
  # cannot save them: it keys on `document.getElementById(msg.id)`, which is
  # null when the panel has never been rendered.
  #
  # So the client announces itself on bind when nothing was parked for it, and
  # we re-send. Columns first, then state -- the order `initialize()` uses,
  # because the response select has to be able to hold a value before the state
  # sets one. Sending state to a widget whose options are still empty is what
  # leaves the card showing the first column instead of the fitted response.
  #
  # Same fix as `blockr.dplyr::js_block_ready_name()`, done by hand because
  # this widget predates that helper and carries its own input binding.
  # See blockr.core#317 for the core-level fix this stands in for.
  shiny::observeEvent(input$formula_input_ready, {
    dat <- tryCatch(data(), error = function(e) NULL)

    if (!is.null(dat)) {
      session$sendCustomMessage(
        "formula-columns",
        list(
          id = ns("formula_input"),
          columns = build_formula_columns(dat),
          responseMode = response_mode
        )
      )
    }

    session$sendCustomMessage(
      "formula-update",
      list(
        id = ns("formula_input"),
        state = normalize_formula_for_js(r_state())
      )
    )
  })

  r_state
}

#' Lightweight typed-column summary for the formula-input JS
#' @keywords internal
#' @noRd
build_formula_columns <- function(df) {
  lapply(colnames(df), function(col) {
    v <- df[[col]]
    type <- if (is.numeric(v)) {
      "numeric"
    } else if (is.factor(v) || is.ordered(v)) {
      "factor"
    } else if (is.logical(v)) {
      "logical"
    } else {
      "character"
    }
    lbl <- attr(v, "label", exact = TRUE)
    list(name = col, type = type, label = if (is.null(lbl)) col else lbl)
  })
}

#' Map a data frame's columns to a name -> class list (for `classify_term()`)
#' @keywords internal
#' @noRd
formula_col_types <- function(df) {
  as.list(vapply(df, function(x) class(x)[1L], character(1)))
}

#' Normalise a formula state before `sendCustomMessage` (auto-unbox guard)
#'
#' `sendCustomMessage` serialises with `auto_unbox = TRUE`, collapsing length-1
#' vectors to JSON scalars. Wrap the array-valued fields in `as.list()` so they
#' always emit JSON arrays.
#' @keywords internal
#' @noRd
normalize_formula_for_js <- function(state) {
  if (is.null(state)) {
    return(state)
  }
  state$terms <- lapply(state$terms %||% list(), function(t) {
    if (!is.null(t$vars)) t$vars <- as.list(t$vars)
    t
  })
  if (is.list(state$response) && !is.null(state$response$args)) {
    state$response$args <- as.list(state$response$args)
  }
  state
}

# small null-coalesce used locally (not relying on base/imported %||%)
`%||%` <- function(x, y) if (is.null(x)) y else x
