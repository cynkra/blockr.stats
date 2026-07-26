#' Statistical Test Block
#'
#' A single adaptive block for running statistical tests. Pick a
#' category and test; the parameter UI adapts to the chosen test.
#' Optional group-by stratification. The adaptive UI is a `renderUI`;
#' alternative / confidence level / null value live in the gear band, and
#' tests that have none of them show no gear.
#'
#' @param type Test type key from `test_config` (default "normality").
#' @param values Numeric (or factor, for categorical tests) column
#'   names to test.
#' @param groups Comparison group column name.
#' @param by Column names for group-by stratification.
#' @param method,alternative,variant,conf_level,null Test parameters.
#' @param ... Forwarded to the internal htest block constructor.
#' @return A block of class `stat_test_block`.
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(new_stat_test_block(), data = list(data = mtcars))
#' }
#' @export
new_stat_test_block <- function(
  type        = "normality",
  values      = character(),
  groups      = character(),
  by          = character(),
  method      = character(),
  alternative = "two.sided",
  variant     = "welch",
  conf_level  = 0.95,
  null        = 0,
  ...
) {
  initial_category <- type_to_category(type)

  cat_choices <- stats::setNames(
    names(test_categories),
    vapply(test_categories, function(c) c$label, character(1))
  )

  ui <- function(id) {
    ns <- NS(id)
    tagList(
      # Blockr.icons + gear-header styles from blockr.dplyr; the in-flow
      # settings band is vendored here (see settings_band_dep()).
      blockr.dplyr::blockr_core_js_dep(),
      blockr.dplyr::blockr_blocks_css_dep(),
      settings_band_dep(),
      div(
        class = "block-container",
        # Alternative / confidence level / null value live in the gear band,
        # like every other blockr block. Tests that have none of them (most
        # normality and categorical tests) get no gear at all.
        conditionalPanel(
          "output.has_advanced", ns = ns,
          div(
            class = "blockr-gear-header",
            tags$button(id = ns("gear"), type = "button",
                        class = "blockr-gear-btn", title = "Advanced options")
          ),
          div(
            id = ns("band"),
            class = "blockr-settings blockr-settings--beak",
            div(class = "blockr-settings__title", "Advanced options"),
            div(
              class = "blockr-settings__grid",
              div(class = "blockr-settings__field--full",
                  uiOutput(ns("advanced_ui")))
            )
          )
        ),
        div(
          class = "block-form-grid",
          div(style = "grid-column: 1 / -1;",
            selectInput(ns("category"), "Category",
              choices = cat_choices, selected = initial_category,
              width = "100%")),
          div(style = "grid-column: 1 / -1;",
            uiOutput(ns("test_ui"))),
          div(style = "grid-column: 1 / -1;",
            selectizeInput(ns("values"), "Values",
              choices = values, selected = values, multiple = TRUE,
              width = "100%",
              options = list(
                placeholder = "Select column(s)...",
                plugins = list("remove_button")))),
          div(style = "grid-column: 1 / -1;",
            selectizeInput(ns("groups"), "Comparison groups",
              choices = groups, selected = groups, multiple = FALSE,
              width = "100%",
              options = list(
                placeholder = "Select a grouping variable..."))),
          div(style = "grid-column: 1 / -1;",
            uiOutput(ns("params_ui"))),
          div(style = "grid-column: 1 / -1;",
            mod_column_selector_ui(
              ns("by_selector"),
              label = tags$span("Group by (optional)",
                style = "font-size:0.875rem;color:#666;font-weight:normal;"),
              initial_choices = by, initial_selected = by))
        )
      ),
      gear_band_script(ns)
    )
  }

  server <- function(id, data) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      r_type        <- as_rv(type)
      r_values      <- as_rv(values)
      r_groups      <- as_rv(groups)
      r_method      <- as_rv(method)
      r_alternative <- as_rv(alternative)
      r_variant     <- as_rv(variant)
      r_conf_level  <- as_rv(conf_level)
      r_null        <- as_rv(null)
      r_category    <- reactiveVal(initial_category)

      r_by_selection <- mod_column_selector_server(
        id = "by_selector",
        get_cols = function() {
          req(data())
          cn <- colnames(data())
          cn[vapply(data(), function(x) {
            is.factor(x) || is.character(x)
          }, logical(1))]
        },
        initial_value = by
      )

      # Category -> set/keep test type
      observeEvent(input$category, {
        r_category(input$category)
        tests <- category_tests(input$category)
        cur <- r_type()
        r_type(if (cur %in% tests) cur else tests[1])
      }, ignoreNULL = TRUE)

      observeEvent(input$test, r_type(input$test),
                   ignoreNULL = TRUE, ignoreInit = TRUE)

      observeEvent(input$values, r_values(input$values),
                   ignoreNULL = FALSE)
      observeEvent(input$groups, r_groups(input$groups),
                   ignoreNULL = FALSE)
      observeEvent(input$method, r_method(input$method))
      observeEvent(input$alternative, r_alternative(input$alternative))
      observeEvent(input$variant, r_variant(input$variant))
      observeEvent(input$conf_level, r_conf_level(input$conf_level))
      observeEvent(input$null, r_null(input$null))

      # Test picker (only when the category has >1 test)
      output$test_ui <- renderUI({
        tests <- category_tests(r_category())
        if (length(tests) < 2) return(NULL)
        choices <- stats::setNames(
          tests, vapply(tests, function(k) test_config[[k]]$label,
                        character(1)))
        sel <- if (r_type() %in% tests) r_type() else tests[1]
        selectInput(ns("test"), "Test", choices = choices,
                    selected = sel, width = "100%")
      })

      # Which of the current test's parameters belong in the gear band.
      # Everything else (method, variant) is a primary choice and stays on
      # the block face.
      adv_params <- reactive({
        cfg <- test_config[[r_type()]]
        req(cfg)
        intersect(c("alternative", "conf_level", "null"), names(cfg$params))
      })

      # Drives the conditionalPanel around the gear: no advanced parameters,
      # no gear. Must keep evaluating while hidden, or the panel can never
      # come back.
      output$has_advanced <- reactive(length(adv_params()) > 0)
      outputOptions(output, "has_advanced", suspendWhenHidden = FALSE)

      # Adaptive parameter UI for the current test (block face)
      output$params_ui <- renderUI({
        ct <- r_type()
        req(ct)
        cfg <- test_config[[ct]]
        req(cfg)
        p <- cfg$params
        # The block can be constructed with method / variant unset, and a
        # test switch can leave a value the new test does not offer. Both
        # fall back to the test's default; a bare `%in%` on character(0)
        # would error out of the renderUI ("argument is of length zero").
        sel_or_default <- function(cur, choices, default) {
          if (length(cur) == 1L && cur %in% choices) cur else default
        }
        bits <- list()
        if ("method" %in% names(p)) {
          bits <- c(bits, list(selectInput(ns("method"),
            p$method$label %||% "Method", choices = p$method$choices,
            selected = sel_or_default(r_method(), p$method$choices,
                                      p$method$default), width = "100%")))
        }
        if ("variant" %in% names(p)) {
          bits <- c(bits, list(selectInput(ns("variant"),
            p$variant$label %||% "Variance assumption",
            choices = p$variant$choices,
            selected = sel_or_default(r_variant(), p$variant$choices,
                                      p$variant$default), width = "100%")))
        }
        if (!length(bits)) return(NULL)
        do.call(tagList, bits)
      })

      # Advanced parameter UI (gear band)
      output$advanced_ui <- renderUI({
        keys <- adv_params()
        cfg <- test_config[[r_type()]]
        p <- cfg$params
        adv <- list()
        if ("alternative" %in% keys) {
          adv <- c(adv, list(selectInput(ns("alternative"),
            "Alternative",
            choices = c("Two sided" = "two.sided",
                        "Greater" = "greater", "Less" = "less"),
            selected = r_alternative(), width = "100%")))
        }
        if ("conf_level" %in% keys) {
          adv <- c(adv, list(numericInput(ns("conf_level"),
            "Confidence level", value = r_conf_level(),
            min = 0, max = 1, step = 0.01, width = "100%")))
        }
        if ("null" %in% keys) {
          adv <- c(adv, list(numericInput(ns("null"),
            p$null$label %||% "Null value", value = r_null(),
            step = 0.1, width = "100%")))
        }
        if (!length(adv)) return(NULL)
        do.call(tagList, adv)
      })

      observeEvent(colnames(data()), {
        req(data())
        d <- data()
        num <- colnames(d)[vapply(d, is.numeric, logical(1))]
        fac <- colnames(d)[vapply(d, function(x) {
          is.factor(x) || is.character(x)
        }, logical(1))]
        # categorical tests use factor columns as "values"
        cfg <- test_config[[r_type()]]
        val_type <- tryCatch(cfg$inputs$values$type, error = function(e) "numeric")
        vchoices <- if (identical(val_type, "factor")) fac else num
        updateSelectizeInput(session, "values",
          choices = vchoices, selected = r_values(),
          options = list(plugins = list("remove_button")))
        updateSelectizeInput(session, "groups",
          choices = fac, selected = r_groups())
      }, ignoreNULL = FALSE)

      list(
        expr = reactive({
          ct <- r_type()
          req(ct)
          cfg <- test_config[[ct]]
          req(cfg)
          cv <- r_values()
          if (is.null(cv) || !any(nzchar(cv))) return(bquote(NULL))
          if (cfg$inputs$groups$role %||% "hidden" == "required") {
            cg <- r_groups()
            if (is.null(cg) || !any(nzchar(cg))) return(bquote(NULL))
          } else {
            cg <- character()
          }
          ms <- cfg$inputs$values$min_select %||% 1
          if (length(cv) < ms) return(bquote(NULL))
          cp <- cfg$params
          pd <- function(rv, nm) {
            v <- rv()
            if (length(v) == 0 || (is.character(v) && !any(nzchar(v))))
              cp[[nm]]$default else v
          }
          current_params <- list(
            method      = pd(r_method, "method"),
            alternative = pd(r_alternative, "alternative"),
            variant     = pd(r_variant, "variant"),
            conf_level  = r_conf_level(),
            null        = r_null()
          )
          by_cols <- r_by_selection()
          fn <- get(cfg$test_fn, envir = asNamespace("blockr.stats"),
                    mode = "function")
          # NOTE: unlike the other blocks, this expr inlines the stratified_eval
          # and test-fn objects rather than emitting standard R. Deliberate /
          # accepted for now (adaptive dispatch over ~15 tests is awkward to
          # render as plain code); revisit if/when these tests see real use.
          strat_fn <- stratified_eval
          bquote({
            .(strat_fn)(data, by_cols = .(by_cols), fn = .(fn),
              values = .(cv), groups = .(cg),
              params = .(current_params))
          })
        }),
        state = list(
          type = r_type, values = r_values, groups = r_groups,
          by = r_by_selection, method = r_method,
          alternative = r_alternative, variant = r_variant,
          conf_level = r_conf_level, null = r_null
        )
      )
    })
  }

  new_htest_block(
    server, ui, "stat_test_block",
    dat_valid = function(data) stopifnot(is.data.frame(data)),
    expr_type = "bquoted",
    allow_empty_state = c("values", "groups", "by", "method"),
    ...
  )
}
