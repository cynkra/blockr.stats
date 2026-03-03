#' Statistical Test Block
#'
#' A single adaptive block for running statistical tests. Users pick a test
#' type and the UI adapts, showing relevant parameters. Supports optional
#' group-by stratification.
#'
#' @param type Test type key from test_config (default: "normality").
#' @param values Numeric column names to test.
#' @param groups Comparison group column name (for comparison tests).
#' @param by Column names for group-by stratification.
#' @param method Test-specific method (e.g., "shapiro.wilk", "bartlett", "pearson").
#' @param alternative Alternative hypothesis.
#' @param variant Variance assumption for t-test ("welch" or "pooled").
#' @param conf_level Confidence level.
#' @param null Null hypothesis value.
#' @param ... forwarded to [new_block()].
#'
#' @return A htest block object of class \code{stat_test_block}.
#'
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
  # all param input IDs that might exist in the UI
  all_param_ids <- c("method", "alternative", "variant", "conf_level", "null")

  ui <- function(id) {
    ns <- NS(id)
    tagList(
      shinyjs::useShinyjs(),

      # CSS for collapsible advanced section (matches blockr.ggplot pattern)
      tags$style(HTML(sprintf(
        "
        #%s-advanced-options {
          max-height: 0;
          overflow: hidden;
          transition: max-height 0.3s ease-out;
          grid-column: 1 / -1;
          display: grid;
          grid-template-columns: subgrid;
          gap: 15px;
        }
        #%s-advanced-options.expanded {
          max-height: 2000px;
          overflow: visible;
          transition: max-height 0.5s ease-in;
        }
        .block-advanced-toggle {
          cursor: pointer;
          user-select: none;
          padding: 8px 0;
          display: flex;
          align-items: center;
          gap: 6px;
          grid-column: 1 / -1;
          color: #6c757d;
          font-size: 0.875rem;
        }
        .block-advanced-toggle .block-chevron {
          transition: transform 0.2s;
          display: inline-block;
          font-size: 14px;
          font-weight: bold;
        }
        .block-advanced-toggle .block-chevron.rotated {
          transform: rotate(90deg);
        }
        ",
        id,
        id
      ))),

      # --- Test type selector ---
      div(
        class = "block-container",
        div(
          class = "block-form-grid",
          # One-sample row
          div(
            style = "grid-column: 1 / -1;",
            tags$label(
              style = "font-size: 0.75em; color: #6c757d; margin-bottom: 2px; display: block;",
              test_groups$one_sample
            ),
            shinyWidgets::radioGroupButtons(
              inputId = ns("type_one_sample"),
              choices = stats::setNames(
                names(Filter(function(x) x$group == "one_sample", test_config)),
                vapply(Filter(function(x) x$group == "one_sample", test_config),
                       function(x) x$label, character(1))
              ),
              selected = if (test_config[[type]]$group == "one_sample") type else character(0),
              size = "sm"
            )
          ),
          # Comparison row
          div(
            style = "grid-column: 1 / -1;",
            tags$label(
              style = "font-size: 0.75em; color: #6c757d; margin-bottom: 2px; display: block;",
              test_groups$comparison
            ),
            shinyWidgets::radioGroupButtons(
              inputId = ns("type_comparison"),
              choices = stats::setNames(
                names(Filter(function(x) x$group == "comparison", test_config)),
                vapply(Filter(function(x) x$group == "comparison", test_config),
                       function(x) x$label, character(1))
              ),
              selected = if (test_config[[type]]$group == "comparison") type else character(0),
              size = "sm"
            )
          ),
          # Association row
          div(
            style = "grid-column: 1 / -1;",
            tags$label(
              style = "font-size: 0.75em; color: #6c757d; margin-bottom: 2px; display: block;",
              test_groups$association
            ),
            shinyWidgets::radioGroupButtons(
              inputId = ns("type_association"),
              choices = stats::setNames(
                names(Filter(function(x) x$group == "association", test_config)),
                vapply(Filter(function(x) x$group == "association", test_config),
                       function(x) x$label, character(1))
              ),
              selected = if (test_config[[type]]$group == "association") type else character(0),
              size = "sm"
            )
          ),
          # --- Column mappings ---
          div(
            style = "grid-column: 1 / -1;",
            selectizeInput(
              ns("values"),
              label = "Values",
              choices = values,
              selected = values,
              multiple = TRUE,
              width = "100%",
              options = list(
                placeholder = "Select numeric column(s)...",
                plugins = list("remove_button")
              )
            )
          ),
          div(
            id = ns("groups_container"),
            style = "grid-column: 1 / -1;",
            selectizeInput(
              ns("groups"),
              label = "Comparison groups",
              choices = groups,
              selected = groups,
              multiple = FALSE,
              width = "100%",
              options = list(placeholder = "Select a grouping variable...")
            )
          ),
          # --- Test parameters (before group-by) ---
          div(
            id = ns("method_container"),
            style = "grid-column: 1 / -1;",
            selectInput(
              ns("method"),
              label = "Method",
              choices = if (length(method) > 0) method else character(),
              selected = method,
              width = "100%"
            )
          ),
          div(
            id = ns("variant_container"),
            style = "grid-column: 1 / -1;",
            selectInput(
              ns("variant"),
              label = "Variance assumption",
              choices = c("Welch" = "welch", "Pooled" = "pooled"),
              selected = variant,
              width = "100%"
            )
          ),
          # --- Advanced options toggle ---
          div(
            id = ns("advanced_container"),
            style = "grid-column: 1 / -1;",
            div(
              class = "block-advanced-toggle text-muted",
              id = ns("advanced-toggle"),
              onclick = sprintf(
                "
                const section = document.getElementById('%s');
                const chevron = document.querySelector('#%s .block-chevron');
                section.classList.toggle('expanded');
                chevron.classList.toggle('rotated');
                ",
                ns("advanced-options"),
                ns("advanced-toggle")
              ),
              tags$span(class = "block-chevron", "\u203A"),
              "Show advanced options"
            )
          ),
          # --- Advanced options (collapsible) ---
          div(
            id = ns("advanced-options"),
            div(
              id = ns("alternative_container"),
              style = "grid-column: 1 / -1;",
              selectInput(
                ns("alternative"),
                label = "Alternative",
                choices = c("Two sided" = "two.sided", "Greater" = "greater",
                            "Less" = "less"),
                selected = alternative,
                width = "100%"
              )
            ),
            div(
              id = ns("conf_level_container"),
              style = "grid-column: 1 / -1;",
              numericInput(
                ns("conf_level"),
                label = "Confidence level",
                value = conf_level,
                min = 0, max = 1, step = 0.01
              )
            ),
            div(
              id = ns("null_container"),
              style = "grid-column: 1 / -1;",
              numericInput(
                ns("null"),
                label = "Null value",
                value = null,
                step = 0.1
              )
            )
          ),
          # --- Group by (always last, same as blockr.dplyr) ---
          div(
            style = "grid-column: 1 / -1;",
            mod_column_selector_ui(
              ns("by_selector"),
              label = tags$span(
                "Group by (optional)",
                style = "font-size: 0.875rem; color: #666; font-weight: normal;"
              ),
              initial_choices = by,
              initial_selected = by
            )
          )
        )
      )
    )
  }

  server <- function(id, data) {
    moduleServer(id, function(input, output, session) {
      # Initialize reactive values
      r_type        <- as_rv(type)
      r_values      <- as_rv(values)
      r_groups      <- as_rv(groups)
      r_method      <- as_rv(method)
      r_alternative <- as_rv(alternative)
      r_variant     <- as_rv(variant)
      r_conf_level  <- as_rv(conf_level)
      r_null        <- as_rv(null)

      # Group-by selector
      r_by_selection <- mod_column_selector_server(
        id = "by_selector",
        get_cols = function() {
          req(data())
          all_cols <- colnames(data())
          fct_cols <- all_cols[vapply(data(), is.factor, logical(1))]
          chr_cols <- all_cols[vapply(data(), is.character, logical(1))]
          union(fct_cols, chr_cols)
        },
        initial_value = by
      )

      # --- Type switching: mutual exclusion across groups ---
      observeEvent(input$type_one_sample, {
        r_type(input$type_one_sample)
        shinyWidgets::updateRadioGroupButtons(session, "type_comparison",
                                              selected = character(0))
        shinyWidgets::updateRadioGroupButtons(session, "type_association",
                                              selected = character(0))
      }, ignoreNULL = TRUE, ignoreInit = TRUE)

      observeEvent(input$type_comparison, {
        r_type(input$type_comparison)
        shinyWidgets::updateRadioGroupButtons(session, "type_one_sample",
                                              selected = character(0))
        shinyWidgets::updateRadioGroupButtons(session, "type_association",
                                              selected = character(0))
      }, ignoreNULL = TRUE, ignoreInit = TRUE)

      observeEvent(input$type_association, {
        r_type(input$type_association)
        shinyWidgets::updateRadioGroupButtons(session, "type_one_sample",
                                              selected = character(0))
        shinyWidgets::updateRadioGroupButtons(session, "type_comparison",
                                              selected = character(0))
      }, ignoreNULL = TRUE, ignoreInit = TRUE)

      # --- Input binding ---
      observeEvent(input$values, r_values(input$values), ignoreNULL = FALSE)
      observeEvent(input$groups, r_groups(input$groups), ignoreNULL = FALSE)
      observeEvent(input$method, r_method(input$method))
      observeEvent(input$alternative, r_alternative(input$alternative))
      observeEvent(input$variant, r_variant(input$variant))
      observeEvent(input$conf_level, r_conf_level(input$conf_level))
      observeEvent(input$null, r_null(input$null))

      # --- Update column choices when data changes ---
      observeEvent(colnames(data()), {
        req(data())
        # Numeric columns for values
        num_cols <- colnames(data())[vapply(data(), is.numeric, logical(1))]
        placeholder <- if (length(num_cols) == 0) "No numeric columns" else
          "Select numeric column(s)..."
        updateSelectizeInput(session, "values",
          choices = num_cols, selected = r_values(),
          options = list(placeholder = placeholder,
                         plugins = list("remove_button")))

        # Factor/character columns for groups
        fct_cols <- colnames(data())[vapply(data(), is.factor, logical(1))]
        chr_cols <- colnames(data())[vapply(data(), is.character, logical(1))]
        grp_cols <- union(fct_cols, chr_cols)
        placeholder_g <- if (length(grp_cols) == 0) "No character/factor columns" else
          "Select a grouping variable..."
        updateSelectizeInput(session, "groups",
          choices = grp_cols, selected = r_groups(),
          options = list(placeholder = placeholder_g))
      }, ignoreNULL = FALSE)

      # --- Dynamic UI: show/hide based on test type ---
      observe({
        current_type <- r_type()
        req(current_type)
        cfg <- test_config[[current_type]]
        if (is.null(cfg)) return()

        # Show/hide comparison groups
        if (cfg$inputs$groups$role == "hidden") {
          shinyjs::hide("groups_container")
        } else {
          shinyjs::show("groups_container")
        }

        # Show/hide each parameter
        cfg_params <- cfg$params
        has_method <- "method" %in% names(cfg_params)
        has_variant <- "variant" %in% names(cfg_params)
        has_alternative <- "alternative" %in% names(cfg_params)
        has_conf_level <- "conf_level" %in% names(cfg_params)
        has_null <- "null" %in% names(cfg_params)

        # Method
        if (has_method) {
          shinyjs::show("method_container")
          p <- cfg_params$method
          updateSelectInput(session, "method",
            label = p$label %||% "Method",
            choices = p$choices,
            selected = if (r_method() %in% p$choices) r_method() else p$default)
        } else {
          shinyjs::hide("method_container")
        }

        # Variant (only t-test)
        if (has_variant) {
          shinyjs::show("variant_container")
          p <- cfg_params$variant
          updateSelectInput(session, "variant",
            label = p$label %||% "Variance assumption",
            choices = p$choices,
            selected = if (r_variant() %in% p$choices) r_variant() else p$default)
        } else {
          shinyjs::hide("variant_container")
        }

        # Alternative
        if (has_alternative) {
          shinyjs::show("alternative_container")
        } else {
          shinyjs::hide("alternative_container")
        }

        # Confidence level
        if (has_conf_level) {
          shinyjs::show("conf_level_container")
        } else {
          shinyjs::hide("conf_level_container")
        }

        # Null value
        if (has_null) {
          shinyjs::show("null_container")
          p <- cfg_params$null
          updateNumericInput(session, "null", label = p$label %||% "Null value")
        } else {
          shinyjs::hide("null_container")
        }

        # Show/hide advanced section entirely
        has_advanced <- has_alternative || has_conf_level || has_null
        if (has_advanced) {
          shinyjs::show("advanced_container")
        } else {
          shinyjs::hide("advanced_container")
        }
      })

      # --- Return expression and state ---
      list(
        expr = reactive({
          current_type <- r_type()
          req(current_type)
          cfg <- test_config[[current_type]]
          req(cfg)

          current_values <- r_values()
          if (is.null(current_values) || !any(nzchar(current_values))) {
            return(bquote(NULL))
          }

          # For comparison tests, require groups
          if (cfg$inputs$groups$role == "required") {
            current_groups <- r_groups()
            if (is.null(current_groups) || !any(nzchar(current_groups))) {
              return(bquote(NULL))
            }
          } else {
            current_groups <- character()
          }

          # For correlation, require at least 2 value columns
          min_select <- cfg$inputs$values$min_select %||% 1
          if (length(current_values) < min_select) {
            return(bquote(NULL))
          }

          # Build params list, falling back to config defaults when
          # reactive values are still empty (before UI round-trip completes)
          cfg_params <- cfg$params
          param_default <- function(rv, name) {
            val <- rv()
            if (length(val) == 0 || (is.character(val) && !any(nzchar(val)))) {
              cfg_params[[name]]$default
            } else {
              val
            }
          }
          current_params <- list(
            method      = param_default(r_method, "method"),
            alternative = param_default(r_alternative, "alternative"),
            variant     = param_default(r_variant, "variant"),
            conf_level  = r_conf_level(),
            null        = r_null()
          )

          by_cols <- r_by_selection()
          fn <- match.fun(cfg$test_fn)
          strat_fn <- stratified_eval

          bquote({
            .(strat_fn)(
              data,
              by_cols = .(by_cols),
              fn = .(fn),
              values = .(current_values),
              groups = .(current_groups),
              params = .(current_params)
            )
          })
        }),
        state = list(
          type        = r_type,
          values      = r_values,
          groups      = r_groups,
          by          = r_by_selection,
          method      = r_method,
          alternative = r_alternative,
          variant     = r_variant,
          conf_level  = r_conf_level,
          null        = r_null
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
