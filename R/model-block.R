#' Model Block
#'
#' Fit a statistical model: linear (`lm`), generalized linear (`glm`:
#' logistic / Poisson / Gamma), or ANOVA (`aov`). Returns the **fitted
#' model object**; downstream the broom adapter (tidy / glance /
#' augment) turns it into tidy frames for the generic renderers. The
#' block's own preview is a generic `summary(model)`.
#'
#' The fit is committed via an explicit **Fit** action so an expensive
#' model does not re-run on every input keystroke.
#'
#' @param model_type One of `"lm"`, `"logistic"`, `"poisson"`,
#'   `"gamma"`, `"aov"`.
#' @param response Response column (single).
#' @param predictors Numeric predictor column names.
#' @param factors Categorical predictor column names.
#' @param intercept Logical, include intercept (default TRUE).
#' @param ... Forwarded to [new_transform_block()].
#' @return A transform block of class `model_block`.
#' @export
new_model_block <- function(
  model_type = "lm",
  response = character(),
  predictors = character(),
  factors = character(),
  intercept = TRUE,
  ...
) {
  model_choices <- c(
    "Linear (lm)"    = "lm",
    "Logistic (glm)" = "logistic",
    "Poisson (glm)"  = "poisson",
    "Gamma (glm)"    = "gamma",
    "ANOVA (aov)"    = "aov"
  )

  build_expr <- function(mtype, resp, covs, facs, incl_intercept) {
    if (is.null(resp) || length(resp) == 0 || !nzchar(resp)) {
      return(quote(NULL))
    }
    all_preds <- c(covs, facs)
    all_preds <- all_preds[nzchar(all_preds)]
    resp_term <- paste0("`", resp, "`")
    if (length(all_preds) == 0) {
      if (!incl_intercept) return(quote(NULL))
      formula_str <- paste0(resp_term, " ~ 1")
    } else {
      pred_str <- paste0("`", all_preds, "`", collapse = " + ")
      formula_str <- paste0(resp_term,
        if (incl_intercept) " ~ " else " ~ 0 + ", pred_str)
    }
    expr_text <- switch(
      mtype,
      "lm"       = glue::glue("stats::lm({formula_str}, data = data)"),
      "logistic" = glue::glue(
        "stats::glm({formula_str}, data = data, family = stats::binomial())"),
      "poisson"  = glue::glue(
        "stats::glm({formula_str}, data = data, family = stats::poisson())"),
      "gamma"    = glue::glue(
        "stats::glm({formula_str}, data = data, family = stats::Gamma())"),
      "aov"      = glue::glue("stats::aov({formula_str}, data = data)"),
      glue::glue("stats::lm({formula_str}, data = data)")
    )
    parse(text = expr_text)[[1]]
  }

  new_transform_block(
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        r_model_type <- reactiveVal(model_type)
        r_response   <- reactiveVal(response)
        r_predictors <- reactiveVal(predictors)
        r_factors    <- reactiveVal(factors)
        r_intercept  <- reactiveVal(intercept)
        r_initialized <- reactiveVal(FALSE)
        # committed snapshot — only this drives the (expensive) fit
        r_commit <- reactiveVal(NULL)

        observeEvent(input$model_type, r_model_type(input$model_type))
        observeEvent(input$response,   r_response(input$response))
        observeEvent(input$predictors, r_predictors(input$predictors))
        observeEvent(input$factors,    r_factors(input$factors))
        observeEvent(input$intercept,  r_intercept(input$intercept))

        do_commit <- function() {
          r_commit(list(
            mtype = r_model_type(), resp = r_response(),
            covs = r_predictors(), facs = r_factors(),
            intercept = isTRUE(r_intercept())
          ))
        }
        observeEvent(input$fit, do_commit())

        numeric_cols <- function(d) {
          colnames(d)[vapply(d, is.numeric, logical(1))]
        }
        categorical_cols <- function(d) {
          colnames(d)[vapply(d, function(x) is.factor(x) ||
            is.character(x), logical(1))]
        }

        observe({
          if (!r_initialized() && length(colnames(data())) > 0) {
            d <- data()
            updateSelectizeInput(session, "response",
              choices = colnames(d), selected = r_response())
            updateSelectizeInput(session, "predictors",
              choices = numeric_cols(d), selected = r_predictors())
            updateSelectizeInput(session, "factors",
              choices = categorical_cols(d), selected = r_factors())
            r_initialized(TRUE)
            # auto-fit once on load / restore so the block works
            # without forcing a manual click
            do_commit()
          }
        })

        observeEvent(colnames(data()), {
          if (r_initialized()) {
            req(data())
            d <- data()
            num <- numeric_cols(d); cat <- categorical_cols(d)
            all <- colnames(d)
            r_response(intersect(r_response(), all))
            r_predictors(intersect(r_predictors(), num))
            r_factors(intersect(r_factors(), cat))
            updateSelectizeInput(session, "response",
              choices = all, selected = r_response())
            updateSelectizeInput(session, "predictors",
              choices = num, selected = r_predictors())
            updateSelectizeInput(session, "factors",
              choices = cat, selected = r_factors())
          }
        }, ignoreNULL = FALSE)

        list(
          expr = reactive({
            s <- r_commit()
            if (is.null(s)) return(quote(NULL))
            build_expr(s$mtype, s$resp, s$covs, s$facs, s$intercept)
          }),
          state = list(
            model_type = r_model_type,
            response   = r_response,
            predictors = r_predictors,
            factors    = r_factors,
            intercept  = r_intercept
          )
        )
      })
    },
    ui = function(id) {
      tagList(
        shinyjs::useShinyjs(),
        css_responsive_grid(),
        css_single_column("model"),
        div(
          class = "block-container model-block-container",
          div(class = "block-form-grid",
            div(class = "block-section",
              div(class = "block-section-grid",
                div(class = "block-help-text",
                  "Pick a model type and variables, then press Fit."),
                div(class = "block-input-wrapper",
                  style = "grid-column: 1 / -1;",
                  selectInput(NS(id, "model_type"), "Model type",
                    choices = model_choices, selected = model_type,
                    width = "100%")),
                div(class = "block-input-wrapper",
                  style = "grid-column: 1 / -1;",
                  selectizeInput(NS(id, "response"),
                    "Dependent variable (Y)", choices = response,
                    selected = response, multiple = FALSE,
                    width = "100%",
                    options = list(placeholder = "Pick the response..."))),
                div(class = "block-input-wrapper",
                  style = "grid-column: 1 / -1;",
                  selectizeInput(NS(id, "predictors"),
                    "Covariates (numeric predictors)",
                    choices = predictors, selected = predictors,
                    multiple = TRUE, width = "100%",
                    options = list(
                      plugins = list("drag_drop", "remove_button"),
                      persist = FALSE,
                      placeholder = "Pick numeric predictors..."))),
                div(class = "block-input-wrapper",
                  style = "grid-column: 1 / -1;",
                  selectizeInput(NS(id, "factors"),
                    "Factors (categorical predictors)",
                    choices = factors, selected = factors,
                    multiple = TRUE, width = "100%",
                    options = list(
                      plugins = list("drag_drop", "remove_button"),
                      persist = FALSE,
                      placeholder = "Pick factor predictors..."))),
                div(class = "block-input-wrapper",
                  checkboxInput(NS(id, "intercept"),
                    "Include intercept", value = intercept)),
                div(class = "block-input-wrapper",
                  style = "grid-column: 1 / -1;",
                  actionButton(NS(id, "fit"), "Fit",
                    class = "btn-primary", width = "100%"))
              )
            )
          )
        )
      )
    },
    class = "model_block",
    allow_empty_state = c("response", "predictors", "factors"),
    ...
  )
}

#' @export
block_output.model_block <- function(x, result, session) {
  renderPrint({
    if (is.null(result)) {
      cat("Pick variables and press Fit.")
    } else {
      summary(result)
    }
  })
}

#' @export
block_ui.model_block <- function(id, x, ...) {
  tagList(verbatimTextOutput(NS(id, "result")))
}
