#' Normality Check Block
#'
#' Runs Shapiro-Wilk normality tests on one or more numeric variables,
#' optionally split by a grouping variable. Emits a tibble with one row per
#' variable (or variable x group), columns: variable, group (optional),
#' n, W, p, decision.
#'
#' Sample sizes > 5000 fall back to Kolmogorov-Smirnov, which Shapiro-Wilk
#' won't accept; the block does this transparently.
#'
#' @param vars Numeric column names.
#' @param group Optional categorical grouping column.
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @return A `normality_check_block` transform block.
#'
#' @export
new_normality_check_block <- function(
  vars = character(),
  group = character(),
  ...
) {
  new_transform_block(
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        r_vars <- reactiveVal(vars)
        r_group <- reactiveVal(group)
        r_initialized <- reactiveVal(FALSE)

        observeEvent(input$vars, r_vars(input$vars))
        observeEvent(input$group, r_group(input$group))

        observe({
          if (!r_initialized() && length(colnames(data())) > 0) {
            d <- data()
            num <- colnames(d)[vapply(d, is.numeric, logical(1))]
            cat <- colnames(d)[vapply(
              d, function(x) is.factor(x) || is.character(x), logical(1)
            )]
            updateSelectizeInput(session, "vars",
              choices = num, selected = r_vars())
            updateSelectizeInput(session, "group",
              choices = c("(none)" = "", cat), selected = r_group())
            r_initialized(TRUE)
          }
        })

        observeEvent(colnames(data()), {
          if (r_initialized()) {
            req(data())
            d <- data()
            num <- colnames(d)[vapply(d, is.numeric, logical(1))]
            cat <- colnames(d)[vapply(
              d, function(x) is.factor(x) || is.character(x), logical(1)
            )]
            new_vars <- intersect(r_vars(), num)
            new_group <- intersect(r_group(), cat)
            r_vars(new_vars); r_group(new_group)
            updateSelectizeInput(session, "vars",
              choices = num, selected = new_vars)
            updateSelectizeInput(session, "group",
              choices = c("(none)" = "", cat), selected = new_group)
          }
        }, ignoreNULL = FALSE)

        list(
          expr = reactive({
            v <- r_vars()
            v <- v[nzchar(v)]
            g <- r_group()
            if (length(v) == 0) return(quote(NULL))
            cols_str <- paste0("c(", paste0("'", v, "'", collapse = ", "), ")")
            grp_arg <- if (length(g) > 0 && nzchar(g)) {
              paste0("'", g, "'")
            } else {
              "NULL"
            }
            expr_text <- glue::glue(
              "blockr.stats:::normality_check({cols_str}, group = {grp_arg}, data = data)"
            )
            parse(text = expr_text)[[1]]
          }),
          state = list(vars = r_vars, group = r_group)
        )
      })
    },
    ui = function(id) {
      tagList(
        div(
          class = "block-container",
          selectizeInput(
            NS(id, "vars"),
            label = "Variables",
            choices = vars,
            selected = vars,
            multiple = TRUE,
            width = "100%",
            options = list(
              plugins = list("drag_drop", "remove_button"),
              placeholder = "Pick numeric variables..."
            )
          ),
          selectizeInput(
            NS(id, "group"),
            label = "Group by (optional)",
            choices = group,
            selected = group,
            multiple = FALSE,
            width = "100%",
            options = list(placeholder = "(none)")
          )
        )
      )
    },
    class = "normality_check_block",
    allow_empty_state = c("vars", "group"),
    ...
  )
}

#' @noRd
normality_check <- function(vars, group, data) {
  run_one <- function(x, label) {
    x <- x[!is.na(x)]
    n <- length(x)
    if (n < 3) {
      return(tibble::tibble(
        variable = label, n = n, test = NA_character_,
        statistic = NA_real_, p = NA_real_, decision = "n < 3"
      ))
    }
    if (n > 5000) {
      r <- stats::ks.test(x, "pnorm", mean(x), sd(x))
      tibble::tibble(
        variable = label, n = n, test = "Kolmogorov-Smirnov",
        statistic = unname(r$statistic),
        p = r$p.value,
        decision = if (r$p.value < 0.01) "Reject"
          else if (r$p.value < 0.05) "Concern" else "OK"
      )
    } else {
      r <- stats::shapiro.test(x)
      tibble::tibble(
        variable = label, n = n, test = "Shapiro-Wilk",
        statistic = unname(r$statistic),
        p = r$p.value,
        decision = if (r$p.value < 0.01) "Reject"
          else if (r$p.value < 0.05) "Concern" else "OK"
      )
    }
  }

  if (is.null(group)) {
    do.call(rbind, lapply(vars, function(v) run_one(data[[v]], v)))
  } else {
    out <- list()
    for (v in vars) {
      groups <- split(data[[v]], data[[group]])
      for (gname in names(groups)) {
        row <- run_one(groups[[gname]], paste0(v, " [", gname, "]"))
        out[[length(out) + 1]] <- row
      }
    }
    do.call(rbind, out)
  }
}
