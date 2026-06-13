#' Frequency counts / proportions (one- or two-way)
#'
#' One-way: per variable -> level, n, proportion. Two-way (when `by`
#' is set): per variable -> level x by_level contingency in long form
#' (level, by_level, n, proportion). Tidy data frame for the generic
#' renderers; the two-way form also feeds the chi-square test of
#' independence.
#'
#' The raw numeric columns (`n`, `proportion`) stay untouched and
#' `dplyr`-able; a `pct` column (`proportion * 100`) and a hidden `.fmt`
#' template column (`"{n} ({pct:1}%)"`) ride alongside so a renderer can
#' assemble the composite `"38 (54.0%)"` cell via the blockr.bi `.fmt`
#' engine. We scale to a percentage (rather than format the 0-1
#' proportion directly) because `n (pct%)` reads better than `n (prop)`,
#' and pin one decimal (`:1`) for the percentage.
#'
#' @param data A data frame.
#' @param vars Categorical column name(s).
#' @param by Optional second categorical column (crosstab).
#' @return A long tidy data frame.
#' @examples
#' tabulate_freq(warpbreaks, vars = "wool")
#' tabulate_freq(warpbreaks, vars = "wool", by = "tension")
#' @export
tabulate_freq <- function(data, vars, by = NULL) {
  vars <- intersect(vars[nzchar(vars)], names(data))
  if (length(vars) == 0L) {
    return(data.frame(message = "Select categorical variable(s)",
                      stringsAsFactors = FALSE))
  }
  two_way <- !is.null(by) && nzchar(by) && by %in% names(data)
  out <- lapply(vars, function(col) {
    if (two_way) {
      tab <- table(data[[col]], data[[by]], useNA = "ifany")
      df <- as.data.frame(tab, stringsAsFactors = FALSE)
      names(df) <- c("level", "by_level", "n")
      df$variable <- col
      df$by <- by
      df$proportion <- df$n / sum(df$n)
      df[, c("variable", "level", "by", "by_level", "n", "proportion")]
    } else {
      tab <- table(data[[col]], useNA = "ifany")
      data.frame(
        variable   = col,
        level      = names(tab),
        n          = as.integer(tab),
        proportion = as.numeric(tab) / sum(tab),
        stringsAsFactors = FALSE
      )
    }
  })
  res <- do.call(rbind, out)
  # Additive `.fmt` convention: hidden template + a `pct` companion the
  # template references. Numeric `n` / `proportion` are left as-is.
  res$pct <- res$proportion * 100
  res$.fmt <- "{n} ({pct:1}%)"
  res
}

# NOTE (under review): usefulness debated. Convenience block; one-way counts
# overlap dplyr `count()`. Only the two-way crosstab (feeding chi-square) is
# awkward to do otherwise. Kept for now; candidate for removal.
#' Frequencies Block
#'
#' Transform block wrapping [tabulate_freq()]. One-way counts, or a
#' two-way crosstab when a `by` variable is chosen. Emits a tidy data
#' frame for the generic renderers.
#'
#' @param vars Categorical column names.
#' @param by Optional second categorical column (crosstab).
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#' @return A `frequencies_block` transform block.
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(new_frequencies_block(), data = list(data = warpbreaks))
#' }
#' @export
new_frequencies_block <- function(vars = character(), by = "", ...) {
  new_transform_block(
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        r_vars <- reactiveVal(vars)
        r_by <- reactiveVal(by)
        r_initialized <- reactiveVal(FALSE)

        cat_of <- function(d) {
          colnames(d)[vapply(d, function(x) is.factor(x) ||
            is.character(x), logical(1))]
        }

        observeEvent(input$vars, r_vars(input$vars))
        observeEvent(input$by,
          r_by(if (is.null(input$by)) "" else input$by))

        observe({
          if (!r_initialized() && length(colnames(data())) > 0) {
            cc <- cat_of(data())
            updateSelectizeInput(session, "vars",
              choices = cc, selected = r_vars())
            updateSelectizeInput(session, "by",
              choices = c("(none)" = "", stats::setNames(cc, cc)),
              selected = r_by())
            r_initialized(TRUE)
          }
        })

        observeEvent(colnames(data()), {
          if (r_initialized()) {
            req(data())
            cc <- cat_of(data())
            nv <- intersect(r_vars(), cc)
            r_vars(nv)
            updateSelectizeInput(session, "vars",
              choices = cc, selected = nv)
            updateSelectizeInput(session, "by",
              choices = c("(none)" = "", stats::setNames(cc, cc)),
              selected = if (r_by() %in% cc) r_by() else "")
          }
        }, ignoreNULL = FALSE)

        list(
          expr = reactive({
            v <- r_vars()
            v <- v[nzchar(v)]
            b <- r_by()
            b <- if (is.null(b) || !nzchar(b)) NULL else b
            bquote(
              blockr.stats::tabulate_freq(data, vars = .(v), by = .(b)),
              list(v = v, b = b)
            )
          }),
          state = list(vars = r_vars, by = r_by)
        )
      })
    },
    ui = function(id) {
      tagList(
        div(
          class = "block-container",
          selectizeInput(
            NS(id, "vars"),
            label = "Categorical variables",
            choices = vars, selected = vars,
            multiple = TRUE, width = "100%",
            options = list(
              plugins = list("drag_drop", "remove_button"),
              placeholder = "Pick categorical variables..."
            )
          ),
          selectizeInput(
            NS(id, "by"),
            label = "By (crosstab, optional)",
            choices = c("(none)" = "", stats::setNames(vars, vars)),
            selected = by, multiple = FALSE, width = "100%"
          )
        )
      )
    },
    class = "frequencies_block",
    allow_empty_state = c("vars", "by"),
    ...
  )
}
