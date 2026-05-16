#' Homogeneity-of-Variance Check Block
#'
#' Runs Bartlett's test for homogeneity of variance of a numeric variable
#' across groups. (Bartlett is sensitive to non-normality; this is the base-R
#' choice for the prototype, mirroring jamovi's default.)
#'
#' @param dv Numeric column name.
#' @param group Categorical column name with two or more levels.
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @return A `homogeneity_check_block` transform block.
#'
#' @export
new_homogeneity_check_block <- function(
  dv = character(),
  group = character(),
  ...
) {
  new_transform_block(
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        r_dv <- reactiveVal(dv)
        r_group <- reactiveVal(group)
        r_initialized <- reactiveVal(FALSE)

        observeEvent(input$dv, r_dv(input$dv))
        observeEvent(input$group, r_group(input$group))

        observe({
          if (!r_initialized() && length(colnames(data())) > 0) {
            d <- data()
            num <- colnames(d)[vapply(d, is.numeric, logical(1))]
            cat <- colnames(d)[vapply(
              d, function(x) is.factor(x) || is.character(x), logical(1)
            )]
            updateSelectizeInput(session, "dv",
              choices = num, selected = r_dv())
            updateSelectizeInput(session, "group",
              choices = cat, selected = r_group())
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
            new_dv <- intersect(r_dv(), num)
            new_group <- intersect(r_group(), cat)
            r_dv(new_dv); r_group(new_group)
            updateSelectizeInput(session, "dv",
              choices = num, selected = new_dv)
            updateSelectizeInput(session, "group",
              choices = cat, selected = new_group)
          }
        }, ignoreNULL = FALSE)

        list(
          expr = reactive({
            d <- r_dv()
            g <- r_group()
            if (length(d) == 0 || !nzchar(d)) return(quote(NULL))
            if (length(g) == 0 || !nzchar(g)) return(quote(NULL))
            expr_text <- glue::glue(
              "blockr.stats:::homogeneity_check('{d}', '{g}', data = data)"
            )
            parse(text = expr_text)[[1]]
          }),
          state = list(dv = r_dv, group = r_group)
        )
      })
    },
    ui = function(id) {
      tagList(
        div(
          class = "block-container",
          selectizeInput(
            NS(id, "dv"),
            label = "Dependent variable (numeric)",
            choices = dv,
            selected = dv,
            multiple = FALSE,
            width = "100%"
          ),
          selectizeInput(
            NS(id, "group"),
            label = "Grouping variable",
            choices = group,
            selected = group,
            multiple = FALSE,
            width = "100%"
          )
        )
      )
    },
    class = "homogeneity_check_block",
    allow_empty_state = c("dv", "group"),
    ...
  )
}

#' @noRd
homogeneity_check <- function(dv, group, data) {
  formula_str <- paste0("`", dv, "` ~ `", group, "`")
  r <- stats::bartlett.test(stats::as.formula(formula_str), data = data)
  p <- r$p.value
  tibble::tibble(
    test = "Bartlett",
    statistic = unname(r$statistic),
    df = unname(r$parameter),
    p = p,
    decision = if (p < 0.01) "Reject"
      else if (p < 0.05) "Concern" else "OK"
  )
}
