#' Generic Multi-Column Selector Module (Internal)
#'
#' Copied from blockr.dplyr. A reusable Shiny module for selecting multiple
#' columns from a dataset.
#'
#' @param id Character string. Module ID.
#' @param label Label for the selector.
#' @param initial_choices Character vector. Initial choices.
#' @param initial_selected Character vector. Initial selected values.
#' @param width Width of the input.
#'
#' @noRd
mod_column_selector_ui <- function(
  id,
  label,
  initial_choices = character(),
  initial_selected = character(),
  width = NULL
) {
  ns <- NS(id)
  selectInput(
    inputId = ns("columns"),
    label = label,
    choices = initial_choices,
    selected = initial_selected,
    multiple = TRUE,
    width = width
  )
}

#' Generic Multi-Column Selector Server Module (Internal)
#'
#' @param id Character string. Module ID.
#' @param get_cols Reactive function that returns available column names.
#' @param initial_value Character vector. Initial selected columns.
#'
#' @noRd
mod_column_selector_server <- function(id, get_cols, initial_value = character()) {
  moduleServer(id, function(input, output, session) {
    r_selection <- as_rv(initial_value)

    observeEvent(
      input$columns,
      {
        r_selection(input$columns %||% character())
      }
    )

    if (inherits(initial_value, "reactiveVal")) {
      observeEvent(initial_value(), {
        if (!identical(r_selection(), initial_value())) {
          r_selection(initial_value())
          updateSelectInput(session, "columns",
            choices = get_cols(), selected = initial_value())
        }
      }, ignoreInit = TRUE)
    }

    observeEvent(get_cols(), {
      current_cols <- get_cols()
      if (length(current_cols) > 0) {
        updateSelectInput(
          session,
          inputId = "columns",
          choices = current_cols,
          selected = r_selection()
        )
      }
    })

    r_selection
  })
}
