#' HTML dependency for the blocks' controls
#'
#' blockr.ui's shared controls and theme ([blockr.ui::controls_dep()]), then
#' this package's Shiny input binding over them (`inst/js/stats-controls.js`)
#' and its few layout rules (`inst/css/stats-blocks.css`). Asset URLs carry
#' the package version, so bump `Version` after editing either file.
#'
#' @return An [htmltools::tagList()] of [htmltools::htmlDependency] objects.
#' @noRd
stats_controls_dep <- function() {
  htmltools::tagList(
    blockr.ui::controls_dep(),
    htmltools::htmlDependency(
      name = "blockr-stats-controls",
      version = utils::packageVersion("blockr.stats"),
      src = system.file(package = "blockr.stats"),
      script = "js/stats-controls.js",
      stylesheet = "css/stats-blocks.css"
    )
  )
}

#' HTML dependency for the card's click-to-sort
#'
#' Ships with the card itself (from `block_output()`), not with the block's
#' controls: the sort is a property of the rendered table.
#'
#' @return An [htmltools::htmlDependency].
#' @noRd
model_summary_sort_dep <- function() {
  htmltools::htmlDependency(
    name = "blockr-stats-model-summary-sort",
    version = utils::packageVersion("blockr.stats"),
    src = system.file(package = "blockr.stats"),
    script = "js/model-summary-sort.js"
  )
}

#' Options for a JS control
#'
#' `choices` is what `selectInput()` takes: a character vector, named or not.
#' Names are the labels.
#'
#' @param choices Character vector.
#' @return A list of `list(value =, label =)`.
#' @noRd
control_options <- function(choices) {
  choices <- choices %||% character()
  labels <- names(choices) %||% rep("", length(choices))
  labels[is.na(labels)] <- ""
  values <- as.character(unname(choices))
  lapply(seq_along(values), function(i) {
    list(value = values[[i]], label = labels[[i]])
  })
}

#' A blockr.ui control bound as a Shiny input
#'
#' The container `inst/js/stats-controls.js` fills with the control named by
#' `kind`, wrapped in a field with its label. On the block's face fields
#' stack at full width; in the gear tray `size` places them in the grid
#' (small: one column, large: two, full: the row).
#'
#' @param id Namespaced input id.
#' @param kind `"select"`, `"multi"`, `"segmented"`, `"checkbox"`, `"number"`
#'   or `"text"`.
#' @param label Field label; for a checkbox, the words beside the box.
#' @param config List passed to the JS as JSON: `options`, `selected`,
#'   `placeholder`, `allowEmpty`, `labelOnly`, `min`, `max`, `step`.
#' @param size `NULL` for a face field, or `"small"`, `"large"` or `"full"`
#'   for a field in the gear tray.
#' @return A [shiny::div()].
#' @noRd
control_field <- function(id, kind, label, config = list(), size = NULL) {
  if (kind %in% c("checkbox", "segmented")) {
    config$label <- label
  }
  ctl <- div(
    id = id,
    class = "blockr-stats-control",
    `data-kind` = kind,
    `data-config` = as.character(
      jsonlite::toJSON(config, auto_unbox = TRUE, null = "null")
    )
  )
  cls <- if (is.null(size)) {
    "blockr-stats-field"
  } else {
    paste(
      "blockr-settings__field",
      switch(size, small = "blockr-settings__field--small",
             full = "blockr-settings__field--full", NULL)
    )
  }
  div(
    class = cls,
    if (!identical(kind, "checkbox")) tags$span(class = "blockr-label", label),
    ctl
  )
}

#' @param choices Named or unnamed character vector (see
#'   `control_options()`).
#' @param selected Current value(s).
#' @param label_only Show an option's label alone, for a fixed vocabulary
#'   whose values are internal tokens. Off for columns, which show the name
#'   and then the label.
#' @rdname control_field
#' @noRd
select_field <- function(id, label, choices, selected, placeholder = NULL,
                         allow_empty = FALSE, label_only = TRUE,
                         size = NULL) {
  control_field(id, "select", label, list(
    options = control_options(choices),
    selected = if (length(selected)) as.character(selected[[1L]]) else "",
    placeholder = placeholder,
    allowEmpty = allow_empty,
    labelOnly = label_only
  ), size)
}

#' @rdname control_field
#' @noRd
multi_field <- function(id, label, choices, selected, placeholder = NULL,
                        size = NULL) {
  control_field(id, "multi", label, list(
    options = control_options(choices),
    selected = I(as.character(selected %||% character())),
    placeholder = placeholder,
    labelOnly = FALSE
  ), size)
}

#' @rdname control_field
#' @noRd
segmented_field <- function(id, label, choices, selected, size = NULL) {
  control_field(id, "segmented", label, list(
    options = control_options(choices),
    selected = as.character(selected)
  ), size)
}

#' A fixed choice: a segmented control for two or three values, a select
#' for more (the design system's table of controls).
#' @rdname control_field
#' @noRd
choice_field <- function(id, label, choices, selected, size = NULL) {
  if (length(choices) <= 3L) {
    segmented_field(id, label, choices, selected, size = size)
  } else {
    select_field(id, label, choices, selected, size = size)
  }
}

#' @rdname control_field
#' @noRd
checkbox_field <- function(id, label, value, size = "small") {
  control_field(id, "checkbox", label, list(selected = isTRUE(value)), size)
}

#' @rdname control_field
#' @noRd
number_field <- function(id, label, value, min = NULL, max = NULL,
                         step = NULL, size = "small") {
  control_field(id, "number", label, list(
    selected = value, min = min, max = max, step = step
  ), size)
}

#' @rdname control_field
#' @noRd
text_field <- function(id, label, value, placeholder = NULL, size = "large") {
  control_field(id, "text", label, list(
    selected = value %||% "", placeholder = placeholder
  ), size)
}

#' Push new options or a new value to a control
#'
#' The JS applies it without reporting it back as a change.
#'
#' @param session Shiny session.
#' @param id Input id, un-namespaced (the session namespaces it).
#' @param choices New options, or `NULL` to keep them.
#' @param selected New value(s).
#' @noRd
update_control <- function(session, id, choices = NULL, selected = NULL) {
  msg <- list()
  if (!is.null(choices)) {
    msg$options <- control_options(choices)
  }
  msg$selected <- if (length(selected) > 1L) {
    as.list(selected)
  } else if (length(selected) == 1L) {
    selected
  } else {
    list()
  }
  session$sendInputMessage(id, msg)
}

#' The gear and its tray
#'
#' The gear sits right in the header row; the tray opens in flow under it
#' (blockr.ui's `Blockr.gearTray`: it slides, closes by the gear or Escape,
#' and names itself by `aria-label`). A tray with one section has no title.
#'
#' @param ns Module namespace function.
#' @param ... Fields for the tray's grid.
#' @param label The tray's accessible name.
#' @return A [htmltools::tagList()].
#' @noRd
gear_tray <- function(ns, ..., label = "Settings") {
  tagList(
    div(
      class = "blockr-gear-header",
      tags$button(id = ns("gear"), type = "button", class = "blockr-gear-btn")
    ),
    div(
      id = ns("band"),
      class = "blockr-settings blockr-settings--beak",
      div(class = "blockr-settings__grid", ...)
    ),
    tags$script(HTML(sprintf(
      "(function () {
         function go() {
           if (!window.Blockr || !Blockr.statsGear) { setTimeout(go, 50); return; }
           Blockr.statsGear('%s', '%s', '%s');
         }
         go();
       })();",
      ns("gear"), ns("band"), label
    )))
  )
}
