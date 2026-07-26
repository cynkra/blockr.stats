#' HTML dependency for the in-flow gear settings band
#'
#' Vendored verbatim from blockr.viz (the canonical source until the shared
#' layer moves to blockr.ui), the same way blockr.dplyr vendors it: the gear
#' no longer opens a floating popover, it expands a full-width band between
#' the gear header and the block content. Distinct dependency name per
#' package so a stale copy can never shadow a fresh one on a mixed board.
#'
#' Bump the version suffix on every `inst/css/settings-band.css` /
#' `inst/js/settings-band.js` edit (version-pinned asset cache).
#'
#' @return An [htmltools::htmlDependency].
#' @noRd
settings_band_dep <- function() {
  htmltools::htmlDependency(
    name = "blockr-stats-settings-band",
    version = paste0(utils::packageVersion("blockr.stats"), ".1"),
    src = system.file(package = "blockr.stats"),
    script = "js/settings-band.js",
    stylesheet = "css/settings-band.css"
  )
}

#' HTML dependency for the model summary block's gear controls
#'
#' Mounts the design-system components (`Blockr.Select`, `Blockr.checkbox`)
#' into the declarative containers `ms_gear_ui()` renders. Bump the version
#' suffix on every `inst/js/model-summary-gear.js` edit.
#'
#' @return An [htmltools::htmlDependency].
#' @noRd
model_summary_gear_dep <- function() {
  htmltools::htmlDependency(
    name = "blockr-stats-model-summary-gear",
    version = paste0(utils::packageVersion("blockr.stats"), ".1"),
    src = system.file(package = "blockr.stats"),
    script = "js/model-summary-gear.js"
  )
}

#' HTML dependency for the card's click-to-sort
#'
#' Ships with the card itself (from `block_output()`), not with the block's
#' controls: the sort is a property of the rendered table. Bump the version
#' suffix on every `inst/js/model-summary-sort.js` edit.
#'
#' @return An [htmltools::htmlDependency].
#' @noRd
model_summary_sort_dep <- function() {
  htmltools::htmlDependency(
    name = "blockr-stats-model-summary-sort",
    version = paste0(utils::packageVersion("blockr.stats"), ".1"),
    src = system.file(package = "blockr.stats"),
    script = "js/model-summary-sort.js"
  )
}

#' Declarative containers for a JS-mounted gear control
#'
#' The option vocabulary stays in R; the DOM is filled by
#' `inst/js/model-summary-gear.js`. `choices` is a named character vector
#' whose names are the prose labels and whose values are the state tokens --
#' the same shape `selectInput()` takes, so the two are interchangeable.
#'
#' Blockr.Select's option anatomy is value-primary with the label as a
#' sublabel (the documented house anatomy, see the design-system
#' select-controls record), which is why the tokens stay short and readable.
#'
#' @param input_id Namespaced input id the control writes to.
#' @param label Field label.
#' @param choices Named character vector: names are labels, values are tokens.
#' @param selected Currently selected token.
#' @return A [shiny::div()].
#' @noRd
ms_select_field <- function(input_id, label, choices, selected) {
  opts <- lapply(seq_along(choices), function(i) {
    list(value = choices[[i]], label = names(choices)[i])
  })
  div(
    class = "blockr-settings__field",
    `data-ms-select` = label,
    `data-ms-input` = input_id,
    `data-ms-selected` = selected,
    `data-ms-options` = as.character(
      jsonlite::toJSON(opts, auto_unbox = TRUE)
    ),
    tags$label(class = "blockr-label", label)
  )
}

#' @param checks List of `list(input =, label =, checked =)`.
#' @rdname ms_select_field
#' @noRd
ms_check_field <- function(checks) {
  div(
    class = "blockr-settings__field--full blockr-checkbox-row",
    `data-ms-checks` = as.character(jsonlite::toJSON(checks, auto_unbox = TRUE))
  )
}

#' Mount the JS gear controls once the shared components exist
#'
#' Block UI is inserted dynamically and this script can run before
#' `Blockr.Select` / `Blockr.checkbox` are in place, so it polls. Mounting is
#' idempotent (the band carries a flag), which matters because a block UI
#' re-render runs the script again.
#'
#' @param ns Module namespace function.
#' @return A `<script>` tag.
#' @noRd
ms_gear_mount_script <- function(ns) {
  tags$script(HTML(sprintf(
    "(function () {
       function init() {
         var band = document.getElementById('%s');
         if (!band || !window.Blockr || !Blockr.msGear ||
             !Blockr.msGear.mount(band)) {
           setTimeout(init, 50); return;
         }
       }
       init();
     })();",
    ns("band")
  )))
}

#' Wire a gear button to its settings band
#'
#' Paints the gear icon (from `Blockr.icons`, loaded by
#' `blockr.dplyr::blockr_core_js_dep()`) and toggles the band open. Expects
#' a button with id `ns("gear")` and a band with id `ns("band")`. Polls until
#' both the elements and the Blockr namespace exist, because block UI is
#' inserted dynamically and the script can run before either is in place.
#'
#' @param ns Module namespace function.
#' @return A `<script>` tag.
#' @noRd
gear_band_script <- function(ns) {
  tags$script(HTML(sprintf(
    "(function () {
       function init() {
         var gear = document.getElementById('%s'),
             band = document.getElementById('%s');
         if (!gear || !band || !window.Blockr || !Blockr.icons) {
           setTimeout(init, 50); return;
         }
         gear.innerHTML = Blockr.icons.gear;
         gear.addEventListener('click', function () {
           var open = band.classList.toggle('blockr-settings--open');
           gear.classList.toggle('blockr-gear-active', open);
         });
       }
       init();
     })();",
    ns("gear"), ns("band")
  )))
}
