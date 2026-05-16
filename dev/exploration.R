# Data exploration view
#
# Run from workspace root on host:
#   Rscript blockr.stats/dev/exploration.R
#
# Open http://localhost:3838.
#
# Layout
# ------
# A single dock_board with one named view "Exploration":
#
#   data            (static_block, palmerpenguins or iris)
#   desc            (descriptives table via parameters::describe_distribution)
#   freq            (frequencies for categorical columns)
#   correl          (correlation matrix via correlation::correlation)
#   explore_report  (auto-narrative for the descriptives result)

options(
  blockr.dock_is_locked = FALSE,
  blockr.html_table_preview = TRUE,
  blockr.session_url_params = TRUE
)

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.stats")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.session")

# Default to iris if palmerpenguins isn't installed
demo_data <- if (requireNamespace("palmerpenguins", quietly = TRUE)) {
  na.omit(palmerpenguins::penguins)
} else {
  iris
}

board <- new_dock_board(
  blocks = c(
    data = new_static_block(demo_data),
    desc = new_descriptives_block(),
    freq = new_frequencies_block(),
    correl = new_correlation_block(method = "pearson"),
    explore_report = new_report_block()
  ),
  links = links(
    new_link(from = "data", to = "desc"),
    new_link(from = "data", to = "freq"),
    new_link(from = "data", to = "correl"),
    new_link(from = "data", to = "explore_report")
  ),
  layout = dock_layouts(
    Exploration = dock_view(
      "data", "desc", "freq", "correl", "explore_report",
      active = TRUE
    )
  ),
  extensions = list(
    blockr.dag::new_dag_extension()
  )
)

options(shiny.port = 3838L, shiny.host = "0.0.0.0")
serve(board, plugins = custom_plugins(manage_project()))
