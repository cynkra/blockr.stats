# All six blockr.stats blocks on one dock board, for checking them against
# the design system (gear trays, controls, the summary card).
#
#   Rscript dev/ds-demo.R          # port from blockr_port()
#   Rscript dev/ds-demo.R 3843
#
# Uses the INSTALLED packages. After editing inst/ or R/, bump Version and
# run `R CMD INSTALL --no-test-load .` before starting it again.

port <- local({
  arg <- commandArgs(trailingOnly = TRUE)[1L]
  if (!is.na(arg)) as.integer(arg) else blockr_port()
})

library(blockr.core)
library(blockr.dock)
library(blockr.stats)

board <- new_dock_board(
  blocks = c(
    penguins = new_dataset_block(dataset = "penguins",
                                 package = "palmerpenguins"),
    lung = new_dataset_block(dataset = "lung", package = "survival"),
    model = new_model_block(
      model_type = "lm",
      formula = "body_mass_g ~ flipper_length_mm + species + sex"
    ),
    summary = new_model_summary_block(),
    broom = new_broom_block(output = "tidy"),
    stat_test = new_stat_test_block(
      type = "t_test", values = "body_mass_g", groups = "sex"
    ),
    correlate = new_correlate_block(
      vars = c("bill_length_mm", "bill_depth_mm", "flipper_length_mm",
               "body_mass_g")
    ),
    survival = new_survival_block(
      type = "km", time_var = "time", event_var = "status", group_var = "sex"
    )
  ),
  links = list(
    list(from = "penguins", to = "model", input = "data"),
    list(from = "model", to = "summary", input = "data"),
    list(from = "model", to = "broom", input = "data"),
    list(from = "penguins", to = "stat_test", input = "data"),
    list(from = "penguins", to = "correlate", input = "data"),
    list(from = "lung", to = "survival", input = "data")
  ),
  grids = list(
    Model = dock_grid("model", "summary", "broom"),
    Tests = dock_grid("stat_test", "correlate", "survival")
  )
)

cat(sprintf("\nOpen: http://127.0.0.1:%d/\n\n", port))

shiny::runApp(serve(board), port = port, host = "0.0.0.0",
              launch.browser = FALSE)
