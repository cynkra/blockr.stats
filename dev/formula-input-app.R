# blockr.stats — formula-input widget check (dock board, full CSS).
#
# Run from workspace root:
#   Rscript blockr.stats/dev/formula-input-app.R

# options(shiny.port = 3838L, shiny.host = "0.0.0.0")

suppressMessages({
  library(blockr.core)
  library(blockr.dplyr)
  library(blockr.ui)
  library(blockr.dock)
  pkgload::load_all("blockr.stats", quiet = TRUE)
})

board <- new_dock_board(
  blocks = blocks(
    data  = new_dataset_block(dataset = "mtcars", package = "datasets"),
    model = new_model_block(),
    # real survival data: NCCTG lung cancer (time, status 1=cens/2=dead, sex)
    lung  = new_dataset_block(dataset = "lung", package = "survival"),
    surv  = new_survival_block(type = "km", time_var = "time",
                               event_var = "status", group_var = "sex")
  ),
  links = links(
    new_link(from = "data", to = "model", input = "data"),
    new_link(from = "lung", to = "surv", input = "data")
  )
)

serve(board)
