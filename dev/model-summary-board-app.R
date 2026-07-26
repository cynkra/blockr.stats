# Three-block board for the update path: data -> model -> summary card.
# Used by dev/probe-card-flicker.R to watch what the summary's output slot
# does when the MODEL upstream changes (the case that blinks), not just when
# one of its own gear options changes.
#
#   Rscript dev/model-summary-board-app.R 4371

root <- if (file.exists("DESCRIPTION")) ".." else "."

port <- local({
  arg <- commandArgs(trailingOnly = TRUE)[1L]
  env <- Sys.getenv("BLOCKR_PORT", unset = "")
  raw <- if (!is.na(arg)) arg else if (nzchar(env)) env else "3838"
  p <- suppressWarnings(as.integer(raw))
  if (is.na(p)) stop("Not a port: ", raw, call. = FALSE)
  p
})

for (pkg in c("blockr.core", "blockr.stats")) {
  pkgload::load_all(file.path(root, pkg), quiet = TRUE)
}

options(shiny.port = port, shiny.host = "0.0.0.0")

board <- new_board(
  blocks = c(
    data = new_dataset_block(dataset = "mtcars"),
    mdl  = new_model_block(model_type = "lm", formula = "mpg ~ wt + hp"),
    summ = new_model_summary_block()
  ),
  links = links(from = c("data", "mdl"), to = c("mdl", "summ"))
)

cat(sprintf("\nOpen: http://127.0.0.1:%d/\n\n", port))

serve(board)
