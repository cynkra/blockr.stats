# One-block board for checking the model summary card: a fitted lm goes in,
# the card comes out. Deliberately minimal (core + stats only, no dock) so it
# starts in seconds and nothing but this block can explain what you see.
#
#   Rscript dev/model-summary-app.R          # port 3838
#   Rscript dev/model-summary-app.R 4351     # or BLOCKR_PORT=4351

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

fit <- stats::lm(mpg ~ wt + hp + factor(cyl), datasets::mtcars)

cat(sprintf("\nOpen: http://127.0.0.1:%d/\n\n", port))

serve(new_model_summary_block(), data = list(data = fit))
