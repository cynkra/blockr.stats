# Run the stats-101 board against LOCAL source checkouts (your latest
# uncommitted changes to any blockr package). This is the pkgload::load_all()
# counterpart of the shipped, library()-based inst/examples/stats-101.R: it just
# flips the loader and sources it, so the two can never drift.
#
# Run from an R session at the workspace root:
#   source("blockr.stats/dev/stats-101.R")
#
# Or headless, with a port (arg -> BLOCKR_PORT -> 3838):
#   Rscript blockr.stats/dev/stats-101.R
#   Rscript blockr.stats/dev/stats-101.R 4242
#
# (End users without the source checkouts run the shipped copy instead:
#   source(system.file("examples/stats-101.R", package = "blockr.stats")))

port <- local({
  arg <- commandArgs(trailingOnly = TRUE)[1L]
  env <- Sys.getenv("BLOCKR_PORT", unset = "")
  raw <- if (!is.na(arg)) arg else if (nzchar(env)) env else "3838"
  p <- suppressWarnings(as.integer(raw))
  if (is.na(p)) stop("Not a port: ", raw, call. = FALSE)
  p
})

options(shiny.port = port, shiny.host = "0.0.0.0")

cat(sprintf("\nOpen: http://127.0.0.1:%d/\n\n", port))

dev_local <- TRUE
# print.eval = TRUE, or nothing serves under Rscript: the example ends in
# serve(), which RETURNS a shiny.appobj rather than running one, and it is the
# auto-print of that value that launches the app. source()'s print.eval
# defaults to interactive(), so from an interactive session it is already TRUE
# and from Rscript it is FALSE -- the script would build the whole board and
# exit 0 without ever listening.
source("blockr.stats/inst/examples/stats-101.R", print.eval = TRUE)
