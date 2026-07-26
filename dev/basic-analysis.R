# blockr.stats: what a basic analysis looks like.
#
# The smallest board that walks the whole arc: look at the data, describe
# it, compare two groups, fit a model, read the coefficients. Six blocks,
# four views, one dataset (palmerpenguins). For the full tour (survival,
# correlations, diagnostics) see dev/stats-101.R.
#
#   Data      penguins
#   Describe  summary table -> Table 1, split by species
#   Compare   two-sample t-test: body mass by sex
#   Model     lm block (formula widget, preview = the R print)
#               `- model summary -> the card: facts line + coefficient forest
#
# Run from the workspace root (or from blockr.stats/):
#   Rscript blockr.stats/dev/basic-analysis.R          # port 3838
#   Rscript blockr.stats/dev/basic-analysis.R 4242     # or BLOCKR_PORT=4242

root <- if (file.exists("blockr.stats/DESCRIPTION")) "." else ".."

port <- local({
  arg <- commandArgs(trailingOnly = TRUE)[1L]
  env <- Sys.getenv("BLOCKR_PORT", unset = "")
  raw <- if (!is.na(arg)) arg else if (nzchar(env)) env else "3838"
  p <- suppressWarnings(as.integer(raw))
  if (is.na(p)) stop("Not a port: ", raw, call. = FALSE)
  p
})

options(shiny.port = port, shiny.host = "0.0.0.0")

# load_all FIRST: every blockr.* dep from local source, before any option
# value or board code touches a blockr namespace.
for (pkg in c("blockr.ui", "blockr.core", "blockr.dag", "blockr.dock",
              "blockr.viz", "blockr.stats")) {
  pkgload::load_all(file.path(root, pkg), quiet = TRUE)
}

options(
  blockr.dock_is_locked = FALSE,
  blockr.tabular_display = blockr.ui::html_table_display
)

board <- new_dock_board(
  blocks = c(
    # Data: 344 penguins, 3 species, 4 body measurements.
    peng = new_dataset_block(dataset = "penguins",
                             package = "palmerpenguins",
                             block_name = "Penguins"),

    # Describe: mean (SD) of each measurement, one column per species.
    # The summary block emits the annotated "Table 1" frame; the table
    # block renders it (sections, indents, per-species spanners). Its own
    # DT preview is that raw frame, so show only its inputs.
    desc = `attr<-`(
      new_summary_table_block(
        vars = c("body_mass_g", "flipper_length_mm", "bill_length_mm"),
        by = "species", stats = "mean_sd", add_overall = TRUE,
        block_name = "Describe: body measurements"),
      "visible", "inputs"),
    # The table block draws the structured table in its control pane, so
    # here too the default DT pane is the redundant one.
    desc_tbl = `attr<-`(
      new_table_block(block_name = "Table 1 (by species)"),
      "visible", "inputs"),

    # Compare: is mass different between the sexes? Welch two-sample t.
    ttest = new_stat_test_block(
      type = "t_test", values = "body_mass_g", groups = "sex",
      block_name = "Mass by sex (t-test)"),

    # Model: fits and returns the model object. Its preview is the plain R
    # print -- the visual summary is the block downstream, so the fit and the
    # way it is drawn stay independent. Edit the formula and both refit.
    mdl = new_model_block(
      model_type = "lm",
      formula = "body_mass_g ~ flipper_length_mm + species",
      block_name = "Linear model"),

    # Read the coefficients: feeds on the model object directly (it calls
    # tidy + glance itself, no adapter block in between) and renders the
    # summary card. Its own value is still the tidy coefficient frame, so a
    # table or chart could hang off it. All options live in its gear.
    summ = new_model_summary_block(block_name = "Coefficients")
  ),
  links = links(
    from = c("peng", "peng", "peng", "desc", "mdl"),
    to   = c("desc", "ttest", "mdl", "desc_tbl", "summ")
  ),
  extensions = list(blockr.dag::new_dag_extension()),
  grids = list(
    Data     = dock_grid("peng", ext("dag")),
    Describe = dock_grid("desc", "desc_tbl"),
    Compare  = dock_grid("ttest"),
    Model    = dock_grid("mdl", "summ")
  ),
  active = "Model"
)

cat(sprintf("\nOpen: http://127.0.0.1:%d/\n\n", port))

serve(board)
