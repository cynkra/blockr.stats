# Stats Views -- combined demo
#
# Three top-level views (Data / Modeling / T-Tests) in a single
# dock_board. The partner-facing artifact for the prototype.
#
# Run from workspace root on host:
#   Rscript blockr.stats/dev/all-views.R
#
# Open http://localhost:3838.
#
# The app starts on the Data tab with the bundled penguins.csv loaded
# via blockr.io's read block, plus head preview, descriptives, frequencies,
# correlation, and an auto-narrative. The user can pick a different file
# at runtime by switching the read block's source to "upload". Every
# downstream block in the other two views is pre-configured with valid
# defaults derived from the penguins shape, so the workflow renders
# without errors on first load.

options(
  blockr.dock_is_locked = FALSE,
  blockr.html_table_preview = TRUE,
  blockr.session_url_params = FALSE
)

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.io")
pkgload::load_all("blockr.stats")
pkgload::load_all("blockr.extra")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.session")

# Bundled sample dataset (palmerpenguins, CC0). The path is resolved at
# runtime; replace the file or switch source = "upload" to use a different
# dataset.
penguins_csv <- system.file("extdata", "penguins.csv", package = "blockr.stats")

# Defaults derived from the penguins schema; if you swap the CSV for a
# different file, change these too (or pick from the picker UI in the
# blocks at runtime).
default_dv     <- "body_mass_g"
default_pair   <- "bill_length_mm"
default_covs   <- c("bill_length_mm", "flipper_length_mm")
default_factor <- "species"
default_group2 <- "sex"
default_vars   <- c("bill_length_mm", "bill_depth_mm",
                    "flipper_length_mm", "body_mass_g")
default_facets <- c("species", "island", "sex")

board <- new_dock_board(
  blocks = c(
    # Source data block, appears in every view
    data = new_read_block(path = penguins_csv, source = "path"),

    # Data view: read block, head preview, plus descriptives /
    # frequencies / correlation / auto-narrative
    head = new_head_block(n = 20L),
    desc = new_descriptives_block(vars = default_vars),
    freq = new_frequencies_block(vars = default_facets),
    correl = new_correlation_block(
      vars = default_vars,
      method = "pearson"
    ),
    explore_report = new_report_block(),

    # Modeling view
    model = new_model_block(
      model_type = "lm",
      response = default_dv,
      predictors = default_covs,
      factors = default_factor
    ),
    report = new_report_block(),
    perf = new_performance_block(),
    params = new_parameters_block(),
    anova = new_anova_block(),
    effects = new_effectsize_block(),
    stdb = new_standardize_block(),
    check = new_check_model_block(),
    coefplot = new_coefplot_block(),

    # T-Tests view
    ttest = new_ttest_block(
      test_type = "independent",
      dv = default_dv,
      group = default_group2,
      pair = default_pair
    ),
    ttest_params = new_parameters_block(),
    cohen = new_cohens_d_block(),
    norm = new_normality_check_block(
      vars = default_dv,
      group = default_group2
    ),
    homog = new_homogeneity_check_block(
      dv = default_dv,
      group = default_group2
    ),
    nonpar = new_nonparametric_block(
      test_type = "independent",
      dv = default_dv,
      group = default_group2,
      pair = default_pair
    ),
    ttest_report = new_report_block()
  ),
  links = links(
    # Data view
    new_link(from = "data", to = "head"),
    new_link(from = "data", to = "desc"),
    new_link(from = "data", to = "freq"),
    new_link(from = "data", to = "correl"),
    new_link(from = "data", to = "explore_report"),

    # Modeling
    new_link(from = "data", to = "model"),
    new_link(from = "model", to = "report"),
    new_link(from = "model", to = "perf"),
    new_link(from = "model", to = "params"),
    new_link(from = "model", to = "anova"),
    new_link(from = "model", to = "effects"),
    new_link(from = "model", to = "stdb"),
    new_link(from = "model", to = "check"),
    new_link(from = "model", to = "coefplot"),

    # T-Tests
    new_link(from = "data", to = "ttest"),
    new_link(from = "ttest", to = "ttest_params"),
    new_link(from = "ttest", to = "cohen"),
    new_link(from = "data", to = "norm"),
    new_link(from = "data", to = "homog"),
    new_link(from = "data", to = "nonpar"),
    new_link(from = "ttest", to = "ttest_report")
  ),
  layout = dock_layouts(
    Data = dock_view(
      "data", "head", "desc", "freq", "correl", "explore_report",
      active = TRUE
    ),
    Modeling = dock_view(
      "data", "model", "report", "perf", "params",
      "anova", "effects", "stdb", "check", "coefplot"
    ),
    `T-Tests` = dock_view(
      "data", "ttest", "ttest_report", "ttest_params",
      "cohen", "norm", "homog", "nonpar"
    )
  ),
  extensions = list(
    blockr.dag::new_dag_extension()
  )
)

# options(shiny.port = 3838L, shiny.host = "0.0.0.0")
serve(board, plugins = custom_plugins(manage_project()))
