# T-tests view
#
# Run from workspace root:
#   Rscript blockr.stats/dev/ttests.R
#
# Open http://localhost:3838.
#
# Every block in the view starts with valid default selections so the
# whole workflow renders without errors on first load. Defaults are
# computed from the dataset shape -- works whether palmerpenguins is
# installed or we fall back to iris (with a synthesized 2-level factor).

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

# Demo dataset: always has a 2-level factor (required for independent t-test).
demo_data <- if (requireNamespace("palmerpenguins", quietly = TRUE)) {
  na.omit(palmerpenguins::penguins)
} else {
  d <- iris
  d$is_setosa <- factor(ifelse(d$Species == "setosa", "setosa", "other"))
  d
}

# Pick sensible defaults from the dataset shape.
.num_cols <- names(demo_data)[vapply(demo_data, is.numeric, logical(1))]
.fac_cols <- names(demo_data)[vapply(
  demo_data, function(x) is.factor(x) || is.character(x), logical(1)
)]
.two_lvl <- .fac_cols[vapply(
  demo_data[.fac_cols],
  function(x) length(unique(x[!is.na(x)])) == 2L,
  logical(1)
)]
default_dv     <- .num_cols[[length(.num_cols)]]   # last numeric -- usually the headline measurement
default_pair   <- .num_cols[[1L]]                  # second numeric for paired test
default_group2 <- .two_lvl[[1L]]                   # 2-level factor

board <- new_dock_board(
  blocks = c(
    data = new_static_block(demo_data),
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
    new_link(from = "data", to = "ttest"),
    new_link(from = "ttest", to = "ttest_params"),
    new_link(from = "ttest", to = "cohen"),
    new_link(from = "data", to = "norm"),
    new_link(from = "data", to = "homog"),
    new_link(from = "data", to = "nonpar"),
    new_link(from = "ttest", to = "ttest_report")
  ),
  layout = dock_layouts(
    `T-Tests` = dock_view(
      "data", "ttest", "ttest_report", "ttest_params",
      "cohen", "norm", "homog", "nonpar",
      active = TRUE
    )
  ),
  extensions = list(
    blockr.dag::new_dag_extension()
  )
)

options(shiny.port = 3838L, shiny.host = "0.0.0.0")
serve(board, plugins = custom_plugins(manage_project()))
