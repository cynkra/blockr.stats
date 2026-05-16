# SPSS-style stats cell on top of blockr.stats
#
# A hand-composed dashboard: existing blockr.stats blocks arranged in a
# dock_layouts grid that reads as a single SPSS / jamovi output cell,
# even though under the hood it's still a blockr pipeline of atomic
# blocks.
#
# Run from the workspace root (the directory holding blockr.core,
# blockr.stats, blockr.dock, blockr.dag):
#
#   Rscript blockr.stats/dev/spss-cell.R
#
# Then open http://localhost:3838 (devcontainer-forwarded port).
#
# Layout
# ------
# Two top-level views (tabs at the very top, like jamovi's Data view +
# Analyses):
#
#   Data            : the input dataset preview
#   Regression      : the analysis cell, arranged as
#                       summary  (full-width R^2/F/p headline)
#                       tabs(coef, anova)  |  diagnostics
#                       coefplot  (full-width)
#
# The user can drag in further blocks from the right-hand drawer or
# add more views via the view-nav dropdown.

options(
  blockr.dock_is_locked = FALSE,
  blockr.html_table_preview = TRUE,
  blockr.session_url_params = TRUE
)

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.stats")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.dock")

board <- new_dock_board(
  blocks = c(
    data    = new_static_block(iris),
    lm      = new_lm_block(
      response   = "Sepal.Length",
      predictors = c("Petal.Length", "Petal.Width", "Sepal.Width")
    ),
    summary = new_model_summary_block(),
    coef    = new_coef_block(),
    anova   = new_anova_block(),
    check   = new_check_model_block(),
    coefplot = new_coefplot_block()
  ),
  links = links(
    new_link(from = "data", to = "lm"),
    new_link(from = "lm",   to = "summary"),
    new_link(from = "lm",   to = "coef"),
    new_link(from = "lm",   to = "anova"),
    new_link(from = "lm",   to = "check"),
    new_link(from = "lm",   to = "coefplot")
  ),
  layout = dock_layouts(
    Data = list("data"),
    Regression = list(
      list(
        "summary",
        list(c("coef", "anova"), "check"),
        "coefplot"
      )
    )
  ),
  extensions = list(
    blockr.dag::new_dag_extension()
  )
)

# options(shiny.port = 3838L, shiny.host = "0.0.0.0")
serve(board)
