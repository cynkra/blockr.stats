# Modeling view -- the polish template for stats-views
#
# Run from the workspace root on the host:
#   Rscript blockr.stats/dev/modeling.R
#
# Open http://localhost:3838 (devcontainer-forwarded port).
#
# Layout
# ------
# A single dock_board with one named view "Modeling". The view contains:
#
#   data       (static_block, iris)
#   model      (adaptive model_block: pick lm/logistic/multinomial/ordinal/mixed)
#   report     (auto-generated APA-style narrative -- the visible wow)
#   params     (parameters::model_parameters table)
#   anova      (ANOVA F-table)
#   effects    (eta-squared / partial / omega)
#   stdb       (standardized betas)
#   perf       (R^2 / AIC / BIC headline)
#   check      (performance::check_model diagnostic plots)
#   coefplot   (dot-and-whisker coefficient plot)
#
# Default selection: lm of Sepal.Length on Petal.Length + Petal.Width + Species.
# Switch the model_block "Model type" picker to see panels adapt.

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

board <- new_dock_board(
  blocks = c(
    data = new_static_block(iris),
    model = new_model_block(
      model_type = "lm",
      response = "Sepal.Length",
      predictors = c("Petal.Length", "Petal.Width"),
      factors = "Species"
    ),
    report = new_report_block(),
    params = new_parameters_block(),
    anova = new_anova_block(),
    effects = new_effectsize_block(),
    stdb = new_standardize_block(),
    perf = new_performance_block(),
    check = new_check_model_block(),
    coefplot = new_coefplot_block()
  ),
  links = links(
    new_link(from = "data", to = "model"),
    new_link(from = "model", to = "report"),
    new_link(from = "model", to = "params"),
    new_link(from = "model", to = "anova"),
    new_link(from = "model", to = "effects"),
    new_link(from = "model", to = "stdb"),
    new_link(from = "model", to = "perf"),
    new_link(from = "model", to = "check"),
    new_link(from = "model", to = "coefplot")
  ),
  layout = dock_layouts(
    Modeling = dock_view(
      "data", "model", "report", "perf", "params",
      "anova", "effects", "stdb", "check", "coefplot",
      active = TRUE
    )
  ),
  extensions = list(
    blockr.dag::new_dag_extension()
  )
)

options(shiny.port = 3838L, shiny.host = "0.0.0.0")
serve(board, plugins = custom_plugins(manage_project()))
