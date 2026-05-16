# blockr.stats — stats-101 demo board.
#
# The applied-stats-basics spine (Describe / Compare / Associate /
# Regress / Nonparametric) + the Advanced survival tier, all emitting
# tidy frames into the generic drilldown renderers. No easystats, no
# auto-narrative, no bespoke plot blocks.
#
# Run from workspace root:
#   Rscript -e 'options(shiny.port=3838L, shiny.host="127.0.0.1");
#     source("blockr.stats/dev/stats-101.R", echo=FALSE, print.eval=TRUE)'

options(blockr.dock_is_locked = FALSE)

pkgload::load_all("blockr.core",  quiet = TRUE)
pkgload::load_all("blockr.react", quiet = TRUE)
pkgload::load_all("blockr.dock",  quiet = TRUE)
pkgload::load_all("blockr.bi",    quiet = TRUE)
pkgload::load_all("blockr.stats", quiet = TRUE)

board <- new_dock_board(
  blocks = c(
    # data
    peng = new_dataset_block(dataset = "penguins",
                             package = "palmerpenguins"),
    lung = new_dataset_block(dataset = "lung", package = "survival"),

    # Describe
    desc  = new_descriptives_block(),
    freq  = new_frequencies_block(vars = "species", by = "sex"),

    # Associate
    cormat = new_correlation_matrix_block(),

    # Compare / Nonparametric (adaptive test block)
    test = new_stat_test_block(type = "anova_oneway"),

    # Regress: model -> broom tidy -> coef plot; broom augment -> resid
    mdl   = new_model_block(model_type = "lm",
              response = "body_mass_g",
              predictors = c("flipper_length_mm", "bill_length_mm")),
    coefs = new_broom_block(output = "tidy"),
    coefp = new_drilldown_chart_block(
              chart_type = "scatter", x_col = "term",
              y_col = "estimate", series_by = "term",
              lo_col = "conf.low", hi_col = "conf.high",
              block_name = "Coefficient plot"),
    aug   = new_broom_block(output = "augment", qq = TRUE),
    resid = new_drilldown_chart_block(
              chart_type = "scatter", x_col = ".fitted",
              y_col = ".resid", smoother = "loess",
              block_name = "Residuals vs fitted"),

    # Effect size
    es    = new_effect_size_block(measure = "partial_eta2"),

    # Advanced: survival KM -> broom tidy -> step curve
    surv  = new_survival_block(type = "km", time_var = "time",
              event_var = "status", group_var = "sex"),
    kmt   = new_broom_block(output = "tidy"),
    kmp   = new_drilldown_chart_block(
              chart_type = "line", x_col = "time", y_col = "estimate",
              color_by = "strata", series_by = "strata", step = "end",
              block_name = "Kaplan-Meier")
  ),
  links = links(
    from = c("peng", "peng", "peng", "peng", "peng",
             "mdl", "coefs", "mdl", "aug", "mdl",
             "lung", "surv", "kmt"),
    to   = c("desc", "freq", "cormat", "test", "mdl",
             "coefs", "coefp", "aug", "resid", "es",
             "surv", "kmt", "kmp")
  ),
  extensions = list(blockr.react::new_react_extension()),
  layout = dock_layouts(
    Setup       = dock_view("peng", "lung", "react_extension",
                            active = TRUE),
    Describe    = dock_view("desc", "freq"),
    Associate   = dock_view("cormat"),
    Compare     = dock_view("test"),
    Regress     = dock_view("coefp", "resid", "es"),
    Survival    = dock_view("kmp")
  )
)

serve(board)
