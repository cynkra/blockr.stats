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

pkgload::load_all("blockr.ui",    quiet = TRUE)
pkgload::load_all("blockr.core",  quiet = TRUE)
pkgload::load_all("blockr.react", quiet = TRUE)
pkgload::load_all("blockr.dock",  quiet = TRUE)
pkgload::load_all("blockr.bi",    quiet = TRUE)
pkgload::load_all("blockr.stats", quiet = TRUE)

# model formula authored via the formula-input widget; built here from text
mdl_formula <- parse_formula(
  "body_mass_g ~ flipper_length_mm + bill_length_mm"
)

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
    cormat = new_correlate_block(),

    # Compare / Nonparametric (adaptive test block)
    test = new_stat_test_block(type = "anova_oneway"),

    # Regress: model -> broom tidy -> coef plot; broom augment -> resid
    mdl   = new_model_block(model_type = "lm", formula = mdl_formula),
    coefs = new_broom_block(output = "tidy"),
    coefp = new_chart_block(
              chart_type = "scatter", x = "term", y = "estimate",
              series = "term", lo = "conf.low", hi = "conf.high",
              block_name = "Coefficient plot"),
    aug   = new_broom_block(output = "augment", qq = TRUE),
    resid = new_chart_block(
              chart_type = "scatter", x = ".fitted", y = ".resid",
              smoother = "loess", block_name = "Residuals vs fitted"),

    # Effect size
    es    = new_effect_size_block(measure = "partial_eta2"),

    # Advanced: survival KM -> broom tidy -> step curve
    surv  = new_survival_block(type = "km", time_var = "time",
              event_var = "status", group_var = "sex"),
    kmt   = new_broom_block(output = "tidy"),
    kmp   = new_chart_block(
              chart_type = "line", x = "time", y = "estimate",
              color = "strata", series = "strata", step = "end",
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
  layouts = list(
    Setup     = dock_layout("peng", "lung", "react_extension", active = TRUE),
    Describe  = dock_layout("desc", "freq"),
    Associate = dock_layout("cormat"),
    Compare   = dock_layout("test"),
    Regress   = dock_layout("coefp", "resid", "es"),
    Survival  = dock_layout("kmp")
  )
)

serve(board)
