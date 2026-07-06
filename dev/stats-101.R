# blockr.stats — stats-101 demo board.
#
# A model-centric tour, not a method checklist. The star is the
# model card: new_model_block()'s preview already carries the
# coefficient forest, the adj-R2 fit chip, the lm/glm toggle and the
# live formula-input widget. Everything else is the story around it —
# explore the correlations, read the forest, check the diagnostics,
# see the fit, then repeat the whole move on the survival side
# (Kaplan-Meier curve + a Cox hazard-ratio forest that rhymes with
# the regression forest).
#
#   Explore   penguins correlations (heatmap)
#   Model     lm card (forest + adj-R2) ── marginal fit
#   Diagnose  broom augment ─┬─ residuals vs fitted
#                            └─ normal Q-Q
#   Survival  KM card ── Kaplan-Meier step curve
#   Hazards   Cox card ── broom tidy ── hazard-ratio forest
#
# Run from workspace root:
#   Rscript -e 'options(shiny.port=3838L, shiny.host="127.0.0.1");
#     source("blockr.stats/dev/stats-101.R", echo=FALSE, print.eval=TRUE)'

options(blockr.dock_is_locked = FALSE)

pkgload::load_all("blockr.ui",    quiet = TRUE)
pkgload::load_all("blockr.core",  quiet = TRUE)
pkgload::load_all("blockr.react", quiet = TRUE)
pkgload::load_all("blockr.dock",  quiet = TRUE)
pkgload::load_all("blockr.viz",    quiet = TRUE)
pkgload::load_all("blockr.stats", quiet = TRUE)

# model formula authored via the formula-input widget; built here from text
mdl_formula <- parse_formula(
  "body_mass_g ~ flipper_length_mm + bill_length_mm + species"
)

board <- new_dock_board(
  blocks = c(
    # data
    peng = new_dataset_block(dataset = "penguins",
                             package = "palmerpenguins"),
    lung = new_dataset_block(dataset = "lung", package = "survival"),

    # Explore: correlation matrix, shown two ways — the heatmap and a
    # shaded table (diverging fill centred on 0, domain -1..1)
    cormat = new_correlate_block(),
    # The styled cell-visual table lives in the control pane; hide the
    # default DT preview pane ("outputs") so only the shaded heatmap shows.
    cortab = `attr<-`(
               new_table_block(
                 rowname = "var",
                 cell_color = drilldown_table_color("diverging",
                                                    domain = c(-1, 1)),
                 block_name = "Correlation matrix"),
               "visible", "inputs"),

    # Model: the ONE model block — its preview IS the model card
    # (coefficient forest + adj-R2 chip + lm/glm toggle + formula widget)
    mdl = new_model_block(model_type = "lm", formula = mdl_formula,
            block_name = "Linear model"),

    # Marginal: response vs a predictor, by group, with an lm smoother
    marg = new_chart_block(
             chart_type = "scatter", x = "flipper_length_mm",
             y = "body_mass_g", color = "species", series = "species",
             smoother = "lm", block_name = "Marginal: mass vs flipper"),

    # Diagnose: augment (+qq) -> residual diagnostics
    aug   = new_broom_block(output = "augment", qq = TRUE),
    resid = new_chart_block(
              chart_type = "scatter", x = ".fitted", y = ".resid",
              smoother = "loess", block_name = "Residuals vs fitted"),
    qq    = new_chart_block(
              chart_type = "scatter", x = ".qq_theoretical",
              y = ".qq_sample", block_name = "Normal Q-Q"),

    # Survival: KM card -> broom tidy -> step curve
    surv  = new_survival_block(type = "km", time_var = "time",
              event_var = "status", group_var = "sex",
              block_name = "Kaplan-Meier fit"),
    kmt   = new_broom_block(output = "tidy"),
    kmp   = new_chart_block(
              chart_type = "line", x = "time", y = "estimate",
              color = "strata", series = "strata", step = "end",
              block_name = "Kaplan-Meier"),

    # Hazards: Cox card -> broom tidy -> HR forest (mirrors the lm forest).
    # Seeded with one covariate; add age / ph.ecog live in the card and
    # the forest grows.
    cox   = new_survival_block(type = "cox", time_var = "time",
              event_var = "status", group_var = "sex",
              block_name = "Cox PH"),
    coxt  = new_broom_block(output = "tidy"),
    coxp  = new_chart_block(
              chart_type = "scatter", x = "term", y = "estimate",
              series = "term", lo = "conf.low", hi = "conf.high",
              block_name = "Hazard ratios")
  ),
  links = links(
    from = c(
      # penguins fan-out
      "peng", "peng", "peng",
      # correlation heatmap -> shaded table
      "cormat",
      # regression chain
      "mdl", "aug", "aug",
      # survival: KM + Cox off the lung data
      "lung", "surv", "kmt",
      "lung", "cox", "coxt"
    ),
    to   = c(
      "cormat", "mdl", "marg",
      "cortab",
      "aug", "resid", "qq",
      "surv", "kmt", "kmp",
      "cox", "coxt", "coxp"
    )
  ),
  extensions = list(blockr.react::new_react_extension()),
  layouts = list(
    Setup    = dock_layout("peng", "lung", "react_extension"),
    Explore  = dock_layout("cormat", "cortab"),
    Model    = dock_layout("mdl", "marg"),
    Diagnose = dock_layout("resid", "qq"),
    Survival = dock_layout("surv", "kmp"),
    Hazards  = dock_layout("cox", "coxp")
  ),
  active = "Model"
)

serve(board)
