# blockr.stats — basic modeling workflow.
#
# ONE model block (formula-input widget) whose preview is the model
# card. The card already carries the coefficient forest plot, the
# adj-R2 fit chip, and the raw summary() (R toggle) — so the composed
# downstream blocks are only what the card does NOT show: the marginal
# view and the residual diagnostics (mirroring the validated blockr.lm
# prototype, minus its auto-prose / redundant pieces).
#
#   data ─► model ── card (forest + adj-R2 + summary toggle)
#            └─ broom augment ─┬─ residuals vs fitted (drilldown)
#                              └─ normal Q-Q          (drilldown)
#   data ─► marginal: y vs predictor by group         (drilldown)
#
# Run from workspace root:
#   Rscript -e 'options(shiny.port=3838L, shiny.host="127.0.0.1");
#     source("blockr.stats/dev/model-dashboard.R", echo=FALSE, print.eval=TRUE)'

options(blockr.dock_is_locked = FALSE)

pkgload::load_all("blockr.ui",    quiet = TRUE)
pkgload::load_all("blockr.core",  quiet = TRUE)
pkgload::load_all("blockr.dag", quiet = TRUE)
pkgload::load_all("blockr.dock",  quiet = TRUE)
pkgload::load_all("blockr.viz",    quiet = TRUE)
pkgload::load_all("blockr.stats", quiet = TRUE)

# formula state authored via the formula-input widget; built here from text
mdl_formula <- parse_formula(
  "body_mass_g ~ flipper_length_mm + bill_length_mm + species"
)

board <- new_dock_board(
  blocks = c(
    # data (hidden; runs via links)
    peng = new_dataset_block(dataset = "penguins",
                             package = "palmerpenguins"),

    # the ONE model block — its preview is the model card
    mdl = new_model_block(model_type = "lm", formula = mdl_formula,
            block_name = "Linear model"),

    # marginal: response vs a predictor, by group, with lm smoother
    marg = new_chart_block(
             chart_type = "scatter", x = "flipper_length_mm",
             y = "body_mass_g", color = "species", series = "species",
             smoother = "lm", block_name = "Marginal: mass vs flipper"),

    # augment (+qq) -> residual diagnostics
    aug   = new_broom_block(output = "augment", qq = TRUE),
    resid = new_chart_block(
              chart_type = "scatter", x = ".fitted", y = ".resid",
              smoother = "loess", block_name = "Residuals vs fitted"),
    qq    = new_chart_block(
              chart_type = "scatter", x = ".qq_theoretical",
              y = ".qq_sample", block_name = "Normal Q-Q")
  ),
  links = links(
    from = c("peng", "peng", "mdl", "aug", "aug"),
    to   = c("mdl",  "marg", "aug", "resid", "qq")
  ),
  extensions = list(blockr.dag::new_dag_extension())
)

serve(board)
