# blockr.stats — regression playground.
#
# shinyBio-style "turn the knobs on the truth" board, wired entirely
# from generic blocks. No new block types in blockr.stats.
#
# Per panel:
#   simulator (function_block, defaults -> auto inputs)
#     -> marginal plot     (drilldown_chart_block, scatter + lm/loess smoother)
#     -> model_block -> broom_block(augment, qq = TRUE)
#                       -> residuals vs fitted  (drilldown scatter, loess)
#                       -> QQ                   (drilldown scatter)
#
# Plot-side is all drilldown_chart_block; function_block is only used
# where there is no specific block (the simulators).
#
# Run from workspace root:
#   Rscript -e 'options(shiny.port=3838L, shiny.host="127.0.0.1");
#     source("blockr.stats/dev/regression-playground.R", echo=FALSE, print.eval=TRUE)'

options(blockr.dock_is_locked = FALSE)

pkgload::load_all("blockr.ui",    quiet = TRUE)
pkgload::load_all("blockr.core",  quiet = TRUE)
pkgload::load_all("blockr.react", quiet = TRUE)
pkgload::load_all("blockr.dock",  quiet = TRUE)
pkgload::load_all("blockr.viz",    quiet = TRUE)
pkgload::load_all("blockr.extra", quiet = TRUE)
pkgload::load_all("blockr.stats", quiet = TRUE)
pkgload::load_all("blockr.code",  quiet = TRUE)


# ----- simulators ------------------------------------------------------------
# Each fn ignores `data`. Defaults drive the auto-generated UI.

sim_lm <- "function(data,
                    n = 300L,
                    int_lausanne = 10, int_locarno = 20, int_zurich = 30,
                    slope = 1.5, sigma = 1) {
  site <- sample(c('Lausanne','Locarno','Zurich'), n, replace = TRUE)
  x    <- runif(n, 0, 20)
  mu   <- c(Lausanne = int_lausanne,
            Locarno  = int_locarno,
            Zurich   = int_zurich)[site] + slope * x
  data.frame(y = mu + rnorm(n, 0, sigma), x = x, site = factor(site))
}"

sim_lm_inter <- "function(data,
                          n = 300L,
                          int_young = 75, int_old = 85,
                          slope_young = -0.005, slope_old = 0.005,
                          sigma = 2) {
  age <- sample(c('Young','Old'), n, replace = TRUE)
  x   <- runif(n, 0, 1000)
  mu  <- c(Young = int_young, Old = int_old)[age] +
         c(Young = slope_young, Old = slope_old)[age] * x
  data.frame(y = mu + rnorm(n, 0, sigma), x = x, age = factor(age))
}"

sim_lm_quad <- "function(data,
                         n = 300L,
                         intercept = 10, slope_x = 5, slope_x2 = -0.07,
                         sigma = 5) {
  x  <- runif(n, 0, 100)
  mu <- intercept + slope_x * x + slope_x2 * x^2
  data.frame(y = mu + rnorm(n, 0, sigma), x = x)
}"

sim_glm_binom <- "function(data,
                           n = 300L,
                           intercept = 0.01, slope = 1.8) {
  x  <- runif(n, -3, 3)
  lp <- intercept + slope * x
  p  <- 1 / (1 + exp(-lp))
  data.frame(y = rbinom(n, 1, p), x = x)
}"

sim_glm_pois <- "function(data,
                          n = 300L,
                          intercept = 2, log_slope = 0.05) {
  x  <- runif(n, 0, 20)
  mu <- exp(intercept + log_slope * x)
  data.frame(y = rpois(n, mu), x = x)
}"

sim_violate <- "function(data,
                         n = 300L,
                         intercept = 10, slope = 1.5, sigma = 1,
                         error_dist = c('normal','lognormal','heteroscedastic')) {
  error_dist <- match.arg(error_dist)
  x  <- runif(n, 0, 20)
  mu <- intercept + slope * x
  eps <- switch(error_dist,
    normal          = rnorm(n, 0, sigma),
    lognormal       = rlnorm(n, 0, sigma) - exp(sigma^2 / 2),
    heteroscedastic = rnorm(n, 0, sigma * (1 + x / 10))
  )
  data.frame(y = mu + eps, x = x)
}"


# ----- board -----------------------------------------------------------------

board <- new_dock_board(
  blocks = c(
    # function_block requires a data input; sims ignore it.
    seed = new_dataset_block(dataset = "cars", package = "datasets"),

    # --- LM: 1 continuous + 1 factor ---------------------------------------
    s_lm  = new_function_block(fn = sim_lm),
    mp_lm = new_chart_block(
              chart_type = "scatter", x = "x", y = "y",
              color = "site", series = "site", smoother = "lm",
              block_name = "Marginal: y vs x by site"),
    m_lm  = new_model_block(model_type = "lm",
                            formula = parse_formula("y ~ x + site")),
    a_lm  = new_broom_block(output = "augment", qq = TRUE),
    rp_lm = new_chart_block(
              chart_type = "scatter", x = ".fitted", y = ".resid",
              smoother = "loess",
              block_name = "Residuals vs fitted"),
    qq_lm = new_chart_block(
              chart_type = "scatter",
              x = ".qq_theoretical", y = ".qq_sample",
              block_name = "Normal Q-Q"),

    # --- LM with interaction ------------------------------------------------
    s_int  = new_function_block(fn = sim_lm_inter),
    mp_int = new_chart_block(
              chart_type = "scatter", x = "x", y = "y",
              color = "age", series = "age", smoother = "lm",
              block_name = "Marginal: y vs x by age"),
    m_int  = new_model_block(model_type = "lm",
                             formula = parse_formula("y ~ x * age")),
    a_int  = new_broom_block(output = "augment", qq = TRUE),
    rp_int = new_chart_block(
              chart_type = "scatter", x = ".fitted", y = ".resid",
              smoother = "loess",
              block_name = "Residuals vs fitted"),
    qq_int = new_chart_block(
              chart_type = "scatter",
              x = ".qq_theoretical", y = ".qq_sample",
              block_name = "Normal Q-Q"),

    # --- LM with quadratic effect ------------------------------------------
    s_qd  = new_function_block(fn = sim_lm_quad),
    mp_qd = new_chart_block(
              chart_type = "scatter", x = "x", y = "y",
              smoother = "loess",
              block_name = "Marginal: y vs x (loess)"),
    m_qd  = new_model_block(model_type = "lm",
                            formula = parse_formula("y ~ x + I(x^2)")),
    a_qd  = new_broom_block(output = "augment", qq = TRUE),
    rp_qd = new_chart_block(
              chart_type = "scatter", x = ".fitted", y = ".resid",
              smoother = "loess",
              block_name = "Residuals vs fitted"),
    qq_qd = new_chart_block(
              chart_type = "scatter",
              x = ".qq_theoretical", y = ".qq_sample",
              block_name = "Normal Q-Q"),

    # --- GLM binomial ------------------------------------------------------
    s_bn  = new_function_block(fn = sim_glm_binom),
    mp_bn = new_chart_block(
              chart_type = "scatter", x = "x", y = "y",
              smoother = "loess",
              block_name = "Marginal: y vs x"),
    m_bn  = new_model_block(model_type = "logistic",
                            formula = parse_formula("y ~ x")),
    a_bn  = new_broom_block(output = "augment"),
    fp_bn = new_chart_block(
              chart_type = "scatter", x = "x", y = ".fitted",
              smoother = "loess",
              block_name = "Fitted probability vs x"),

    # --- GLM poisson -------------------------------------------------------
    s_ps  = new_function_block(fn = sim_glm_pois),
    mp_ps = new_chart_block(
              chart_type = "scatter", x = "x", y = "y",
              smoother = "loess",
              block_name = "Marginal: y vs x"),
    m_ps  = new_model_block(model_type = "poisson",
                            formula = parse_formula("y ~ x")),
    a_ps  = new_broom_block(output = "augment"),
    fp_ps = new_chart_block(
              chart_type = "scatter", x = "x", y = ".fitted",
              smoother = "loess",
              block_name = "Fitted mean vs x"),

    # --- Violating assumptions --------------------------------------------
    s_vi  = new_function_block(fn = sim_violate),
    mp_vi = new_chart_block(
              chart_type = "scatter", x = "x", y = "y",
              smoother = "lm",
              block_name = "Marginal: y vs x"),
    m_vi  = new_model_block(model_type = "lm",
                            formula = parse_formula("y ~ x")),
    a_vi  = new_broom_block(output = "augment", qq = TRUE),
    rp_vi = new_chart_block(
              chart_type = "scatter", x = ".fitted", y = ".resid",
              smoother = "loess",
              block_name = "Residuals vs fitted"),
    qq_vi = new_chart_block(
              chart_type = "scatter",
              x = ".qq_theoretical", y = ".qq_sample",
              block_name = "Normal Q-Q")
  ),
  links = links(
    from = c(
      # seed -> each simulator
      "seed", "seed", "seed", "seed", "seed", "seed",
      # LM: sim -> marginal; sim -> model -> augment -> resid + qq
      "s_lm",  "s_lm",  "m_lm",  "a_lm",  "a_lm",
      # Interaction
      "s_int", "s_int", "m_int", "a_int", "a_int",
      # Quadratic
      "s_qd",  "s_qd",  "m_qd",  "a_qd",  "a_qd",
      # Binomial: sim -> marginal; sim -> model -> augment -> fitted-vs-x
      "s_bn",  "s_bn",  "m_bn",  "a_bn",
      # Poisson
      "s_ps",  "s_ps",  "m_ps",  "a_ps",
      # Violations
      "s_vi",  "s_vi",  "m_vi",  "a_vi",  "a_vi"
    ),
    to = c(
      "s_lm",  "s_int", "s_qd",  "s_bn",  "s_ps",  "s_vi",
      "mp_lm", "m_lm",  "a_lm",  "rp_lm", "qq_lm",
      "mp_int","m_int", "a_int", "rp_int","qq_int",
      "mp_qd", "m_qd",  "a_qd",  "rp_qd", "qq_qd",
      "mp_bn", "m_bn",  "a_bn",  "fp_bn",
      "mp_ps", "m_ps",  "a_ps",  "fp_ps",
      "mp_vi", "m_vi",  "a_vi",  "rp_vi", "qq_vi"
    )
  ),
  extensions = list(blockr.react::new_react_extension()),
  layouts = list(
    # model card (forest + adj-R2 + summary toggle) sits next to the
    # simulator so the formula-input widget is visible alongside its inputs.
    LM          = dock_layout("s_lm",  "m_lm",  "mp_lm",  "rp_lm",  "qq_lm",  active = TRUE),
    Interaction = dock_layout("s_int", "m_int", "mp_int", "rp_int", "qq_int"),
    Quadratic   = dock_layout("s_qd",  "m_qd",  "mp_qd",  "rp_qd",  "qq_qd"),
    Binomial    = dock_layout("s_bn",  "m_bn",  "mp_bn",  "fp_bn"),
    Poisson     = dock_layout("s_ps",  "m_ps",  "mp_ps",  "fp_ps"),
    Violations  = dock_layout("s_vi",  "m_vi",  "mp_vi", "rp_vi", "qq_vi")
  )
)

serve(board, plugins = custom_plugins(generate_flat_code()))
