# Stats Views — Implementation

## Package scaffolding

### `blockr.stats/DESCRIPTION` updates

Add to `Imports:`:

```
parameters,
effectsize,
performance,        # already imported via check_model_block
report,
correlation,
nnet,
MASS,
lme4
```

(Existing imports — `shiny`, `blockr.core`, `broom`, `glue`, etc. — stay.)

Verify CRAN install of all four easystats packages plus nnet/MASS/lme4 succeeds in the dev environment before starting block work.

### File layout

All new R files live in `blockr.stats/R/`. One file per block, following the existing convention:

```
blockr.stats/
├── R/
│   ├── model-block.R                 # EXTEND existing
│   ├── parameters-block.R            # NEW
│   ├── performance-block.R           # NEW
│   ├── effectsize-block.R            # NEW
│   ├── standardize-block.R           # NEW
│   ├── report-block.R                # NEW
│   ├── descriptives-block.R          # NEW
│   ├── correlation-block.R           # NEW
│   ├── frequencies-block.R           # NEW
│   ├── ttest-block.R                 # NEW
│   ├── cohens-d-block.R              # NEW
│   ├── normality-check-block.R       # NEW
│   ├── homogeneity-check-block.R     # NEW
│   └── nonparametric-block.R         # NEW
├── tests/testthat/
│   └── test-<each-new-block>.R       # one test file per new block
└── dev/
    ├── exploration.R                 # NEW — view script
    ├── modeling.R                    # NEW — view script
    ├── ttests.R                      # NEW — view script
    └── all-views.R                   # NEW — multi-view board (the demo)
```

## Reference patterns to copy

Every new block follows one of two reference patterns already in `blockr.stats`:

| Pattern | Reference | New blocks using it |
|---|---|---|
| **Adaptive block with type picker** | `blockr.stats/R/model-block.R:24-` (existing) | `model_block` (extend), `ttest_block` (new) |
| **Output block: model-in, tibble-out, custom HTML render** | `blockr.stats/R/model-summary-block.R:22-434` | All other new blocks |
| **Output block: model-in, plot-out** | `blockr.stats/R/check-model-block.R:28-74` | (none new — reusing existing) |

Authoring conventions per `blockr.docs/patterns/r-driven-blocks.md` (especially state-name-matches-constructor-arg, no `req()` use, forward `...` to parent).

## Block-by-block sketches

### Extend `model-block.R`

Add to the `model_choices` vector:

```r
model_choices <- c(
  "Linear (lm)" = "lm",
  "Logistic (glm)" = "logistic",
  "Poisson (glm)" = "poisson",
  "Gamma (glm)" = "gamma",
  "Multinomial" = "multinom",
  "Ordinal" = "polr",
  "Mixed Linear" = "lmer",
  "Mixed Logistic" = "glmer"
)
```

Add a fifth role-picker reactive `r_random_effects` (multi-select categorical columns), shown only when `r_model_type() %in% c("lmer", "glmer")` — `shinyjs::toggle` based on type.

Extend the expression builder switch (around `model-block.R` lines that build `expr_text` for each `model_type`):

```r
"multinom" = glue::glue("nnet::multinom({formula_str}, data = data, trace = FALSE)"),
"polr"     = glue::glue("MASS::polr({formula_str}, data = data, Hess = TRUE)"),
"lmer"     = {
  re <- paste0("(1 | ", r_random_effects(), ")", collapse = " + ")
  glue::glue("lme4::lmer({formula_str} + {re}, data = data)")
},
"glmer"    = {
  re <- paste0("(1 | ", r_random_effects(), ")", collapse = " + ")
  glue::glue("lme4::glmer({formula_str} + {re}, data = data, family = binomial)")
}
```

Random slopes (`(1 + slope | group)`) deferred — random intercepts only for prototype.

Update `model_choices` UI input (radioGroupButtons or similar) to include the new types.

### `parameters-block.R` — new

```r
new_parameters_block <- function(
  ci = 0.95,
  exponentiate = FALSE,
  ...
) {
  new_transform_block(
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        r_ci <- reactiveVal(ci)
        r_exp <- reactiveVal(exponentiate)
        observeEvent(input$ci, r_ci(input$ci))
        observeEvent(input$exponentiate, r_exp(input$exponentiate))

        list(
          expr = reactive({
            bquote({
              .res <- parameters::model_parameters(
                data, ci = .(r_ci()), exponentiate = .(r_exp())
              )
              tibble::as_tibble(.res)
            })
          }),
          state = list(ci = r_ci, exponentiate = r_exp)
        )
      })
    },
    ui = function(id) {
      tagList(
        sliderInput(NS(id, "ci"), "Confidence level", min = 0.8, max = 0.99,
          value = ci, step = 0.01),
        checkboxInput(NS(id, "exponentiate"), "Exponentiate (for GLM)",
          value = exponentiate)
      )
    },
    class = "parameters_block",
    ...
  )
}

# Custom render for publication-style table
#' @export
block_output.parameters_block <- function(x, result, session) {
  renderUI({
    if (is.null(result)) return(htmltools::tags$em("No model"))
    htmltools::HTML(knitr::kable(result, format = "html",
      table.attr = 'class="parameters-block-table"'))
  })
}

#' @export
block_ui.parameters_block <- function(id, x, ...) {
  tagList(uiOutput(NS(id, "result")))
}
```

Same shape for `performance-block`, `effectsize-block`, `standardize-block`. Each wraps one easystats function, exposes 1-3 options (CI, method, type), emits the resulting tibble, custom-renders publication-style. Sketches not repeated — all follow this template.

### `report-block.R` — new

```r
new_report_block <- function(...) {
  new_transform_block(
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        list(
          expr = reactive({
            bquote({
              .txt <- as.character(report::report(data))
              tibble::tibble(text = .txt)
            })
          }),
          state = list()
        )
      })
    },
    ui = function(id) tagList(),
    class = "report_block",
    ...
  )
}

#' @export
block_output.report_block <- function(x, result, session) {
  renderUI({
    if (is.null(result) || nrow(result) == 0) return(htmltools::tags$em("No report"))
    htmltools::tags$div(
      class = "report-narrative",
      lapply(result$text, function(p) htmltools::tags$p(p))
    )
  })
}

#' @export
block_ui.report_block <- function(id, x, ...) tagList(uiOutput(NS(id, "result")))
```

### `ttest-block.R` — new (adaptive)

Same shape as `model_block.R` but with t-test types:

```r
new_ttest_block <- function(
  test_type = "independent",   # "one_sample", "paired", "independent"
  variant = "welch",            # "student", "welch" — independent only
  dv = character(),
  group = character(),          # independent only
  pair = character(),           # paired only
  test_value = 0,               # one_sample only
  ...
) { ... }
```

Type picker via `shinyWidgets::radioGroupButtons` with icons. Role pickers shown/hidden via `shinyjs::toggle`. Expression builder:

```r
"one_sample"  = glue::glue("stats::t.test(data${dv}, mu = {test_value})"),
"paired"      = glue::glue("stats::t.test(data${dv}, data${pair}, paired = TRUE)"),
"independent" = glue::glue(
  "stats::t.test(data${dv} ~ data${group}, var.equal = {variant == 'student'})"
)
```

Output: `htest` object. Downstream `parameters_block`, `cohens_d_block`, `report_block` all consume it (they polymorph via easystats S3).

### `descriptives-block.R`, `correlation-block.R`, `frequencies-block.R` — new

Standard data-in / tibble-out output blocks consuming a `data.frame`. Each wraps one easystats function:

- `descriptives`: `parameters::describe_distribution(data, select = r_vars())`
- `correlation`: `correlation::correlation(data, select = r_vars(), method = r_method())`
- `frequencies`: hand-rolled — `lapply` over selected categorical columns, build `table()` results into a long tibble.

UI exposes a column picker (multi-select, type-filtered) and 1-2 options (method for correlation, sort for frequencies). Custom render to publication-style HTML table.

### `cohens-d-block.R`, `normality-check-block.R`, `homogeneity-check-block.R` — new

- `cohens_d_block`: consumes htest or data + grouping; calls `effectsize::cohens_d(...)`. Exposes pooled / Hedges correction toggles.
- `normality_check_block`: consumes data; calls `performance::check_normality()`. Outputs the test result (Shapiro-Wilk W and p, plus interpretation flag). Renders a small badge: "OK" (green) if p > 0.05, "Concern" (yellow) if 0.01-0.05, "Reject" (red) if p < 0.01 — pattern from `blockr.stats/R/model-summary-block.R:285-291`.
- `homogeneity_check_block`: consumes data + grouping; calls `performance::check_homogeneity()`. Same badge pattern.
- `nonparametric_block`: adaptive type picker mirroring `ttest_block` (One-sample / Paired / Independent). Same role pickers; expression dispatches to `stats::wilcox.test(...)` with the paired / mu / formula form per type. Output: the `htest` object. Downstream consumers (`parameters_block`, `report_block`) work without changes since `wilcox.test` returns the same `htest` class as `t.test`.

## Edge cases (per block where relevant)

- **`model_block` mixed types**: when `r_random_effects()` is empty for `lmer`/`glmer`, return identity expression — don't try to fit. UI hint: "Random effects required for mixed models."
- **`parameters_block`** on a multinomial/ordinal model: the result has multiple coefficient sets (one per outcome category for multinom, one per threshold for polr). Verify `parameters::model_parameters()` handles this; render the resulting tibble as-is — no custom reshaping.
- **`report_block`** on a model with too many predictors: the narrative gets long. Cap at first 5 effects in the rendered output? Or render full and let user scroll? Default to scroll, decide if a partner complaints.
- **`correlation_block`** with only one variable selected: `correlation::correlation()` requires ≥2 variables. Return a placeholder result with a hint; don't crash.
- **`ttest_block`** with `independent` type when grouping variable has !=2 levels: `t.test()` errors. Wrap in `tryCatch`, render a clear message.
- **`normality_check_block`** with n > 5000: Shapiro-Wilk fails by definition. Switch to Kolmogorov-Smirnov fallback — `performance::check_normality()` handles this but verify behavior.

## Tests

Each new block gets `tests/testthat/test-<name>-block.R` covering:

1. **Constructor returns a block object** (`expect_s3_class`).
2. **Pure-R helper functions** (expression builders, e.g., the `t.test()` formula construction): test directly without Shiny.
3. **`testServer()` for the block_server**: verify reactive state flows through to the right expression and that evaluating the expression against a fixed data frame produces the expected output shape (column names, nrow). Pattern from `blockr.docs/patterns/r-driven-blocks.md` Tier 2.

No `shinytest2` — all R-driven, all coverable with `testServer()`.

Reference test pattern: `blockr.stats/tests/testthat/test-lm-block.R` (existing).

## Demo scripts (`blockr.stats/dev/`)

### Exploration script (`exploration.R`)

```r
pkgload::load_all("blockr.core"); pkgload::load_all("blockr.stats")
pkgload::load_all("blockr.dock"); pkgload::load_all("blockr.dag")

board <- new_dock_board(
  blocks = c(
    data = new_static_block(palmerpenguins::penguins),
    desc = new_descriptives_block(),
    correl = new_correlation_block(),
    freq = new_frequencies_block(),
    explore_report = new_report_block()
  ),
  links = links(
    new_link(from = "data", to = "desc"),
    new_link(from = "data", to = "correl"),
    new_link(from = "data", to = "freq"),
    new_link(from = "desc", to = "explore_report")
  ),
  layout = dock_layouts(
    Exploration = dock_view(
      "data", "desc", "freq", "correl", "explore_report",
      active = TRUE
    )
  ),
  extensions = list(blockr.dag::new_dag_extension())
)
options(shiny.port = 3838L, shiny.host = "0.0.0.0")
serve(board)
```

### Modeling script (`modeling.R`) and t-tests script (`ttests.R`)

Same shape, different blocks per the design's per-view block lists. See `3-design.md` for the full block IDs and links.

### Combined script (`all-views.R`)

The full multi-view board from `3-design.md`. The artifact partners actually click through.

## Verification

In order:

1. Install updated `blockr.stats` from local source. Verify all new deps resolve.
2. `Rscript -e 'devtools::document()'` — clean.
3. `Rscript -e 'devtools::test()'` — all tests pass.
4. `cd /tmp && Rscript -e 'devtools::check("/workspace/blockr.stats", quiet = TRUE, error_on = "never")'` — 0 errors, 0 warnings.
5. Run each demo script on the host on port 3838. Open in browser, click through:
   - **Rung 1 (View user)**: variables can be picked, output panels populate.
   - Switching the model-type picker updates panels coherently (no stale/broken state).
   - The auto-narrative paragraph is the visible wow on first load.
   - The diagnostic plots and assumption checks render.
   - Switching views (Exploration / Modeling / T-Tests) shows the right blocks per view.
   - **Rung 2 (Composer)**: in the modeling view, drag a `blockr.dplyr::new_filter_block()` from the right-hand drawer between `data` and `model`. Filter rows on a categorical column. Confirm downstream blocks update with the filtered subset.
   - **Rung 3 (Power user)**: drop a `blockr.extra::new_function_block()` into a view, write `kruskal.test(data$Sepal.Length, data$Species)` (or analogous) inside it, confirm the result renders and the generated R code is the canonical R the user wrote.
6. Compare output side-by-side with jamovi for the same dataset + same analysis. Note polish gaps; address before declaring done.

## Implementation order (suggested)

1. DESCRIPTION updates + dep installation.
2. **Modeling view first**: extend `model_block`, build `parameters`, `performance`, `effectsize`, `standardize`, `report` blocks, write `dev/modeling.R`, ship end-to-end. Use it as the polish template.
3. **Exploration view**: build `descriptives`, `correlation`, `frequencies` blocks. Apply the polish pattern from modeling.
4. **T-tests view**: build `ttest_block`, `cohens_d`, `normality_check`, `homogeneity_check` blocks. Apply the polish pattern.
5. Combined `all-views.R`. Final partner-facing artifact.
6. Tests, check, devtools, partner demo.

The modeling view is built first deliberately — it's the polish target, and getting one view to jamovi-quality teaches the team what "jamovi-quality" actually requires. The other two views follow the same pattern.
