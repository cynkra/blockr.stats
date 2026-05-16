# Stats Views — Design

## Substrate decision: R-driven blocks

All blocks in this prototype are R-driven (per `blockr.docs/patterns/r-driven-blocks.md`). Rationale:

- The blocks we need are not multi-row builders or autocomplete editors. They are "configure inputs, render result" blocks where the JS-driven payoff is small.
- The team is new to blockr; R-driven is the documented on-ramp and uses only `testServer()` for testing.
- All existing `blockr.stats` blocks are R-driven — staying R keeps stylistic consistency with the package we're extending.
- Visual polish (publication-style tables, narrative paragraphs, headline panels) goes through `renderUI` + hand-rolled HTML/CSS via an `htmlDependency`, the same pattern `blockr.stats/R/model-summary-block.R:421-434` already uses.
- "Rewrite to JS later" is explicitly supported by the docs. If a partner reaction names a specific UX gap (e.g., "I want to drag predictors between covariates and factors"), upgrade that one block.

## Block taxonomy

Two adaptive blocks (with internal type pickers) and a small set of one-purpose output blocks. The model/test object flows; the output blocks polymorph over it via easystats and broom S3 dispatch.

### Adaptive block 1 — `new_model_block()`

Replaces / extends the existing `blockr.stats/R/model-block.R` to cover the full lm-family.

- **Type picker** (`shinyWidgets::radioGroupButtons` with icons, the `blockr.ggplot` pattern): Linear / Logistic / Multinomial / Ordinal / Mixed Linear / Mixed Logistic.
- **Variable roles** (selectizeInput, multi where applicable):
    - DV (single numeric — or factor for Logistic/Multinomial/Ordinal)
    - Covariates (numeric, multi)
    - Factors (categorical, multi)
    - Random-effects grouping (categorical, multi) — only visible when type is Mixed Linear / Mixed Logistic
- **Intercept toggle** (checkbox).
- **Expression building**: a thin internal switch by type calls the right fitting function. Linear → `stats::lm`; Logistic → `stats::glm(family = binomial)`; Multinomial → `nnet::multinom`; Ordinal → `MASS::polr`; Mixed Linear → `lme4::lmer` with formula `(1 | group)` from the random-effects picker; Mixed Logistic → `lme4::glmer(family = binomial)` with the same formula construction.
- **Output**: the fitted model object (`lm` / `glm` / `multinom` / `polr` / `lmerMod` / `glmerMod`).

### Adaptive block 2 — `new_ttest_block()`

- **Type picker**: One-sample / Paired / Independent.
- **Variant picker** (Independent only): Student / Welch.
- **Variable roles**:
    - DV (numeric, single)
    - Grouping variable (categorical with two levels, single — Independent only)
    - Pair (numeric, single — Paired only)
    - Test value (numeric input — One-sample only)
- **Output**: the `htest` object from `stats::t.test()`.

### Output blocks (one per easystats function)

Each is small (~50-100 lines), R-driven, with a `block_output` method rendering the result via `renderUI` + hand-rolled HTML and a styled `<pre>` or `<table>` element.

| Block | Wraps | Consumes | Emits (downstream) | Notes |
|---|---|---|---|---|
| `new_parameters_block` | `parameters::model_parameters` | model object | tibble of coefficients | Replaces default `broom::tidy` formatting; renders categorical level breakouts |
| `new_performance_block` | `performance::model_performance` | model object | one-row tibble (R²/F/AIC/...) | Polymorphic — pseudo-R² for GLM, ICC for mixed |
| `new_effectsize_block` | `effectsize::eta_squared` | model object (or aov) | tibble of η² / partial η² / ω² | |
| `new_standardize_block` | `effectsize::standardize_parameters` | model object | tibble of β_std with CI | |
| `new_report_block` | `report::report` | model/htest object | tibble with single `text` column | Block_output renders the formatted paragraph; tibble enables downstream composition |
| `new_descriptives_block` | `parameters::describe_distribution` | data.frame | tibble per variable | For data exploration view |
| `new_correlation_block` | `correlation::correlation` | data.frame | long-format tibble | For data exploration view |
| `new_frequencies_block` | `parameters::report_table` over `table()` (or hand-rolled) | data.frame | tibble of counts/proportions | For data exploration view |
| `new_cohens_d_block` | `effectsize::cohens_d` | htest or data | tibble with d + CI | For t-tests view |
| `new_normality_check_block` | `performance::check_normality` | model/data | tibble or rendered result | For t-tests view |
| `new_homogeneity_check_block` | `performance::check_homogeneity` | model/data | tibble or rendered result | For t-tests view (Independent only) |
| `new_nonparametric_block` | `stats::wilcox.test` | data.frame (consumed like `new_ttest_block`) | `htest` object | Adaptive type picker mirroring `new_ttest_block`: One-sample / Paired (Wilcoxon signed-rank) / Independent (Mann-Whitney U). Uses the same role pickers and shows in the t-tests view. |

Existing blocks reused unchanged: `new_anova_block`, `new_check_model_block`, `new_coefplot_block` (already in `blockr.stats`).

### Panels in requirements vs blocks in design

The Phase 2 requirements describe panels at the user level (e.g. "Effect sizes" as one panel). The design splits some of these into multiple atomic blocks composed into a single panel position in the layout. Specifically: the "Effect sizes" panel is composed of `new_effectsize_block` (η² / partial η² / ω²) + `new_standardize_block` (β_std). They render side-by-side or stacked within the same layout slot. The requirement is met by composition; the atomic block boundary is a design choice that keeps each block one-purpose and reusable.

### Why one block per easystats function (not consolidated)

- Each is small, atomic, testable in isolation.
- Composability: a power user wiring blocks gets a clean menu of named outputs, each with a clear contract.
- Polymorphism: each easystats function already does the right thing across model types — no need to combine inside our blocks.
- Keeps the blocks at rung-2 / rung-3 of the graduation ladder reachable: a power user can drop just `new_parameters_block` into any view consuming any model, without inheriting unrelated effect-size or report machinery.

## How blocks connect

```
data ──→ model_block ──┬──→ parameters
                       ├──→ anova
                       ├──→ effectsize
                       ├──→ standardize
                       ├──→ performance
                       ├──→ check_model
                       ├──→ coefplot
                       └──→ report

data ──→ ttest_block ──┬──→ parameters (handles htest)
                       ├──→ cohens_d
                       ├──→ normality_check
                       ├──→ homogeneity_check
                       └──→ report

data ──→ descriptives
data ──→ correlation
data ──→ frequencies
data ──→ scatter_plot (via blockr.ggplot or similar)
```

The model object class determines what the output blocks do via S3 dispatch; the wiring is identical across model types.

## View mechanism

A view = a single board with multiple top-level **views** declared via `blockr.dock::dock_layouts(...)`, each defined with `dock_view(...)` (the cedx-poc pattern). One named view per top-level tab.

### View declaration

Following the cedx-poc style:

```r
new_dock_board(
  blocks = c(
    data = new_static_block(iris),
    # exploration
    desc = new_descriptives_block(),
    correl = new_correlation_block(),
    freq = new_frequencies_block(),
    explore_report = new_report_block(),
    # modeling
    model = new_model_block(),
    params = new_parameters_block(),
    anova = new_anova_block(),
    effects = new_effectsize_block(),
    stdb = new_standardize_block(),
    perf = new_performance_block(),
    check = new_check_model_block(),
    coefplot = new_coefplot_block(),
    report = new_report_block(),
    # t-tests
    ttest = new_ttest_block(),
    ttest_params = new_parameters_block(),
    cohen = new_cohens_d_block(),
    norm = new_normality_check_block(),
    homog = new_homogeneity_check_block(),
    nonpar = new_nonparametric_block(),
    ttest_report = new_report_block()
  ),
  links = links(
    new_link(from = "data", to = "desc"),
    new_link(from = "data", to = "correl"),
    new_link(from = "data", to = "freq"),
    new_link(from = "desc", to = "explore_report"),
    new_link(from = "data", to = "model"),
    new_link(from = "data", to = "ttest"),
    new_link(from = "data", to = "nonpar"),
    new_link(from = "model", to = "params"),
    new_link(from = "model", to = "anova"),
    new_link(from = "model", to = "effects"),
    new_link(from = "model", to = "stdb"),
    new_link(from = "model", to = "perf"),
    new_link(from = "model", to = "check"),
    new_link(from = "model", to = "coefplot"),
    new_link(from = "model", to = "report"),
    new_link(from = "ttest", to = "ttest_params"),
    new_link(from = "ttest", to = "cohen"),
    new_link(from = "data", to = "norm"),
    new_link(from = "data", to = "homog"),
    new_link(from = "ttest", to = "ttest_report")
  ),
  layout = dock_layouts(
    Exploration = dock_view("data", "desc", "freq", "correl", "explore_report"),
    Modeling = dock_view(
      "data", "model", "report", "params", "anova",
      "effects", "stdb", "perf", "check", "coefplot",
      active = TRUE
    ),
    `T-Tests` = dock_view(
      "data", "ttest", "ttest_report", "ttest_params",
      "cohen", "norm", "homog", "nonpar"
    )
  )
)
```

### Layout grid

Initial layout grid is **not** specified in code. `dock_view(...)` flat-lists block IDs; the dock UI arranges panels by default and the user can drag-to-rearrange at runtime. We trade authoring effort for runtime flexibility. If a specific view's default arrangement turns out to need pinning, we switch that view to nested-list grid syntax later — the API supports both.

### Shared blocks across views

The `data` block appears in all three views. In `blockr.dock`, the same block ID can be listed in multiple `dock_view(...)` calls; the panel renders in each view that lists it, but the block's state is shared. Same dataset feeds every analysis.

### Default active view

`active = TRUE` on the Modeling view. That's the polished one we want partners to land on first.

## Auto-narrative rendering

`new_report_block` consumes a model or htest object, calls `report::report()`, and:

- **block_output**: renders the formatted paragraph in a styled `<div>` via `renderUI`, with a copy-to-clipboard button. CSS lives alongside the block, similar to `model-summary-block`'s pattern.
- **expr / result**: emits a tibble with one column `text`, one row per paragraph the report produces. Downstream blocks can `dplyr::summarise(paste(text, collapse = "\n\n"))` to concatenate narratives, or render as a custom gt / markdown.

The rendered narrative is the visible wow on first inspection.

## Where the work lives

- All new blocks: in `blockr.stats/R/`. Files follow the existing convention (`<name>-block.R`).
- `blockr.stats/DESCRIPTION` Imports gain: `parameters`, `effectsize`, `report`, `correlation`, `nnet`, `MASS`, `lme4`. Existing imports stay.
- View scripts: `blockr.stats/dev/exploration.R`, `blockr.stats/dev/modeling.R`, `blockr.stats/dev/ttests.R`, plus a single `blockr.stats/dev/all-views.R` that constructs the multi-view board with all three.
- No new package. The function block (`blockr.extra::new_function_block`) is referenced but not modified.

## What's deliberately not specified at design level

- Exact column names emitted by each output block — easystats / broom decide; we use what they emit.
- Exact widget choices for every input (selectInput vs selectizeInput vs radioGroupButtons) — case-by-case in implementation.
- Per-block CSS — small files per block, following `model-summary-block`'s precedent.
- Default datasets in the demo views — implementation decides; iris / penguins / mtcars are candidates.

## Risks and mitigations

- **`report::report()` is opinionated** — its output may not be exactly the format some users want. Mitigation: the rendered text is verbatim from `report`; if the format is wrong, complain upstream rather than monkeypatch.
- **Mixed-effects formula construction is fragile** — `(1 | group)` syntax and multi-grouping are easy to get wrong. Mitigation: keep prototype to random intercepts only; document random slopes as deferred.
- **Reactive update churn on type-picker switch** — switching model type may briefly produce stale output panels. Mitigation: gate the model object reactive on a `req(input$type)` check; the existing model_block's pattern handles this.
- **Polish bar on three views is real work** — bigger than the previous "one polished + two rough" framing. Mitigation: ship the modeling view first end-to-end, use it as the template, replicate the polish pattern across exploration and t-tests.
