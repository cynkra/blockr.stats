# Stats Views — Requirements

## Three views ship in the prototype

All three views ship at the same polish bar — jamovi-quality, end-to-end. The content differs per view; the quality target does not.

| View | Content |
|---|---|
| **Modeling** | Single adaptive model block (lm-family from linear through mixed-effects) + the panels listed below. ANOVA folded in via factor predictors + F-table emphasis. |
| **Data exploration** | Descriptives, frequencies, correlation matrix, optional plots. Auto-narrative for descriptives and correlation. |
| **T-tests** | One-sample / paired / independent, with per-group descriptives, Cohen's d, assumption checks (normality, homogeneity), nonparametric alternative, auto-narrative. |

## Modeling view: panels

The modeling view is the "jamovi quality" deliverable. It ships with these panels, each backed by a block:

- **Variable roles** — DV (single numeric), covariates (numeric, multi), factors (categorical, multi), random effects (for mixed-effects models). Same role pickers regardless of model type.
- **Model type picker** — Linear, Logistic, Multinomial, Ordinal, Mixed Linear, Mixed Logistic. Selecting a type updates which roles are required and which output panels are relevant.
- **Model fit headline** — R² / F / p for lm, pseudo-R² + LR test for GLM, ICC + marginal/conditional R² for mixed models. Source: `performance::model_performance()`.
- **Coefficients table** — Source: `parameters::model_parameters()`. Replaces the default `broom::tidy()` formatting because parameters renders categorical level breakouts, proper variable labels, and scientific notation appropriately.
- **ANOVA F-table** — Source: `stats::anova()` via the existing `anova_block`, polymorphic over the model type.
- **Effect sizes** — η² / partial η² / ω² via `effectsize::eta_squared()`; standardized β via `effectsize::standardize_parameters()`.
- **Diagnostic plots** — Existing `check_model_block` in blockr.stats, which uses `performance::check_model()`.
- **Auto-narrative** — APA-style paragraph via `report::report()`. The unique wedge over jamovi.

Each panel is an output block consuming the model object. They polymorph naturally because easystats and broom methods exist for `lm`, `glm`, `lmer`, `glmer`, `polr`, `multinom`.

## Data exploration view: panels

- **Variable picker** — multi-select numeric and/or categorical columns.
- **Descriptives table** — n, missing, mean, median, SD, min, max, skewness, kurtosis, percentiles. Source: `parameters::describe_distribution()` or equivalent easystats helper.
- **Frequencies table** — counts and proportions for categorical columns. Source: hand-rolled or `parameters::report_table` over `table()`.
- **Correlation matrix** — Pearson / Spearman / Kendall, with significance flagging and CIs. Source: `correlation::correlation()` from easystats.
- **Distribution plots** (optional toggle) — histogram, density, or boxplot per selected variable.
- **Auto-narrative** — `report::report()` for descriptives and the correlation result.

## T-tests view: panels

- **Test type picker** — One-sample / Paired / Independent. The block adapts; the role pickers update accordingly.
- **Variable roles** — DV (numeric, single), grouping variable (categorical with two levels, for independent); pair (for paired); test value (for one-sample).
- **Variant picker** (independent only) — Student / Welch.
- **Descriptives per group** — n, mean, SD, SE per group. Source: `parameters::describe_distribution()` or analogous.
- **T-test result table** — t, df, p, mean difference, CI. Source: `parameters::model_parameters(t.test(...))`.
- **Effect size** — Cohen's d with CI via `effectsize::cohens_d()`.
- **Assumption checks** — Shapiro-Wilk for normality (per group); Levene's for homogeneity of variance (independent only). Source: `performance::check_normality()` and `performance::check_homogeneity()`.
- **Nonparametric alternative** — Mann-Whitney U (independent) or Wilcoxon signed-rank (paired). Toggleable.
- **Auto-narrative** — `report::report(t.test(...))`.

## Output substrate

The easystats family (`parameters`, `performance`, `effectsize`, `report`, plus the already-used `see` / `modelsummary` / `performance` deps) is the substrate. `broom` covers what easystats doesn't. **No `jmv` dependency anywhere.**

Generated R code under any block must be canonical R that runs unchanged outside the workspace — `lm()`, `dplyr::summarise()`, `parameters::model_parameters()`, `report::report()`. Not `jmv::*` calls.

## How blocks compose

- The model block emits the model object (`lm`, `glm`, `lmer`, `glmer`, `polr`, or `multinom` depending on type picked).
- Each output block consumes the model object via the relevant easystats / broom method and emits a tibble (or, for plots, renders to a panel).
- Tibble outputs flow downstream — a student can pipe the coefficient table into a filter or join, save it, render it as a custom gt table, or feed it into another analysis.
- The view's role: arrange these blocks via `dock_layouts()` so they read as one coordinated cell rather than five disconnected panels.

## The view as deliverable

A view = an R script that constructs a `dock_board` with multiple tabbed views via `dock_layouts()`, in the same style as `blockr.sandbox/dev/cedx-poc.R`. The prototype ships these scripts; format details (saved JSON, preset function, etc.) are deferred to the design phase.

The script doesn't need to be parameterized over data or variable choices for the prototype. The student loads the dataset via the dataset block in the view, picks columns in the role pickers, and the workflow runs. The script is the workflow definition; the data and variable selections are runtime state.

## Graduation ladder rungs

All four rungs in the motivation must work in the prototype:

1. **View user** — opens a view, picks variables, sees output. No code, no wiring. The default and primary path.
2. **Composer** — drags additional blocks (filter, mutate, plot from the broader blockr ecosystem) into a view. Still no code.
3. **Power user** — drops a `function_block` (from `blockr.extra`) into a view, writes a few lines of R inside it. Real R, no new block to author.
4. **Block developer** — writes a new blockr block. Out of scope for the prototype but the architecture supports it (the existing block authoring docs in `blockr.docs/patterns/r-driven-blocks.md` apply unchanged).

Rungs 1-3 must be exercisable in the demo. Rung 4 is implicit in the architecture.

## Where the work lives

- **`blockr.stats`** is the home for all the new blocks. Existing 9 blocks stay; the model block is extended to cover multinomial / ordinal / mixed-effects on top of the current lm/glm coverage. New blocks added:
    - For modeling: `new_parameters_block`, `new_effectsize_block`, `new_standardize_block`, `new_performance_block`, `new_report_block`.
    - For data exploration: `new_descriptives_block`, `new_correlation_block`, `new_frequencies_block`.
    - For t-tests: `new_ttest_block`, `new_cohens_d_block`, `new_normality_check_block`, `new_homogeneity_check_block`, `new_nonparametric_block`.

    The nonparametric alternative (Mann-Whitney / Wilcoxon signed-rank) ships as a separate adaptive block (`new_nonparametric_block`) rather than as a toggle inside `new_ttest_block`, mirroring the t-test block's type picker. This keeps each block atomic and one-purpose.

    Imports gain `parameters`, `effectsize`, `report`, `correlation` (the easystats package), plus `nnet`, `MASS`, `lme4` for the new model families.
- **`blockr.extra`** continues to host the function block — no change, just used in the prototype.
- **View scripts** live in `blockr.stats/dev/` (or wherever the lm-related demos already live). One script per view. Convention follows `blockr.sandbox/dev/cedx-poc.R`.
- **No new package** for the prototype. If the catalog grows beyond what fits in `blockr.stats` + `blockr.extra`, splitting comes later.

## Polish bar — operational definition

The bar applies uniformly to all three views: jamovi-quality, end-to-end. Concretely:

- A user opens any of the three views, picks variables (and a model type / test type where applicable), and sees all the panels populated with publication-ready formatting within a few seconds.
- The output is at least as readable as jamovi's equivalent — proper variable labels, formatted CIs, sensible significant-digit handling, no raw column names like `Sepal.Length[mean]` leaking through.
- The auto-narrative (`report::report`) is the visible wow in every view that has one.
- Where the view has a type picker (model type, t-test type), switching it updates the panels coherently — irrelevant panels hide, type-specific panels appear, no broken or stale state.
- Assumption checks where applicable (diagnostic plots in modeling, normality + homogeneity in t-tests) actually flag violations — not just rendered for show.
- Generated R code under any block is the R a working data scientist would write.

## Permanent non-goals

These are architectural decisions; we won't revisit them without a new spec:

- Wrapping `jmv` (or any analogous "thin shim around a black-box stats engine"). Closed by prior experiment.
- Replicating SPSS feature-for-feature. Decades of accumulation; we won't chase the menu.
- A custom Shiny shell that hides blocks (the "γ" option from the abandoned spec). The block layer stays visible; that's the point of the graduation ladder.
- Hiding generated R code from the user. The code IS a wedge.

## Deferred non-goals

Out of scope for the prototype, plausible later if the bet validates:

- Bayesian variants of the covered analyses
- Factor analysis / PCA / EFA / CFA
- MANOVA / multivariate methods
- Survival analysis
- Time series / forecasting
- SEM
- Power analysis tools

The catch-up trajectory targets jamovi-class breadth on the analyses it makes sense to cover, not SPSS-class breadth. If any of these become a partner-driven priority, they're added to a future spec.

## Success criteria

The prototype is "done enough" when:

1. The three views (modeling, data exploration, t-tests) all open, run, and produce output.
2. Each of the three views hits the polish bar above.
3. The graduation ladder is exercisable: a partner can be shown rungs 1-3 in the same session.
4. The partner's reaction tells us whether to invest further. The prototype is the conversation tool, not the finished product.
