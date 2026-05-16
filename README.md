# blockr.stats

Statistical analysis blocks for [blockr](https://github.com/blockr-org/blockr): descriptives, frequencies, correlation, an adaptive test family, linear and generalized linear models, effect sizes, and survival analysis. Every block emits tidy data frames and stays canonical R underneath, aimed at no-code statistical workflows.

## Installation

```r
pak::pak("blockr-org/blockr.stats")
```

Runtime dependencies are all on CRAN: `broom`, `survival`, `cmprsk`, `effsize`, `moments`, `nortest`, `dplyr`, `glue`, and base `stats`. Model output is tidied through `broom` rather than the easystats family.

## Block catalog

| Block | Wraps | Notes |
|---|---|---|
| `new_model_block()` | `stats::lm`, `stats::glm`, `stats::aov` | Adaptive: pick `model_type` (`lm`, `glm` logistic/poisson, `aov`); role pickers adapt. |
| `new_lm_block()` | `stats::lm` | Linear-only convenience constructor. |
| `new_broom_block()` | `broom::tidy`, `broom::glance`, `broom::augment` | Generic adapter: turns any fitted model into a tidy / glance / augment frame. |
| `new_descriptives_block()` | base `stats` + `moments` | Per-variable mean / SD / quantiles / skew / kurtosis. |
| `new_frequencies_block()` | `table()` | Counts and proportions for categorical columns, optional `by`. |
| `new_correlation_matrix_block()` | `stats::cor` | Pairwise correlation matrix (Pearson / Spearman / Kendall). |
| `new_stat_test_block()` | `stats` test family | Adaptive `type`: one-sample / paired / two-sample t, Wilcoxon, one-way ANOVA, Kruskal-Wallis, chi-square independence, correlation, normality, homogeneity. |
| `new_padjust_block()` | `stats::p.adjust` | Multiple-comparison p-value adjustment. |
| `new_effect_size_block()` | `effsize`, `stats::aov` | Partial eta² / omega² and related effect sizes. |
| `new_survival_block()` | `survival`, `cmprsk` | Kaplan-Meier (`km`), Cox (`cox`), cumulative incidence (`cif`). |

## Demo

A hand-composed `dock_board` script lives in `dev/`:

- `dev/stats-101.R` — the applied-stats spine (Describe / Compare / Associate / Regress / Nonparametric) plus the advanced survival tier, emitting tidy frames into the generic drilldown renderers.

Run from the workspace root:

```bash
Rscript -e 'options(shiny.port=3838L, shiny.host="127.0.0.1"); source("blockr.stats/dev/stats-101.R", echo=FALSE, print.eval=TRUE)'
```

Open `http://localhost:3838`.

## Workflow

```
data ──► descriptives / frequencies / correlation_matrix
data ──► stat_test ──► padjust
data ──► model ──┬──► broom (tidy / glance / augment)
                 └──► effect_size
data ──► survival ──► broom (tidy step curve)
```

`new_broom_block()` is the hinge: model and survival blocks fit, `broom` tidies, and downstream blocks (tables, drilldown charts) render the tidy frame.

## Design

Per-package design notes and runnable scripts live in `dev/`. Cross-package specs for the no-code stats direction live in `blockr.design/` (see the `/blockr-spec` skill).
