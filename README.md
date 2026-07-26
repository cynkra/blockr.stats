# blockr.stats

Statistical analysis blocks for [blockr](https://github.com/blockr-org/blockr): linear and generalized linear models, an adaptive test family, correlations, and survival analysis, plus a card that renders a fitted model. Six blocks. Every block emits tidy data frames and stays canonical R underneath, aimed at no-code statistical workflows.

## Installation

```r
pak::pak("blockr-org/blockr.stats")
```

Runtime dependencies are all on CRAN: `broom`, `survival`, `cmprsk`, `moments`, `nortest`, `dplyr`, and base `stats`. Model output is tidied through `broom` rather than the easystats family.

## Block catalog

| Block | Wraps | Notes |
|---|---|---|
| `new_model_block()` | `stats::lm`, `stats::glm`, `stats::aov` | Adaptive: pick `model_type` (`lm`, `glm` logistic/poisson, `aov`); role pickers adapt. |
| `new_model_summary_block()` | `broom::tidy`, `broom::glance` | Renders a fitted model as a card: model facts line + coefficient table with an inline forest. Feeds on the model object directly, so no adapter block in between; its value is the tidy coefficient frame. |
| `new_broom_block()` | `broom::tidy`, `broom::glance`, `broom::augment` | Generic adapter: turns any fitted model into a tidy / glance / augment frame. |
| `new_correlate_block()` | `stats::cor` | Pairwise correlation matrix (pearson / spearman / kendall); renders as a heatmap table. |
| `new_stat_test_block()` | `stats` test family | Adaptive `type`: one-sample / paired / two-sample t, Wilcoxon, one-way ANOVA, Kruskal-Wallis, chi-square independence, correlation, normality, homogeneity. |
| `new_survival_block()` | `survival`, `cmprsk` | Kaplan-Meier (`km`), Cox (`cox`), cumulative incidence (`cif`). |

Counts, proportions and per-variable descriptives are not blocks here: `blockr.viz::new_summary_table_block()` covers them for mixed types and by-group columns, and `blockr.dplyr` covers the rest.

## Demo

Hand-composed `dock_board` scripts live in `dev/`:

- `dev/basic-analysis.R` — start here. The smallest board that walks a whole analysis: penguins, a Table 1 by species, a two-sample t-test, an `lm` card and its tidy coefficients. Six blocks, four views.
- `dev/stats-101.R` — the full tour: the applied-stats spine (Describe / Compare / Associate / Regress / Nonparametric) plus the advanced survival tier, emitting tidy frames into the generic drilldown renderers.

Run from the workspace root:

```bash
Rscript blockr.stats/dev/basic-analysis.R          # port 3838, or pass one
Rscript -e 'options(shiny.port=3838L, shiny.host="127.0.0.1"); source("blockr.stats/dev/stats-101.R", echo=FALSE, print.eval=TRUE)'
```

Open `http://127.0.0.1:3838`.

## Workflow

```
data ──► correlate ──► table (heatmap)
data ──► stat_test
data ──► model ──► broom (tidy / glance / augment)
data ──► survival ──► broom (tidy step curve)
```

`new_model_summary_block()` is the usual next step after a fit: it tidies the model itself and draws the card. `new_broom_block()` is the hinge when you want the frame rather than the card: model and survival blocks fit, `broom` tidies, and downstream blocks (tables, drilldown charts) render the tidy frame.

## Design

Per-package design notes and runnable scripts live in `dev/`. Cross-package specs for the no-code stats direction live in `blockr.design/` (see the `/blockr-spec` skill).
