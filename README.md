# blockr.stats

Statistical analysis blocks for [blockr](https://github.com/blockr-org/blockr) — linear models, generalized linear models, mixed-effects models, t-tests, descriptives, correlation, and easystats-backed output blocks. Aimed at no-code statistical workflows for thesis writers and intro-stats users while staying canonical R underneath.

## Installation

```r
pak::pak("blockr-org/blockr.stats")
```

The package depends on the easystats family (`parameters`, `performance`, `effectsize`, `report`, `correlation`) plus `nnet`, `MASS`, `lme4` for the non-lm model families. All on CRAN.

## Block catalog

### Modeling

| Block | Wraps | Notes |
|---|---|---|
| `new_model_block()` | `stats::lm`, `stats::glm`, `nnet::multinom`, `MASS::polr`, `lme4::lmer`, `lme4::glmer` | Adaptive: pick model type, role pickers adapt (covariates / factors / random effects). |
| `new_lm_block()` | `stats::lm` | Linear-only convenience constructor. |
| `new_parameters_block()` | `parameters::model_parameters()` | Publication-style coefficient table. |
| `new_performance_block()` | `performance::model_performance()` | Model fit headline (R², AIC, BIC, RMSE). |
| `new_anova_block()` | `stats::anova()` | F-table. |
| `new_effectsize_block()` | `effectsize::eta_squared()` (and ω², ε² variants) | Partial / full effect sizes. |
| `new_standardize_block()` | `effectsize::standardize_parameters()` | Standardized β with CI. |
| `new_check_model_block()` | `performance::check_model()` | 6-panel diagnostic plots with interpretation cues. |
| `new_diagnostic_plot_block()` | base R `plot.lm` | 2×2 diagnostics (Residuals vs Fitted, Q-Q, Scale-Location, Leverage). |
| `new_residual_explorer_block()` | echarts4r | Interactive residual scatter. |
| `new_coef_block()` | `broom::tidy()` | Tidy coefficients table. |
| `new_coefplot_block()` | `modelsummary::modelplot()` | Dot-and-whisker plot. |
| `new_model_summary_block()` | broom + custom HTML | 3-column headline (coefficients / statistics / diagnostics). |
| `new_regression_3d_block()` | plotly | Interactive 3D scatter + regression plane (2-predictor models). |

### T-tests

| Block | Wraps | Notes |
|---|---|---|
| `new_ttest_block()` | `stats::t.test()` | Adaptive: one-sample / paired / independent (Welch / Student). |
| `new_cohens_d_block()` | `effectsize::effectsize()` | Cohen's d from a t-test result. |
| `new_normality_check_block()` | `stats::shapiro.test()` (or KS for n > 5000) | Per-group decision flag (OK / Concern / Reject). |
| `new_homogeneity_check_block()` | `stats::bartlett.test()` | Same flag pattern. |
| `new_nonparametric_block()` | `stats::wilcox.test()` | Adaptive: same shape as `new_ttest_block`. |

### Data exploration & narrative

| Block | Wraps | Notes |
|---|---|---|
| `new_descriptives_block()` | `parameters::describe_distribution()` | Per-variable mean / SD / IQR / skew / kurtosis / range. |
| `new_correlation_block()` | `correlation::correlation()` | Pearson / Spearman / Kendall, long-format tibble. |
| `new_frequencies_block()` | `table()` | Counts and proportions for categorical columns. |
| `new_report_block()` | `report::report()` | Auto-generated APA-style narrative paragraph. The wedge over jamovi for thesis writers. |

## Demos

Hand-composed `dock_board` scripts in `dev/`:

- `dev/all-views.R` — combined three-tab board (Data / Modeling / T-Tests). The reference demo. Runs on the bundled penguins CSV.
- `dev/modeling.R` — modeling view only.
- `dev/exploration.R` — data exploration view only.
- `dev/ttests.R` — t-tests view only.

Run from the workspace root:

```bash
Rscript blockr.stats/dev/all-views.R
```

Open `http://localhost:3838`.

The combined demo lands on the **Data** tab with the bundled `inst/extdata/penguins.csv` loaded via `blockr.io::new_read_block()`. Switch the read block's `source` to `"upload"` at runtime to load a different file.

## Workflow

```
data ──► model ──┬──► parameters / anova / effectsize / standardize / performance
                 ├──► report  (APA narrative)
                 ├──► check_model  (diagnostic plots)
                 └──► coefplot

data ──► ttest ──┬──► parameters
                 ├──► cohens_d
                 └──► report

data ──► descriptives / correlation / frequencies
data ──► normality_check / homogeneity_check / nonparametric
```

## Design

The four-phase spec for the no-code stats direction lives in `dev/stats-views-spec/` (motivation → requirements → design → implementation). It explains the audience, the wedges over jamovi/SPSS, the four-rung graduation ladder (view user → composer → power user with `function_block` → block developer), and which jamovi-class analyses we cover vs defer.
