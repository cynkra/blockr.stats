# Renders every state of the model summary card to a standalone HTML file, so
# the card can be eyeballed without a running board. Not a test: the visual is
# the point.
#
#   Rscript dev/model-summary-card-preview.R [out.html]

pkgload::load_all(quiet = TRUE)

out <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(out)) out <- "dev/model-summary-card-preview.html"

has_penguins <- requireNamespace("palmerpenguins", quietly = TRUE)
peng <- if (has_penguins) {
  stats::na.omit(palmerpenguins::penguins[c("body_mass_g", "flipper_length_mm",
                                            "species", "sex")])
}

fit_lm <- if (has_penguins) {
  stats::lm(body_mass_g ~ flipper_length_mm + species, peng)
} else {
  stats::lm(mpg ~ wt + hp + factor(cyl), mtcars)
}
fit_mixed <- stats::lm(mpg ~ wt + hp + factor(cyl), mtcars)
fit_glm <- stats::glm(am ~ wt + hp, mtcars, family = stats::binomial())
fit_cox <- survival::coxph(
  survival::Surv(time, status) ~ age + sex + ph.ecog, survival::lung
)
fit_km <- survival::survfit(survival::Surv(time, status) ~ sex, survival::lung)

case <- function(title, note, card) {
  htmltools::tags$section(
    class = "case",
    htmltools::tags$h2(title),
    htmltools::tags$p(class = "note", note),
    htmltools::tags$div(class = "frame", card)
  )
}

cases <- htmltools::tagList(
  case(
    "Default (V15)",
    "Everything on: facts line, forest column with CI whiskers, significance chips, intercept.",
    model_summary_card(model_summary(fit_lm))
  ),
  case(
    "Mixed magnitudes",
    paste("mtcars: hp is 0.03 while the intercept is 35. The column shares one",
          "decimal count, taken from the typical term, and the intercept is",
          "out of both the scale and the format."),
    model_summary_card(model_summary(fit_mixed))
  ),
  case(
    "Narrow panel (V15a): effect column off",
    "One flip turns the card into a plain numeric table; chips are the default.",
    model_summary_card(model_summary(fit_lm, effect_column = FALSE))
  ),
  case(
    "The p-value column, as an option",
    "For the analyst who wants the number rather than the threshold.",
    model_summary_card(model_summary(fit_lm, significance = "p"))
  ),
  case(
    "The chip ladder",
    paste("A model with a spread of p-values: 1%, 5% and 10% all appear",
          "(0.1% is in the cases above). The three significant levels deepen",
          "one step at a time; 10% is the neutral grey badge, marked as",
          "borderline rather than read as a result; above 10%, no chip."),
    model_summary_card(model_summary(
      stats::lm(mpg ~ wt + hp + disp + qsec + drat + am, mtcars),
      significance = "chips", effect_column = FALSE
    ))
  ),
  case(
    "Standard error instead of CI (V15b)",
    "Shorter whiskers that mean something else, so the headers say so.",
    model_summary_card(model_summary(fit_lm, uncertainty = "se"))
  ),
  case(
    "Logistic: auto ratio scale",
    "Log-odds are unreadable, so auto exponentiates: reference at 1, log axis, OR header.",
    model_summary_card(model_summary(fit_glm))
  ),
  case(
    "Cox: hazard ratios",
    "Same block, different model class: HR column, concordance and events in the facts.",
    model_summary_card(model_summary(fit_cox))
  ),
  case(
    "Stars, 99% CI, no intercept",
    "Options composing.",
    model_summary_card(model_summary(
      fit_lm, uncertainty = "ci99", significance = "stars", intercept = FALSE
    ))
  ),
  case(
    "No uncertainty at all",
    "Dot only, no interval column: the bare estimate for a slide.",
    model_summary_card(model_summary(fit_lm, uncertainty = "none"))
  ),
  case(
    "Kaplan-Meier: no coefficients",
    "tidy(survfit) is curve points, so the card degrades instead of drawing 200 rows.",
    model_summary_card(model_summary(fit_km))
  ),
  case(
    "Tidy frame in, no model",
    "Straight from a broom adapter: coefficients render, model facts are unavailable.",
    model_summary_card(model_summary(broom::tidy(fit_lm, conf.int = TRUE)))
  )
)

page <- htmltools::tags$html(
  htmltools::tags$head(
    htmltools::tags$meta(charset = "utf-8"),
    htmltools::tags$title("Model summary card"),
    css_summary_card(),
    htmltools::tags$style(htmltools::HTML(
      "body { margin: 0; padding: 28px; background: #f6f7f9;
              font-family: -apple-system, 'Segoe UI', Roboto, sans-serif;
              color: #111827; }
       .case { max-width: 720px; margin: 0 auto 28px; }
       h2 { font-size: 15px; margin: 0 0 4px; }
       .note { margin: 0 0 10px; font-size: 13px; color: #6b7280; }
       .frame { background: #fff; border: 1px solid #e5e7eb; border-radius: 8px;
                padding: 14px 16px; }"
    ))
  ),
  htmltools::tags$body(cases)
)

htmltools::save_html(page, out)
cat("wrote", normalizePath(out), "\n")
