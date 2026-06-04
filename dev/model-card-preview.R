# blockr.stats — model card preview harness.
#
# Standalone Shiny app (no blockr board) to eyeball model_summary_html()
# across model types and tune the styling fast. Controls on the left,
# the rendered card on the right.
#
# Run from workspace root:
#   Rscript -e 'options(shiny.port=3838L, shiny.host="127.0.0.1");
#     source("blockr.stats/dev/model-card-preview.R", echo=FALSE, print.eval=TRUE)'

pkgload::load_all("blockr.stats", quiet = TRUE)
library(shiny)

penguins <- palmerpenguins::penguins
lung <- survival::lung

fit_model <- function(type) {
  switch(type,
    lm       = stats::lm(body_mass_g ~ flipper_length_mm + bill_length_mm + species,
                         data = penguins),
    logistic = stats::glm(I(sex == "male") ~ body_mass_g + species,
                          data = penguins, family = stats::binomial()),
    poisson  = stats::glm(round(body_mass_g / 100) ~ flipper_length_mm,
                          data = penguins, family = stats::poisson()),
    aov      = stats::aov(body_mass_g ~ species + sex, data = penguins),
    cox      = survival::coxph(survival::Surv(time, status) ~ age + sex, data = lung),
    km       = survival::survfit(survival::Surv(time, status) ~ sex, data = lung)
  )
}

ui <- fluidPage(
  tags$head(css_model_summary()),
  tags$style(HTML("body { padding: 24px; background: #fff; }
                   .card-stage { max-width: 640px; }")),
  titlePanel("Model card preview"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("type", "Model", c(
        "Linear (lm)" = "lm", "Logistic (glm)" = "logistic",
        "Poisson (glm)" = "poisson", "ANOVA (aov)" = "aov",
        "Cox PH" = "cox", "Kaplan-Meier" = "km")),
      numericInput("digits", "Digits", value = 3, min = 1, max = 6, step = 1),
      sliderInput("conf", "Conf. level", min = 0.8, max = 0.99,
                  value = 0.95, step = 0.01)
    ),
    mainPanel(
      width = 9,
      div(class = "card-stage", uiOutput("card")),
      tags$hr(),
      tags$details(tags$summary("raw broom output"),
                   verbatimTextOutput("raw"))
    )
  )
)

server <- function(input, output, session) {
  model <- reactive(fit_model(input$type))

  output$card <- renderUI({
    model_summary_html(model(), conf_level = input$conf, digits = input$digits)
  })

  output$raw <- renderPrint({
    cat("tidy:\n");  print(tryCatch(broom::tidy(model(), conf.int = TRUE),
                                     error = function(e) broom::tidy(model())))
    cat("\nglance:\n"); print(tryCatch(broom::glance(model()), error = function(e) e$message))
  })
}

shinyApp(ui, server)
