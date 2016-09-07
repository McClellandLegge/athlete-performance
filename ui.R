
shinyUI(fluidPage(
  tags$head(
    tags$style(HTML("@import url('http://bootswatch.com/simplex/bootstrap.css');"))
  ),

  fluidRow(
    mainPanel(
      column(6,
             br(), br(), list(img(src = "mr_wordmark_crossfit1.png")), br(), br(), br()
      ),
      column(6, h1("Athlete Performance", style = "color: #2b112f; font-family: Bitter,Open Sans,sans-serif;
                   font-weight: 700;"))
    )
  ),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      h3("Inputs")
      , uiOutput("movement_selector")
      , uiOutput("athlete_selector")
      , h3("Prediction")
      # , uiOutput("estimate_selector")
      , uiOutput("class_time")
      , uiOutput("lift_type")
      , dateInput("pred_date", "Prediction Date")
      , numericInput("pred_sets", "Sets", min = 1, value = 1)
      , numericInput("pred_reps", "Reps", min = 1, value = 1)
      , numericInput("pred_temp", "Temperature (F)", value = 80)
      , numericInput("pred_hum", "Humidity (%)", min = 0, max = 100,  value = 60)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      h2("Historical Data")
      , dataTableOutput("athlete_history")
      , br()
      , plotlyOutput("trend")
      , h2("Linear Model")
      , verbatimTextOutput("model_summary")
      , br()
      , plotlyOutput("fitted_vs_residuals")
      , br()
      , plotlyOutput("qq")
      , br()
      , plotlyOutput("resid_vs_leverage")
    )
  )
))
