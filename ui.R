
shinyUI(fluidPage(
  useShinyjs(),

  tags$head(
    # Include our custom CSS
    tags$style(HTML("@import url('http://bootswatch.com/simplex/bootstrap.css');"))
    , includeCSS("www/busy_style.css")
    , includeScript("www/busy.js")
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
  tabsetPanel(
    tabPanel("Model"
             , sidebarLayout(
               sidebarPanel(
                   h3("Inputs")
                  , uiOutput("movement_selector")
                  , uiOutput("athlete_selector")
                  , checkboxInput("include_weather", "Include Weather", value = TRUE)
                  , checkboxInput("include_lifttype", "Include Lift Type", value = TRUE)
                  , checkboxInput("include_classtime", "Include Class Time", value = TRUE)
                  , h3("Prediction")
                  , uiOutput("class_time")
                  , uiOutput("lift_type")
                  , dateInput("pred_date", "Prediction Date")
                  , numericInput("pred_sets", "Sets", min = 1, value = 1)
                  , numericInput("pred_reps", "Reps", min = 1, value = 1)
                  , numericInput("pred_temp", "Temperature (F)", value = 80)
                  , numericInput("pred_hum", "Humidity (%)", min = 0, max = 100,  value = 60)
                   )
               , mainPanel(
                 h2("Historical Performance & Prediction")
                 , htmlOutput("prediction")
                 , plotlyOutput("trend")
                 , h2("Model")
                 , verbatimTextOutput("model_summary"))
               )
             )
    , tabPanel("Diagnostics",
               div(class = "busy",
                   p("Loading..."),
                   img(src = "ajax-loader.gif")
               )
               , div(class = "outer"
                 , h2("Diagnostic Plots")
                 , plotOutput("corr_check")
                 , br()
                 , plotlyOutput("fitted_vs_residuals")
                 , br()
                 , plotlyOutput("qq")
                 , br()
                 , plotlyOutput("resid_vs_leverage")
               ) #/ div#outer
             )
    , tabPanel("Data", br(),  dataTableOutput("athlete_history"))
    )
  )
)
