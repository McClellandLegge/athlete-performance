
shinyUI(fluidPage(
  useShinyjs(),

  tags$head(
    tags$title("Movement Republic Athlete Performance")
    # Include our custom CSS
    , tags$style(HTML("@import url('http://bootswatch.com/simplex/bootstrap.css');"))
      , includeCSS("www/busy_style.css")
      , includeScript("www/busy.js")
    , shiny::tags$link(rel = "shortcut icon", href = "http://movementrepublic.com/wp-content/uploads/2015/01/MR_favicon.ico")
  ),

  fluidRow(

      div(style = "width: 100%; display: table;",
          div(style = "display: table-row")
          , div(style = "width: 600px; display: table-cell;"
              , a(href = "http://movementrepublic.com/"
                  , target = "_blank"
                  , img(src = "mr_wordmark_crossfit1.png")))
        , div(style = "display: table-cell;"
              , span("Athlete Performance", style = "color: #2b112f; font-family: Bitter,Open Sans,sans-serif;
                   font-weight: 700; font-size: 400%;"))
      )
      ,br()
  ),
  tabsetPanel(
    tabPanel("Model"
             , sidebarLayout(
               sidebarPanel(
                   h3("Inputs")
                  , uiOutput("movement_selector")
                  , uiOutput("athlete_selector")
                  , checkboxInput("include_temp", "Include Temperature", value = TRUE)
                  , checkboxInput("include_hum", "Include Humidity", value = TRUE)
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
                  ,  div(a(href = "https://github.com/McClellandLegge/athlete-performance/issues"
                                                 , target = "_blank"
                                                 , img(src = "octocat.png", width = "50px"))
                         , span("Report a bug/Request a feature"
                                , style = "color: #2b112f; font-family: Bitter,Open Sans,sans-serif;font-weight: 700;"))
                  , div(a(href = "http://robotoaster.co/"
                          , target = "_blank"
                          , img(src = "robo_toaster.png", width = "50px"))
                        , span("Check out some other cool kids"
                               , style = "color: #2b112f; font-family: Bitter,Open Sans,sans-serif;font-weight: 700;"))
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
