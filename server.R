
shinyServer(function(input, output, session) {

# Dynamic Inputs ----------------------------------------------------------

  output$movement_selector <- renderUI({
    mvts <- sort(unique(athlete_performance$Component))
    selectizeInput("movement", "Movement"
                   , choices = mvts
                   , multiple = FALSE
    )
  }) #/ movement_selector

  output$athlete_selector <- renderUI({
    shiny::validate(
      need(input$movement, "Choose a movement")
    )
    athletes <- dplyr::filter(athlete_performance, Component == input$movement) %>%
      dplyr::select(Name, Last) %>%
      unique %>%
      arrange(Last) %>%
      dplyr::select(Name) %>%
      unlist(use.names = FALSE)

    selectizeInput("athlete", "Athlete"
                   , choices = athletes
                   , multiple = FALSE
                   # , selected = "S. Harvey"
    )
  }) #/ athlete_selector

  output$class_time <- renderUI({
    athlete_data() %>%
      dplyr::select(`Time of Day`) %>%
      unique %>%
      arrange(`Time of Day`) %>%
      unlist(use.names = FALSE) %>%
      selectizeInput("pred_class", "Class Time"
                     , choices = .
                     , multiple = FALSE
      )
  }) #/ class_time

  output$lift_type <- renderUI({
    athlete_data() %>%
      dplyr::select(`Type`) %>%
      unique %>%
      arrange(`Type`) %>%
      unlist(use.names = FALSE) %>%
      selectizeInput("pred_type", "Lift Type"
                     , choices = .
                     , multiple = FALSE
                     , selected = "Max"
      )
  }) #/ lift_type

# Data Objects ------------------------------------------------------------

  athlete_data <- reactive({
    shiny::validate(
      need(input$athlete, "")
    )
    dplyr::filter(athlete_performance
                  , Component == input$movement
                  , Name == input$athlete) %>%
      arrange(desc(Date))
  }) #/ athlete_data

  model_fit <- reactive({
    set <- athlete_data()

    frml <- Formula(Weight ~ Reps + Sets + Date)
    if (input$include_temp) frml <- update(frml, . ~ . + Temperature)
    if (input$include_hum) frml <- update(frml, . ~ . + Humidity)
    if (input$include_lifttype) frml <- update(frml, . ~ . + Type)
    if (input$include_classtime) frml <- update(frml, . ~ . + I(`Time of Day`))

    shiny::validate(
      need(nrow(set) > 10, "Not enough data for a model!")
    )

    lm(frml, data = set)
  }) #/ model_fit

  pred_data <- reactive({
    if (input$include_temp)
      shiny::validate(need(input$pred_temp, ""))
    if (input$include_hum)
      shiny::validate(need(input$pred_hum, ""))
    if (input$include_lifttype)
      shiny::validate(need(input$pred_type, ""))
    if (input$include_classtime)
      shiny::validate(need(input$pred_class, ""))

    init <- list(
      Date = input$pred_date
      , Sets = input$pred_sets
      , Reps = input$pred_reps
      , Type = input$pred_type
      , Temperature = input$pred_temp
      , Humidity = input$pred_hum
      , `Time of Day` = input$pred_class
    )

    as.data.table(init)
  }) #/ pred_data

  pred_value <- reactive({
    predict(model_fit(), pred_data())
  }) #/ pred_value

  model_rsq <- reactive({
    summary(model_fit())$r.squared
  }) #/ model_rsq

# Plot Output -------------------------------------------------------------

  output$trend <- renderPlotly({

    history <- athlete_data()
    prediction <- pred_data()
    mfit <- model_fit()

    prediction <- transform(
      prediction
      , Result = "Predicted Value"
      , Comment = ""
      , Weight = pred_value()
    )

    history <- transform(history, `Julian Date` = as.numeric(Date))

    plot_ly(history, x = Sets, y = Reps, z = Weight
            , text = paste(Result, Comment), type = "scatter3d", mode = "markers"
            , hoverinfo = "text", symbols = c("cross", "square", "triangle-down")
            , symbol = Type) %>%
      add_trace(data = prediction, x = Sets, y = Reps, z = Weight
                , text = paste(Sets, "x", Reps, "@", round(Weight), "Prediction")
                , type = "scatter3d", mode = "markers"
                , hoverinfo = "text", showlegend = FALSE)
  }) #/ trend

  output$corr_check <- renderPlot({
    dplyr::select(athlete_data(), Temperature, Humidity, Sets, Reps, Weight, Type) %>%
      ggpairs(data = ., aes(colour = Type), columns = c("Temperature", "Humidity", "Reps", "Weight", "Sets"))
  })

  output$fitted_vs_residuals <- renderPlotly({
    RegressionPlots(model_fit(), type = "fitted_vs_residuals")
  }) #/ fitted_vs_residuals

  output$qq <- renderPlotly({
    RegressionPlots(model_fit(), type = "qq")
  }) #/ qq

  output$resid_vs_leverage <- renderPlotly({
    RegressionPlots(model_fit(), type = "resid_vs_leverage")
  }) #/ resid_vs_leverage

# Data Output -------------------------------------------------------------

  output$athlete_history <- renderDataTable({
    athlete_data() %>%
      dplyr::select(Name, Date, `Time of Day`, `Rep Scheme`, Type, Result, Component, Temperature, Humidity) %>%
      datatable(filter = "top")
  }) #/ athlete_history

# Other Output ------------------------------------------------------------

  output$prediction <- renderPrint({
    shiny::validate(
      need(pred_value(), "")
    )

    pv <- 2.5 * round(pred_value() / 2.5)
    pvkg <- pv * 0.45359237
    rs <- round(100 * model_rsq())

    coefs <- model_fit()$coefficients

    pred_list_items <- coef_list_items <- list()

    k <- 1
    if (input$include_lifttype) {
      pred_list_items[[k]] <- shiny::tags$li(paste0("A ", input$pred_type, " type lift"))
      coef_list_items[[k]] <- shiny::tags$li(paste0("A ", input$pred_type, " type lift"))
      k <- k + 1
    }

    if (input$include_temp) {
      pred_list_items[[k]] <- shiny::tags$li(paste0("Temperature at ", input$pred_temp, " degrees Fahrenheit"))
      k <- k + 1
    }
    if (input$include_hum) {
      pred_list_items[[k]] <- shiny::tags$li(paste0("With ", input$pred_hum, "% humidity"))
      k <- k + 1
    }

    if (input$include_classtime) {
      pred_list_items[[k]] <- shiny::tags$li(paste0("At a ", input$pred_class, " class"))
    }

    div(
      h4("Prediction Interpretation")
      , p(paste0("The model current model (which explains ", rs, "% of the variance) predicts that for:"))
      , shiny::tags$ul(
        pred_list_items
      )
      , p(paste0("you will lift ", pv, "lbs/", round(pvkg, 1), "kg for ", input$pred_sets, " set(s) of ", input$pred_reps, " rep(s)."))
      , p(paste0("Coach John says: Kickass!"))
      , p(paste0("Coach Sarah says: ...ehhh, you probably should add some weight."))
      , p(paste0("Coach Dustin says: stop looking at the model and meet me by the whiteboard, NOW!"))
      # , h4("Model Coefficient Interpretation")

      , h4("Additional Notes")
      , p("If these results look weird, make sure your prediction makes sense and that your % variance explained is reasonably high (> 60%). For instance you should not try to predict your weight for 1 Set of 1 Reps for an EMOM lift!")
      , p("Additionally if you do not have a lot of data, or are predicting outside of any lift you have attempted (e.g. predicting a 1 rep max never having performed one), your results may not make sense.")
    )
  }) #/ prediction

  output$model_summary <- renderPrint({
    summary(model_fit())
  }) #/ model_summary

# Observers ---------------------------------------------------------------

  # manual inclusion/exclusion
  observe({
    toggle(id = "pred_temp", anim = TRUE, condition = input$include_temp)
    toggle(id = "pred_hum", anim = TRUE, condition = input$include_hum)
    toggle(id = "pred_type", anim = TRUE, condition = input$include_lifttype)
    toggle(id = "pred_class", anim = TRUE, condition = input$include_classtime)
  }, priority = 97)

  # qc inclusion/exclusion
  observe({
    if (length(unique(athlete_data()$`Time of Day`)) < 2) {
      updateCheckboxInput(session, "include_classtime", value = FALSE)
      hide("class_time", anim = TRUE)
      disable(id = "include_classtime")
    } else {
      enable(id = "include_classtime")
      show("class_time", anim = TRUE)
    }
    if (length(unique(athlete_data()$Type)) < 2) {
      updateCheckboxInput(session, "include_lifttype", value = FALSE)
      hide("lift_type", anim = TRUE)
      disable(id = "include_lifttype")
    } else {
      enable(id = "include_lifttype")
      show("lift_type", anim = TRUE)
    }

  }, priority = 98)

  observeEvent(input$athlete, {
    updateCheckboxInput(session, "include_classtime", value = TRUE)
    updateCheckboxInput(session, "include_lifttype", value = TRUE)

    if (!is.null(athlete_data())) {
      weath_corr <- with(athlete_data(), cor(Temperature, Humidity))
      if (coalesce(weath_corr, 1) > 0.6) {
        updateCheckboxInput(session, "include_hum", value = FALSE)
        hide("pred_hum", anim = TRUE)
        disable(id = "include_hum")
      }
    }

  }, priority = 99)

})
