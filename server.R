
shinyServer(function(input, output) {

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
                   , selected = "S. Harvey"
                   )
  }) #/ athlete_selector

  athlete_data <- reactive({
    shiny::validate(
      need(input$athlete, "")
    )
    dplyr::filter(athlete_performance
                  , Component == input$movement
                  , Name == input$athlete) %>%
      arrange(desc(Date))
  }) #/ athlete_data

  output$athlete_history <- renderDataTable({
    athlete_data() %>%
      dplyr::select(Name, Date, `Rep Scheme`, Result, Component, Temperature, Humidity) %>%
      datatable(filter = "top")
  }) #/ athlete_history

  model_fit <- reactive({
    set <- athlete_data()
    if (length(unique(set$`Time of Day`)) > 1) {
      lm(Weight ~ Sets + Reps + Date + Type + Temperature + Humidity + I(`Time of Day`), data = set)
    } else {
      lm(Weight ~ Sets + Reps + Date + Type + Temperature + Humidity, data = set)
    }

  }) #/ model_fit

  output$model_summary <- renderPrint({
    summary(model_fit())
  }) #/ model_summary

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

  pred_data <- reactive({
    shiny::validate(
      need(input$pred_date, "Select a class date")
      , need(input$pred_sets, "Select a number of sets")
      , need(input$pred_reps, "Select a number of reps")
      , need(input$pred_class, "Select a class time")
      , need(input$pred_type, "Select a lift type")
      , need(input$pred_temp, "Select a temperature")
      , need(input$pred_hum, "Select a humidity")
    )
    data.table(
      Date = input$pred_date
      , Sets = input$pred_sets
      , Reps = input$pred_reps
      , Type = input$pred_type
      , Temperature = input$pred_temp
      , Humidity = input$pred_hum
      , `Time of Day` = input$pred_class
    )

  }) #/ pred_data

  pred_value <- reactive({
    predict(model_fit(), pred_data())
  }) #/ pred_value

  output$prediction <- renderPrint({
    pred_value()
  }) #/ prediction

  model_rsq <- reactive({
    summary(model_fit())$r.squared
  }) #/ model_rsq

  output$estimate_selector <- renderUI({
    if (model_rsq() < 0.75) {
      radioButtons("est_type", "Prediction Type"
                   , choices = c("Statistical Model", "Common Percentages")
                   , selected = "Common Percentages")
    } else {
      radioButtons("est_type", "Prediction Type"
                   , choices = c("Statistical Model", "Common Percentages")
                   , selected = "Statistical Model")
    }

  }) #/ estimate_selector

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
                , hoverinfo = "text", showlegend = FALSE) %>%
      layout(title = "Historical Performance")
  }) #/ trend

  output$fitted_vs_residuals <- renderPlotly({
    RegressionPlots(model_fit(), type = "fitted_vs_residuals")
  }) #/ fitted_vs_residuals

  output$qq <- renderPlotly({
    RegressionPlots(model_fit(), type = "qq")
  }) #/ qq

  output$resid_vs_leverage <- renderPlotly({
    RegressionPlots(model_fit(), type = "resid_vs_leverage")
  }) #/ resid_vs_leverage


})
