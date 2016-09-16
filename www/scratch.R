library("GGally")

set <- athlete_performance[Name == "S. Harvey"]
setnames(set, tolower(gsub(" ", "_", colnames(set))))
dat <- dplyr::select(set, temperature, humidity, sets, reps, weight, type)


nn <- data.matrix(dat)
ggpairs(dat, aes(colour = type), columns = c("Temperature", "Humidity", "Reps", "Weight", "Sets"))

data <- GGally::upgrade_scatmat_data(data)
data.choose <- data[, columns]
dn <- data.choose[sapply(data.choose, is.numeric)]


cor(as.matrix(dat))



So you want to have a button and when you click it, both a javascript function and some R code get called? I was able to do this with the onclick function from the shinyjs package (disclaimer: I wrote the package)

library(shinyjs)

jsCode <- "
shinyjs.geocodeAddr = function(params) {
alert('JavaScript called!');
// geocodeAddressStreet(...)
}
"

js_linktomr <-"
a(http://movementrepublic.com/

runApp(shinyApp(
  ui = fluidPage(
    useShinyjs(),
    extendShinyjs(text = jsCode),
    actionButton("btn", "Click me")
  ),
  server = function(input, output, session) {
    onclick("btn", {
      js$geocodeAddr()
      cat("R called as well")
    })
  }

))




https://www.google.com/url?hl=en&q=http://mandrillapp.com/track/click/30002421/dropbox.com?p%3DeyJzIjoieHU0RE5CSE9WLTRyWGhjZVZ1S3RaMGJoaHBRIiwidiI6MSwicCI6IntcInVcIjozMDAwMjQyMSxcInZcIjoxLFwidXJsXCI6XCJodHRwczpcXFwvXFxcL2Ryb3Bib3guY29tXFxcL3NoXFxcL2I1cTZ2cmgzZTdhZDJyNlxcXC9BQUN2OUhMMTlKRE4xS1V3SEJsX0p0anZhP2RsPTBcIixcImlkXCI6XCI0NDY0ZmJiYjM0Njg0N2I4ODk5OTQ0MzA3Yjg2MjM1YlwiLFwidXJsX2lkc1wiOltcImJkNTliYmY3MDc3MGIyMjgwZTRkNDVkYWYyOWQwYjQ4MjdiNTMwMDNcIl19In0&source=gmail&ust=1473879420857000&usg=AFQjCNE8oJD0c-WqEAkADXR3xuUaNfrFcg


https://www.dropbox.com/sh/b5q6vrh3e7ad2r6/AACv9HL19JDN1KUwHBl_Jtjva?dl=0&preview=IMG_0676.JPG

dl_from_dropbox <- function(x, key) {
  require(RCurl)
  bin <- getBinaryURL(paste0("https://dl.dropboxusercontent.com/s/", key, "/", x),
                      ssl.verifypeer = FALSE)
  con <- file(x, open = "wb")
  writeBin(bin, con)
  close(con)
  message(noquote(paste(x, "read into", getwd())))
}
# Example:
dl_from_dropbox("GViewer_Embeds.txt", "06fqlz6gswj80nj")
shell.exec("GViewer_Embeds.txt")


runApp(shinyApp(
  ui = fluidPage(
    sidebarLayout(
      sidebarPanel(
        checkboxInput("include_weather", "Include Weather", value = TRUE)
        , checkboxInput("include_lifttype", "Include Lift Type", value = TRUE)
        , checkboxInput("include_classtime", "Include Class Time", value = TRUE)
      )
      , mainPanel(
        verbatimTextOutput("model_summary")
        , verbatimTextOutput("dt")
      )
    )

  ),
  server = function(input, output, session) {
    library("Formula")

    model_fit <- reactive({
      frml <- Formula(Weight ~ Reps + Sets + Date)
      if (input$include_weather) frml <- update(frml, . ~ . + Temperature + Humidity)
      if (input$include_lifttype) frml <- update(frml, . ~ . + Type)
      if (input$include_classtime) frml <- update(frml, . ~ . + `Time of Day`)
      frml
    })

    dummy <- reactive({
      list(
        one = input$include_weather
        , two = input$non_existant
      )
    })

    output$dt <- renderPrint({
      as.data.table(dummy())
    })


    output$model_summary <- renderPrint({
      print(model_fit())
      # summary(model_fit())
    }) #/ model_summary
  }
))
