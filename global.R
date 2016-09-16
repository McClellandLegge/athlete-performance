
library("shiny")
library("GGally")
library("data.table")
library("reshape2")
library("plyr")
library("dplyr")
library("dtplyr")
library("plotly")
library("DT")
library("lubridate")
library("shinyjs")

# Data Load ---------------------------------------------------------------

one_rep <- readRDS("data/one_rep.rds")[variable == "Baechle"]

athlete_performance <- readRDS("data/athlete_performance.rds")

# Modelling ---------------------------------------------------------------

orf <- lm(value ~ Reps, data = one_rep)

# Functions ---------------------------------------------------------------

capit <- function(x) {
  if (all(sapply(x, is.na))) return(NA)
  paste0(toupper(substring(x, 1, 1)), substring(x, 2, nchar(x)))
}

camel <- function(x)
  # convert text string to camel case
{
  y <- tolower(x)
  sapply(strsplit(y, " "), function(x) {
    if (all(sapply(x, is.na))) return(NA)
    paste(capit(x), collapse = " ")
  })
}

get_weather_data <- function(x) {
  extract <- function(x) {
    set <- x$history$observations
    set$date %>%
      as.data.table %>%
      transform(
        temp = set$tempi
        , humidity = set$hum) %>%
      dplyr::select(year, mon, mday, hour, min, temp, humidity)
  }

  dta <- format(x, "%Y%m%d") %>%
    paste0("http://api.wunderground.com/api/1cf032104d646cd6/history_", ., "/q/IL/Chicago.json") %>%
    httr::GET(.) %>%
    httr::content(., "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON() %>%
    extract

  saveRDS(dta, paste0("data/chicago_weather_", gsub("-", "_", x), ".rds"))
}

RegressionPlots <- function(fit, type){

  # Extract fitted values from lm() object
  Fitted.Values <-  fitted(fit)

  # Extract residuals from lm() object
  Residuals <-  resid(fit)

  # Extract standardized residuals from lm() object
  Standardized.Residuals <- MASS::stdres(fit)

  # Extract fitted values for lm() object
  Theoretical.Quantiles <- qqnorm(Residuals, plot.it = F)$x

  # Square root of abs(residuals)
  Root.Residuals <- sqrt(abs(Standardized.Residuals))

  # Calculate Leverage
  Leverage <- lm.influence(fit)$hat

  # Create data frame
  # Will be used as input to plot_ly

  regMat <- data.frame(Fitted.Values,
                       Residuals,
                       Standardized.Residuals,
                       Theoretical.Quantiles,
                       Root.Residuals,
                       Leverage)

  # Plot using Plotly

  # Fitted vs Residuals
  # For scatter plot smoother
  if (type == "fitted_vs_residuals") {
    LOESS1 <- loess.smooth(Fitted.Values, Residuals)

    plt <- regMat %>%
      plot_ly(x = Fitted.Values, y = Residuals,
              type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
              marker = list(size = 10, opacity = 0.5), showlegend = F) %>%

      add_trace(x = LOESS1$x, y = LOESS1$y, type = "scatter", mode = "line", name = "Smooth",
                line = list(width = 2)) %>%

      layout(title = "Residuals vs Fitted Values", plot_bgcolor = "#e6e6e6", width = 1000, showlegend = FALSE)
  } else if (type == "qq") {

    # QQ Pot
    plt <- regMat %>%
      plot_ly(x = Theoretical.Quantiles, y = Standardized.Residuals,
              type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
              marker = list(size = 10, opacity = 0.5), showlegend = F) %>%

      add_trace(x = Theoretical.Quantiles, y = Theoretical.Quantiles, type = "scatter", mode = "line", name = "",
                line = list(width = 2)) %>%

      layout(title = "Q-Q Plot", plot_bgcolor = "#e6e6e6", showlegend = FALSE)
  } else if (type == "scale_location") {
  # Scale Location
  # For scatter plot smoother
  # LOESS2 <- loess.smooth(Fitted.Values, Root.Residuals)
  #
  # plt3 <- regMat %>%
  #   plot_ly(x = Fitted.Values, y = Root.Residuals,
  #           type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
  #           marker = list(size = 10, opacity = 0.5), showlegend = F) %>%
  #
  #   add_trace(x = LOESS2$x, y = LOESS2$y, type = "scatter", mode = "line", name = "Smooth",
  #             line = list(width = 2)) %>%
  #
  #   layout(title = "Scale Location", plot_bgcolor = "#e6e6e6", width = 1000)

  } else if (type == "resid_vs_leverage") {
  # Residuals vs Leverage
  # For scatter plot smoother
  LOESS3 <- loess.smooth(Leverage, Residuals)

  plt <- regMat %>%
    plot_ly(x = Leverage, y = Residuals,
            type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
            marker = list(size = 10, opacity = 0.5), showlegend = F) %>%

    add_trace(x = LOESS3$x, y = LOESS3$y, type = "scatter", mode = "line", name = "Smooth",
              line = list(width = 2)) %>%

    layout(title = "Leverage vs Residuals", plot_bgcolor = "#e6e6e6", showlegend = FALSE)

  }
  return(plt)
}
