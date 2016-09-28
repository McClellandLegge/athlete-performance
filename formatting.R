source("global.R")

# Libraries ---------------------------------------------------------------

# we don't like these loaded because of certain troublesome function masks
library("jsonlite")
library("httr")

# Athlete Performance -----------------------------------------------------

res_fmt <- "(\\d+) x (\\d+) @ (\\d+) lbs"
nm_fmt <- "([A-z])[A-z]+ ([A-z_]+)"

pr_ap <- list.files(
  path = "data"
  , pattern = "athlete_performance"
  , full.names = TRUE) %>%
  grep("\\.csv", value = TRUE, x = .) %>%
  lapply(fread, header = TRUE, sep = ",") %>%
  rbindlist %>%
  transform(
    Name = camel(sub(nm_fmt, "\\1\\. \\2", x = `Athlete Name`))
    , Last = camel(sub(nm_fmt, "\\2", x = `Athlete Name`))
    , Date = as.Date(Date, format = "%m/%d/%Y")
    , Sets = as.numeric(sub(res_fmt, "\\1", x = Result))
    , Reps = as.numeric(sub(res_fmt, "\\2", x = Result))
    , Weight = as.numeric(sub(res_fmt, "\\3", x = Result))
    , Type = ifelse(grepl("EMOM", `Rep Scheme`), "EMOM",
                    ifelse(grepl("%", `Rep Scheme`), "Percentage", "Max"))
  ) %>%
  transform(
    year = year(Date)
    , mon = month(Date)
    , mday = mday(Date)
    , hour = ifelse(nchar(`Class Name`) == 0, 12,
                    ifelse(grepl("PM", `Class Name`),
                           as.numeric(sub("(\\d+)[: ](.*)", "\\1", `Class Name`)) + 12
                           , as.numeric(sub("(\\d+)[: ](.*)", "\\1", `Class Name`))
                           ))
    , min = 0
  ) %>%
  transform(
    classtime = as.POSIXct(paste0(year, "-", mon, "-", mday, " ", hour, ":", min, ":00 CDT"))
  ) %>%
  transform(
    dummy = classtime
  ) %>%
  dplyr::select(-`Athlete Name`)

# One Rep Conversion ------------------------------------------------------

one_rep <- fread("data/one_rep_max_est.csv") %>%
  melt(id.vars = "Reps")

# saveRDS(one_rep, "data/one_rep.rds")

# Weather Data ------------------------------------------------------------

need_dates <- sort(unique(athlete_performance$Date))

have_dates <- list.files("data", pattern = "chicago_weather") %>%
  sub("chicago_weather_([0-9_]+)\\.rds", "\\1", x = .) %>%
  as.Date(format = "%Y_%m_%d")

get_dates <- need_dates[!need_dates %in% have_dates]
if (length(get_dates) > 0) {
  for (i in seq_along(get_dates)) {
    get_weather_data(get_dates[i])
    Sys.sleep(10)
  }
}

weather <- list.files("data", full.names = TRUE, pattern = "chicago_weather") %>%
  lapply(readRDS) %>%
  rbindlist %>%
  transform(
    px_start = as.POSIXct(paste0(year, "-", mon, "-", mday, " ", hour, ":", min, ":00 CDT"))
  ) %>%
  unique() %>%
  transform(
    secdiff = c(diff(px_start), 60) - 1
    , `Time of Day` = ifelse(as.numeric(hour) < 9, "Morning",
                             ifelse(as.numeric(hour) > 16, "Afternoon/Night", "Mid-day"))
  ) %>%
  transform(
    px_end = px_start + secdiff
  ) %>%
  dplyr::select(px_start, px_end, temp, humidity, `Time of Day`)

# Merges ------------------------------------------------------------------

setkeyv(pr_ap, c("classtime", "dummy"))
setkeyv(weather, c("px_start", "px_end"))
athlete_performance <- foverlaps(pr_ap, weather, by.x = c("classtime", "dummy")) %>%
  dplyr::select(-dummy, -px_start, -px_end) %>%
  rename(Temperature = temp, Humidity = humidity) %>%
  transform(Temperature = as.numeric(Temperature)
            , Humidity = as.numeric(Humidity))

# Adjustments -------------------------------------------------------------

# This day is really a 1-rep max day, is really influencing the max lift
athlete_performance[Date == '2016-01-29', Type := "Max"]

saveRDS(athlete_performance, "data/athlete_performance.rds")
