# Packages ----------------------------------------------------------------
library(tidyverse)
library(httr)

hourly.old <- read.csv("hourly.csv")

hourly.old$date <- parse_datetime(hourly.old$date)

# AirNow ------------------------------------------------------------------
airnow.url.frame <- "https://www.airnowapi.org/aq/data/?startDate=XXX&endDate=XXX&parameters=OZONE,PM25,PM10,CO,NO2,SO2&BBOX=-77.147558,38.792889,-76.888006,39.005574&dataType=A&format=text/csv&verbose=1&nowcastonly=1&includerawconcentrations=1&API_KEY=3AC62974-39D1-4E52-9D31-9F7BC30231AF"

current.date.time <- Sys.time()

c.date <- substr(current.date.time, start = 1, stop = 10)
c.time <- substr(current.date.time, start = 12, stop = 13)
current.date <- str_c(c.date, "T", c.time)
current.date.parsed <- parse_datetime(current.date)

airnow.url <- str_replace_all(airnow.url.frame, pattern = "XXX", replacement = current.date)

airnow.current <- read.csv(url(airnow.url), header = FALSE)

airnow.current <- airnow.current %>% 
  rename(latitude = V1,
         longitude = V2, 
         date = V3, 
         parameter_name = V4, 
         raw_concentration = V5,
         AQI = V6,
         category_number = V7,
         location = V8, 
         agency = V9) %>% 
  mutate(date = current.date.parsed)

category_reference <- data.frame(category_number = c(1, 2, 3, 4, 5, 6),
                                 category = c("good", "moderate", 
                                              "unhealthy for sensitive groups",
                                              "unhealthy", 
                                              "very unhealthy",
                                              "hazardous"))

airnow.current <- merge(airnow.current, category_reference)

# Hourly Traffic ----------------------------------------------------------
current.date.time <- Sys.time()

c.date <- substr(current.date.time, start = 1, stop = 10)
c.time <- substr(current.date.time, start = 12, stop = 13)
current.date <- str_c(c.date, "T", c.time, ":00")
current.date.parsed <- parse_datetime(current.date)

traffic.url <- GET("https://api.midway.tomtom.com/ranking/liveHourly/USA_washington",
                   accept_json())

traffic.raw <- content(traffic.url, type = "application/json")

traffic.data.raw <- traffic.raw$data

traffic.data <- data.frame(matrix(unlist(traffic.data.raw), 
                                  nrow = length(traffic.data.raw), 
                                  byrow = T))

traffic.data <- traffic.data  %>% 
  dplyr::rename(jams_delay = X1, 
                traffic_index_live = X2, 
                update_time = X3, 
                jams_length = X4, 
                jams_count = X5, 
                traffic_index_week_ago = X6, 
                update_time_week_ago = X7) %>% 
  mutate(date = current.date.parsed) %>% 
  select(date, traffic_index_live, jams_delay, jams_length, jams_count)

traffic.current <- tail(traffic.data, n = 1)

# Merge Data --------------------------------------------------------------
hourly.current <- merge(airnow.current, traffic.current, by = "date")

hourly.combine <- rbind(hourly.old, hourly.current)

write.csv(hourly.combine, file = "hourly.csv", row.names = FALSE)




