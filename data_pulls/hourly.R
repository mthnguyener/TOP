# Packages ----------------------------------------------------------------
library(tidyverse)
library(httr)

hourly.old <- read.csv("hourly.csv")

hourly.old$date <- parse_datetime(hourly.old$date)

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

date.frame <- seq(1:nrow(traffic.data))
for(i in 1:nrow(traffic.data)){
  date.frame[i] <- as.character(current.date.parsed - (3600*i))
}
date.frame <- rev(date.frame)

traffic.current <- traffic.data  %>% 
  dplyr::rename(jams_delay = X1, 
                traffic_index_live = X2, 
                update_time = X3, 
                jams_length = X4, 
                jams_count = X5, 
                traffic_index_week_ago = X6, 
                update_time_week_ago = X7) %>% 
  mutate(date = date.frame) %>% 
  select(date, traffic_index_live, jams_delay, jams_length, jams_count)

traffic.current$date <- parse_datetime(traffic.current$date)

# AirNow ------------------------------------------------------------------
airnow.url.frame <- "https://www.airnowapi.org/aq/data/?startDate=XXX&endDate=XXX&parameters=OZONE,PM25,PM10,CO,NO2,SO2&BBOX=-77.147558,38.792889,-76.888006,39.005574&dataType=A&format=text/csv&verbose=1&nowcastonly=1&includerawconcentrations=1&API_KEY=3AC62974-39D1-4E52-9D31-9F7BC30231AF"

c.date1 <- substr(date.frame, start = 1, stop = 10)
c.time1 <- substr(date.frame, start = 12, stop = 13)

for(i in 1:length(c.time1)){
  if(c.time1[i] == ""){
    c.time1[i] <- "00"
  }
}

current.date1 <- str_c(c.date1, "T", c.time1)

airnow.url <- str_replace_all(airnow.url.frame, pattern = "XXX", replacement = current.date1)

airnow.datalist <- list()

for(i in 1:length(airnow.url)){
  dat <- read.csv(url(airnow.url[i]), header = FALSE)
  dat$date <- date.frame[i]
  airnow.datalist[[i]] <- dat
}

airnow.current <- do.call(rbind, airnow.datalist)

airnow.current <- airnow.current %>% 
  rename(latitude = V1,
         longitude = V2, 
         date.old = V3, 
         parameter_name = V4, 
         raw_concentration = V5,
         AQI = V6,
         category_number = V7,
         location = V8, 
         agency = V9)  %>% 
  select(-date.old)

airnow.current$date <- parse_datetime(airnow.current$date)

category_reference <- data.frame(category_number = c(1, 2, 3, 4, 5, 6),
                                 category = c("good", "moderate", 
                                              "unhealthy for sensitive groups",
                                              "unhealthy", 
                                              "very unhealthy",
                                              "hazardous"))

airnow.current <- merge(airnow.current, category_reference)

# Merge Data --------------------------------------------------------------
hourly.current <- merge(airnow.current, traffic.current, by = "date")

hourly.combine <- rbind(hourly.old, hourly.current)

hourly.combine <- hourly.combine %>% 
  distinct()

write.csv(hourly.combine, file = "hourly.csv", row.names = FALSE)


