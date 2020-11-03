# Packages ----------------------------------------------------------------
library(tidyverse)

# Hourly Traffic ----------------------------------------------------------
traffic.old <- read.csv("hourly_traffic.csv")

traffic.old$date <- parse_datetime(traffic.old$date)

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
  select(date, jams_delay, jams_length, jams_count)
  
traffic.current <- tail(traffic.data, n = 1)

traffic.combine <- rbind(traffic.old, traffic.current)

write.csv(traffic.combine, file = "hourly_traffic.csv", row.names = FALSE)



