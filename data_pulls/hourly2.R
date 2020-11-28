# Packages ----------------------------------------------------------------
library(tidyverse)
library(httr)
library(xml2)
library(lubridate)
library(owmr)

hourly.old <- read_csv(str_c(getwd(), "/TOP/data/hourly.csv"),
                       col_types = cols(temp = col_number(),
                                        pressure = col_number(),
                                        humidity = col_number(),
                                        temp_min = col_number(),
                                        temp_max = col_number(),
                                        weather_main = col_character(),
                                        weather_description = col_character(),
                                        wind_speed = col_number(),
                                        wind_deg = col_number(),
                                        feels_like = col_number(),
                                        sunrise = col_datetime(),
                                        sunset = col_datetime()))

traffic.flow.old <- read_csv(str_c(getwd(), "/TOP/data/traffic_flow.csv"))

hourly.old$date <- lubridate::as_datetime(hourly.old$date)
traffic.flow.old$date <- lubridate::as_datetime(traffic.flow.old$date)

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
  dplyr::select(date, traffic_index_live, jams_delay, jams_length, jams_count)

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
  dplyr::select(-date.old)

airnow.current$date <- parse_datetime(airnow.current$date)

category_reference <- data.frame(category_number = c(1, 2, 3, 4, 5, 6),
                                 category = c("good", "moderate", 
                                              "unhealthy for sensitive groups",
                                              "unhealthy", 
                                              "very unhealthy",
                                              "hazardous"))

airnow.current <- merge(airnow.current, category_reference)

# Weather Data ------------------------------------------------------------
OWM_API_KEY <- owmr_settings("74c3f949312a3f8099ebeb5869517c74")
weather <- get_current("District of Columbia", units = "imperial") %>% 
  owmr_as_tibble() 

weather <- weather %>% 
  dplyr::select(temp:temp_max, weather_main, weather_description,
                wind_speed, wind_deg, dt_sunrise_txt, dt_sunset_txt,
                feels_like) %>% 
  rename(sunrise = dt_sunrise_txt,
         sunset = dt_sunset_txt) %>% 
  mutate(date = max(traffic.current$date),
         temp = as.numeric(temp),
         pressure = as.numeric(pressure),
         humidity = as.numeric(humidity),
         temp_min = as.numeric(temp_min),
         temp_max = as.numeric(temp_max),
         weather_main = as.character(weather_main),
         weather_description = as.character(weather_description),
         wind_speed = as.numeric(wind_speed),
         wind_deg = as.numeric(wind_deg),
         feels_like = as.numeric(feels_like))

# Merge Data --------------------------------------------------------------
hourly.current <- merge(airnow.current, traffic.current, by = "date")

hourly.current2 <- merge(hourly.current, weather, by = "date", all.x = TRUE)

hourly.combine <- rbind(hourly.old, hourly.current2)

hourly.combine <- hourly.combine %>% 
  distinct()

write.csv(hourly.combine, str_c(getwd(), "/TOP/data/hourly.csv"), row.names = FALSE)

# Traffic Flow ------------------------------------------------------------
street <- read_csv(str_c(getwd(), "/TOP/data/street.csv"))

coords <- data.frame(lat = as.character(street$lat),
                     lon = as.character(street$lon))

traffic.flow.url.frame <- "https://api.tomtom.com/traffic/services/4/flowSegmentData/absolute/10/xml?key=1IwzJekH63GrmYMnoGuAj5snLxHC2oOA&point=XXX,YYY"

traffic.flow.url.frame1 <- str_replace_all(traffic.flow.url.frame, 
                                           pattern = "XXX", 
                                           replacement = coords$lat)

traffic.flow.url <- str_replace_all(traffic.flow.url.frame1, 
                                    pattern = "YYY", 
                                    replacement = coords$lon)

traffic.flow.datalist <- list()

for(i in 1:length(traffic.flow.url)){
  traffic.flow.raw <- read_xml(traffic.flow.url[i])
  traffic.flow.raw.list <- as_list(traffic.flow.raw)
  traffic.flow.data <- as.data.frame(unlist(traffic.flow.raw.list))
  
  traffic.flow.datalist[[i]] <- 
    data.frame(lat = coords$lat[i],
               lon = coords$lon[i],
               frc = traffic.flow.data[1,],
               current_speed = traffic.flow.data[2,],
               free_flow_speed = traffic.flow.data[3,],
               current_travel_time = traffic.flow.data[4,],
               free_flow_travel_time = traffic.flow.data[5,],
               confidence = traffic.flow.data[6,],
               road_closure = traffic.flow.data[7,],
               all_lat_lon = 
                 str_c(traffic.flow.data[8:nrow(traffic.flow.data),], 
                       collapse = ", "),
               date = current.date.parsed)
  Sys.sleep(1) 
}

traffic.flow.current <- do.call(rbind, traffic.flow.datalist)

functional_road_class <- 
  data.frame(frc = c("FRC0", "FRC1", "FRC2", "FRC3", "FRC4",
                     "FRC5", "FRC6"),
             frc_name = c("motorway, freeway or other major road",
                          "major road, less important than a motorway",
                          "other major road",
                          "secondary road",
                          "local connecting road",
                          "local road of high importance",
                          "local road"))

traffic.flow.current <- merge(traffic.flow.current, functional_road_class)

traffic.flow.current <- merge(traffic.flow.current, street, by = c("lat", "lon"))

airnow.sensor.loc <- hourly.combine %>% 
  dplyr::select(latitude, longitude, parameter_name, location) %>% 
  distinct()

asl.o <- airnow.sensor.loc %>% 
  filter(airnow.sensor.loc$parameter_name == "OZONE")
asl.s <- airnow.sensor.loc %>% 
  filter(airnow.sensor.loc$parameter_name == "SO2")
asl.p <- airnow.sensor.loc %>% 
  filter(airnow.sensor.loc$parameter_name == "PM2.5")
asl.n <- airnow.sensor.loc %>% 
  filter(airnow.sensor.loc$parameter_name == "NO2")

traffic.flow.current$ozone_loc <- NA
traffic.flow.current$so2_loc <- NA
traffic.flow.current$pm2.5_loc <- NA
traffic.flow.current$no2_loc <- NA

traffic.flow.current <- traffic.flow.current %>% 
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon))

for(i in 1:nrow(traffic.flow.current)){
  position.o <- which.min(abs((traffic.flow.current$lat[i] - asl.o$latitude)) + 
                            abs((traffic.flow.current$lon[i] - asl.o$longitude)))
  
  position.s <- which.min(abs((traffic.flow.current$lat[i] - asl.s$latitude)) + 
                            abs((traffic.flow.current$lon[i] - asl.s$longitude)))
  
  position.p <- which.min(abs((traffic.flow.current$lat[i] - asl.p$latitude)) + 
                            abs((traffic.flow.current$lon[i] - asl.p$longitude)))
  
  position.n <- which.min(abs((traffic.flow.current$lat[i] - asl.n$latitude)) + 
                            abs((traffic.flow.current$lon[i] - asl.n$longitude)))
  
  traffic.flow.current$ozone_loc[i] <- asl.o[position.o,4]
  traffic.flow.current$so2_loc[i] <- asl.s[position.s,4]
  traffic.flow.current$pm2.5_loc[i] <- asl.p[position.p,4]
  traffic.flow.current$no2_loc[i] <- asl.n[position.n,4]
}

traffic.flow.current$anc <- str_replace(traffic.flow.current$anc, "ANC ", "")
traffic.flow.current$police_service_area <- 
  str_replace(traffic.flow.current$police_service_area, "Police Service Area ", "")
traffic.flow.current$ward <- 
  str_replace(traffic.flow.current$ward, "Ward ", "")
traffic.flow.current$neighborhood_cluster <-
  str_replace(traffic.flow.current$neighborhood_cluster, "Cluster ", "")
traffic.flow.current$police_district <- 
  str_replace(traffic.flow.current$police_district, "Police District - ", "")
traffic.flow.current$police_sector <- 
  str_replace(traffic.flow.current$police_sector, "Police Sector ", "")
traffic.flow.current$voter_precinct <- 
  str_replace(traffic.flow.current$voter_precinct, "Precinct ", "")
traffic.flow.current$single_member_district <-
  str_replace(traffic.flow.current$single_member_district, "SMD ", "")

traffic.flow.current <- traffic.flow.current %>% 
  mutate(single_member_district = dplyr::recode(single_member_district, "SMC 4A04" = "4A04"))

traffic.flow.current$single_member_district <- as.character(traffic.flow.current$single_member_district)

traffic.flow.combine <- rbind(traffic.flow.old, traffic.flow.current)

traffic.flow.combine <- apply(traffic.flow.combine,2,as.character)

write.csv(traffic.flow.combine, str_c(getwd(), "/TOP/data/traffic_flow.csv"), row.names = FALSE)


