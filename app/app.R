# Load Packages -----------------------------------------------------------
library(shiny)
library(tidyverse)

# Load Data ---------------------------------------------------------------
hourly <- read_csv("hourly.csv")
traffic.flow <- read_csv("traffic_flow.csv")

# Tidy Data ---------------------------------------------------------------
airnow.sensor.loc <- hourly %>% 
  select(latitude, longitude, parameter_name, location) %>% 
  distinct()

asl.o <- airnow.sensor.loc %>% 
  filter(airnow.sensor.loc$parameter_name == "OZONE")
asl.s <- airnow.sensor.loc %>% 
  filter(airnow.sensor.loc$parameter_name == "SO2")
asl.p <- airnow.sensor.loc %>% 
  filter(airnow.sensor.loc$parameter_name == "PM2.5")
asl.n <- airnow.sensor.loc %>% 
  filter(airnow.sensor.loc$parameter_name == "NO2")

traffic.flow$ozone_loc <- NA
traffic.flow$so2_loc <- NA
traffic.flow$pm2.5_loc <- NA
traffic.flow$no2_loc <- NA

for(i in 1:nrow(traffic.flow)){
  position.o <- which.min(abs((traffic.flow$lat[i] - asl.o$latitude)) + 
                            abs((traffic.flow$lon[i] - asl.o$longitude)))
  
  position.s <- which.min(abs((traffic.flow$lat[i] - asl.s$latitude)) + 
                            abs((traffic.flow$lon[i] - asl.s$longitude)))
  
  position.p <- which.min(abs((traffic.flow$lat[i] - asl.p$latitude)) + 
                            abs((traffic.flow$lon[i] - asl.p$longitude)))
  
  position.n <- which.min(abs((traffic.flow$lat[i] - asl.n$latitude)) + 
                            abs((traffic.flow$lon[i] - asl.n$longitude)))
  
  traffic.flow$ozone_loc[i] <- asl.o[position.o,4]
  traffic.flow$so2_loc[i] <- asl.s[position.s,4]
  traffic.flow$pm2.5_loc[i] <- asl.p[position.p,4]
  traffic.flow$no2_loc[i] <- asl.n[position.n,4]
}

an.slim <- hourly %>% 
  select(date, location, parameter_name, category, category_number, 
         raw_concentration, AQI, location)

an.slim.o <- subset(an.slim, parameter_name == "OZONE")
an.slim.s <- subset(an.slim, parameter_name == "SO2")
an.slim.p <- subset(an.slim, parameter_name == "PM2.5")
an.slim.n <- subset(an.slim, parameter_name == "NO2")

an.slim.o <- an.slim.o %>% 
  rename(ozone_loc = location, 
         ozone_loc_cat_num = category_number, 
         ozone_loc_raw_conc = raw_concentration,
         ozone_loc_aqi = AQI,
         ozone_loc_cat = category) %>% 
  select(-parameter_name)
an.slim.s <- an.slim.s %>% 
  rename(so2_loc = location, 
         so2_loc_cat_num = category_number, 
         so2_loc_raw_conc = raw_concentration,
         so2_loc_aqi = AQI,
         so2_loc_cat = category) %>% 
  select(-parameter_name)
an.slim.p <- an.slim.p %>% 
  rename(pm2.5_loc = location, 
         pm2.5_loc_cat_num = category_number, 
         pm2.5_loc_raw_conc = raw_concentration,
         pm2.5_loc_aqi = AQI,
         pm2.5_loc_cat = category) %>% 
  select(-parameter_name)
an.slim.n <- an.slim.n %>% 
  rename(no2_loc = location, 
         no2_loc_cat_num = category_number, 
         no2_loc_raw_conc = raw_concentration,
         no2_loc_aqi = AQI,
         no2_loc_cat = category) %>% 
  select(-parameter_name)

traffic.flow <- merge(traffic.flow, an.slim.o, by = c("date", "ozone_loc"))
traffic.flow <- merge(traffic.flow, an.slim.s, by = c("date", "so2_loc"))
traffic.flow <- merge(traffic.flow, an.slim.p, by = c("date", "pm2.5_loc"))
traffic.flow <- merge(traffic.flow, an.slim.n, by = c("date", "no2_loc"))

rm(an.slim, an.slim.n, an.slim.p, an.slim.s, an.slim.o,
   asl.n, asl.o, asl.p, asl.s)

# User Interface ----------------------------------------------------------
location.types <- c("Quadrant", "Ward", "Zip Code", 
                    "Advisory Neighborhood Commission",
                    "Census Tract",
                    "Single Member District",
                    "Voter Precinct")

ui <- fluidPage(
  titlePanel("Assessment of Air Quality and Traffic Volume"),
  h4("Washington, D.C."),
  
  tabsetPanel(
    tabPanel("Citywide"),
    tabPanel("Location Search",
             sidebarLayout(
               sidebarPanel(
                 selectInput("loc.type", "Which location type?",
                             choices = location.types, 
                             selected = "Ward")
               ),
               mainPanel(h3("Overview"))
             ))
  )
)

# Server ------------------------------------------------------------------


server <- function(input, output) {
  
  
}


# Run ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)