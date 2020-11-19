# Load Packages -----------------------------------------------------------
library(shiny)
library(tidyverse)
library(scales)
library(gridExtra)
library(grid)
library(png)
library(leaflet)
library(ggplot2)
library(shinyTime)
library(shinythemes)
library(lubridate)
library(stringr)
library(shinyjs)

# Load Data ---------------------------------------------------------------
hourly <- read_csv("hourly.csv")
traffic.flow <- read_csv("traffic_flow.csv")

agencies <- as.data.frame(unique(hourly$agency)) %>%
  rename(c("agency"="unique(hourly$agency)"))

parameters <- as.data.frame(unique(hourly$parameter_name)) %>%
  rename(c("agency"="unique(hourly$parameter_name)"))

parameter_wider <- hourly %>%
  group_by(date, location, parameter_name) %>%
  pivot_wider(names_from = parameter_name, values_from = AQI)

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

traffic.flow$anc <- str_replace(traffic.flow$anc, "ANC ", "")
traffic.flow$police_service_area <- 
  str_replace(traffic.flow$police_service_area, "Police Service Area ", "")
traffic.flow$ward <- 
  str_replace(traffic.flow$ward, "Ward ", "")
traffic.flow$neighborhood_cluster <-
  str_replace(traffic.flow$neighborhood_cluster, "Cluster ", "")
traffic.flow$police_district <- 
  str_replace(traffic.flow$police_district, "Police District - ", "")
traffic.flow$police_sector <- 
  str_replace(traffic.flow$police_sector, "Police Sector ", "")
traffic.flow$voter_precinct <- 
  str_replace(traffic.flow$voter_precinct, "Precinct ", "")
traffic.flow$single_member_district <-
  str_replace(traffic.flow$single_member_district, "SMD ", "")

traffic.flow$census_tract <-
  as.character(traffic.flow$census_tract)

for(i in 1:nrow(traffic.flow)){
  if(nchar(traffic.flow$census_tract[i]) == 3){
    traffic.flow$census_tract[i] <- str_c("000", traffic.flow$census_tract[i])
  }else if(nchar(traffic.flow$census_tract[i]) == 4){
    traffic.flow$census_tract[i] <- str_c("00", traffic.flow$census_tract[i])
  }else if(nchar(traffic.flow$census_tract[i]) == 5){
    traffic.flow$census_tract[i] <- str_c("0", traffic.flow$census_tract[i])
  }
}

traffic.flow$cat.average.num <- 
  round(((traffic.flow$ozone_loc_cat_num + 
            traffic.flow$so2_loc_cat_num + 
            traffic.flow$pm2.5_loc_cat_num + 
            traffic.flow$no2_loc_cat_num) / 4), digits = 0)
traffic.flow$cat.average <- NA

for(i in 1:nrow(traffic.flow)){
  if(traffic.flow$cat.average.num[i] == 1){
    traffic.flow$cat.average <- "good"
  }else if(traffic.flow$cat.average.num[i] == 2){
    traffic.flow$cat.average <- "moderate"
  }else if(traffic.flow$cat.average.num[i] == 3){
    traffic.flow$cat.average <- "unhealthy for sensitive groups"
  }else if(traffic.flow$cat.average.num[i] == 4){
    traffic.flow$cat.average <- "unhealthy"
  }else if(traffic.flow$cat.average.num[i] == 5){
    traffic.flow$cat.average <- "very unhealthy"
  }else if(traffic.flow$cat.average.num[i] == 6){
    traffic.flow$cat.average <- "hazardous"
  }
}

traffic.flow$cat.average <- str_to_title(traffic.flow$cat.average)

traffic.flow$zip_code <- as.character(traffic.flow$zip_code)

for(i in 1:nrow(hourly)){
  if(hourly$location[i] == "DCNearRoad"){
    hourly$location[i] <- "DC Near Road"
  }else if(hourly$location[i] == "RIVER_Terrace"){
    hourly$location[i] <- "River Terrace"
  }else if(hourly$location[i] == "TakomaRec"){
    hourly$location[i] <- "Tokoma Rec"
  }else if(hourly$location[i] == "AURORA HILLS"){
    hourly$location[i] <- "Aurora Hills"
  }
}

for(i in 1:nrow(parameter_wider)){
  if(parameter_wider$location[i] == "DCNearRoad"){
    parameter_wider$location[i] <- "DC Near Road"
  }else if(parameter_wider$location[i] == "RIVER_Terrace"){
    parameter_wider$location[i] <- "River Terrace"
  }else if(parameter_wider$location[i] == "TakomaRec"){
    parameter_wider$location[i] <- "Tokoma Rec"
  }else if(parameter_wider$location[i] == "AURORA HILLS"){
    parameter_wider$location[i] <- "Aurora Hills"
  }
}

traffic.flow <- traffic.flow %>% 
  mutate(single_member_district = dplyr::recode(single_member_district, "SMC 4A04" = "4A04"))


# User Interface ----------------------------------------------------------
location.types <- c("Quadrant", "Ward", "Zip Code", 
                    "Advisory Neighborhood Commission",
                    "Census Tract",
                    "Single Member District",
                    "Voter Precinct")

sensor.location <- c(unique(parameter_wider$location))
sensor.types <- c("Average Across Sensors", "Single Sensor")

current.date.time <- Sys.time()

c.date <- substr(current.date.time, start = 1, stop = 10)
c.time <- substr(current.date.time, start = 12, stop = 13)

ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("slate"),
  wellPanel(titlePanel("Assessment of Air Quality and Traffic Volume"),
            h4("Washington, D.C.")),
  
  tabsetPanel(
    tabPanel("Citywide",
             sidebarLayout(
               sidebarPanel(
                 h5("Metric Comparisons"),
                 checkboxInput("currenttime.o", "Current Date", value = TRUE), 
                 checkboxInput("lastweek.o", "Previous Week", value = TRUE), 
                 checkboxInput("historical.o", "Historical", value = TRUE),
                 dateInput("date2", "What day?",
                           value = c.date),
                 sliderInput("hour2", "Which hour?",
                             value = c.time, min = 0, max = 24),
                 selectInput("sensor.type", "Which sensor type?",
                             choices = sensor.types, 
                             selected = "Average Across Sensors"),
                 selectInput("location.type", "Which sensor location?",
                             choices = sensor.location, 
                             selected = "DC Near Road"),
                 uiOutput(outputId = "text1"),
                 checkboxInput("traffic", "On", value = TRUE),
                 uiOutput(outputId = "text2"),
                 checkboxInput("ozone", "OZONE", value = TRUE),
                 checkboxInput("so2", "SO2"),
                 checkboxInput("pm25", "PM2.5", value = TRUE),
                 checkboxInput("no2", "NO2"),
                 h5("Traffic Parameters"),
                 checkboxInput("tindex", "Traffic Index", value = TRUE), 
                 checkboxInput("jdelay", "Jams Delay"), 
                 checkboxInput("jlength", "Jams Length"),
                 checkboxInput("jcount", "Jams Count", value = TRUE)
               ),
               
               mainPanel(
                 tags$style(type="text/css",
                            ".shiny-output-error { visibility: hidden; }",
                            ".shiny-output-error:before { visibility: hidden; }"),
                 h3("Overview"),
                 textOutput("current.congestion.o"),
                 textOutput("current.air.quality.o"),
                 h4("Air Quality and Traffic"),
                 textOutput("airtable.title.o"),
                 tableOutput("aqitable"),
                 textOutput("airtable.title2.o"),
                 tableOutput("aqitable2"),
                 tableOutput("traffictable"),
                 plotOutput("airtable.graph2"),
                 h3("Daily AQI"),
                 plotOutput("myaqi"),
                 h3("Map"),
                 leafletOutput("mymap"))
             )),
    tabPanel("Location Search",
             sidebarLayout(
               sidebarPanel(
                 h5("Metric Comparisons"),
                 checkboxInput("currenttime", "Current Date", value = TRUE), 
                 checkboxInput("lastweek", "Previous Week", value = TRUE), 
                 checkboxInput("historical", "Historical", value = TRUE),
                 dateInput("date1", "What day?",
                           value = c.date),
                 sliderInput("hour", "Which hour?",
                             value = c.time, min = 0, max = 24),
                 selectInput("loc.type", "Which location type?",
                             choices = location.types, 
                             selected = "Quadrant"),
                 selectInput("loc.type2", "Which location?", "",
                             selected = ""),
                 span(textOutput("warning.text"), style = "color:red"),
                 h5("Air Quality Parameters"),
                 checkboxInput("ozone1", "Ozone", value = TRUE), 
                 checkboxInput("so2.1", "SO2"), 
                 checkboxInput("pm2.5.1", "PM 2.5", value = TRUE),
                 checkboxInput("no2.1", "NO2"),
                 h5("Traffic Parameters"),
                 checkboxInput("cspeed", "Current Speed", value = TRUE), 
                 checkboxInput("ffspeed", "Free Flow Speed", value = TRUE), 
                 checkboxInput("ctravel", "Current Travel Time"),
                 checkboxInput("fftravel", "Free Flow Travel Time")
               ),
               mainPanel(
                 tags$style(type="text/css",
                            ".shiny-output-error { visibility: hidden; }",
                            ".shiny-output-error:before { visibility: hidden; }"),
                 h3("Overview"),
                 textOutput("current.speed"),
                 textOutput("current.travel.time"),
                 textOutput("current.air.quality"),
                 h4("Air Quality and Traffic in Location"),
                 textOutput("airtable.title"),
                 tableOutput("airtable"),
                 textOutput("airtable.title2"),
                 tableOutput("airtable2"),
                 plotOutput("airtable.graph"),
                 h3("Within Location"),
                 plotOutput("location.graph"),
                 h3("Location Type Comparison"),
                 h4("Air Quality"),
                 plotOutput("loc.type.graph1"),
                 h4("Traffic Volume"),
                 plotOutput("loc.type.graph2"))
             )),
    tabPanel("Raw Citywide Data",
             dataTableOutput("static.city")),
    tabPanel("Raw Location Specific Data",
             dataTableOutput("static.location")),
    tabPanel("Background",
             h3("Traffic Data"),
             p("Citywide traffic data is pulled hourly from TomTom, a Dutch location technology firm. TomTom sources their traffic information from government and third-party data (e.g., induction loops in roads, cameras, and traffic surveillance) as well as new sources from anonymous mobile phone users. Data is obtained through TomTom's live traffic index for Washington, D.C. as well as their Traffic API. Below is the information available for the citywide and location search features."),
             h4("Citywide"),
             tableOutput("hourly.back"),
             h4("Location Search"),
             tableOutput("traffic.flow.back"),
             h3("Air Quality"),
             p("Air quality data is pulled from AirNow, the United States government's hub for air quality data. AirNow is a partnership between the U.S. Environmental Protection Agency, National Oceanic and Atmospheric Administration, National Park Service, NASA, Center for Disease Control, and tribal, state, and local air quality agencies. The data encompasses the U.S. Air Quality Index (AQI) which translates raw concentration levels of air pollutants into an index on whether the air quality is healthy or unhealthy."),
             tableOutput("airnow.back"),
             h5("Air Pollutants"),
             p("The AirNow sensors in Washington, D.C. include air quality data for four major pollutants - Ozone, Particulate Matter (PM 2.5), Sulfur Dioxide, and Nitrogen Dioxide. The recommended course of action for each of the pollutants and air quality levels of concern is provided below."),
             tableOutput("pollutants.back"))
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  observe({
    if(input$loc.type == "Quadrant"){
      x <- unique(traffic.flow$quadrant)
    }else if(input$loc.type == "Ward"){
      x <- unique(traffic.flow$ward)
    }else if(input$loc.type == "Zip Code"){
      x <- unique(traffic.flow$zip_code)
    }else if(input$loc.type == "Advisory Neighborhood Commission"){
      x <- unique(traffic.flow$anc)
    }else if(input$loc.type == "Census Tract"){
      x <- unique(traffic.flow$census_tract)
    }else if(input$loc.type == "Single Member District"){
      x <- unique(traffic.flow$single_member_district)
    }else if(input$loc.type == "Voter Precinct"){
      x <- unique(traffic.flow$voter_precinct)
    }
    
    updateSelectInput(session, inputId = "loc.type2", 
                      label = "Which location?", 
                      choices = x, selected = x[1])
  })
  
  ## Citywide Tab
  # Bold Labels
  output$text1 <- renderText({
    HTML(paste0("<b>","Turn on Traffic Index?","</b>"))
  })
  output$text2 <- renderText({
    HTML(paste0("<b>","Air Quality Parameters","</b>"))
  })
  
  ## Overview numbers
  output$current.air.quality.o <- renderText({
    
    cs1 <- hourly %>% 
      filter(date == parse_datetime(str_c(as.character(input$date2), 
                                          " ", 
                                          as.character(input$hour2), 
                                          ":00:00")))
    
    ls1 <- hourly %>% 
      filter(date == as_datetime(str_c(as.character(input$date2), 
                                       " ", 
                                       as.character(input$hour2), 
                                       ":00:00")) - 604800)
    
    if(input$sensor.type == "Average Across Sensors"){
      cs1 <- cs1
      ls1 <- ls1
    }else if(input$sensor.type == "Single Sensor"){
      cs1 <- cs1 %>% 
        filter(location == input$location.type)
      ls1 <- ls1 %>% 
        filter(location == input$location.type)
    }
    
    
    cs2 <- mean(cs1$category_number, na.rm = TRUE)
    ls2 <- mean(ls1$category_number, na.rm = TRUE)
    
    
    if(is.numeric(cs2) == TRUE){
      cs2 <- round(cs2, digits = 0) 
    }else if(is.na(cs2) == TRUE){
      cs2 <- "NaN"
    }
    
    if(is.numeric(ls2) == TRUE){
      ls2 <- round(ls2, digits = 0) 
    }else if(is.na(ls2) == TRUE){
      ls2 <- "NaN"
    }
    
    if(cs2 == 1){
      cs3 <- str_c("Current average air quality: ", "Good")
    }else if(cs2 == 2){
      cs3 <- str_c("Current average air quality: ", "Moderate")
    }else if(cs2 == 3){
      cs3 <- str_c("Current average air quality: ", "Unhealthy for sensitive groups")
    }else if(cs2 == 4){
      cs3 <- str_c("Current average air quality: ", "Unhealthy")
    }else if(cs2 == 5){
      cs3 <- str_c("Current average air quality: ", "Very unhealthy")
    }else if(cs2 == 6){
      cs3 <- str_c("Current average air quality: ", "Hazardous")
    }else if(cs2 == "NaN"){
      cs3 <- str_c("Current average air quality: ", "NaN")
    }
    
    if(ls2 == 1){
      ls3 <- str_c("Average air quality last week: ", "Good")
    }else if(ls2 == 2){
      ls3 <- str_c("Average air quality last week: ", "Moderate")
    }else if(ls2 == 3){
      ls3 <- str_c("Average air quality last week: ", "Unhealthy for sensitive groups")
    }else if(ls2 == 4){
      ls3 <- str_c("Average air quality last week: ", "Unhealthy")
    }else if(ls2 == 5){
      ls3 <- str_c("Average air quality last week: ", "Very unhealthy")
    }else if(ls2 == 6){
      ls3 <- str_c("Average air quality last week: ", "Hazardous")
    }else if(ls2 == "NaN"){
      ls3 <- str_c("Average air quality last week: ", "NaN")
    }
    
    if(input$currenttime.o == TRUE){ 
      if(input$lastweek.o == TRUE){ 
        str_c(cs3, " | ", ls3)
      }else{
        cs3
      }
    }else{
      if(input$lastweek.o == TRUE){ 
        ls3
      }else{
        
      }
    }
    
    
  })
  output$current.congestion.o <- renderText({
    
    cs1 <- hourly %>% 
      filter(date == parse_datetime(str_c(as.character(input$date2), 
                                          " ", 
                                          as.character(input$hour2), 
                                          ":00:00")))
    
    ls1 <- hourly %>% 
      filter(date == as_datetime(str_c(as.character(input$date2), 
                                       " ", 
                                       as.character(input$hour2), 
                                       ":00:00")) - 604800)
    
    cs2 <- mean(cs1$traffic_index_live, na.rm = TRUE)
    ls2 <- mean(ls1$traffic_index_live, na.rm = TRUE)
    
    cs3 <- str_c("Current traffic congestion level: ", cs2, "%")
    ls3 <- str_c("Traffic congestion level last week: ", ls2, "%")
    
    if(input$currenttime.o == TRUE){ 
      if(input$lastweek.o == TRUE){ 
        str_c(cs3, " | ", ls3)
      }else{
        cs3
      }
    }else{
      if(input$lastweek.o == TRUE){ 
        ls3
      }else{
        
      }
    }
    
    
  })
  
  ## Overview tables
  output$airtable.title.o <- renderText({
    if(input$currenttime.o == TRUE){
      str_c("Current Date")
    }
  })
  output$airtable.title2.o <- renderText({
    if(input$lastweek.o == TRUE){
      str_c("Last Week")
    }
  })
  output$aqitable <- renderTable({
    hourly <- hourly %>% 
      filter(date == parse_datetime(str_c(as.character(input$date2), 
                                          " ", 
                                          as.character(input$hour2), 
                                          ":00:00")))
    
    if(input$sensor.type == "Average Across Sensors"){
      hourly <- hourly
    }else if(input$sensor.type == "Single Sensor"){
      hourly <- hourly %>% 
        filter(location == input$location.type)
    }
    
    oz <- hourly %>%
      filter(parameter_name == "OZONE")
    so <- hourly %>%
      filter(parameter_name == "SO2")
    no <- hourly %>%
      filter(parameter_name == "NO2")
    pm <- hourly %>%
      filter(parameter_name == "PM2.5")
    
    df <- data.frame(stat = c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max."),
                     Ozone = c(round(summary(oz$AQI), digits = 2)),
                     SO2 = c(round(summary(so$AQI), digits = 2)),
                     PM2.5 = c(round(summary(pm$AQI), digits = 2)),
                     NO2 = c(round(summary(no$AQI), digits = 2)))
    
    if(input$ozone == TRUE){
      if(input$so2 == TRUE){
        if(input$pm25 == TRUE){
          if(input$no2 == TRUE){
            p2 <- df[,c(1, 2, 3, 4, 5)]
          }else if(input$no2 == FALSE){
            p2 <- df[,c(1, 2, 3, 4)]
          }
        }else if(input$pm25 == FALSE){
          if(input$no2 == TRUE){
            p2 <- df[,c(1, 2, 3, 5)]
          }else if(input$no2 == FALSE){
            p2 <- df[,c(1, 2, 3)]
          }
        }
      }else if(input$so2 == FALSE){
        if(input$pm25 == TRUE){
          if(input$no2 == TRUE){
            p2 <- df[,c(1, 2, 4, 5)]
          }else if(input$no2 == FALSE){
            p2 <- df[,c(1, 2, 4)]
          }
        }else if(input$pm25 == FALSE){
          if(input$no2 == TRUE){
            p2 <- df[,c(1, 2, 5)]
          }else if(input$no2 == FALSE){
            p2 <- df[,c(1, 2)]
          }
        }
      }
    }else if(input$ozone == FALSE){
      if(input$so2 == TRUE){
        if(input$pm25 == TRUE){
          if(input$no2 == TRUE){
            p2 <- df[,c(1, 3, 4, 5)]
          }else if(input$no2 == FALSE){
            p2 <- df[,c(1, 3, 4)]
          }
        }else if(input$pm25 == FALSE){
          if(input$no2 == TRUE){
            p2 <- df[,c(1, 3, 5)]
          }else if(input$no2 == FALSE){
            p2 <- df[,c(1, 3)]
          }
        }
      }else if(input$so2 == FALSE){
        if(input$pm25 == TRUE){
          if(input$no2 == TRUE){
            p2 <- df[,c(1, 4, 5)]
          }else if(input$no2 == FALSE){
            p2 <- df[,c(1, 4)]
          }
        }else if(input$pm25 == FALSE){
          if(input$no2 == TRUE){
            p2 <- df[,c(1, 5)]
          }else if(input$no2 == FALSE){
            p2 <- df[,c(1)]
          }
        }
      }
    }
    
    if(input$currenttime.o == TRUE){
      p2
    }
    
  })
  output$aqitable2 <- renderTable({
    hourly <- hourly %>% 
      filter(date == as_datetime(str_c(as.character(input$date2), 
                                       " ", 
                                       as.character(input$hour2), 
                                       ":00:00")) - 604800)
    
    if(input$sensor.type == "Average Across Sensors"){
      hourly <- hourly
    }else if(input$sensor.type == "Single Sensor"){
      hourly <- hourly %>% 
        filter(location == input$location.type)
    }
    
    oz <- hourly %>%
      filter(parameter_name == "OZONE")
    so <- hourly %>%
      filter(parameter_name == "SO2")
    no <- hourly %>%
      filter(parameter_name == "NO2")
    pm <- hourly %>%
      filter(parameter_name == "PM2.5")
    
    df <- data.frame(stat = c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max."),
                     Ozone = c(round(summary(oz$AQI), digits = 2)),
                     SO2 = c(round(summary(so$AQI), digits = 2)),
                     PM2.5 = c(round(summary(pm$AQI), digits = 2)),
                     NO2 = c(round(summary(no$AQI), digits = 2)))
    
    if(input$ozone == TRUE){
      if(input$so2 == TRUE){
        if(input$pm25 == TRUE){
          if(input$no2 == TRUE){
            p2 <- df[,c(1, 2, 3, 4, 5)]
          }else if(input$no2 == FALSE){
            p2 <- df[,c(1, 2, 3, 4)]
          }
        }else if(input$pm25 == FALSE){
          if(input$no2 == TRUE){
            p2 <- df[,c(1, 2, 3, 5)]
          }else if(input$no2 == FALSE){
            p2 <- df[,c(1, 2, 3)]
          }
        }
      }else if(input$so2 == FALSE){
        if(input$pm25 == TRUE){
          if(input$no2 == TRUE){
            p2 <- df[,c(1, 2, 4, 5)]
          }else if(input$no2 == FALSE){
            p2 <- df[,c(1, 2, 4)]
          }
        }else if(input$pm25 == FALSE){
          if(input$no2 == TRUE){
            p2 <- df[,c(1, 2, 5)]
          }else if(input$no2 == FALSE){
            p2 <- df[,c(1, 2)]
          }
        }
      }
    }else if(input$ozone == FALSE){
      if(input$so2 == TRUE){
        if(input$pm25 == TRUE){
          if(input$no2 == TRUE){
            p2 <- df[,c(1, 3, 4, 5)]
          }else if(input$no2 == FALSE){
            p2 <- df[,c(1, 3, 4)]
          }
        }else if(input$pm25 == FALSE){
          if(input$no2 == TRUE){
            p2 <- df[,c(1, 3, 5)]
          }else if(input$no2 == FALSE){
            p2 <- df[,c(1, 3)]
          }
        }
      }else if(input$so2 == FALSE){
        if(input$pm25 == TRUE){
          if(input$no2 == TRUE){
            p2 <- df[,c(1, 4, 5)]
          }else if(input$no2 == FALSE){
            p2 <- df[,c(1, 4)]
          }
        }else if(input$pm25 == FALSE){
          if(input$no2 == TRUE){
            p2 <- df[,c(1, 5)]
          }else if(input$no2 == FALSE){
            p2 <- df[,c(1)]
          }
        }
      }
    }
    
    if(input$lastweek.o == TRUE){
      p2
    }
    
  })
  output$traffictable <- renderTable({
    cs1 <- hourly %>% 
      filter(date == parse_datetime(str_c(as.character(input$date2), 
                                          " ", 
                                          as.character(input$hour2), 
                                          ":00:00")))
    df1 <- data.frame(time = c("current"),
                      traffic_index = c(round(summary(cs1$traffic_index_live), digits = 2)),
                      jams_delay = c(round(summary(cs1$jams_delay), digits = 2)),
                      jams_length = c(round(summary(cs1$jams_length), digits = 2)),
                      jams_count = c(round(summary(cs1$jams_count), digits = 2)))
    
    ls1 <- hourly %>% 
      filter(date == as_datetime(str_c(as.character(input$date2), 
                                       " ", 
                                       as.character(input$hour2), 
                                       ":00:00")) - 604800)
    
    df2 <- data.frame(time = c("last week"),
                      traffic_index = c(round(summary(ls1$traffic_index_live), digits = 2)),
                      jams_delay = c(round(summary(ls1$jams_delay), digits = 2)),
                      jams_length = c(round(summary(ls1$jams_length), digits = 2)),
                      jams_count = c(round(summary(ls1$jams_count), digits = 2)))
    
    if(input$tindex == TRUE){
      if(input$jdelay == TRUE){
        if(input$jlength == TRUE){
          if(input$jcount == TRUE){
            df1 <- df1[,c(1, 2, 3, 4, 5)]
          }else if(input$jcount == FALSE){
            df1 <- df1[,c(1, 2, 3, 4)]
          }
        }else if(input$jlength == FALSE){
          if(input$jcount == TRUE){
            df1 <- df1[,c(1, 2, 3, 5)]
          }else if(input$jcount == FALSE){
            df1 <- df1[,c(1, 2, 3)]
          }
        }
      }else if(input$jdelay == FALSE){
        if(input$jlength == TRUE){
          if(input$jcount == TRUE){
            df1 <- df1[,c(1, 2, 4, 5)]
          }else if(input$jcount == FALSE){
            df1 <- df1[,c(1, 2, 4)]
          }
        }else if(input$jlength == FALSE){
          if(input$jcount == TRUE){
            df1 <- df1[,c(1, 2, 5)]
          }else if(input$jcount == FALSE){
            df1 <- df1[,c(1, 2)]
          }
        }
      }
    }else if(input$tindex == FALSE){
      if(input$jdelay == TRUE){
        if(input$jlength == TRUE){
          if(input$jcount == TRUE){
            df1 <- df1[,c(1, 3, 4, 5)]
          }else if(input$jcount == FALSE){
            df1 <- df1[,c(1, 3, 4)]
          }
        }else if(input$jlength == FALSE){
          if(input$jcount == TRUE){
            df1 <- df1[,c(1, 3, 5)]
          }else if(input$jcount == FALSE){
            df1 <- df1[,c(1, 3)]
          }
        }
      }else if(input$jdelay == FALSE){
        if(input$jlength == TRUE){
          if(input$jcount == TRUE){
            df1 <- df1[,c(1, 4, 5)]
          }else if(input$jcount == FALSE){
            df1 <- df1[,c(1, 4)]
          }
        }else if(input$jlength == FALSE){
          if(input$jcount == TRUE){
            df1 <- df1[,c(1, 5)]
          }else if(input$jcount == FALSE){
            df1 <- df1[,c(1)]
          }
        }
      }
    }
    
    if(input$tindex == TRUE){
      if(input$jdelay == TRUE){
        if(input$jlength == TRUE){
          if(input$jcount == TRUE){
            df2 <- df2[,c(1, 2, 3, 4, 5)]
          }else if(input$jcount == FALSE){
            df2 <- df2[,c(1, 2, 3, 4)]
          }
        }else if(input$jlength == FALSE){
          if(input$jcount == TRUE){
            df2 <- df2[,c(1, 2, 3, 5)]
          }else if(input$jcount == FALSE){
            df2 <- df2[,c(1, 2, 3)]
          }
        }
      }else if(input$jdelay == FALSE){
        if(input$jlength == TRUE){
          if(input$jcount == TRUE){
            df2 <- df2[,c(1, 2, 4, 5)]
          }else if(input$jcount == FALSE){
            df2 <- df2[,c(1, 2, 4)]
          }
        }else if(input$jlength == FALSE){
          if(input$jcount == TRUE){
            df2 <- df2[,c(1, 2, 5)]
          }else if(input$jcount == FALSE){
            df2 <- df2[,c(1, 2)]
          }
        }
      }
    }else if(input$tindex == FALSE){
      if(input$jdelay == TRUE){
        if(input$jlength == TRUE){
          if(input$jcount == TRUE){
            df2 <- df2[,c(1, 3, 4, 5)]
          }else if(input$jcount == FALSE){
            df2 <- df2[,c(1, 3, 4)]
          }
        }else if(input$jlength == FALSE){
          if(input$jcount == TRUE){
            df2 <- df2[,c(1, 3, 5)]
          }else if(input$jcount == FALSE){
            df2 <- df2[,c(1, 3)]
          }
        }
      }else if(input$jdelay == FALSE){
        if(input$jlength == TRUE){
          if(input$jcount == TRUE){
            df2 <- df2[,c(1, 4, 5)]
          }else if(input$jcount == FALSE){
            df2 <- df2[,c(1, 4)]
          }
        }else if(input$jlength == FALSE){
          if(input$jcount == TRUE){
            df2 <- df2[,c(1, 5)]
          }else if(input$jcount == FALSE){
            df2 <- df2[,c(1)]
          }
        }
      }
    }
    
    if(input$currenttime.o == TRUE){
      if(input$lastweek.o == TRUE){
        rbind(df1[1,], df2[1,])
      }else{
        df1[1,]
      }
    }else{
      if(input$lastweek.o == TRUE){
        df2[2,]
      }else{
        
      }
    }
    
  })
  
  ## Bar chart 
  output$airtable.graph2 <- renderPlot({
    if(input$currenttime.o == TRUE & input$lastweek.o == TRUE){
      if(input$sensor.type == "Average Across Sensors"){  
        x <- parameter_wider %>% 
          filter(agency == "District of Columbia - Department of Energy and Environment")
      }else if(input$sensor.type == "Single Sensor"){
        x <- parameter_wider %>% 
          filter(location == input$location.type)
      }
      
      cs1 <- x %>% 
        filter(date == parse_datetime(str_c(as.character(input$date2), 
                                            " ", 
                                            as.character(input$hour2), 
                                            ":00:00")))
      
      ls1 <- x %>% 
        filter(date == as_datetime(str_c(as.character(input$date2), 
                                         " ", 
                                         as.character(input$hour2), 
                                         ":00:00")) - 604800)
      
      com.s1 <- rbind(cs1, ls1)
      
      cs3 <- com.s1 %>% 
        group_by(date) %>% 
        summarize(Ozone = mean(OZONE, na.rm = TRUE),
                  SO2 = mean(SO2, na.rm = TRUE),
                  PM2.5 = mean(PM2.5, na.rm = TRUE),
                  NO2 = mean(NO2, na.rm = TRUE))
      
      if(input$ozone == TRUE){
        if(input$so2 == TRUE){
          if(input$pm25 == TRUE){
            if(input$no2 == TRUE){
              cs3 <- cs3[,c(1, 2, 3, 4, 5)]
            }else if(input$no2 == FALSE){
              cs3 <- cs3[,c(1, 2, 3, 4)]
            }
          }else if(input$pm25 == FALSE){
            if(input$no2 == TRUE){
              cs3 <- cs3[,c(1, 2, 3, 5)]
            }else if(input$no2 == FALSE){
              cs3 <- cs3[,c(1, 2, 3)]
            }
          }
        }else if(input$so2 == FALSE){
          if(input$pm25 == TRUE){
            if(input$no2 == TRUE){
              cs3 <- cs3[,c(1, 2, 4, 5)]
            }else if(input$no2 == FALSE){
              cs3 <- cs3[,c(1, 2, 4)]
            }
          }else if(input$pm25 == FALSE){
            if(input$no2 == TRUE){
              cs3 <- cs3[,c(1, 2, 5)]
            }else if(input$no2 == FALSE){
              cs3 <- cs3[,c(1, 2)]
            }
          }
        }
      }else if(input$ozone == FALSE){
        if(input$so2 == TRUE){
          if(input$pm25 == TRUE){
            if(input$no2 == TRUE){
              cs3 <- cs3[,c(1, 3, 4, 5)]
            }else if(input$no2 == FALSE){
              cs3 <- cs3[,c(1, 3, 4)]
            }
          }else if(input$pm25 == FALSE){
            if(input$no2 == TRUE){
              cs3 <- cs3[,c(1, 3, 5)]
            }else if(input$no2 == FALSE){
              cs3 <- cs3[,c(1, 3)]
            }
          }
        }else if(input$so2 == FALSE){
          if(input$pm25 == TRUE){
            if(input$no2 == TRUE){
              cs3 <- cs3[,c(1, 4, 5)]
            }else if(input$no2 == FALSE){
              cs3 <- cs3[,c(1, 4)]
            }
          }else if(input$pm25 == FALSE){
            if(input$no2 == TRUE){
              cs3 <- cs3[,c(1, 5)]
            }else if(input$no2 == FALSE){
              cs3 <- cs3[,c(1)]
            }
          }
        }
      }
      
      cs3 <- pivot_longer(cs3, -date, names_to = "param")
      
      cs3$date <- str_c(substr(cs3$date, start = 1, stop = 10), " T",
                        substr(cs3$date, start = 12, stop = 13))
      
      p2 <- ggplot(data = cs3, aes(x = param, y = value, fill = as.character(date))) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(x = "Air Quality Parameters", y = "Air Quality Index", fill = "Date",
             title = "Weekly Comparison of AQI and Traffic") +
        annotate("text", x = unique(cs3$param)[1], y = 45, label = "Healthy") +
        geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
        annotate("text", x = unique(cs3$param)[1], y = 95, label = "Moderate") +
        geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
        annotate("text", x = unique(cs3$param)[1], y = 145, label = "Unhealthy for Sensitive Groups") +
        geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1)
      
      cs4 <- com.s1 %>% 
        group_by(date) %>% 
        summarize(Traffic_Index = mean(traffic_index_live, na.rm = TRUE),
                  Jams_Delay = mean(jams_delay, na.rm = TRUE),
                  Jams_Length = mean(jams_length, na.rm = TRUE),
                  Jams_Count = mean(jams_count, na.rm = TRUE))
      
      if(input$tindex == TRUE){
        if(input$jdelay == TRUE){
          if(input$jlength == TRUE){
            if(input$jcount == TRUE){
              cs4 <- cs4[,c(1, 2, 3, 4, 5)]
            }else if(input$jcount == FALSE){
              cs4 <- cs4[,c(1, 2, 3, 4)]
            }
          }else if(input$jlength == FALSE){
            if(input$jcount == TRUE){
              cs4 <- cs4[,c(1, 2, 3, 5)]
            }else if(input$jcount == FALSE){
              cs4 <- cs4[,c(1, 2, 3)]
            }
          }
        }else if(input$jdelay == FALSE){
          if(input$jlength == TRUE){
            if(input$jcount == TRUE){
              cs4 <- cs4[,c(1, 2, 4, 5)]
            }else if(input$jcount == FALSE){
              cs4 <- cs4[,c(1, 2, 4)]
            }
          }else if(input$jlength == FALSE){
            if(input$jcount == TRUE){
              cs4 <- cs4[,c(1, 2, 5)]
            }else if(input$jcount == FALSE){
              cs4 <- cs4[,c(1, 2)]
            }
          }
        }
      }else if(input$tindex == FALSE){
        if(input$jdelay == TRUE){
          if(input$jlength == TRUE){
            if(input$jcount == TRUE){
              cs4 <- cs4[,c(1, 3, 4, 5)]
            }else if(input$jcount == FALSE){
              cs4 <- cs4[,c(1, 3, 4)]
            }
          }else if(input$jlength == FALSE){
            if(input$jcount == TRUE){
              cs4 <- cs4[,c(1, 3, 5)]
            }else if(input$jcount == FALSE){
              cs4 <- cs4[,c(1, 3)]
            }
          }
        }else if(input$jdelay == FALSE){
          if(input$jlength == TRUE){
            if(input$jcount == TRUE){
              cs4 <- cs4[,c(1, 4, 5)]
            }else if(input$jcount == FALSE){
              cs4 <- cs4[,c(1, 4)]
            }
          }else if(input$jlength == FALSE){
            if(input$jcount == TRUE){
              cs4 <- cs4[,c(1, 5)]
            }else if(input$jcount == FALSE){
              cs4 <- cs4[,c(1)]
            }
          }
        }
      }
      
      cs4 <- pivot_longer(cs4, -date, names_to = "param")
      
      cs4$date <- str_c(substr(cs4$date, start = 1, stop = 10), " T",
                        substr(cs4$date, start = 12, stop = 13))
      
      p1 <- ggplot(data = cs4, aes(x = param, y = value, fill = as.character(date))) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(x = "Traffic Parameters", y = "Traffic Value", fill = "Date")
      
      grid.arrange(p2, p1, ncol = 2)
    }
  })
  
  ## Overview graph for the Overview tab
  output$myaqi <- renderPlot({
    if(input$historical.o == TRUE){
      if(input$sensor.type == "Average Across Sensors"){
        x <- parameter_wider %>% 
          filter(agency == "District of Columbia - Department of Energy and Environment")
        
      }else if(input$sensor.type == "Single Sensor"){
        x <- parameter_wider %>% 
          filter(location == input$location.type)
      }
      
      param.slim <- parameter_wider %>% 
        select(date, traffic_index_live) 
      
      param.slim <- param.slim[,2:3]
      
      param.slim <- param.slim %>% 
        distinct()
      
      x <- x %>% 
        group_by(date) %>% 
        summarize(Ozone = mean(OZONE, na.rm = TRUE),
                  SO2 = mean(SO2, na.rm = TRUE),
                  PM2.5 = mean(PM2.5, na.rm = TRUE),
                  NO2 = mean(NO2, na.rm = TRUE))
      
      x <- merge(x, param.slim, by = "date")
      
      colors <- c("Ozone" = "blue", "PM2.5" = "orange", 
                  "SO2" = "red", "NO2" = "green")
      
      #ptitle <- str_c("AQI and Traffic")
      p1 <- 
        ggplot(data = x, aes(x = date)) +
        labs(x = "Date", y = "Air Quality Index",
             title = "AQI and Traffic",
             color = "Parameter") +
        scale_color_manual(values = colors) +
        annotate("text", x = as_datetime("2020-10-28 22:00:00"), y = 45, label = "Healthy") +
        geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1.5) +
        annotate("text", x = as_datetime("2020-10-28 22:00:00"), y = 95, label = "Moderate") +
        geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1.5) +
        annotate("text", x = as_datetime("2020-10-31 18:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
        geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1.5)
      
      po <- geom_line(aes(y = Ozone, color = "Ozone"))
      pc <- geom_line(aes(y = SO2, color = "SO2"))
      ph <- geom_line(aes(y = PM2.5, color = "PM2.5"))
      pl <- geom_line(aes(y = NO2, color = "NO2"))
      #pt <- labs(title = ptitle, x = "Date", y = "")
      
      if(input$ozone == TRUE){
        if(input$so2 == TRUE){
          if(input$pm25 == TRUE){
            if(input$no2 == TRUE){
              plot1 <- p1 + po + pc + ph + pl #+ pt
            } else if(input$no2 == FALSE){
              plot1 <- p1 + po + pc + ph #+ pt
            }
          }else if(input$pm25 == FALSE){
            if(input$no2 == TRUE){
              plot1 <- p1 + po + pc + pl #+ pt
            } else if(input$no2 == FALSE){
              plot1 <- p1 + po + pc #+ pt
            }
          }
        } else if(input$so2 == FALSE){
          if(input$pm25 == TRUE){
            if(input$no2 == TRUE){
              plot1 <- p1 + po + ph + pl #+ pt
            } else if(input$no2 == FALSE){
              plot1 <- p1 + po + ph #+ pt
            }
          }else if(input$pm25 == FALSE){
            if(input$no2 == TRUE){
              plot1 <- p1 + po + pl #+ pt
            } else if(input$no2 == FALSE){
              plot1 <- p1 + po #+ pt
            }
          }
        }
      } else if(input$ozone == FALSE){
        if(input$so2 == TRUE){
          if(input$pm25 == TRUE){
            if(input$no2 == TRUE){
              plot1 <- p1 + pc + ph + pl #+ pt
            } else if(input$no2 == FALSE){
              plot1 <- p1 + pc + ph #+ pt
            }
          }else if(input$pm25 == FALSE){
            if(input$no2 == TRUE){
              plot1 <- p1 + pc + pl #+ pt
            } else if(input$no2 == FALSE){
              plot1 <- p1 + pc #+ pt
            }
          }
        } else if(input$so2 == FALSE){
          if(input$pm25 == TRUE){
            if(input$no2 == TRUE){
              plot1 <- p1 + ph + pl #+ pt
            } else if(input$no2 == FALSE){
              plot1 <- p1 + ph #+ pt
            }
          }else if(input$pm25 == FALSE){
            if(input$no2 == TRUE){
              plot1 <- p1 + pl #+ pt
            } else if(input$no2 == FALSE){
              plot1 <- p1 #+ pt
            }
          }
        }
      }
      
      param.slim <- parameter_wider %>% 
        select(date, traffic_index_live, jams_delay, jams_length, jams_count) 
      
      param.slim <- param.slim[,2:6]
      
      param.slim <- param.slim %>% 
        distinct() %>% 
        rename(Traffic_Index = traffic_index_live,
               Jams_Delay = jams_delay,
               Jams_Length = jams_length,
               Jams_Count = jams_count)
      
      colors <- c("Traffic_Index" = "blue", "Jams_Delay" = "orange", 
                  "Jams_Length" = "red", "Jams_Count" = "green")
      
      #ptitle <- str_c("AQI and Traffic")
      p1 <- 
        ggplot(data = param.slim, aes(x = date)) +
        labs(x = "Date", y = "Traffic Value", title = " ",
             color = "Parameter") +
        scale_color_manual(values = colors) 
      
      po <- geom_line(aes(y = Traffic_Index, color = "Traffic_Index"))
      pc <- geom_line(aes(y = Jams_Delay, color = "Jams_Delay"))
      ph <- geom_line(aes(y = Jams_Length, color = "Jams_Length"))
      pl <- geom_line(aes(y = Jams_Count, color = "Jams_Count"))
      #pt <- labs(title = ptitle, x = "Date", y = "")
      
      if(input$tindex == TRUE){
        if(input$jdelay == TRUE){
          if(input$jlength == TRUE){
            if(input$jcount == TRUE){
              plot2 <- p1 + po + pc + ph + pl #+ pt
            } else if(input$jcount == FALSE){
              plot2 <- p1 + po + pc + ph #+ pt
            }
          }else if(input$jlength == FALSE){
            if(input$jcount == TRUE){
              plot2 <- p1 + po + pc + pl #+ pt
            } else if(input$jcount == FALSE){
              plot2 <- p1 + po + pc #+ pt
            }
          }
        } else if(input$jdelay == FALSE){
          if(input$jlength == TRUE){
            if(input$jcount == TRUE){
              plot2 <- p1 + po + ph + pl #+ pt
            } else if(input$jcount == FALSE){
              plot2 <- p1 + po + ph #+ pt
            }
          }else if(input$jlength == FALSE){
            if(input$jcount == TRUE){
              plot2 <- p1 + po + pl #+ pt
            } else if(input$jcount == FALSE){
              plot2 <- p1 + po #+ pt
            }
          }
        }
      } else if(input$tindex == FALSE){
        if(input$jdelay == TRUE){
          if(input$jlength == TRUE){
            if(input$jcount == TRUE){
              plot2 <- p1 + pc + ph + pl #+ pt
            } else if(input$jcount == FALSE){
              plot2 <- p1 + pc + ph #+ pt
            }
          }else if(input$jlength == FALSE){
            if(input$jcount == TRUE){
              plot2 <- p1 + pc + pl #+ pt
            } else if(input$jcount == FALSE){
              plot2 <- p1 + pc #+ pt
            }
          }
        } else if(input$jdelay == FALSE){
          if(input$jlength == TRUE){
            if(input$jcount == TRUE){
              plot2 <- p1 + ph + pl #+ pt
            } else if(input$jcount == FALSE){
              plot2 <- p1 + ph #+ pt
            }
          }else if(input$jlength == FALSE){
            if(input$jcount == TRUE){
              plot2 <- p1 + pl #+ pt
            } else if(input$jcount == FALSE){
              plot2 <- p1 #+ pt
            }
          }
        }
      }
      grid.arrange(plot1, plot2, ncol = 2)
    }
  })
  
  ## Map for the Overview tab
  output$mymap <- renderLeaflet({
    if(input$sensor.type == "Average Across Sensors"){
      hourly <- hourly
    }else if(input$sensor.type == "Single Sensor"){
      hourly <- hourly %>% 
        filter(location == input$location.type)
    }
    
    if(input$traffic==TRUE){
      leaflet(hourly) %>% addTiles() %>%
        addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                   radius = ~(AQI+12)*10, popup = ~location, color = "#FF0000") %>%
        addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                   radius = ~(traffic_index_live+5)*10, popup = ~location, 
                   color = "#000000") %>%
        addLegend("bottomright", colors =c("#FF0000",  "#000000"),
                  labels= c("AQI", "Traffic"), 
                  opacity = 1)
    }else{
      leaflet(hourly) %>% addTiles() %>%
        addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                   radius = ~(AQI+12)*10, popup = ~location, color = "#FF0000") %>% 
        addLegend("bottomright", colors =c("#FF0000"),
                  labels= c("AQI"), 
                  opacity = 1)
      
    }
  })
  
  ## Current and Last Week Spec Tables
  output$airtable.title <- renderText({
    if(input$currenttime == TRUE){
      str_c("Current Date")
    }
  })
  output$airtable.title2 <- renderText({
    if(input$lastweek == TRUE){
      str_c("Last Week")
    }
  })
  output$airtable <- renderTable({
    if(input$currenttime == TRUE){
      cs1 <- traffic.flow %>% 
        filter(date == parse_datetime(str_c(as.character(input$date1), 
                                            " ", 
                                            as.character(input$hour), 
                                            ":00:00")))
      
      if(input$loc.type == "Quadrant"){
        cs2 <- cs1 %>% filter(quadrant == input$loc.type2)
      }else if(input$loc.type == "Ward"){
        cs2 <- cs1 %>% filter(ward == input$loc.type2)
      }else if(input$loc.type == "Zip Code"){
        cs2 <- cs1 %>% filter(zip_code == input$loc.type2)
      }else if(input$loc.type == "Advisory Neighborhood Commission"){
        cs2 <- cs1 %>% filter(anc == input$loc.type2)
      }else if(input$loc.type == "Census Tract"){
        cs2 <- cs1 %>% filter(census_tract == input$loc.type2)
      }else if(input$loc.type == "Single Member District"){
        cs2 <- cs1 %>% filter(single_member_district == input$loc.type2)
      }else if(input$loc.type == "Voter Precinct"){
        cs2 <- cs1 %>% filter(voter_precinct == input$loc.type2)
      }
      
      df1 <- data.frame(stat = c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max."),
                        Ozone = c(round(summary(cs2$ozone_loc_aqi), digits = 2)),
                        SO2 = c(round(summary(cs2$so2_loc_aqi), digits = 2)),
                        PM2.5 = c(round(summary(cs2$pm2.5_loc_aqi), digits = 2)),
                        NO2 = c(round(summary(cs2$no2_loc_aqi), digits = 2)))
      
      if(input$ozone1 == TRUE){
        if(input$so2.1 == TRUE){
          if(input$pm2.5.1 == TRUE){
            if(input$no2.1 == TRUE){
              p2 <- df1[,c(1, 2, 3, 4, 5)]
            }else if(input$no2.1 == FALSE){
              p2 <- df1[,c(1, 2, 3, 4)]
            }
          }else if(input$pm2.5.1 == FALSE){
            if(input$no2.1 == TRUE){
              p2 <- df1[,c(1, 2, 3, 5)]
            }else if(input$no2.1 == FALSE){
              p2 <- df1[,c(1, 2, 3)]
            }
          }
        }else if(input$so2.1 == FALSE){
          if(input$pm2.5.1 == TRUE){
            if(input$no2.1 == TRUE){
              p2 <- df1[,c(1, 2, 4, 5)]
            }else if(input$no2.1 == FALSE){
              p2 <- df1[,c(1, 2, 4)]
            }
          }else if(input$pm2.5.1 == FALSE){
            if(input$no2.1 == TRUE){
              p2 <- df1[,c(1, 2, 5)]
            }else if(input$no2.1 == FALSE){
              p2 <- df1[,c(1, 2)]
            }
          }
        }
      }else if(input$ozone1 == FALSE){
        if(input$so2.1 == TRUE){
          if(input$pm2.5.1 == TRUE){
            if(input$no2.1 == TRUE){
              p2 <- df1[,c(1, 3, 4, 5)]
            }else if(input$no2.1 == FALSE){
              p2 <- df1[,c(1, 3, 4)]
            }
          }else if(input$pm2.5.1 == FALSE){
            if(input$no2.1 == TRUE){
              p2 <- df1[,c(1, 3, 5)]
            }else if(input$no2.1 == FALSE){
              p2 <- df1[,c(1, 3)]
            }
          }
        }else if(input$so2.1 == FALSE){
          if(input$pm2.5.1 == TRUE){
            if(input$no2.1 == TRUE){
              p2 <- df1[,c(1, 4, 5)]
            }else if(input$no2.1 == FALSE){
              p2 <- df1[,c(1, 4)]
            }
          }else if(input$pm2.5.1 == FALSE){
            if(input$no2.1 == TRUE){
              p2 <- df1[,c(1, 5)]
            }else if(input$no2.1 == FALSE){
              p2 <- df1[,c(1)]
            }
          }
        }
      }
      
      df1 <- data.frame(stat = c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max."),
                        C.Speed = c(round(summary(cs2$current_speed), digits = 2)),
                        FF.Speed = c(round(summary(cs2$free_flow_speed), digits = 2)),
                        C.Travel.Time = c(round(summary(cs2$current_travel_time), digits = 2)),
                        FF.Travel.Time = c(round(summary(cs2$free_flow_travel_time), digits = 2)))
      
      if(input$cspeed == TRUE){
        if(input$ffspeed == TRUE){
          if(input$ctravel == TRUE){
            if(input$fftravel == TRUE){
              p1 <- df1[,c(1, 2, 3, 4, 5)]
            }else if(input$fftravel == FALSE){
              p1 <- df1[,c(1, 2, 3, 4)]
            }
          }else if(input$ctravel == FALSE){
            if(input$fftravel == TRUE){
              p1 <- df1[,c(1, 2, 3, 5)]
            }else if(input$fftravel == FALSE){
              p1 <- df1[,c(1, 2, 3)]
            }
          }
        }else if(input$ffspeed == FALSE){
          if(input$ctravel == TRUE){
            if(input$fftravel == TRUE){
              p1 <- df1[,c(1, 2, 4, 5)]
            }else if(input$fftravel == FALSE){
              p1 <- df1[,c(1, 2, 4)]
            }
          }else if(input$ctravel == FALSE){
            if(input$fftravel == TRUE){
              p1 <- df1[,c(1, 2, 5)]
            }else if(input$fftravel == FALSE){
              p1 <- df1[,c(1, 2)]
            }
          }
        }
      }else if(input$cspeed == FALSE){
        if(input$ffspeed == TRUE){
          if(input$ctravel == TRUE){
            if(input$fftravel == TRUE){
              p1 <- df1[,c(1, 3, 4, 5)]
            }else if(input$fftravel == FALSE){
              p1 <- df1[,c(1, 3, 4)]
            }
          }else if(input$ctravel == FALSE){
            if(input$fftravel == TRUE){
              p1 <- df1[,c(1, 3, 5)]
            }else if(input$fftravel == FALSE){
              p1 <- df1[,c(1, 3)]
            }
          }
        }else if(input$ffspeed == FALSE){
          if(input$ctravel == TRUE){
            if(input$fftravel == TRUE){
              p1 <- df1[,c(1, 4, 5)]
            }else if(input$fftravel == FALSE){
              p1 <- df1[,c(1, 4)]
            }
          }else if(input$ctravel == FALSE){
            if(input$fftravel == TRUE){
              p1 <- df1[,c(1, 5)]
            }else if(input$fftravel == FALSE){
              p1 <- df1[,c(1)]
            }
          }
        }
      }
      
      merge(p2, p1, by = "stat")
    }
  })
  output$airtable2 <- renderTable({
    if(input$lastweek == TRUE){
      ls1 <- traffic.flow %>% 
        filter(date == as_datetime(str_c(as.character(input$date1), 
                                         " ", 
                                         as.character(input$hour), 
                                         ":00:00")) - 604800)
      
      if(input$loc.type == "Quadrant"){
        ls2 <- ls1 %>% filter(quadrant == input$loc.type2)
      }else if(input$loc.type == "Ward"){
        ls2 <- ls1 %>% filter(ward == input$loc.type2)
      }else if(input$loc.type == "Zip Code"){
        ls2 <- ls1 %>% filter(zip_code == input$loc.type2)
      }else if(input$loc.type == "Advisory Neighborhood Commission"){
        ls2 <- ls1 %>% filter(anc == input$loc.type2)
      }else if(input$loc.type == "Census Tract"){
        ls2 <- ls1 %>% filter(census_tract == input$loc.type2)
      }else if(input$loc.type == "Single Member District"){
        ls2 <- ls1 %>% filter(single_member_district == input$loc.type2)
      }else if(input$loc.type == "Voter Precinct"){
        ls2 <- ls1 %>% filter(voter_precinct == input$loc.type2)
      }
      
      df1 <- data.frame(stat = c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max."),
                        Ozone = c(round(summary(ls2$ozone_loc_aqi), digits = 2)),
                        SO2 = c(round(summary(ls2$so2_loc_aqi), digits = 2)),
                        PM2.5 = c(round(summary(ls2$pm2.5_loc_aqi), digits = 2)),
                        NO2 = c(round(summary(ls2$no2_loc_aqi), digits = 2)))
      
      if(input$ozone1 == TRUE){
        if(input$so2.1 == TRUE){
          if(input$pm2.5.1 == TRUE){
            if(input$no2.1 == TRUE){
              p2 <- df1[,c(1, 2, 3, 4, 5)]
            }else if(input$no2.1 == FALSE){
              p2 <- df1[,c(1, 2, 3, 4)]
            }
          }else if(input$pm2.5.1 == FALSE){
            if(input$no2.1 == TRUE){
              p2 <- df1[,c(1, 2, 3, 5)]
            }else if(input$no2.1 == FALSE){
              p2 <- df1[,c(1, 2, 3)]
            }
          }
        }else if(input$so2.1 == FALSE){
          if(input$pm2.5.1 == TRUE){
            if(input$no2.1 == TRUE){
              p2 <- df1[,c(1, 2, 4, 5)]
            }else if(input$no2.1 == FALSE){
              p2 <- df1[,c(1, 2, 4)]
            }
          }else if(input$pm2.5.1 == FALSE){
            if(input$no2.1 == TRUE){
              p2 <- df1[,c(1, 2, 5)]
            }else if(input$no2.1 == FALSE){
              p2 <- df1[,c(1, 2)]
            }
          }
        }
      }else if(input$ozone1 == FALSE){
        if(input$so2.1 == TRUE){
          if(input$pm2.5.1 == TRUE){
            if(input$no2.1 == TRUE){
              p2 <- df1[,c(1, 3, 4, 5)]
            }else if(input$no2.1 == FALSE){
              p2 <- df1[,c(1, 3, 4)]
            }
          }else if(input$pm2.5.1 == FALSE){
            if(input$no2.1 == TRUE){
              p2 <- df1[,c(1, 3, 5)]
            }else if(input$no2.1 == FALSE){
              p2 <- df1[,c(1, 3)]
            }
          }
        }else if(input$so2.1 == FALSE){
          if(input$pm2.5.1 == TRUE){
            if(input$no2.1 == TRUE){
              p2 <- df1[,c(1, 4, 5)]
            }else if(input$no2.1 == FALSE){
              p2 <- df1[,c(1, 4)]
            }
          }else if(input$pm2.5.1 == FALSE){
            if(input$no2.1 == TRUE){
              p2 <- df1[,c(1, 5)]
            }else if(input$no2.1 == FALSE){
              p2 <- df1[,c(1)]
            }
          }
        }
      }
      
      df1 <- data.frame(stat = c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max."),
                        C.Speed = c(round(summary(ls2$current_speed), digits = 2)),
                        FF.Speed = c(round(summary(ls2$free_flow_speed), digits = 2)),
                        C.Travel.Time = c(round(summary(ls2$current_travel_time), digits = 2)),
                        FF.Travel.Time = c(round(summary(ls2$free_flow_travel_time), digits = 2)))
      
      if(input$cspeed == TRUE){
        if(input$ffspeed == TRUE){
          if(input$ctravel == TRUE){
            if(input$fftravel == TRUE){
              p1 <- df1[,c(1, 2, 3, 4, 5)]
            }else if(input$fftravel == FALSE){
              p1 <- df1[,c(1, 2, 3, 4)]
            }
          }else if(input$ctravel == FALSE){
            if(input$fftravel == TRUE){
              p1 <- df1[,c(1, 2, 3, 5)]
            }else if(input$fftravel == FALSE){
              p1 <- df1[,c(1, 2, 3)]
            }
          }
        }else if(input$ffspeed == FALSE){
          if(input$ctravel == TRUE){
            if(input$fftravel == TRUE){
              p1 <- df1[,c(1, 2, 4, 5)]
            }else if(input$fftravel == FALSE){
              p1 <- df1[,c(1, 2, 4)]
            }
          }else if(input$ctravel == FALSE){
            if(input$fftravel == TRUE){
              p1 <- df1[,c(1, 2, 5)]
            }else if(input$fftravel == FALSE){
              p1 <- df1[,c(1, 2)]
            }
          }
        }
      }else if(input$cspeed == FALSE){
        if(input$ffspeed == TRUE){
          if(input$ctravel == TRUE){
            if(input$fftravel == TRUE){
              p1 <- df1[,c(1, 3, 4, 5)]
            }else if(input$fftravel == FALSE){
              p1 <- df1[,c(1, 3, 4)]
            }
          }else if(input$ctravel == FALSE){
            if(input$fftravel == TRUE){
              p1 <- df1[,c(1, 3, 5)]
            }else if(input$fftravel == FALSE){
              p1 <- df1[,c(1, 3)]
            }
          }
        }else if(input$ffspeed == FALSE){
          if(input$ctravel == TRUE){
            if(input$fftravel == TRUE){
              p1 <- df1[,c(1, 4, 5)]
            }else if(input$fftravel == FALSE){
              p1 <- df1[,c(1, 4)]
            }
          }else if(input$ctravel == FALSE){
            if(input$fftravel == TRUE){
              p1 <- df1[,c(1, 5)]
            }else if(input$fftravel == FALSE){
              p1 <- df1[,c(1)]
            }
          }
        }
      }
      
      merge(p2, p1, by = "stat")
    }
  })
  
  ## Week Comparison Bar Chart
  output$airtable.graph <- renderPlot({
    if(input$currenttime == TRUE & input$lastweek == TRUE){
      cs1 <- traffic.flow %>% 
        filter(date == parse_datetime(str_c(as.character(input$date1), 
                                            " ", 
                                            as.character(input$hour), 
                                            ":00:00")))
      
      ls1 <- traffic.flow %>% 
        filter(date == as_datetime(str_c(as.character(input$date1), 
                                         " ", 
                                         as.character(input$hour), 
                                         ":00:00")) - 604800)
      
      com.s1 <- rbind(cs1, ls1)
      
      cs2 <- com.s1 %>% filter(quadrant == "SE")
      
      if(input$loc.type == "Quadrant"){
        cs2 <- com.s1 %>% filter(quadrant == input$loc.type2)
      }else if(input$loc.type == "Ward"){
        cs2 <- com.s1 %>% filter(ward == input$loc.type2)
      }else if(input$loc.type == "Zip Code"){
        cs2 <- com.s1 %>% filter(zip_code == input$loc.type2)
      }else if(input$loc.type == "Advisory Neighborhood Commission"){
        cs2 <- com.s1 %>% filter(anc == input$loc.type2)
      }else if(input$loc.type == "Census Tract"){
        cs2 <- com.s1 %>% filter(census_tract == input$loc.type2)
      }else if(input$loc.type == "Single Member District"){
        cs2 <- com.s1 %>% filter(single_member_district == input$loc.type2)
      }else if(input$loc.type == "Voter Precinct"){
        cs2 <- com.s1 %>% filter(voter_precinct == input$loc.type2)
      }
      
      cs3 <- cs2 %>% 
        group_by(date) %>% 
        summarize(Ozone = mean(ozone_loc_aqi, na.rm = TRUE),
                  SO2 = mean(so2_loc_aqi, na.rm = TRUE),
                  PM2.5 = mean(pm2.5_loc_aqi, na.rm = TRUE),
                  NO2 = mean(no2_loc_aqi, na.rm = TRUE))
      
      if(input$ozone1 == TRUE){
        if(input$so2.1 == TRUE){
          if(input$pm2.5.1 == TRUE){
            if(input$no2.1 == TRUE){
              cs3 <- cs3[,c(1, 2, 3, 4, 5)]
            }else if(input$no2.1 == FALSE){
              cs3 <- cs3[,c(1, 2, 3, 4)]
            }
          }else if(input$pm2.5.1 == FALSE){
            if(input$no2.1 == TRUE){
              cs3 <- cs3[,c(1, 2, 3, 5)]
            }else if(input$no2.1 == FALSE){
              cs3 <- cs3[,c(1, 2, 3)]
            }
          }
        }else if(input$so2.1 == FALSE){
          if(input$pm2.5.1 == TRUE){
            if(input$no2.1 == TRUE){
              cs3 <- cs3[,c(1, 2, 4, 5)]
            }else if(input$no2.1 == FALSE){
              cs3 <- cs3[,c(1, 2, 4)]
            }
          }else if(input$pm2.5.1 == FALSE){
            if(input$no2.1 == TRUE){
              cs3 <- cs3[,c(1, 2, 5)]
            }else if(input$no2.1 == FALSE){
              cs3 <- cs3[,c(1, 2)]
            }
          }
        }
      }else if(input$ozone1 == FALSE){
        if(input$so2.1 == TRUE){
          if(input$pm2.5.1 == TRUE){
            if(input$no2.1 == TRUE){
              cs3 <- cs3[,c(1, 3, 4, 5)]
            }else if(input$no2.1 == FALSE){
              cs3 <- cs3[,c(1, 3, 4)]
            }
          }else if(input$pm2.5.1 == FALSE){
            if(input$no2.1 == TRUE){
              cs3 <- cs3[,c(1, 3, 5)]
            }else if(input$no2.1 == FALSE){
              cs3 <- cs3[,c(1, 3)]
            }
          }
        }else if(input$so2.1 == FALSE){
          if(input$pm2.5.1 == TRUE){
            if(input$no2.1 == TRUE){
              cs3 <- cs3[,c(1, 4, 5)]
            }else if(input$no2.1 == FALSE){
              cs3 <- cs3[,c(1, 4)]
            }
          }else if(input$pm2.5.1 == FALSE){
            if(input$no2.1 == TRUE){
              cs3 <- cs3[,c(1, 5)]
            }else if(input$no2.1 == FALSE){
              cs3 <- cs3[,c(1)]
            }
          }
        }
      }
      
      cs3 <- pivot_longer(cs3, -date, names_to = "param")
      
      cs3$date <- str_c(substr(cs3$date, start = 1, stop = 10), " T",
                        substr(cs3$date, start = 12, stop = 13))
      
      p2 <- ggplot(data = cs3, aes(x = param, y = value, fill = as.character(date))) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(x = "Air Quality Parameters", y = "Air Quality Index", fill = "Date",
             title = "Weekly Comparison of AQI and Traffic") +
        annotate("text", x = unique(cs3$param)[1], y = 45, label = "Healthy") +
        geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
        annotate("text", x = unique(cs3$param)[1], y = 95, label = "Moderate") +
        geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
        annotate("text", x = unique(cs3$param)[1], y = 145, label = "Unhealthy for Sensitive Groups") +
        geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1)
      
      cs4 <- cs2 %>% 
        group_by(date) %>% 
        summarize(Current_Speed = mean(current_speed, na.rm = TRUE),
                  Free_Flow_Speed = mean(free_flow_speed, na.rm = TRUE),
                  Current_Travel_Time = mean(current_travel_time, na.rm = TRUE),
                  Free_Flow_Travel_Time = mean(free_flow_travel_time, na.rm = TRUE))
      
      if(input$cspeed == TRUE){
        if(input$ffspeed == TRUE){
          if(input$ctravel == TRUE){
            if(input$fftravel == TRUE){
              cs4 <- cs4[,c(1, 2, 3, 4, 5)]
            }else if(input$fftravel == FALSE){
              cs4 <- cs4[,c(1, 2, 3, 4)]
            }
          }else if(input$ctravel == FALSE){
            if(input$fftravel == TRUE){
              cs4 <- cs4[,c(1, 2, 3, 5)]
            }else if(input$fftravel == FALSE){
              cs4 <- cs4[,c(1, 2, 3)]
            }
          }
        }else if(input$ffspeed == FALSE){
          if(input$ctravel == TRUE){
            if(input$fftravel == TRUE){
              cs4 <- cs4[,c(1, 2, 4, 5)]
            }else if(input$fftravel == FALSE){
              cs4 <- cs4[,c(1, 2, 4)]
            }
          }else if(input$ctravel == FALSE){
            if(input$fftravel == TRUE){
              cs4 <- cs4[,c(1, 2, 5)]
            }else if(input$fftravel == FALSE){
              cs4 <- cs4[,c(1, 2)]
            }
          }
        }
      }else if(input$cspeed == FALSE){
        if(input$ffspeed == TRUE){
          if(input$ctravel == TRUE){
            if(input$fftravel == TRUE){
              cs4 <- cs4[,c(1, 3, 4, 5)]
            }else if(input$fftravel == FALSE){
              cs4 <- cs4[,c(1, 3, 4)]
            }
          }else if(input$ctravel == FALSE){
            if(input$fftravel == TRUE){
              cs4 <- cs4[,c(1, 3, 5)]
            }else if(input$fftravel == FALSE){
              cs4 <- cs4[,c(1, 3)]
            }
          }
        }else if(input$ffspeed == FALSE){
          if(input$ctravel == TRUE){
            if(input$fftravel == TRUE){
              cs4 <- cs4[,c(1, 4, 5)]
            }else if(input$fftravel == FALSE){
              cs4 <- cs4[,c(1, 4)]
            }
          }else if(input$ctravel == FALSE){
            if(input$fftravel == TRUE){
              cs4 <- cs4[,c(1, 5)]
            }else if(input$fftravel == FALSE){
              cs4 <- cs4[,c(1)]
            }
          }
        }
      }
      
      cs4 <- pivot_longer(cs4, -date, names_to = "param")
      
      cs4$date <- str_c(substr(cs4$date, start = 1, stop = 10), " T",
                        substr(cs4$date, start = 12, stop = 13))
      
      p1 <- ggplot(data = cs4, aes(x = param, y = value, fill = as.character(date))) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(x = "Traffic Parameters", y = "Traffic Value", fill = "Date")
      
      grid.arrange(p2, p1, ncol = 2)
    }
  })
  
  ## Overview Numbers 
  output$current.speed <- renderText({
    cs1 <- traffic.flow %>% 
      filter(date == parse_datetime(str_c(as.character(input$date1), 
                                          " ", 
                                          as.character(input$hour), 
                                          ":00:00")))
    
    ls1 <- traffic.flow %>% 
      filter(date == as_datetime(str_c(as.character(input$date1), 
                                       " ", 
                                       as.character(input$hour), 
                                       ":00:00")) - 604800)
    
    if(input$loc.type == "Quadrant"){
      cs2 <- mean(cs1$current_speed[cs1$quadrant == input$loc.type2], na.rm = TRUE)
      cs3 <- mean(cs1$free_flow_speed[cs1$quadrant == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Ward"){
      cs2 <- mean(cs1$current_speed[cs1$ward == input$loc.type2], na.rm = TRUE)
      cs3 <- mean(cs1$free_flow_speed[cs1$ward == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Zip Code"){
      cs2 <- mean(cs1$current_speed[cs1$zip_code == input$loc.type2], na.rm = TRUE)
      cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Advisory Neighborhood Commission"){
      cs2 <- mean(cs1$current_speed[cs1$anc == input$loc.type2], na.rm = TRUE)
      cs3 <- mean(cs1$free_flow_speed[cs1$anc == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Census Tract"){
      cs2 <- mean(cs1$current_speed[cs1$census_tract == input$loc.type2], na.rm = TRUE)
      cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Single Member District"){
      cs2 <- mean(cs1$current_speed[cs1$single_member_district == input$loc.type2], na.rm = TRUE)
      cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Voter Precinct"){
      cs2 <- mean(cs1$current_speed[cs1$voter_precinct == input$loc.type2], na.rm = TRUE)
      cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == input$loc.type2], na.rm = TRUE)
    }
    
    if(input$loc.type == "Quadrant"){
      ls2 <- mean(ls1$current_speed[ls1$quadrant == input$loc.type2], na.rm = TRUE)
      ls3 <- mean(ls1$free_flow_speed[ls1$quadrant == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Ward"){
      ls2 <- mean(ls1$current_speed[ls1$ward == input$loc.type2], na.rm = TRUE)
      ls3 <- mean(ls1$free_flow_speed[ls1$ward == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Zip Code"){
      ls2 <- mean(ls1$current_speed[ls1$zip_code == input$loc.type2], na.rm = TRUE)
      ls3 <- mean(ls1$free_flow_speed[ls1$zip_code == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Advisory Neighborhood Commission"){
      ls2 <- mean(ls1$current_speed[ls1$anc == input$loc.type2], na.rm = TRUE)
      ls3 <- mean(ls1$free_flow_speed[ls1$anc == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Census Tract"){
      ls2 <- mean(ls1$current_speed[ls1$census_tract == input$loc.type2], na.rm = TRUE)
      ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Single Member District"){
      ls2 <- mean(ls1$current_speed[ls1$single_member_district == input$loc.type2], na.rm = TRUE)
      ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Voter Precinct"){
      ls2 <- mean(ls1$current_speed[ls1$voter_precinct == input$loc.type2], na.rm = TRUE)
      ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == input$loc.type2], na.rm = TRUE)
    }
    
    
    
    if(input$currenttime == TRUE){ 
      if(input$lastweek == TRUE){
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph) | ", 
              "Speed last week: ", round(ls2, digits = 0), " mph (Free flow speed: ", round(ls3, digits = 0), " mph)")
      }else{
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }
    }else{
      if(input$lastweek == TRUE){
        str_c("Speed last week: ", round(ls2, digits = 0), " mph (Free flow speed: ", round(ls3, digits = 0), " mph)")
      }else{
        
      }
    }
    
    
  })
  output$current.travel.time <- renderText({
    cs1 <- traffic.flow %>% 
      filter(date == parse_datetime(str_c(as.character(input$date1), 
                                          " ", 
                                          as.character(input$hour), 
                                          ":00:00")))
    
    ls1 <- traffic.flow %>% 
      filter(date == as_datetime(str_c(as.character(input$date1), 
                                       " ", 
                                       as.character(input$hour), 
                                       ":00:00")) - 604800)
    
    if(input$loc.type == "Quadrant"){
      cs2 <- mean(cs1$current_travel_time[cs1$quadrant == input$loc.type2], na.rm = TRUE)
      cs3 <- mean(cs1$free_flow_travel_time[cs1$quadrant == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Ward"){
      cs2 <- mean(cs1$current_travel_time[cs1$ward == input$loc.type2], na.rm = TRUE)
      cs3 <- mean(cs1$free_flow_travel_time[cs1$ward == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Zip Code"){
      cs2 <- mean(cs1$current_travel_time[cs1$zip_code == input$loc.type2], na.rm = TRUE)
      cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Advisory Neighborhood Commission"){
      cs2 <- mean(cs1$current_travel_time[cs1$anc == input$loc.type2], na.rm = TRUE)
      cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Census Tract"){
      cs2 <- mean(cs1$current_travel_time[cs1$census_tract == input$loc.type2], na.rm = TRUE)
      cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Single Member District"){
      cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == input$loc.type2], na.rm = TRUE)
      cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Voter Precinct"){
      cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == input$loc.type2], na.rm = TRUE)
      cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == input$loc.type2], na.rm = TRUE)
    }
    
    if(input$loc.type == "Quadrant"){
      ls2 <- mean(ls1$current_travel_time[ls1$quadrant == input$loc.type2], na.rm = TRUE)
      ls3 <- mean(ls1$free_flow_travel_time[ls1$quadrant == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Ward"){
      ls2 <- mean(ls1$current_travel_time[ls1$ward == input$loc.type2], na.rm = TRUE)
      ls3 <- mean(ls1$free_flow_travel_time[ls1$ward == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Zip Code"){
      ls2 <- mean(ls1$current_travel_time[ls1$zip_code == input$loc.type2], na.rm = TRUE)
      ls3 <- mean(ls1$free_flow_travel_time[ls1$zip_code == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Advisory Neighborhood Commission"){
      ls2 <- mean(ls1$current_travel_time[ls1$anc == input$loc.type2], na.rm = TRUE)
      ls3 <- mean(ls1$free_flow_travel_time[ls1$anc == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Census Tract"){
      ls2 <- mean(ls1$current_travel_time[ls1$census_tract == input$loc.type2], na.rm = TRUE)
      ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Single Member District"){
      ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == input$loc.type2], na.rm = TRUE)
      ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Voter Precinct"){
      ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == input$loc.type2], na.rm = TRUE)
      ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == input$loc.type2], na.rm = TRUE)
    }  
    
    if(input$currenttime == TRUE){ 
      if(input$lastweek == TRUE){
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph) | ",
              "Travel time last week: ", round(ls2, digits = 0), " mph (Free flow travel time: ", round(ls3, digits = 0), " mph)")
      }else{
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }
    }else{
      if(input$lastweek == TRUE){
        str_c("Travel time last week: ", round(ls2, digits = 0), " mph (Free flow travel time: ", round(ls3, digits = 0), " mph)")
      }else{
        
      }
    }
    
    
  })
  output$current.air.quality <- renderText({
    cs1 <- traffic.flow %>% 
      filter(date == parse_datetime(str_c(as.character(input$date1), 
                                          " ", 
                                          as.character(input$hour), 
                                          ":00:00")))
    
    ls1 <- traffic.flow %>% 
      filter(date == as_datetime(str_c(as.character(input$date1), 
                                       " ", 
                                       as.character(input$hour), 
                                       ":00:00")) - 604800)
    
    if(input$loc.type == "Quadrant"){
      cs2 <- mean(cs1$cat.average.num[cs1$quadrant == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Ward"){
      cs2 <- mean(cs1$cat.average.num[cs1$ward == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Zip Code"){
      cs2 <- mean(cs1$cat.average.num[cs1$zip_code == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Advisory Neighborhood Commission"){
      cs2 <- mean(cs1$cat.average.num[cs1$anc == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Census Tract"){
      cs2 <- mean(cs1$cat.average.num[cs1$census_tract == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Single Member District"){
      cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Voter Precinct"){
      cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == input$loc.type2], na.rm = TRUE)
    }
    
    if(input$loc.type == "Quadrant"){
      ls2 <- mean(ls1$cat.average.num[ls1$quadrant == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Ward"){
      ls2 <- mean(ls1$cat.average.num[ls1$ward == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Zip Code"){
      ls2 <- mean(ls1$cat.average.num[ls1$zip_code == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Advisory Neighborhood Commission"){
      ls2 <- mean(ls1$cat.average.num[ls1$anc == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Census Tract"){
      ls2 <- mean(ls1$cat.average.num[ls1$census_tract == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Single Member District"){
      ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == input$loc.type2], na.rm = TRUE)
    }else if(input$loc.type == "Voter Precinct"){
      ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == input$loc.type2], na.rm = TRUE)
    }
    
    if(is.numeric(cs2) == TRUE){
      cs2 <- round(cs2, digits = 0) 
    }else if(is.na(cs2) == TRUE){
      cs2 <- "NaN"
    }
    
    if(is.numeric(ls2) == TRUE){
      ls2 <- round(ls2, digits = 0) 
    }else if(is.na(ls2) == TRUE){
      ls2 <- "NaN"
    }
    
    if(cs2 == 1){
      cs3 <- str_c("Current average air quality: ", "Good")
    }else if(cs2 == 2){
      cs3 <- str_c("Current average air quality: ", "Moderate")
    }else if(cs2 == 3){
      cs3 <- str_c("Current average air quality: ", "Unhealthy for sensitive groups")
    }else if(cs2 == 4){
      cs3 <- str_c("Current average air quality: ", "Unhealthy")
    }else if(cs2 == 5){
      cs3 <- str_c("Current average air quality: ", "Very unhealthy")
    }else if(cs2 == 6){
      cs3 <- str_c("Current average air quality: ", "Hazardous")
    }else if(cs2 == "NaN"){
      cs3 <- str_c("Current average air quality: ", "NaN")
    }
    
    if(ls2 == 1){
      ls3 <- str_c("Average air quality last week: ", "Good")
    }else if(ls2 == 2){
      ls3 <- str_c("Average air quality last week: ", "Moderate")
    }else if(ls2 == 3){
      ls3 <- str_c("Average air quality last week: ", "Unhealthy for sensitive groups")
    }else if(ls2 == 4){
      ls3 <- str_c("Average air quality last week: ", "Unhealthy")
    }else if(ls2 == 5){
      ls3 <- str_c("Average air quality last week: ", "Very unhealthy")
    }else if(ls2 == 6){
      ls3 <- str_c("Average air quality last week: ", "Hazardous")
    }else if(ls2 == "NaN"){
      ls3 <- str_c("Average air quality last week: ", "NaN")
    }
    
    if(input$currenttime == TRUE){ 
      if(input$lastweek == TRUE){ 
        str_c(cs3, " | ", ls3)
      }else{
        cs3
      }
    }else{
      if(input$lastweek == TRUE){ 
        ls3
      }else{
        
      }
    }
    
  })
  
  ## Historical Graphs
  output$location.graph <- renderPlot({
    if(input$historical == TRUE){
      if(input$loc.type == "Quadrant"){
        cs2 <- traffic.flow %>% filter(quadrant == input$loc.type2)
      }else if(input$loc.type == "Ward"){
        cs2 <- traffic.flow %>% filter(ward == input$loc.type2)
      }else if(input$loc.type == "Zip Code"){
        cs2 <- traffic.flow %>% filter(zip_code == input$loc.type2)
      }else if(input$loc.type == "Advisory Neighborhood Commission"){
        cs2 <- traffic.flow %>% filter(anc == input$loc.type2)
      }else if(input$loc.type == "Census Tract"){
        cs2 <- traffic.flow %>% filter(census_tract == input$loc.type2)
      }else if(input$loc.type == "Single Member District"){
        cs2 <- traffic.flow %>% filter(single_member_district == input$loc.type2)
      }else if(input$loc.type == "Voter Precinct"){
        cs2 <- traffic.flow %>% filter(voter_precinct == input$loc.type2)
      }
      
      cs3 <- cs2 %>% 
        group_by(date) %>% 
        summarize(mean_ozone_loc_aqi = mean(ozone_loc_aqi, na.rm = TRUE),
                  mean_so2_loc_aqi = mean(so2_loc_aqi, na.rm = TRUE),
                  mean_pm2.5_loc_aqi = mean(pm2.5_loc_aqi, na.rm = TRUE),
                  mean_no2_loc_aqi = mean(no2_loc_aqi, na.rm = TRUE))
      
      cs4 <- cs2 %>% 
        group_by(date) %>% 
        summarize(mean_current_speed = mean(current_speed, na.rm = TRUE),
                  mean_free_flow_speed = mean(free_flow_speed, na.rm = TRUE),
                  mean_current_travel_time = mean(current_travel_time, na.rm = TRUE),
                  mean_free_flow_travel_time = mean(free_flow_travel_time, na.rm = TRUE))
      
      colors <- c("Ozone" = "blue", "SO2" = "red", "PM 2.5" = "Orange",
                  "NO2" = "green")
      
      g <- cs3 %>% 
        ggplot(data = ., aes(x = date)) +
        labs(x = "Date", y = "Air Quality Index", 
             title = str_c(input$loc.type, ": ", input$loc.type2),
             color = "Parameter") +
        scale_color_manual(values = colors)  +
        annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
        geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
        annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
        geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
        annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
        geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1)
      g1 <- geom_line(aes(y = mean_ozone_loc_aqi, color = "Ozone"))
      g2 <- geom_line(aes(y = mean_so2_loc_aqi, color = "SO2"))
      g3 <- geom_line(aes(y = mean_pm2.5_loc_aqi, color = "PM 2.5"))
      g4 <- geom_line(aes(y = mean_no2_loc_aqi, color = "NO2"))
      
      if(input$ozone1 == TRUE){
        if(input$so2.1 == TRUE){
          if(input$pm2.5.1 == TRUE){
            if(input$no2.1 == TRUE){
              p1 <- g + g1 + g2 + g3 + g4
            }else if(input$no2.1 == FALSE){
              p1 <- g + g1 + g2 + g3
            }
          }else if(input$pm2.5.1 == FALSE){
            if(input$no2.1 == TRUE){
              p1 <- g + g1 + g2 + g4
            }else if(input$no2.1 == FALSE){
              p1 <- g + g1 + g2
            }
          }
        }else if(input$so2.1 == FALSE){
          if(input$pm2.5.1 == TRUE){
            if(input$no2.1 == TRUE){
              p1 <- g + g1 + g3 + g4
            }else if(input$no2.1 == FALSE){
              p1 <- g + g1 + g3
            }
          }else if(input$pm2.5.1 == FALSE){
            if(input$no2.1 == TRUE){
              p1 <- g + g1 + g4
            }else if(input$no2.1 == FALSE){
              p1 <- g + g1
            }
          }
        }
      }else if(input$ozone1 == FALSE){
        if(input$so2.1 == TRUE){
          if(input$pm2.5.1 == TRUE){
            if(input$no2.1 == TRUE){
              p1 <- g + g2 + g3 + g4
            }else if(input$no2.1 == FALSE){
              p1 <- g + g2 + g3
            }
          }else if(input$pm2.5.1 == FALSE){
            if(input$no2.1 == TRUE){
              p1 <- g + g2 + g4
            }else if(input$no2.1 == FALSE){
              p1 <- g + g2
            }
          }
        }else if(input$so2.1 == FALSE){
          if(input$pm2.5.1 == TRUE){
            if(input$no2.1 == TRUE){
              p1 <- g + g3 + g4
            }else if(input$no2.1 == FALSE){
              p1 <- g + g3
            }
          }else if(input$pm2.5.1 == FALSE){
            if(input$no2.1 == TRUE){
              p1 <- g + g4 
            }else if(input$no2.1 == FALSE){
              p1 <- g
            }
          }
        }
      }
      
      colors <- c("Current Speed" = "blue", "Free Flow Speed" = "red", 
                  "Current Travel Time" = "Orange",
                  "Free Flow Travel Time" = "green")
      
      g <- cs4 %>% 
        ggplot(data = ., aes(x = date)) +
        labs(x = "Date", y = "Traffic Value",
             title = str_c(input$loc.type, ": ", input$loc.type2),
             color = "Parameter") +
        scale_color_manual(values = colors)
      g1 <- geom_line(aes(y = mean_current_speed, color = "Current Speed"))
      g2 <- geom_line(aes(y = mean_free_flow_speed, color = "Free Flow Speed"))
      g3 <- geom_line(aes(y = mean_current_travel_time, color = "Current Travel Time"))
      g4 <- geom_line(aes(y = mean_free_flow_travel_time, color = "Free Flow Travel Time"))
      
      if(input$cspeed == TRUE){
        if(input$ffspeed == TRUE){
          if(input$ctravel == TRUE){
            if(input$fftravel == TRUE){
              p2 <- g + g1 + g2 + g3 + g4
            }else if(input$fftravel == FALSE){
              p2 <- g + g1 + g2 + g3
            }
          }else if(input$ctravel == FALSE){
            if(input$fftravel == TRUE){
              p2 <- g + g1 + g2 + g4
            }else if(input$fftravel == FALSE){
              p2 <- g + g1 + g2
            }
          }
        }else if(input$ffspeed == FALSE){
          if(input$ctravel == TRUE){
            if(input$fftravel == TRUE){
              p2 <- g + g1 + g3 + g4
            }else if(input$fftravel == FALSE){
              p2 <- g + g1 + g3
            }
          }else if(input$ctravel == FALSE){
            if(input$fftravel == TRUE){
              p2 <- g + g1 + g4
            }else if(input$fftravel == FALSE){
              p2 <- g + g1
            }
          }
        }
      }else if(input$cspeed == FALSE){
        if(input$ffspeed == TRUE){
          if(input$ctravel == TRUE){
            if(input$fftravel == TRUE){
              p2 <- g + g2 + g3 + g4
            }else if(input$fftravel == FALSE){
              p2 <- g + g2 + g3
            }
          }else if(input$ctravel == FALSE){
            if(input$fftravel == TRUE){
              p2 <- g + g2 + g4
            }else if(input$fftravel == FALSE){
              p2 <- g + g2
            }
          }
        }else if(input$ffspeed == FALSE){
          if(input$ctravel == TRUE){
            if(input$fftravel == TRUE){
              p2 <- g + g3 + g4
            }else if(input$fftravel == FALSE){
              p2 <- g + g3
            }
          }else if(input$ctravel == FALSE){
            if(input$fftravel == TRUE){
              p2 <- g + g4 
            }else if(input$fftravel == FALSE){
              p2 <- g
            }
          }
        }
      }
      grid.arrange(p1, p2, ncol = 2)
    }
  }) ## filtered location
  output$loc.type.graph1 <- renderPlot({
    if(input$historical == TRUE){
      if(input$loc.type == "Quadrant"){
        cs3 <- traffic.flow %>% 
          group_by(date, quadrant) %>% 
          summarize(mean_ozone_loc_aqi = mean(ozone_loc_aqi, na.rm = TRUE),
                    mean_so2_loc_aqi = mean(so2_loc_aqi, na.rm = TRUE),
                    mean_pm2.5_loc_aqi = mean(pm2.5_loc_aqi, na.rm = TRUE),
                    mean_no2_loc_aqi = mean(no2_loc_aqi, na.rm = TRUE))
        
        p1 <- ggplot(data = cs3, aes(x = date)) +
          labs(x = "Date", y = "Ozone AQI",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_ozone_loc_aqi, color = quadrant))  +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
          annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1)
        
        p2 <- ggplot(data = cs3, aes(x = date)) +
          labs(x = "Date", y = "SO2 AQI",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_so2_loc_aqi, color = quadrant))  +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
          annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1)
        
        p3 <- ggplot(data = cs3, aes(x = date)) +
          labs(x = "Date", y = "PM 2.5 AQI",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_pm2.5_loc_aqi, color = quadrant))  +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
          annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1)
        
        p4 <- ggplot(data = cs3, aes(x = date)) +
          labs(x = "Date", y = "NO2 AQI",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_no2_loc_aqi, color = quadrant))  +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
          annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1)
      }else if(input$loc.type == "Ward"){
        cs3 <- traffic.flow %>% 
          group_by(date, ward) %>% 
          summarize(mean_ozone_loc_aqi = mean(ozone_loc_aqi, na.rm = TRUE),
                    mean_so2_loc_aqi = mean(so2_loc_aqi, na.rm = TRUE),
                    mean_pm2.5_loc_aqi = mean(pm2.5_loc_aqi, na.rm = TRUE),
                    mean_no2_loc_aqi = mean(no2_loc_aqi, na.rm = TRUE))
        
        p1 <- ggplot(data = cs3, aes(x = date)) +
          labs(x = "Date", y = "Ozone AQI",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_ozone_loc_aqi, color = ward))  +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
          annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1)
        
        p2 <- ggplot(data = cs3, aes(x = date)) +
          labs(x = "Date", y = "SO2 AQI",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_so2_loc_aqi, color = ward))  +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
          annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1)
        
        p3 <- ggplot(data = cs3, aes(x = date)) +
          labs(x = "Date", y = "PM 2.5 AQI",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_pm2.5_loc_aqi, color = ward))  +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
          annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1)
        
        p4 <- ggplot(data = cs3, aes(x = date)) +
          labs(x = "Date", y = "NO2 AQI",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_no2_loc_aqi, color = ward))  +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
          annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1)
      }else if(input$loc.type == "Zip Code"){
        cs3 <- traffic.flow %>% 
          group_by(date, zip_code) %>% 
          summarize(mean_ozone_loc_aqi = mean(ozone_loc_aqi, na.rm = TRUE),
                    mean_so2_loc_aqi = mean(so2_loc_aqi, na.rm = TRUE),
                    mean_pm2.5_loc_aqi = mean(pm2.5_loc_aqi, na.rm = TRUE),
                    mean_no2_loc_aqi = mean(no2_loc_aqi, na.rm = TRUE))
        
        p1 <- ggplot(data = cs3, aes(x = date)) +
          labs(x = "Date", y = "Ozone AQI",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_ozone_loc_aqi, color = zip_code)) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
          annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1)
        
        p2 <- ggplot(data = cs3, aes(x = date)) +
          labs(x = "Date", y = "SO2 AQI",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_so2_loc_aqi, color = zip_code))  +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
          annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1)
        
        p3 <- ggplot(data = cs3, aes(x = date)) +
          labs(x = "Date", y = "PM 2.5 AQI",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_pm2.5_loc_aqi, color = zip_code))  +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
          annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1)
        
        p4 <- ggplot(data = cs3, aes(x = date)) +
          labs(x = "Date", y = "NO2 AQI",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_no2_loc_aqi, color = zip_code))  +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
          annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1)
      }else if(input$loc.type == "Advisory Neighborhood Commission"){
        cs3 <- traffic.flow %>% 
          group_by(date, anc) %>% 
          summarize(mean_ozone_loc_aqi = mean(ozone_loc_aqi, na.rm = TRUE),
                    mean_so2_loc_aqi = mean(so2_loc_aqi, na.rm = TRUE),
                    mean_pm2.5_loc_aqi = mean(pm2.5_loc_aqi, na.rm = TRUE),
                    mean_no2_loc_aqi = mean(no2_loc_aqi, na.rm = TRUE))
        
        p1 <- ggplot(data = cs3, aes(x = date)) +
          labs(x = "Date", y = "Ozone AQI",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_ozone_loc_aqi, color = anc)) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
          annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1)
        
        p2 <- ggplot(data = cs3, aes(x = date)) +
          labs(x = "Date", y = "SO2 AQI",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_so2_loc_aqi, color = anc))  +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
          annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1)
        
        p3 <- ggplot(data = cs3, aes(x = date)) +
          labs(x = "Date", y = "PM 2.5 AQI",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_pm2.5_loc_aqi, color = anc))  +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
          annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1)
        
        p4 <- ggplot(data = cs3, aes(x = date)) +
          labs(x = "Date", y = "NO2 AQI",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_no2_loc_aqi, color = anc))  +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
          annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1)
      }else if(input$loc.type == "Census Tract"){
        cs3 <- traffic.flow %>% 
          group_by(date, census_tract) %>% 
          summarize(mean_ozone_loc_aqi = mean(ozone_loc_aqi, na.rm = TRUE),
                    mean_so2_loc_aqi = mean(so2_loc_aqi, na.rm = TRUE),
                    mean_pm2.5_loc_aqi = mean(pm2.5_loc_aqi, na.rm = TRUE),
                    mean_no2_loc_aqi = mean(no2_loc_aqi, na.rm = TRUE))
        
        p1 <- ggplot(data = cs3, aes(x = date)) +
          labs(x = "Date", y = "Ozone AQI",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_ozone_loc_aqi, color = census_tract)) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
          annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1)
        
        p2 <- ggplot(data = cs3, aes(x = date)) +
          labs(x = "Date", y = "SO2 AQI",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_so2_loc_aqi, color = census_tract))  +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
          annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1)
        
        p3 <- ggplot(data = cs3, aes(x = date)) +
          labs(x = "Date", y = "PM 2.5 AQI",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_pm2.5_loc_aqi, color = census_tract))  +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
          annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1)
        
        p4 <- ggplot(data = cs3, aes(x = date)) +
          labs(x = "Date", y = "NO2 AQI",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_no2_loc_aqi, color = census_tract))  +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
          annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1)
      }else if(input$loc.type == "Single Member District"){
        cs3 <- traffic.flow %>% 
          group_by(date, single_member_district) %>% 
          summarize(mean_ozone_loc_aqi = mean(ozone_loc_aqi, na.rm = TRUE),
                    mean_so2_loc_aqi = mean(so2_loc_aqi, na.rm = TRUE),
                    mean_pm2.5_loc_aqi = mean(pm2.5_loc_aqi, na.rm = TRUE),
                    mean_no2_loc_aqi = mean(no2_loc_aqi, na.rm = TRUE))
        
        p1 <- ggplot(data = cs3, aes(x = date)) +
          labs(x = "Date", y = "Ozone AQI",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_ozone_loc_aqi, color = single_member_district)) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
          annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1)
        
        p2 <- ggplot(data = cs3, aes(x = date)) +
          labs(x = "Date", y = "SO2 AQI",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_so2_loc_aqi, color = single_member_district))  +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
          annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1)
        
        p3 <- ggplot(data = cs3, aes(x = date)) +
          labs(x = "Date", y = "PM 2.5 AQI",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_pm2.5_loc_aqi, color = single_member_district)) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
          annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) 
        
        p4 <- ggplot(data = cs3, aes(x = date)) +
          labs(x = "Date", y = "NO2 AQI",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_no2_loc_aqi, color = single_member_district))  +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
          annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1)
      }else if(input$loc.type == "Voter Precinct"){
        cs3 <- traffic.flow %>% 
          group_by(date, voter_precinct) %>% 
          summarize(mean_ozone_loc_aqi = mean(ozone_loc_aqi, na.rm = TRUE),
                    mean_so2_loc_aqi = mean(so2_loc_aqi, na.rm = TRUE),
                    mean_pm2.5_loc_aqi = mean(pm2.5_loc_aqi, na.rm = TRUE),
                    mean_no2_loc_aqi = mean(no2_loc_aqi, na.rm = TRUE))
        
        p1 <- ggplot(data = cs3, aes(x = date)) +
          labs(x = "Date", y = "Ozone AQI",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_ozone_loc_aqi, color = voter_precinct)) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
          annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1)
        
        p2 <- ggplot(data = cs3, aes(x = date)) +
          labs(x = "Date", y = "SO2 AQI",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_so2_loc_aqi, color = voter_precinct))  +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
          annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1)
        
        p3 <- ggplot(data = cs3, aes(x = date)) +
          labs(x = "Date", y = "PM 2.5 AQI",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_pm2.5_loc_aqi, color = voter_precinct))  +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
          annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1)
        
        p4 <- ggplot(data = cs3, aes(x = date)) +
          labs(x = "Date", y = "NO2 AQI",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_no2_loc_aqi, color = voter_precinct))  +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
          annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1)
      }
      
      if(input$ozone1 == TRUE){
        if(input$so2.1 == TRUE){
          if(input$pm2.5.1 == TRUE){
            if(input$no2.1 == TRUE){
              grid.arrange(p1, p2, p3, p4, ncol = 2)
            }else if(input$no2.1 == FALSE){
              grid.arrange(p1, p2, p3, ncol = 2)
            }
          }else if(input$pm2.5.1 == FALSE){
            if(input$no2.1 == TRUE){
              grid.arrange(p1, p2, p4, ncol = 2)
            }else if(input$no2.1 == FALSE){
              grid.arrange(p1, p2, ncol = 2)
            }
          }
        }else if(input$so2.1 == FALSE){
          if(input$pm2.5.1 == TRUE){
            if(input$no2.1 == TRUE){
              grid.arrange(p1, p3, p4, ncol = 2)
            }else if(input$no2.1 == FALSE){
              grid.arrange(p1, p3, ncol = 2)
            }
          }else if(input$pm2.5.1 == FALSE){
            if(input$no2.1 == TRUE){
              grid.arrange(p1, p4, ncol = 2)
            }else if(input$no2.1 == FALSE){
              grid.arrange(p1, ncol = 2)
            }
          }
        }
      }else if(input$ozone1 == FALSE){
        if(input$so2.1 == TRUE){
          if(input$pm2.5.1 == TRUE){
            if(input$no2.1 == TRUE){
              grid.arrange(p2, p3, p4, ncol = 2)
            }else if(input$no2.1 == FALSE){
              grid.arrange(p2, p3, ncol = 2)
            }
          }else if(input$pm2.5.1 == FALSE){
            if(input$no2.1 == TRUE){
              grid.arrange(p2, p4, ncol = 2)
            }else if(input$no2.1 == FALSE){
              grid.arrange(p2, ncol = 2)
            }
          }
        }else if(input$so2.1 == FALSE){
          if(input$pm2.5.1 == TRUE){
            if(input$no2.1 == TRUE){
              grid.arrange(p3, p4, ncol = 2)
            }else if(input$no2.1 == FALSE){
              grid.arrange(p3, ncol = 2)
            }
          }else if(input$pm2.5.1 == FALSE){
            if(input$no2.1 == TRUE){
              grid.arrange(p4, ncol = 2)
            }else if(input$no2.1 == FALSE){
              ggplot(data = cs3, aes(x = date)) +
                labs(x = "Date", y = "Air Quality Index",
                     title = str_c(input$loc.type))
            }
          }
        }
      }
    }
  }) ## comparison of location, air
  output$loc.type.graph2 <- renderPlot({
    if(input$historical == TRUE){
      if(input$loc.type == "Quadrant"){
        cs4 <- traffic.flow %>% 
          group_by(date, quadrant) %>% 
          summarize(mean_current_speed = mean(current_speed, na.rm = TRUE),
                    mean_free_flow_speed = mean(free_flow_speed, na.rm = TRUE),
                    mean_current_travel_time = mean(current_travel_time, na.rm = TRUE),
                    mean_free_flow_travel_time = mean(free_flow_travel_time, na.rm = TRUE))
        
        p1 <- ggplot(data = cs4, aes(x = date)) +
          labs(x = "Date", y = "Current Speed",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_current_speed, color = quadrant))
        
        p2 <- ggplot(data = cs4, aes(x = date)) +
          labs(x = "Date", y = "Free Flow Speed",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_free_flow_speed, color = quadrant)) 
        
        p3 <- ggplot(data = cs4, aes(x = date)) +
          labs(x = "Date", y = "Current Travel Time",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_current_travel_time, color = quadrant)) 
        
        p4 <- ggplot(data = cs4, aes(x = date)) +
          labs(x = "Date", y = "Free Flow Travel Time",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_free_flow_travel_time, color = quadrant)) 
      }else if(input$loc.type == "Ward"){
        cs4 <- traffic.flow %>% 
          group_by(date, ward) %>% 
          summarize(mean_current_speed = mean(current_speed, na.rm = TRUE),
                    mean_free_flow_speed = mean(free_flow_speed, na.rm = TRUE),
                    mean_current_travel_time = mean(current_travel_time, na.rm = TRUE),
                    mean_free_flow_travel_time = mean(free_flow_travel_time, na.rm = TRUE))
        
        p1 <- ggplot(data = cs4, aes(x = date)) +
          labs(x = "Date", y = "Current Speed",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_current_speed, color = ward))
        
        p2 <- ggplot(data = cs4, aes(x = date)) +
          labs(x = "Date", y = "Free Flow Speed",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_free_flow_speed, color = ward)) 
        
        p3 <- ggplot(data = cs4, aes(x = date)) +
          labs(x = "Date", y = "Current Travel Time",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_current_travel_time, color = ward)) 
        
        p4 <- ggplot(data = cs4, aes(x = date)) +
          labs(x = "Date", y = "Free Flow Travel Time",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_free_flow_travel_time, color = ward)) 
      }else if(input$loc.type == "Zip Code"){
        cs4 <- traffic.flow %>% 
          group_by(date, zip_code) %>% 
          summarize(mean_current_speed = mean(current_speed, na.rm = TRUE),
                    mean_free_flow_speed = mean(free_flow_speed, na.rm = TRUE),
                    mean_current_travel_time = mean(current_travel_time, na.rm = TRUE),
                    mean_free_flow_travel_time = mean(free_flow_travel_time, na.rm = TRUE))
        
        p1 <- ggplot(data = cs4, aes(x = date)) +
          labs(x = "Date", y = "Current Speed",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_current_speed, color = zip_code))
        
        p2 <- ggplot(data = cs4, aes(x = date)) +
          labs(x = "Date", y = "Free Flow Speed",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_free_flow_speed, color = zip_code)) 
        
        p3 <- ggplot(data = cs4, aes(x = date)) +
          labs(x = "Date", y = "Current Travel Time",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_current_travel_time, color = zip_code)) 
        
        p4 <- ggplot(data = cs4, aes(x = date)) +
          labs(x = "Date", y = "Free Flow Travel Time",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_free_flow_travel_time, color = zip_code)) 
      }else if(input$loc.type == "Advisory Neighborhood Commission"){
        cs4 <- traffic.flow %>% 
          group_by(date, anc) %>% 
          summarize(mean_current_speed = mean(current_speed, na.rm = TRUE),
                    mean_free_flow_speed = mean(free_flow_speed, na.rm = TRUE),
                    mean_current_travel_time = mean(current_travel_time, na.rm = TRUE),
                    mean_free_flow_travel_time = mean(free_flow_travel_time, na.rm = TRUE))
        
        p1 <- ggplot(data = cs4, aes(x = date)) +
          labs(x = "Date", y = "Current Speed",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_current_speed, color = anc))
        
        p2 <- ggplot(data = cs4, aes(x = date)) +
          labs(x = "Date", y = "Free Flow Speed",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_free_flow_speed, color = anc)) 
        
        p3 <- ggplot(data = cs4, aes(x = date)) +
          labs(x = "Date", y = "Current Travel Time",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_current_travel_time, color = anc)) 
        
        p4 <- ggplot(data = cs4, aes(x = date)) +
          labs(x = "Date", y = "Free Flow Travel Time",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_free_flow_travel_time, color = anc)) 
      }else if(input$loc.type == "Census Tract"){
        cs4 <- traffic.flow %>% 
          group_by(date, census_tract) %>% 
          summarize(mean_current_speed = mean(current_speed, na.rm = TRUE),
                    mean_free_flow_speed = mean(free_flow_speed, na.rm = TRUE),
                    mean_current_travel_time = mean(current_travel_time, na.rm = TRUE),
                    mean_free_flow_travel_time = mean(free_flow_travel_time, na.rm = TRUE))
        
        p1 <- ggplot(data = cs4, aes(x = date)) +
          labs(x = "Date", y = "Current Speed",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_current_speed, color = census_tract))
        
        p2 <- ggplot(data = cs4, aes(x = date)) +
          labs(x = "Date", y = "Free Flow Speed",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_free_flow_speed, color = census_tract)) 
        
        p3 <- ggplot(data = cs4, aes(x = date)) +
          labs(x = "Date", y = "Current Travel Time",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_current_travel_time, color = census_tract)) 
        
        p4 <- ggplot(data = cs4, aes(x = date)) +
          labs(x = "Date", y = "Free Flow Travel Time",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_free_flow_travel_time, color = census_tract)) 
      }else if(input$loc.type == "Single Member District"){
        cs4 <- traffic.flow %>% 
          group_by(date, single_member_district) %>% 
          summarize(mean_current_speed = mean(current_speed, na.rm = TRUE),
                    mean_free_flow_speed = mean(free_flow_speed, na.rm = TRUE),
                    mean_current_travel_time = mean(current_travel_time, na.rm = TRUE),
                    mean_free_flow_travel_time = mean(free_flow_travel_time, na.rm = TRUE))
        
        p1 <- ggplot(data = cs4, aes(x = date)) +
          labs(x = "Date", y = "Current Speed",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_current_speed, color = single_member_district))
        
        p2 <- ggplot(data = cs4, aes(x = date)) +
          labs(x = "Date", y = "Free Flow Speed",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_free_flow_speed, color = single_member_district)) 
        
        p3 <- ggplot(data = cs4, aes(x = date)) +
          labs(x = "Date", y = "Current Travel Time",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_current_travel_time, color = single_member_district)) 
        
        p4 <- ggplot(data = cs4, aes(x = date)) +
          labs(x = "Date", y = "Free Flow Travel Time",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_free_flow_travel_time, color = single_member_district)) 
      }else if(input$loc.type == "Voter Precinct"){
        cs4 <- traffic.flow %>% 
          group_by(date, voter_precinct) %>% 
          summarize(mean_current_speed = mean(current_speed, na.rm = TRUE),
                    mean_free_flow_speed = mean(free_flow_speed, na.rm = TRUE),
                    mean_current_travel_time = mean(current_travel_time, na.rm = TRUE),
                    mean_free_flow_travel_time = mean(free_flow_travel_time, na.rm = TRUE))
        
        p1 <- ggplot(data = cs4, aes(x = date)) +
          labs(x = "Date", y = "Current Speed",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_current_speed, color = voter_precinct))
        
        p2 <- ggplot(data = cs4, aes(x = date)) +
          labs(x = "Date", y = "Free Flow Speed",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_free_flow_speed, color = voter_precinct)) 
        
        p3 <- ggplot(data = cs4, aes(x = date)) +
          labs(x = "Date", y = "Current Travel Time",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_current_travel_time, color = voter_precinct)) 
        
        p4 <- ggplot(data = cs4, aes(x = date)) +
          labs(x = "Date", y = "Free Flow Travel Time",
               title = str_c(input$loc.type)) +
          geom_line(aes(y = mean_free_flow_travel_time, color = voter_precinct)) 
      }
      
      if(input$cspeed == TRUE){
        if(input$ffspeed == TRUE){
          if(input$ctravel == TRUE){
            if(input$fftravel == TRUE){
              grid.arrange(p1, p2, p3, p4, ncol = 2)
            }else if(input$fftravel == FALSE){
              grid.arrange(p1, p2, p3, ncol = 2)
            }
          }else if(input$ctravel == FALSE){
            if(input$fftravel == TRUE){
              grid.arrange(p1, p2, p4, ncol = 2)
            }else if(input$fftravel == FALSE){
              grid.arrange(p1, p2, ncol = 2)
            }
          }
        }else if(input$ffspeed == FALSE){
          if(input$ctravel == TRUE){
            if(input$fftravel == TRUE){
              grid.arrange(p1, p3, p4, ncol = 2)
            }else if(input$fftravel == FALSE){
              grid.arrange(p1, p3, ncol = 2)
            }
          }else if(input$ctravel == FALSE){
            if(input$fftravel == TRUE){
              grid.arrange(p1, p4, ncol = 2)
            }else if(input$fftravel == FALSE){
              grid.arrange(p1, ncol = 2)
            }
          }
        }
      }else if(input$cspeed == FALSE){
        if(input$ffspeed == TRUE){
          if(input$ctravel == TRUE){
            if(input$fftravel == TRUE){
              grid.arrange(p2, p3, p4, ncol = 2)
            }else if(input$fftravel == FALSE){
              grid.arrange(p2, p3, ncol = 2)
            }
          }else if(input$ctravel == FALSE){
            if(input$fftravel == TRUE){
              grid.arrange(p2, p4, ncol = 2)
            }else if(input$fftravel == FALSE){
              grid.arrange(p2, ncol = 2)
            }
          }
        }else if(input$ffspeed == FALSE){
          if(input$ctravel == TRUE){
            if(input$fftravel == TRUE){
              grid.arrange(p3, p4, ncol = 2)
            }else if(input$fftravel == FALSE){
              grid.arrange(p3, ncol = 2)
            }
          }else if(input$ctravel == FALSE){
            if(input$fftravel == TRUE){
              grid.arrange(p4, ncol = 2)
            }else if(input$fftravel == FALSE){
              ggplot(data = cs4, aes(x = date)) +
                labs(x = "Date", y = "Air Quality Index",
                     title = str_c(input$loc.type))
            }
          }
        }
      }
    }
  }) ## comparison of location, traffic
  
  ## Raw Data Tables
  output$static.city <- renderDataTable({
    hourly
  })
  output$static.location <- renderDataTable({
    traffic.flow %>% 
      select(-all_lat_lon) %>% 
      select(-street.url)
  })
  
  ## graphics for background tab 
  output$airnow.back <- renderTable({
    data.frame(`Level of Concern` = c("Good", "Moderate", 
                                      "Unhealthy for Sensitive Groups", 
                                      "Unhealthy", "Very Unhealthy", "Hazardous"),
               `AQI Values` = c("0 to 50", "51 to 100", "101 to 150", 
                                "151 to 200", "201 to 300", "301 and higher"),
               Description = c("Air quality is satisfactory, and air pollution poses little or no risk.",
                               "Air quality is acceptable. However, there may be a risk for some people, particularly those who are unusually sensitive to air pollution.",
                               "Members of sensitive groups may experience health effects. The general public is less likely to be affected.",
                               "Some members of the general public may experience health effects; members of sensitive groups may experience more serious health effects.",
                               "Health alert: the risk of health effects is increased for everyone.",
                               "Health warming of emergency conditions: everyone is more likely to be affected."))
    
  })
  output$traffic.flow.back <- renderTable({
    data.frame(Metric = c("Current Speed",
                          "Free Flow Speed",
                          "Current Travel Time",
                          "Free Flow Travel Time"),
               Description = c("The current average speed at the selected location in miles per hour.",
                               "The average expected speed at the selected location in miles per hour under ideal conditions.",
                               "The current travel time at the selected location in seconds.",
                               "The expected travel time at the selected location in seconds under ideal conditions."))
  })
  output$hourly.back <- renderTable({
    data.frame(Metric = c("Traffic Index",
                          "Jams Count",
                          "Jams Delay",
                          "Jams Length"),
               Description = c("Congestion level across Washington, D.C. in percent.",
                               "Total number of traffic jams across Washington, D.C.",
                               "Delay time from traffic jams across Washington, D.C. in seconds.",
                               "Total length of traffic jams across Washington, D.C. in km."))
  })
  output$pollutants.back <- renderTable({
    data.frame("Abbrv." = c("O3", "PM2.5", "SO2", "NO2"),
               "Pollutant" = c("Ozone", "Particulate Matter", 
                               "Sulfur Dioxide", "Nitrogen Dioxide"),
               "Good" = c("None", "None", "None", "None"),
               "Moderate" = c("Unusually sensitive people should consider reducing prolonged or heavy outdoor exertion.", "Unusually sensitive people should consider reducing prolonged or heavy exertion.", "None", "Unusually sensitive inidividuals should consider limiting prolonged exertion especially near busy roads."),
               "Sensitive" = c("People with lung disease (such as asthma), children, older adults, people who are active outdoors (including outdoor workers), people with certain genetic variants, and people with diets limited in certain nutrients should reduce prolonged or heavy outdoor exertion.", "People with heart or lung disease, older adults, children, and people of lower socioeconomic status should reduce prolonged or heavy exertion.", "People with asthma should consider limiting outdoor exertion.", "People with asthma, children, and older adults should limit prolonged exertion especially near busy roads."),
               "Unhealthy" = c("People with lung disease (such as asthma), children, older adults, people who are active outdoors (including outdoor workers), people with certain genetic variants, and people with diets limited in certain nutrients should avoid prolonged or heavy outdoor exertion; everyone should reduce prolonged or heavy outdoor exertion.", "People with heart or lung disease, older adults, children, or people of lower socioeconomic status should avoid prolonged or heavy exertion; everyone else should reduce prolonged or heavy exertion.", "Children, people with asthma, or other lung diseases should limit outdoor exertion.", "People with asthma, children, and older adults should avoid prolonged exertion near roadways; everyone else should limit prolonged exertion especially near busy roads."),
               "Very Unhealthy" = c("People with lung disease (such as asthma), children, older adults, people who are active outdoors (including outdoor workers), people with certain genetic variants, and people with diets limited in certain nutrients should avoid all outdoor exertion; everyone elese should reduce outdoor exertion.", "People with heart or lung disease, older adults, children, and people of lower socioeconomic status should avoid all physical activity outdoors. Everyone else should avoid prolonged or heavy exertion.", "Children, people with asthma, or other lung diseases should avoid outdoor exertions; everyone else should reduce outdoor exertion.", "People with asthma, children, and older adults should avoid all outdoor exertion; everyone else should avoid prolonged exertion especially near busy roads."),
               "Hazardous" = c("Everyone should avoid all outdoor exertion.", "Everyone should avoid all physical activity outdoors; people with heart or lung disease, older adults, children, and people of lower socioeconomic status should remain indoors and keep activity levels low.", "Children, people with asthma, or other lung diseases should remain indoors; everyone else should avoid outdoor exertion.", "People with asthma, children, and older adults should remain indoors; everyone else should avoid all outdoor exertion."))
  })
  
  observeEvent(input$historical.o,{
    if(input$historical.o == TRUE){
      show("myaqi")
    }else if(input$historical.o == FALSE){
      hide("myaqi")
    }
  }
  )
  
  observeEvent(input$historical,{
    if(input$historical == TRUE){
      show("location.graph");show("loc.type.graph1");show("loc.type.graph2")
    }else if(input$historical == FALSE){
      hide("location.graph");hide("loc.type.graph1");hide("loc.type.graph2")
    }
  }
  )
  
  observeEvent(input$currenttime.o,{
    if(input$currenttime.o == TRUE){
      show("airtable.graph2")
    }else if(input$currenttime.o == FALSE){
      hide("airtable.graph2")
    }
  }
  )
  
  observeEvent(input$currenttime,{
    if(input$currenttime == TRUE){
      show("airtable.graph")
    }else if(input$currenttime == FALSE){
      hide("airtable.graph")
    }
  }
  )
  
  
}

# Run ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)

