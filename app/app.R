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

ui <- fluidPage(theme = shinytheme("slate"),
  wellPanel(titlePanel("Assessment of Air Quality and Traffic Volume"),
            h4("Washington, D.C."),
            h4("American University Team - Chace Paulson, Minh Nguyen & Shalini Ramachandra")),
  
  tabsetPanel(
    tabPanel("Citywide",
             sidebarLayout(
               sidebarPanel(
                 h5("Metric Comparisons"),
                 checkboxInput("currenttime.o", "Current Date", value = TRUE), 
                 checkboxInput("lastweek.o", "Previous Week"), 
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
                 checkboxInput("ozone", "OZONE"),
                 checkboxInput("so2", "SO2"),
                 checkboxInput("pm25", "PM2.5", value = TRUE),
                 checkboxInput("no2", "NO2")
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
                 textInput("loc.type2", "Which location?",
                           value = "SE"),
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
             dataTableOutput("static.location"))
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output) {
  ## Citywide Tab
  # Bold Labels
  output$text1 <- renderText({
    HTML(paste0("<b>","Turn on Traffic Index?","</b>"))
  })
  
  output$text2 <- renderText({
    HTML(paste0("<b>","Air Quality Parameters","</b>"))
  })
  
  # AQI SUMMARY
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
      if(input$location.type == "McMillan NCORE"){
        cs1 <- cs1 %>% 
          filter(location == "McMillan NCORE")
        ls1 <- ls1 %>% 
          filter(location == "McMillan NCORE")
      }else if(input$location.type == "DC Near Road"){
        cs1 <- cs1 %>% 
          filter(location == "DC Near Road")
        ls1 <- ls1 %>% 
          filter(location == "DC Near Road")
      }else if(input$location.type == "King Greenleaf Rec Center"){
        cs1 <- cs1 %>% 
          filter(location == "King Greenleaf Rec Center")
        ls1 <- ls1 %>% 
          filter(location == "King Greenleaf Rec Center")
      }else if(input$location.type == "River Terrace"){
        cs1 <- cs1 %>% 
          filter(location == "River Terrace")
        ls1 <- ls1 %>% 
          filter(location == "River Terrace")
      }else if(input$location.type == "McMillan Reservoir"){
        cs1 <- cs1 %>% 
          filter(location == "McMillan Reservoir")
        ls1 <- ls1 %>% 
          filter(location == "McMillan Reservoir")
      }else if(input$location.type == "Tokoma Rec"){
        cs1 <- cs1 %>% 
          filter(location == "Tokoma Rec")
        ls1 <- ls1 %>% 
          filter(location == "Tokoma Rec")
      }else if(input$location.type == "Aurora Hills"){
        cs1 <- cs1 %>% 
          filter(location == "Aurora Hills")
        ls1 <- ls1 %>% 
          filter(location == "Aurora Hills")
      }
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
      if(input$location.type == "McMillan NCORE"){
        hourly <- hourly %>% 
          filter(location == "McMillan NCORE")
      }else if(input$location.type == "DC Near Road"){
        hourly <- hourly %>% 
          filter(location == "DC Near Road")
      }else if(input$location.type == "King Greenleaf Rec Center"){
        hourly <- hourly %>% 
          filter(location == "King Greenleaf Rec Center")
      }else if(input$location.type == "River Terrace"){
        hourly <- hourly %>% 
          filter(location == "River Terrace")
      }else if(input$location.type == "McMillan Reservoir"){
        hourly <- hourly %>% 
          filter(location == "McMillan Reservoir")
      }else if(input$location.type == "Tokoma Rec"){
        hourly <- hourly %>% 
          filter(location == "Tokoma Rec")
      }else if(input$location.type == "Aurora Hills"){
        hourly <- hourly %>% 
          filter(location == "Aurora Hills")
      }
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
      if(input$location.type == "McMillan NCORE"){
        hourly <- hourly %>% 
          filter(location == "McMillan NCORE")
      }else if(input$location.type == "DC Near Road"){
        hourly <- hourly %>% 
          filter(location == "DC Near Road")
      }else if(input$location.type == "King Greenleaf Rec Center"){
        hourly <- hourly %>% 
          filter(location == "King Greenleaf Rec Center")
      }else if(input$location.type == "River Terrace"){
        hourly <- hourly %>% 
          filter(location == "River Terrace")
      }else if(input$location.type == "McMillan Reservoir"){
        hourly <- hourly %>% 
          filter(location == "McMillan Reservoir")
      }else if(input$location.type == "Tokoma Rec"){
        hourly <- hourly %>% 
          filter(location == "Tokoma Rec")
      }else if(input$location.type == "Aurora Hills"){
        hourly <- hourly %>% 
          filter(location == "Aurora Hills")
      }
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
  
  ## First graph for the Overview tab
  #output$trafficaqi <- renderPlot({
  #hourly %>%
  #filter(parameter_name == "PM2.5") %>%
  #ggplot(aes(x=jams_count, y = AQI)) +
  #geom_point() +
  #ggtitle(paste0("Traffic Data")) +
  #xlab("Traffic Index") + ylab("AQI")
  #})
  
  
  ## Second graph for the Overview tab
  output$myaqi <- renderPlot({
    if(input$historical.o == TRUE){
      if(input$sensor.type == "Average Across Sensors"){
        x <- parameter_wider %>% 
          filter(agency == "District of Columbia - Department of Energy and Environment")
        
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
                    "SO2" = "red", "NO2" = "green",
                    "traffic" = "black")
        
        #ptitle <- str_c("AQI and Traffic")
        p1 <- 
          ggplot(data = x, aes(x = date)) +
          labs(x = "Date", y = "AQI",
               title = "AQI and Traffic",
               color = "Parameter") +
          scale_color_manual(values = colors) +
          annotate("text", x = as_datetime(1603836000), y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1.5) +
          annotate("text", x = as_datetime(1603836000), y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1.5) +
          annotate("text", x = as_datetime(1603836000), y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1.5)
        
        ptra <- geom_line(aes(y = traffic_index_live, color = "traffic"))
        po <- geom_line(aes(y = Ozone, color = "Ozone"))
        pc <- geom_line(aes(y = SO2, color = "SO2"))
        ph <- geom_line(aes(y = PM2.5, color = "PM2.5"))
        pl <- geom_line(aes(y = NO2, color = "NO2"))
        #pt <- labs(title = ptitle, x = "Date", y = "")
        
        if(input$traffic == TRUE) {
          if(input$ozone == TRUE){
            if(input$so2 == TRUE){
              if(input$pm25 == TRUE){
                if(input$no2 == TRUE){
                  p1 + ptra + po + pc + ph + pl #+ pt
                } else if(input$no2 == FALSE){
                  p1 + ptra + po + pc + ph #+ pt
                }
              }else if(input$pm25 == FALSE){
                if(input$no2 == TRUE){
                  p1 + ptra + po + pc + pl #+ pt
                } else if(input$no2 == FALSE){
                  p1 + ptra + po + pc #+ pt
                }
              }
            } else if(input$so2 == FALSE){
              if(input$pm25 == TRUE){
                if(input$no2 == TRUE){
                  p1 + ptra + po + ph + pl #+ pt
                } else if(input$no2 == FALSE){
                  p1 + ptra + po + ph #+ pt
                }
              }else if(input$pm25 == FALSE){
                if(input$no2 == TRUE){
                  p1 + ptra + po + pl #+ pt
                } else if(input$no2 == FALSE){
                  p1 + ptra + po #+ pt
                }
              }
            }
          } else if(input$ozone == FALSE){
            if(input$so2 == TRUE){
              if(input$pm25 == TRUE){
                if(input$no2 == TRUE){
                  p1 + ptra + pc + ph + pl #+ pt
                } else if(input$no2 == FALSE){
                  p1 + ptra + pc + ph #+ pt
                }
              }else if(input$pm25 == FALSE){
                if(input$no2 == TRUE){
                  p1 + ptra + pc + pl #+ pt
                } else if(input$no2 == FALSE){
                  p1 + ptra + pc #+ pt
                }
              }
            } else if(input$so2 == FALSE){
              if(input$pm25 == TRUE){
                if(input$no2 == TRUE){
                  p1 + ptra + ph + pl #+ pt
                } else if(input$no2 == FALSE){
                  p1 + ptra + ph #+ pt
                }
              }else if(input$pm25 == FALSE){
                if(input$no2 == TRUE){
                  p1 + ptra + pl #+ pt
                } else if(input$no2 == FALSE){
                  p1 + ptra #+ pt
                }
              }
            }
          }
          # TRAFFIC IS FALSE
        }else if(input$traffic == FALSE){
          if(input$ozone == TRUE){
            if(input$so2 == TRUE){
              if(input$pm25 == TRUE){
                if(input$no2 == TRUE){
                  p1 + po + pc + ph + pl #+ pt
                } else if(input$no2 == FALSE){
                  p1 + po + pc + ph #+ pt
                }
              }else if(input$pm25 == FALSE){
                if(input$no2 == TRUE){
                  p1 + po + pc + pl #+ pt
                } else if(input$no2 == FALSE){
                  p1 + po + pc #+ pt
                }
              }
            } else if(input$so2 == FALSE){
              if(input$pm25 == TRUE){
                if(input$no2 == TRUE){
                  p1 + po + ph + pl #+ pt
                } else if(input$no2 == FALSE){
                  p1 + po + ph #+ pt
                }
              }else if(input$pm25 == FALSE){
                if(input$no2 == TRUE){
                  p1 + po + pl #+ pt
                } else if(input$no2 == FALSE){
                  p1 + po #+ pt
                }
              }
            }
          } else if(input$ozone == FALSE){
            if(input$so2 == TRUE){
              if(input$pm25 == TRUE){
                if(input$no2 == TRUE){
                  p1 + pc + ph + pl #+ pt
                } else if(input$no2 == FALSE){
                  p1 + pc + ph #+ pt
                }
              }else if(input$pm25 == FALSE){
                if(input$no2 == TRUE){
                  p1 + pc + pl #+ pt
                } else if(input$no2 == FALSE){
                  p1 + pc #+ pt
                }
              }
            } else if(input$so2 == FALSE){
              if(input$pm25 == TRUE){
                if(input$no2 == TRUE){
                  p1 + ph + pl #+ pt
                } else if(input$no2 == FALSE){
                  p1 + ph #+ pt
                }
              }else if(input$pm25 == FALSE){
                if(input$no2 == TRUE){
                  p1 + pl #+ pt
                } else if(input$no2 == FALSE){
                  p1 #+ pt
                }
              }
            }
          }
        }
      }else if(input$sensor.type == "Single Sensor"){
        if(input$location.type == "McMillan NCORE"){
          x <- parameter_wider %>% 
            filter(location == "McMillan NCORE")
        }else if(input$location.type == "DC Near Road"){
          x <- parameter_wider %>% 
            filter(location == "DC Near Road")
        }else if(input$location.type == "King Greenleaf Rec Center"){
          x <- parameter_wider %>% 
            filter(location == "King Greenleaf Rec Center")
        }else if(input$location.type == "River Terrace"){
          x <- parameter_wider %>% 
            filter(location == "River Terrace")
        }else if(input$location.type == "McMillan Reservoir"){
          x <- parameter_wider %>% 
            filter(location == "McMillan Reservoir")
        }else if(input$location.type == "Tokoma Rec"){
          x <- parameter_wider %>% 
            filter(location == "Tokoma Rec")
        }else if(input$location.type == "Aurora Hills"){
          x <- parameter_wider %>% 
            filter(location == "Aurora Hills")
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
                    "SO2" = "red", "NO2" = "green",
                    "traffic" = "black")
        
        #ptitle <- str_c("AQI and Traffic")
        p1 <- 
          ggplot(data = x, aes(x = date)) +
          labs(x = "Date", y = "AQI",
               title = "AQI and Traffic",
               color = "Parameter") +
          scale_color_manual(values = colors) +
          annotate("text", x = as_datetime(1603836000), y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1.5) +
          annotate("text", x = as_datetime(1603836000), y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1.5) +
          annotate("text", x = as_datetime(1603836000), y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1.5)
        
        ptra <- geom_line(aes(y = traffic_index_live, color = "traffic"))
        po <- geom_line(aes(y = Ozone, color = "Ozone"))
        pc <- geom_line(aes(y = SO2, color = "SO2"))
        ph <- geom_line(aes(y = PM2.5, color = "PM2.5"))
        pl <- geom_line(aes(y = NO2, color = "NO2"))
        #pt <- labs(title = ptitle, x = "Date", y = "")
        
        if(input$traffic == TRUE) {
          if(input$ozone == TRUE){
            if(input$so2 == TRUE){
              if(input$pm25 == TRUE){
                if(input$no2 == TRUE){
                  p1 + ptra + po + pc + ph + pl #+ pt
                } else if(input$no2 == FALSE){
                  p1 + ptra + po + pc + ph #+ pt
                }
              }else if(input$pm25 == FALSE){
                if(input$no2 == TRUE){
                  p1 + ptra + po + pc + pl #+ pt
                } else if(input$no2 == FALSE){
                  p1 + ptra + po + pc #+ pt
                }
              }
            } else if(input$so2 == FALSE){
              if(input$pm25 == TRUE){
                if(input$no2 == TRUE){
                  p1 + ptra + po + ph + pl #+ pt
                } else if(input$no2 == FALSE){
                  p1 + ptra + po + ph #+ pt
                }
              }else if(input$pm25 == FALSE){
                if(input$no2 == TRUE){
                  p1 + ptra + po + pl #+ pt
                } else if(input$no2 == FALSE){
                  p1 + ptra + po #+ pt
                }
              }
            }
          } else if(input$ozone == FALSE){
            if(input$so2 == TRUE){
              if(input$pm25 == TRUE){
                if(input$no2 == TRUE){
                  p1 + ptra + pc + ph + pl #+ pt
                } else if(input$no2 == FALSE){
                  p1 + ptra + pc + ph #+ pt
                }
              }else if(input$pm25 == FALSE){
                if(input$no2 == TRUE){
                  p1 + ptra + pc + pl #+ pt
                } else if(input$no2 == FALSE){
                  p1 + ptra + pc #+ pt
                }
              }
            } else if(input$so2 == FALSE){
              if(input$pm25 == TRUE){
                if(input$no2 == TRUE){
                  p1 + ptra + ph + pl #+ pt
                } else if(input$no2 == FALSE){
                  p1 + ptra + ph #+ pt
                }
              }else if(input$pm25 == FALSE){
                if(input$no2 == TRUE){
                  p1 + ptra + pl #+ pt
                } else if(input$no2 == FALSE){
                  p1 + ptra #+ pt
                }
              }
            }
          }
          # TRAFFIC IS FALSE
        }else if(input$traffic == FALSE){
          if(input$ozone == TRUE){
            if(input$so2 == TRUE){
              if(input$pm25 == TRUE){
                if(input$no2 == TRUE){
                  p1 + po + pc + ph + pl #+ pt
                } else if(input$no2 == FALSE){
                  p1 + po + pc + ph #+ pt
                }
              }else if(input$pm25 == FALSE){
                if(input$no2 == TRUE){
                  p1 + po + pc + pl #+ pt
                } else if(input$no2 == FALSE){
                  p1 + po + pc #+ pt
                }
              }
            } else if(input$so2 == FALSE){
              if(input$pm25 == TRUE){
                if(input$no2 == TRUE){
                  p1 + po + ph + pl #+ pt
                } else if(input$no2 == FALSE){
                  p1 + po + ph #+ pt
                }
              }else if(input$pm25 == FALSE){
                if(input$no2 == TRUE){
                  p1 + po + pl #+ pt
                } else if(input$no2 == FALSE){
                  p1 + po #+ pt
                }
              }
            }
          } else if(input$ozone == FALSE){
            if(input$so2 == TRUE){
              if(input$pm25 == TRUE){
                if(input$no2 == TRUE){
                  p1 + pc + ph + pl #+ pt
                } else if(input$no2 == FALSE){
                  p1 + pc + ph #+ pt
                }
              }else if(input$pm25 == FALSE){
                if(input$no2 == TRUE){
                  p1 + pc + pl #+ pt
                } else if(input$no2 == FALSE){
                  p1 + pc #+ pt
                }
              }
            } else if(input$so2 == FALSE){
              if(input$pm25 == TRUE){
                if(input$no2 == TRUE){
                  p1 + ph + pl #+ pt
                } else if(input$no2 == FALSE){
                  p1 + ph #+ pt
                }
              }else if(input$pm25 == FALSE){
                if(input$no2 == TRUE){
                  p1 + pl #+ pt
                } else if(input$no2 == FALSE){
                  p1 #+ pt
                }
              }
            }
          }
        }
      }
    }
  })
  
  ## Third graph for the Overview tab
  output$mymap <- renderLeaflet({
    if(input$sensor.type == "Average Across Sensors"){
      hourly <- hourly
    }else if(input$sensor.type == "Single Sensor"){
      if(input$location.type == "McMillan NCORE"){
        hourly <- hourly %>% 
          filter(location == "McMillan NCORE")
      }else if(input$location.type == "DC Near Road"){
        hourly <- hourly %>% 
          filter(location == "DC Near Road")
      }else if(input$location.type == "King Greenleaf Rec Center"){
        hourly <- hourly %>% 
          filter(location == "King Greenleaf Rec Center")
      }else if(input$location.type == "River Terrace"){
        hourly <- hourly %>% 
          filter(location == "River Terrace")
      }else if(input$location.type == "McMillan Reservoir"){
        hourly <- hourly %>% 
          filter(location == "McMillan Reservoir")
      }else if(input$location.type == "Tokoma Rec"){
        hourly <- hourly %>% 
          filter(location == "Tokoma Rec")
      }else if(input$location.type == "Aurora Hills"){
        hourly <- hourly %>% 
          filter(location == "Aurora Hills")
      }
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
                   radius = ~(AQI+12)*10, popup = ~location, color = "#FF0000")
      
    }
  })
  
  ## Location Search
  output$warning.text <- renderText({
    if(input$loc.type == "Quadrant"){
      if(input$loc.type2 != "SE" & input$loc.type2 != "SW" &
         input$loc.type2 != "NE" & input$loc.type2 != "NW"){
        "Warning: Enter a valid value for quadrant location (e.g., SE)."
      }
    }else if(input$loc.type == "Ward"){
      if(input$loc.type2 != "1" & input$loc.type2 != "2" & 
         input$loc.type2 != "3" & input$loc.type2 != "4" & 
         input$loc.type2 != "5" & input$loc.type2 != "6" & 
         input$loc.type2 != "7" & input$loc.type2 != "8"){
        "Warning: Enter a valid value for ward location (e.g., 1)."
      }
    }else if(input$loc.type == "Zip Code"){
      if(input$loc.type2 != "20032" & input$loc.type2 != "20037" &
         input$loc.type2 != "20227" & input$loc.type2 != "20019" &
         input$loc.type2 != "20020" & input$loc.type2 != "20018" &
         input$loc.type2 != "20016" & input$loc.type2 != "20024" &
         input$loc.type2 != "20002" & input$loc.type2 != "20001" &
         input$loc.type2 != "20009" & input$loc.type2 != "20008" &
         input$loc.type2 != "20007" & input$loc.type2 != "20017" &
         input$loc.type2 != "20005" & input$loc.type2 != "20003" &
         input$loc.type2 != "20011" & input$loc.type2 != "20010" &
         input$loc.type2 != "20374" & input$loc.type2 != "20015" &
         input$loc.type2 != "20012"){
        "Warning: Enter a valid value for zip code location (e.g., 20012)."
      }
    }else if(input$loc.type == "Advisory Neighborhood Commission"){
      if(input$loc.type2 != "8E" & input$loc.type2 != "2A" & 
         input$loc.type2 != "8C" & input$loc.type2 != "8D" & 
         input$loc.type2 != "7F" & input$loc.type2 != "7C" & 
         input$loc.type2 != "7E" & input$loc.type2 != "5C" & 
         input$loc.type2 != "3D" & input$loc.type2 != "6D" & 
         input$loc.type2 != "5D" & input$loc.type2 != "1B" & 
         input$loc.type2 != "2B" & input$loc.type2 != "3C" & 
         input$loc.type2 != "2E" & input$loc.type2 != "5E" & 
         input$loc.type2 != "5B" & input$loc.type2 != "6E" & 
         input$loc.type2 != "2F" & input$loc.type2 != "4C" & 
         input$loc.type2 != "3E" & input$loc.type2 != "6B" &
         input$loc.type2 != "1A" & input$loc.type2 != "6A" & 
         input$loc.type2 != "6C" & input$loc.type2 != "8A" & 
         input$loc.type2 != "3G" & input$loc.type2 != "4B" & 
         input$loc.type2 != "4A" & input$loc.type2 != "5A" & 
         input$loc.type2 != "4D" & input$loc.type2 != "3F"){
        "Warning: Enter a valid value for advisory neighborhood commission location (e.g., 3F)."
      }
    }else if(input$loc.type == "Census Tract"){
      if(input$loc.type2 != "007304" & input$loc.type2 != "005600" & 
         input$loc.type2 != "009804" & input$loc.type2 != "009807" & 
         input$loc.type2 != "009811" & input$loc.type2 != "010800" & 
         input$loc.type2 != "009700" & input$loc.type2 != "010400" &
         input$loc.type2 != "006202" & input$loc.type2 != "007301" & 
         input$loc.type2 != "009603" & input$loc.type2 != "007708" & 
         input$loc.type2 != "007804" & input$loc.type2 != "007707" & 
         input$loc.type2 != "009902" & input$loc.type2 != "007807" &
         input$loc.type2 != "007806" & input$loc.type2 != "007809" & 
         input$loc.type2 !=  "009905" & input$loc.type2 !=  "009000" & 
         input$loc.type2 != "009400" & input$loc.type2 != "000901" & 
         input$loc.type2 != "006400" & input$loc.type2 != "008803" &
         input$loc.type2 != "003400" & input$loc.type2 != "004201" & 
         input$loc.type2 != "000600" & input$loc.type2 != "000300" & 
         input$loc.type2 != "008702" & input$loc.type2 != "009302" & 
         input$loc.type2 != "001002" & input$loc.type2 != "003301" &
         input$loc.type2 != "004702" & input$loc.type2 != "010100" & 
         input$loc.type2 !=  "000400" & input$loc.type2 != "010200" & 
         input$loc.type2 != "010500" & input$loc.type2 != "008701" & 
         input$loc.type2 != "002501" & input$loc.type2 != "009503" &
         input$loc.type2 != "011100" & input$loc.type2 != "004600" & 
         input$loc.type2 != "001001" & input$loc.type2 != "002400" & 
         input$loc.type2 != "006500" & input$loc.type2 != "004400" & 
         input$loc.type2 != "009102" & input$loc.type2 != "002900" &
         input$loc.type2 != "000502" & input$loc.type2 != "009201" & 
         input$loc.type2 != "010600" & input$loc.type2 != "004902" & 
         input$loc.type2 !=  "008302" & input$loc.type2 != "007200" & 
         input$loc.type2 != "006802" & input$loc.type2 != "006600" &
         input$loc.type2 != "008001" & input$loc.type2 != "006700" & 
         input$loc.type2 != "008100" & input$loc.type2 != "007601" & 
         input$loc.type2 != "007100" & input$loc.type2 != "008904" & 
         input$loc.type2 != "008200" & input$loc.type2 != "008002" &
         input$loc.type2 != "001401" & input$loc.type2 != "001902" & 
         input$loc.type2 != "001804" & input$loc.type2 != "001500" & 
         input$loc.type2 != "009509" & input$loc.type2 != "002101" & 
         input$loc.type2 != "001301" & input$loc.type2 != "002102" &
         input$loc.type2 != "002600" & input$loc.type2 != "001901" & 
         input$loc.type2 != "002001" & input$loc.type2 != "001803" & 
         input$loc.type2 != "001100" & input$loc.type2 != "001600"){
        "Warning: Enter a valid value for census tract location (e.g., 001100)."
      }
    }else if(input$loc.type == "Single Member District"){
      if(input$loc.type2 != "8E04" & input$loc.type2 != "2A03" & 
         input$loc.type2 != "8C07" & input$loc.type2 != "8C03" & 
         input$loc.type2 != "8D06" & input$loc.type2 != "8D01" & 
         input$loc.type2 != "2A07" & input$loc.type2 != "8E05" & 
         input$loc.type2 != "2A01" & input$loc.type2 != "8D03" & 
         input$loc.type2 != "8C05" & input$loc.type2 != "7F01" & 
         input$loc.type2 != "7F06" & input$loc.type2 != "7C03" &   
         input$loc.type2 != "7E01" & input$loc.type2 != "7E02" & 
         input$loc.type2 != "7C06" & input$loc.type2 != "7C07" & 
         input$loc.type2 != "7C04" & input$loc.type2 != "7E06" & 
         input$loc.type2 != "5C03" & input$loc.type2 != "5C01" & 
         input$loc.type2 != "3D02" & input$loc.type2 != "6D06" & 
         input$loc.type2 != "5D01" & input$loc.type2 != "1B01" & 
         input$loc.type2 != "2B09" & input$loc.type2 != "3C09" &  
         input$loc.type2 != "2E01" & input$loc.type2 != "5E03" & 
         input$loc.type2 != "5B03" & input$loc.type2 != "3C06" & 
         input$loc.type2 != "5E08" & input$loc.type2 != "6E07" & 
         input$loc.type2 != "2B05" & input$loc.type2 != "3C08" & 
         input$loc.type2 != "6D05" & input$loc.type2 != "6D03" & 
         input$loc.type2 != "2F08" & input$loc.type2 != "5E04" & 
         input$loc.type2 != "4C03" & input$loc.type2 != "5B01" & 
         input$loc.type2 != "5C02" & input$loc.type2 != "5E05" & 
         input$loc.type2 != "3E05" & input$loc.type2 != "4C08" & 
         input$loc.type2 != "6B01" & input$loc.type2 != "1B02" & 
         input$loc.type2 != "5C06" & input$loc.type2 != "1A04" & 
         input$loc.type2 != "3C03" & input$loc.type2 != "5E01" & 
         input$loc.type2 != "6A01" & input$loc.type2 != "2F06" & 
         input$loc.type2 != "6C03" & input$loc.type2 != "6D07" & 
         input$loc.type2 != "6B09" & input$loc.type2 != "6B02" & 
         input$loc.type2 != "5C04" & input$loc.type2 != "6A02" & 
         input$loc.type2 != "6B05" & input$loc.type2 != "6A03" & 
         input$loc.type2 != "8A03" & input$loc.type2 != "6B07" & 
         input$loc.type2 != "5D05" & input$loc.type2 != "6B06" & 
         input$loc.type2 != "6A08" & input$loc.type2 != "3G05" & 
         input$loc.type2 != "4B06" & input$loc.type2 != "4A04" & 
         input$loc.type2 != "3G04" & input$loc.type2 != "5A08" & 
         input$loc.type2 != "4D03" & input$loc.type2 != "3F03" & 
         input$loc.type2 != "3E02" & input$loc.type2 != "4A08" & 
         input$loc.type2 != "4B04" & input$loc.type2 != "4A06" & 
         input$loc.type2 != "4A07" & input$loc.type2 != "3E04" & 
         input$loc.type2 != "3D03" & input$loc.type2 != "4A02"){
        "Warning: Enter a valid value for single member district location (e.g., 4A02)."
      }
    }else if(input$loc.type == "Voter Precinct"){
      if(input$loc.type2 != "120" & input$loc.type2 != "3" & 
         input$loc.type2 != "122" & input$loc.type2 != "126" & 
         input$loc.type2 != "125" & input$loc.type2 != "2" & 
         input$loc.type2 != "121" & input$loc.type2 != "123" & 
         input$loc.type2 != "129" & input$loc.type2 != "102" & 
         input$loc.type2 != "132" & input$loc.type2 != "97" & 
         input$loc.type2 != "106" & input$loc.type2 != "110" & 
         input$loc.type2 != "95" & input$loc.type2 != "93" & 
         input$loc.type2 != "94" & input$loc.type2 != "105" & 
         input$loc.type2 != "139" & input$loc.type2 != "69" & 
         input$loc.type2 != "9" & input$loc.type2 != "127" & 
         input$loc.type2 != "76" & input$loc.type2 != "20" & 
         input$loc.type2 != "141" & input$loc.type2 != "27" & 
         input$loc.type2 != "6" & input$loc.type2 != "75" & 
         input$loc.type2 != "73" & input$loc.type2 != "29" & 
         input$loc.type2 != "135" & input$loc.type2 != "1" & 
         input$loc.type2 != "17" & input$loc.type2 != "12" & 
         input$loc.type2 != "128" & input$loc.type2 != "48" & 
         input$loc.type2 != "67" & input$loc.type2 != "72" & 
         input$loc.type2 != "19" & input$loc.type2 != "30" & 
         input$loc.type2 != "45" & input$loc.type2 != "130" & 
         input$loc.type2 != "22" & input$loc.type2 != "42" & 
         input$loc.type2 != "26" & input$loc.type2 != "74" & 
         input$loc.type2 != "82" & input$loc.type2 != "85" & 
         input$loc.type2 != "131" & input$loc.type2 != "91" & 
         input$loc.type2 != "89" & input$loc.type2 != "71" & 
         input$loc.type2 != "81" & input$loc.type2 != "88" & 
         input$loc.type2 != "133" & input$loc.type2 != "79" &  
         input$loc.type2 != "86" & input$loc.type2 != "50" & 
         input$loc.type2 != "59" & input$loc.type2 != "60" & 
         input$loc.type2 != "51" & input$loc.type2 != "66" & 
         input$loc.type2 != "56" & input$loc.type2 != "138" & 
         input$loc.type2 != "57" & input$loc.type2 != "31" &  
         input$loc.type2 != "53" & input$loc.type2 != "61" & 
         input$loc.type2 != "32" & input$loc.type2 != "62"){
        "Warning: Enter a valid value for voter precinct location (e.g., 62)."
      }
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
        if(input$loc.type2 == "SE"){
          cs2 <- cs1 %>% filter(quadrant == "SE")
        }else if(input$loc.type2 == "SW"){
          cs2 <- cs1 %>% filter(quadrant == "SW")
        }else if(input$loc.type2 == "NE"){
          cs2 <- cs1 %>% filter(quadrant == "NE")
        }else if(input$loc.type2 == "NW"){
          cs2 <- cs1 %>% filter(quadrant == "NW")
        }
      }else if(input$loc.type == "Ward"){
        if(input$loc.type2 == "1"){
          cs2 <- cs1 %>% filter(ward == "1")
        }else if(input$loc.type2 == "2"){
          cs2 <- cs1 %>% filter(ward == "2")
        }else if(input$loc.type2 == "3"){
          cs2 <- cs1 %>% filter(ward == "3")
        }else if(input$loc.type2 == "4"){
          cs2 <- cs1 %>% filter(ward == "4")
        }else if(input$loc.type2 == "5"){
          cs2 <- cs1 %>% filter(ward == "5")
        }else if(input$loc.type2 == "6"){
          cs2 <- cs1 %>% filter(ward == "6")
        }else if(input$loc.type2 == "7"){
          cs2 <- cs1 %>% filter(ward == "7")
        }else if(input$loc.type2 == "8"){
          cs2 <- cs1 %>% filter(ward == "8")
        }
      }else if(input$loc.type == "Zip Code"){
        if(input$loc.type2 == "20227"){
          cs2 <- cs1 %>% filter(zip_code == "20227")
        }else if(input$loc.type2 == "20032"){
          cs2 <- cs1 %>% filter(zip_code == "20032")
        }else if(input$loc.type2 == "20037"){
          cs2 <- cs1 %>% filter(zip_code == "20037")
        }else if(input$loc.type2 == "20019"){
          cs2 <- cs1 %>% filter(zip_code == "20019")
        }else if(input$loc.type2 == "20020"){
          cs2 <- cs1 %>% filter(zip_code == "20020")
        }else if(input$loc.type2 == "20018"){
          cs2 <- cs1 %>% filter(zip_code == "20018")
        }else if(input$loc.type2 == "20024"){
          cs2 <- cs1 %>% filter(zip_code == "20024")
        }else if(input$loc.type2 == "20002"){
          cs2 <- cs1 %>% filter(zip_code == "20002")
        }else if(input$loc.type2 == "20003"){
          cs2 <- cs1 %>% filter(zip_code == "20003")
        }else if(input$loc.type2 == "20001"){
          cs2 <- cs1 %>% filter(zip_code == "20001")
        }else if(input$loc.type2 == "20005"){
          cs2 <- cs1 %>% filter(zip_code == "20005")
        }else if(input$loc.type2 == "20009"){
          cs2 <- cs1 %>% filter(zip_code == "20009")
        }else if(input$loc.type2 == "20017"){
          cs2 <- cs1 %>% filter(zip_code == "20017")
        }else if(input$loc.type2 == "20010"){
          cs2 <- cs1 %>% filter(zip_code == "20010")
        }else if(input$loc.type2 == "20016"){
          cs2 <- cs1 %>% filter(zip_code == "20016")
        }else if(input$loc.type2 == "20008"){
          cs2 <- cs1 %>% filter(zip_code == "20008")
        }else if(input$loc.type2 == "20011"){
          cs2 <- cs1 %>% filter(zip_code == "20011")
        }else if(input$loc.type2 == "20007"){
          cs2 <- cs1 %>% filter(zip_code == "20007")
        }else if(input$loc.type2 == "20374"){
          cs2 <- cs1 %>% filter(zip_code == "20374")
        }else if(input$loc.type2 == "20015"){
          cs2 <- cs1 %>% filter(zip_code == "20015")
        }else if(input$loc.type2 == "20012"){
          cs2 <- cs1 %>% filter(zip_code == "20012")
        }
      }else if(input$loc.type == "Advisory Neighborhood Commission"){
        if(input$loc.type2 == "2A"){
          cs2 <- cs1 %>% filter(anc == "2A")
        }else if(input$loc.type2 == "8D"){
          cs2 <- cs1 %>% filter(anc == "8D")
        }else if(input$loc.type2 == "8E"){
          cs2 <- cs1 %>% filter(anc == "8E")
        }else if(input$loc.type2 == "8C"){
          cs2 <- cs1 %>% filter(anc == "8C")
        }else if(input$loc.type2 == "7F"){
          cs2 <- cs1 %>% filter(anc == "7F")
        }else if(input$loc.type2 == "7C"){
          cs2 <- cs1 %>% filter(anc == "7C")
        }else if(input$loc.type2 == "7E"){
          cs2 <- cs1 %>% filter(anc == "7E")
        }else if(input$loc.type2 == "5C"){
          cs2 <- cs1 %>% filter(anc == "5C")
        }else if(input$loc.type2 == "6D"){
          cs2 <- cs1 %>% filter(anc == "6D")
        }else if(input$loc.type2 == "5D"){
          cs2 <- cs1 %>% filter(anc == "5D")
        }else if(input$loc.type2 == "6B"){
          cs2 <- cs1 %>% filter(anc == "6B")
        }else if(input$loc.type2 == "5E"){
          cs2 <- cs1 %>% filter(anc == "5E")
        }else if(input$loc.type2 == "2F"){
          cs2 <- cs1 %>% filter(anc == "2F")
        }else if(input$loc.type2 == "2B"){
          cs2 <- cs1 %>% filter(anc == "2B")
        }else if(input$loc.type2 == "1B"){
          cs2 <- cs1 %>% filter(anc == "1B")
        }else if(input$loc.type2 == "6E"){
          cs2 <- cs1 %>% filter(anc == "6E")
        }else if(input$loc.type2 == "5B"){
          cs2 <- cs1 %>% filter(anc == "5B")
        }else if(input$loc.type2 == "1A"){
          cs2 <- cs1 %>% filter(anc == "1A")
        }else if(input$loc.type2 == "3C"){
          cs2 <- cs1 %>% filter(anc == "3C")
        }else if(input$loc.type2 == "6A"){
          cs2 <- cs1 %>% filter(anc == "6A")
        }else if(input$loc.type2 == "3D"){
          cs2 <- cs1 %>% filter(anc == "3D")
        }else if(input$loc.type2 == "4C"){
          cs2 <- cs1 %>% filter(anc == "4C")
        }else if(input$loc.type2 == "3E"){
          cs2 <- cs1 %>% filter(anc == "3E")
        }else if(input$loc.type2 == "2E"){
          cs2 <- cs1 %>% filter(anc == "2E")
        }else if(input$loc.type2 == "6C"){
          cs2 <- cs1 %>% filter(anc == "6C")
        }else if(input$loc.type2 == "8A"){
          cs2 <- cs1 %>% filter(anc == "8A")
        }else if(input$loc.type2 == "4D"){
          cs2 <- cs1 %>% filter(anc == "4D")
        }else if(input$loc.type2 == "4B"){
          cs2 <- cs1 %>% filter(anc == "4B")
        }else if(input$loc.type2 == "4A"){
          cs2 <- cs1 %>% filter(anc == "4A")
        }else if(input$loc.type2 == "3G"){
          cs2 <- cs1 %>% filter(anc == "3G")
        }else if(input$loc.type2 == "3F"){
          cs2 <- cs1 %>% filter(anc == "3F")
        }else if(input$loc.type2 == "5A"){
          cs2 <- cs1 %>% filter(anc == "5A")
        }
      }else if(input$loc.type == "Census Tract"){
        if(input$loc.type2 == "006202"){
          cs2 <- cs1 %>% filter(census_tract == "006202")
        }else if(input$loc.type2 == "009811"){
          cs2 <- cs1 %>% filter(census_tract == "009811")
        }else if(input$loc.type2 == "009807"){
          cs2 <- cs1 %>% filter(census_tract == "009807")
        }else if(input$loc.type2 == "009700"){
          cs2 <- cs1 %>% filter(census_tract == "009700")
        }else if(input$loc.type2 == "009804"){
          cs2 <- cs1 %>% filter(census_tract == "009804")
        }else if(input$loc.type2 == "007304"){
          cs2 <- cs1 %>% filter(census_tract == "007304")
        }else if(input$loc.type2 == "007301"){
          cs2 <- cs1 %>% filter(census_tract == "007301")
        }else if(input$loc.type2 == "010400"){
          cs2 <- cs1 %>% filter(census_tract == "010400")
        }else if(input$loc.type2 == "010800"){
          cs2 <- cs1 %>% filter(census_tract == "010800")
        }else if(input$loc.type2 == "005600"){
          cs2 <- cs1 %>% filter(census_tract == "005600")
        }else if(input$loc.type2 == "009603"){
          cs2 <- cs1 %>% filter(census_tract == "009603")
        }else if(input$loc.type2 == "007809"){
          cs2 <- cs1 %>% filter(census_tract == "007809")
        }else if(input$loc.type2 == "009902"){
          cs2 <- cs1 %>% filter(census_tract == "009902")
        }else if(input$loc.type2 == "009000"){
          cs2 <- cs1 %>% filter(census_tract == "009000")
        }else if(input$loc.type2 == "009905"){
          cs2 <- cs1 %>% filter(census_tract == "009905")
        }else if(input$loc.type2 == "007707"){
          cs2 <- cs1 %>% filter(census_tract == "007707")
        }else if(input$loc.type2 == "007806"){
          cs2 <- cs1 %>% filter(census_tract == "007806")
        }else if(input$loc.type2 == "007804"){
          cs2 <- cs1 %>% filter(census_tract == "007804")
        }else if(input$loc.type2 == "007807"){
          cs2 <- cs1 %>% filter(census_tract == "007807")
        }else if(input$loc.type2 == "007708"){
          cs2 <- cs1 %>% filter(census_tract == "007708")
        }else if(input$loc.type2 == "010500"){
          cs2 <- cs1 %>% filter(census_tract == "010500")
        }else if(input$loc.type2 == "006400"){
          cs2 <- cs1 %>% filter(census_tract == "006400")
        }else if(input$loc.type2 == "008803"){
          cs2 <- cs1 %>% filter(census_tract == "008803")
        }else if(input$loc.type2 == "010200"){
          cs2 <- cs1 %>% filter(census_tract == "010200")
        }else if(input$loc.type2 == "006500"){
          cs2 <- cs1 %>% filter(census_tract == "006500")
        }else if(input$loc.type2 == "008702"){
          cs2 <- cs1 %>% filter(census_tract == "008702")
        }else if(input$loc.type2 == "003301"){
          cs2 <- cs1 %>% filter(census_tract == "003301")
        }else if(input$loc.type2 == "004902"){
          cs2 <- cs1 %>% filter(census_tract == "004902")
        }else if(input$loc.type2 == "004201"){
          cs2 <- cs1 %>% filter(census_tract == "004201")
        }else if(input$loc.type2 == "003400"){
          cs2 <- cs1 %>% filter(census_tract == "003400")
        }else if(input$loc.type2 == "004400"){
          cs2 <- cs1 %>% filter(census_tract == "004400")
        }else if(input$loc.type2 == "011100"){
          cs2 <- cs1 %>% filter(census_tract == "011100")
        }else if(input$loc.type2 == "004702"){
          cs2 <- cs1 %>% filter(census_tract == "004702")
        }else if(input$loc.type2 == "009302"){
          cs2 <- cs1 %>% filter(census_tract == "009302")
        }else if(input$loc.type2 == "008701"){
          cs2 <- cs1 %>% filter(census_tract == "008701")
        }else if(input$loc.type2 == "010100"){
          cs2 <- cs1 %>% filter(census_tract == "010100")
        }else if(input$loc.type2 == "002900"){
          cs2 <- cs1 %>% filter(census_tract == "002900")
        }else if(input$loc.type2 == "004600"){
          cs2 <- cs1 %>% filter(census_tract == "004600")
        }else if(input$loc.type2 == "009400"){
          cs2 <- cs1 %>% filter(census_tract == "009400")
        }else if(input$loc.type2 == "001002"){
          cs2 <- cs1 %>% filter(census_tract == "001002")
        }else if(input$loc.type2 == "010600"){
          cs2 <- cs1 %>% filter(census_tract == "010600")
        }else if(input$loc.type2 == "000901"){
          cs2 <- cs1 %>% filter(census_tract == "000901")
        }else if(input$loc.type2 == "000502"){
          cs2 <- cs1 %>% filter(census_tract == "000502")
        }else if(input$loc.type2 == "009102"){
          cs2 <- cs1 %>% filter(census_tract == "009102")
        }else if(input$loc.type2 == "009201"){
          cs2 <- cs1 %>% filter(census_tract == "009201")
        }else if(input$loc.type2 == "002400"){
          cs2 <- cs1 %>% filter(census_tract == "002400")
        }else if(input$loc.type2 == "001001"){
          cs2 <- cs1 %>% filter(census_tract == "001001")
        }else if(input$loc.type2 == "000600"){
          cs2 <- cs1 %>% filter(census_tract == "000600")
        }else if(input$loc.type2 == "002501"){
          cs2 <- cs1 %>% filter(census_tract == "002501")
        }else if(input$loc.type2 == "009503"){
          cs2 <- cs1 %>% filter(census_tract == "009503")
        }else if(input$loc.type2 == "000300"){
          cs2 <- cs1 %>% filter(census_tract == "000300")
        }else if(input$loc.type2 == "000400"){
          cs2 <- cs1 %>% filter(census_tract == "000400")
        }else if(input$loc.type2 == "006700"){
          cs2 <- cs1 %>% filter(census_tract == "006700")
        }else if(input$loc.type2 == "007200"){
          cs2 <- cs1 %>% filter(census_tract == "007200")
        }else if(input$loc.type2 == "008302"){
          cs2 <- cs1 %>% filter(census_tract == "008302")
        }else if(input$loc.type2 == "006600"){
          cs2 <- cs1 %>% filter(census_tract == "006600")
        }else if(input$loc.type2 == "008904"){
          cs2 <- cs1 %>% filter(census_tract == "008904")
        }else if(input$loc.type2 == "008100"){
          cs2 <- cs1 %>% filter(census_tract == "008100")
        }else if(input$loc.type2 == "006802"){
          cs2 <- cs1 %>% filter(census_tract == "006802")
        }else if(input$loc.type2 == "007601"){
          cs2 <- cs1 %>% filter(census_tract == "007601")
        }else if(input$loc.type2 == "007100"){
          cs2 <- cs1 %>% filter(census_tract == "007100")
        }else if(input$loc.type2 == "008200"){
          cs2 <- cs1 %>% filter(census_tract == "008200")
        }else if(input$loc.type2 == "008001"){
          cs2 <- cs1 %>% filter(census_tract == "008001")
        }else if(input$loc.type2 == "008002"){
          cs2 <- cs1 %>% filter(census_tract == "008002")
        }else if(input$loc.type2 == "002101"){
          cs2 <- cs1 %>% filter(census_tract == "002101")
        }else if(input$loc.type2 == "001901"){
          cs2 <- cs1 %>% filter(census_tract == "001901")
        }else if(input$loc.type2 == "002600"){
          cs2 <- cs1 %>% filter(census_tract == "002600")
        }else if(input$loc.type2 == "001401"){
          cs2 <- cs1 %>% filter(census_tract == "001401")
        }else if(input$loc.type2 == "001803"){
          cs2 <- cs1 %>% filter(census_tract == "001803")
        }else if(input$loc.type2 == "001301"){
          cs2 <- cs1 %>% filter(census_tract == "001301")
        }else if(input$loc.type2 == "002102"){
          cs2 <- cs1 %>% filter(census_tract == "002102")
        }else if(input$loc.type2 == "001804"){
          cs2 <- cs1 %>% filter(census_tract == "001804")
        }else if(input$loc.type2 == "002001"){
          cs2 <- cs1 %>% filter(census_tract == "002001")
        }else if(input$loc.type2 == "001902"){
          cs2 <- cs1 %>% filter(census_tract == "001902")
        }else if(input$loc.type2 == "001100"){
          cs2 <- cs1 %>% filter(census_tract == "001100")
        }else if(input$loc.type2 == "001600"){
          cs2 <- cs1 %>% filter(census_tract == "001600")
        }else if(input$loc.type2 == "009509"){
          cs2 <- cs1 %>% filter(census_tract == "009509")
        }else if(input$loc.type2 == "001500"){
          cs2 <- cs1 %>% filter(census_tract == "001500")
        }
      }else if(input$loc.type == "Single Member District"){
        if(input$loc.type2 == "2A01"){
          cs2 <- cs1 %>% filter(single_member_district == "2A01")
        }else if(input$loc.type2 == "8D03"){
          cs2 <- cs1 %>% filter(single_member_district == "8D03")
        }else if(input$loc.type2 == "8D01"){
          cs2 <- cs1 %>% filter(single_member_district == "8D01")
        }else if(input$loc.type2 == "8D06"){
          cs2 <- cs1 %>% filter(single_member_district == "8D06")
        }else if(input$loc.type2 == "8E05"){
          cs2 <- cs1 %>% filter(single_member_district == "8E05")
        }else if(input$loc.type2 == "8C07"){
          cs2 <- cs1 %>% filter(single_member_district == "8C07")
        }else if(input$loc.type2 == "8E04"){
          cs2 <- cs1 %>% filter(single_member_district == "8E04")
        }else if(input$loc.type2 == "8C05"){
          cs2 <- cs1 %>% filter(single_member_district == "8C05")
        }else if(input$loc.type2 == "8C03"){
          cs2 <- cs1 %>% filter(single_member_district == "8C03")
        }else if(input$loc.type2 == "2A07"){
          cs2 <- cs1 %>% filter(single_member_district == "2A07")
        }else if(input$loc.type2 == "2A03"){
          cs2 <- cs1 %>% filter(single_member_district == "2A03")
        }else if(input$loc.type2 == "7F01"){
          cs2 <- cs1 %>% filter(single_member_district == "7F01")
        }else if(input$loc.type2 == "7C04"){
          cs2 <- cs1 %>% filter(single_member_district == "7C04")
        }else if(input$loc.type2 == "7E02"){
          cs2 <- cs1 %>% filter(single_member_district == "7E02")
        }else if(input$loc.type2 == "5C03"){
          cs2 <- cs1 %>% filter(single_member_district == "5C03")
        }else if(input$loc.type2 == "7E06"){
          cs2 <- cs1 %>% filter(single_member_district == "7E06")
        }else if(input$loc.type2 == "7E01"){
          cs2 <- cs1 %>% filter(single_member_district == "7E01")
        }else if(input$loc.type2 == "7C07"){
          cs2 <- cs1 %>% filter(single_member_district == "7C07")
        }else if(input$loc.type2 == "7C03"){
          cs2 <- cs1 %>% filter(single_member_district == "7C03")
        }else if(input$loc.type2 == "7C06"){
          cs2 <- cs1 %>% filter(single_member_district == "7C06")
        }else if(input$loc.type2 == "7F06"){
          cs2 <- cs1 %>% filter(single_member_district == "7F06")
        }else if(input$loc.type2 == "6D03"){
          cs2 <- cs1 %>% filter(single_member_district == "6D03")
        }else if(input$loc.type2 == "6D06"){
          cs2 <- cs1 %>% filter(single_member_district == "6D06")
        }else if(input$loc.type2 == "5D01"){
          cs2 <- cs1 %>% filter(single_member_district == "5D01")
        }else if(input$loc.type2 == "6D05"){
          cs2 <- cs1 %>% filter(single_member_district == "6D05")
        }else if(input$loc.type2 == "6B01"){
          cs2 <- cs1 %>% filter(single_member_district == "6B01")
        }else if(input$loc.type2 == "5E03"){
          cs2 <- cs1 %>% filter(single_member_district == "5E03")
        }else if(input$loc.type2 == "5E08"){
          cs2 <- cs1 %>% filter(single_member_district == "5E08")
        }else if(input$loc.type2 == "2F06"){
          cs2 <- cs1 %>% filter(single_member_district == "2F06")
        }else if(input$loc.type2 == "2B09"){
          cs2 <- cs1 %>% filter(single_member_district == "2B09")
        }else if(input$loc.type2 == "1B01"){
          cs2 <- cs1 %>% filter(single_member_district == "1B01")
        }else if(input$loc.type2 == "1B02"){
          cs2 <- cs1 %>% filter(single_member_district == "1B02")
        }else if(input$loc.type2 == "5C02"){
          cs2 <- cs1 %>% filter(single_member_district == "5C02")
        }else if(input$loc.type2 == "6E07"){
          cs2 <- cs1 %>% filter(single_member_district == "6E07")
        }else if(input$loc.type2 == "5B03"){
          cs2 <- cs1 %>% filter(single_member_district == "5B03")
        }else if(input$loc.type2 == "5E04"){
          cs2 <- cs1 %>% filter(single_member_district == "5E04")
        }else if(input$loc.type2 == "2F08"){
          cs2 <- cs1 %>% filter(single_member_district == "2F08")
        }else if(input$loc.type2 == "1A04"){
          cs2 <- cs1 %>% filter(single_member_district == "1A04")
        }else if(input$loc.type2 == "5E05"){
          cs2 <- cs1 %>% filter(single_member_district == "5E05")
        }else if(input$loc.type2 == "5C01"){
          cs2 <- cs1 %>% filter(single_member_district == "5C01")
        }else if(input$loc.type2 == "3C06"){
          cs2 <- cs1 %>% filter(single_member_district == "3C06")
        }else if(input$loc.type2 == "6A01"){
          cs2 <- cs1 %>% filter(single_member_district == "6A01")
        }else if(input$loc.type2 == "2B05"){
          cs2 <- cs1 %>% filter(single_member_district == "2B05")
        }else if(input$loc.type2 == "3D02"){
          cs2 <- cs1 %>% filter(single_member_district == "3D02")
        }else if(input$loc.type2 == "3C03"){
          cs2 <- cs1 %>% filter(single_member_district == "3C03")
        }else if(input$loc.type2 == "5B01"){
          cs2 <- cs1 %>% filter(single_member_district == "5B01")
        }else if(input$loc.type2 == "5C06"){
          cs2 <- cs1 %>% filter(single_member_district == "5C06")
        }else if(input$loc.type2 == "5E01"){
          cs2 <- cs1 %>% filter(single_member_district == "5E01")
        }else if(input$loc.type2 == "4C08"){
          cs2 <- cs1 %>% filter(single_member_district == "4C08")
        }else if(input$loc.type2 == "3E05"){
          cs2 <- cs1 %>% filter(single_member_district == "3E05")
        }else if(input$loc.type2 == "3C09"){
          cs2 <- cs1 %>% filter(single_member_district == "3C09")
        }else if(input$loc.type2 == "4C03"){
          cs2 <- cs1 %>% filter(single_member_district == "4C03")
        }else if(input$loc.type2 == "2E01"){
          cs2 <- cs1 %>% filter(single_member_district == "2E01")
        }else if(input$loc.type2 == "3C08"){
          cs2 <- cs1 %>% filter(single_member_district == "3C08")
        }else if(input$loc.type2 == "6B05"){
          cs2 <- cs1 %>% filter(single_member_district == "6B05")
        }else if(input$loc.type2 == "5C04"){
          cs2 <- cs1 %>% filter(single_member_district == "5C04")
        }else if(input$loc.type2 == "6D07"){
          cs2 <- cs1 %>% filter(single_member_district == "6D07")
        }else if(input$loc.type2 == "6C03"){
          cs2 <- cs1 %>% filter(single_member_district == "6C03")
        }else if(input$loc.type2 == "6B02"){
          cs2 <- cs1 %>% filter(single_member_district == "6B02")
        }else if(input$loc.type2 == "5D05"){
          cs2 <- cs1 %>% filter(single_member_district == "5D05")
        }else if(input$loc.type2 == "6A03"){
          cs2 <- cs1 %>% filter(single_member_district == "6A03")
        }else if(input$loc.type2 == "6B09"){
          cs2 <- cs1 %>% filter(single_member_district == "6B09")
        }else if(input$loc.type2 == "8A03"){
          cs2 <- cs1 %>% filter(single_member_district == "8A03")
        }else if(input$loc.type2 == "6B07"){
          cs2 <- cs1 %>% filter(single_member_district == "6B07")
        }else if(input$loc.type2 == "6B06"){
          cs2 <- cs1 %>% filter(single_member_district == "6B06")
        }else if(input$loc.type2 == "6A02"){
          cs2 <- cs1 %>% filter(single_member_district == "6A02")
        }else if(input$loc.type2 == "6A08"){
          cs2 <- cs1 %>% filter(single_member_district == "6A08")
        }else if(input$loc.type2 == "4D03"){
          cs2 <- cs1 %>% filter(single_member_district == "4D03")
        }else if(input$loc.type2 == "4B04"){
          cs2 <- cs1 %>% filter(single_member_district == "4B04")
        }else if(input$loc.type2 == "4A08"){
          cs2 <- cs1 %>% filter(single_member_district == "4A08")
        }else if(input$loc.type2 == "3D03"){
          cs2 <- cs1 %>% filter(single_member_district == "3D03")
        }else if(input$loc.type2 == "3G05"){
          cs2 <- cs1 %>% filter(single_member_district == "3G05")
        }else if(input$loc.type2 == "4A07"){
          cs2 <- cs1 %>% filter(single_member_district == "4A07")
        }else if(input$loc.type2 == "3E02"){
          cs2 <- cs1 %>% filter(single_member_district == "3E02")
        }else if(input$loc.type2 == "3F03"){
          cs2 <- cs1 %>% filter(single_member_district == "3F03")
        }else if(input$loc.type2 == "4B06"){
          cs2 <- cs1 %>% filter(single_member_district == "4B06")
        }else if(input$loc.type2 == "4A04"){
          cs2 <- cs1 %>% filter(single_member_district == "4A04")
        }else if(input$loc.type2 == "4A06"){
          cs2 <- cs1 %>% filter(single_member_district == "4A06")
        }else if(input$loc.type2 == "3E04"){
          cs2 <- cs1 %>% filter(single_member_district == "3E04")
        }else if(input$loc.type2 == "4A02"){
          cs2 <- cs1 %>% filter(single_member_district == "4A02")
        }else if(input$loc.type2 == "5A08"){
          cs2 <- cs1 %>% filter(single_member_district == "5A08")
        }else if(input$loc.type2 == "3G04"){
          cs2 <- cs1 %>% filter(single_member_district == "3G04")
        }
      }else if(input$loc.type == "Voter Precinct"){
        if(input$loc.type2 == "129"){
          cs2 <- cs1 %>% filter(voter_precinct == "129")
        }else if(input$loc.type2 == "125"){
          cs2 <- cs1 %>% filter(voter_precinct == "125")
        }else if(input$loc.type2 == "126"){
          cs2 <- cs1 %>% filter(voter_precinct == "126")
        }else if(input$loc.type2 == "121"){
          cs2 <- cs1 %>% filter(voter_precinct == "121")
        }else if(input$loc.type2 == "122"){
          cs2 <- cs1 %>% filter(voter_precinct == "122")
        }else if(input$loc.type2 == "120"){
          cs2 <- cs1 %>% filter(voter_precinct == "120")
        }else if(input$loc.type2 == "123"){
          cs2 <- cs1 %>% filter(voter_precinct == "123")
        }else if(input$loc.type2 == "2"){
          cs2 <- cs1 %>% filter(voter_precinct == "2")
        }else if(input$loc.type2 == "3"){
          cs2 <- cs1 %>% filter(voter_precinct == "3")
        }else if(input$loc.type2 == "102"){
          cs2 <- cs1 %>% filter(voter_precinct == "102")
        }else if(input$loc.type2 == "94"){
          cs2 <- cs1 %>% filter(voter_precinct == "94")
        }else if(input$loc.type2 == "110"){
          cs2 <- cs1 %>% filter(voter_precinct == "110")
        }else if(input$loc.type2 == "139"){
          cs2 <- cs1 %>% filter(voter_precinct == "139")
        }else if(input$loc.type2 == "105"){
          cs2 <- cs1 %>% filter(voter_precinct == "105")
        }else if(input$loc.type2 == "106"){
          cs2 <- cs1 %>% filter(voter_precinct == "106")
        }else if(input$loc.type2 == "93"){
          cs2 <- cs1 %>% filter(voter_precinct == "93")
        }else if(input$loc.type2 == "97"){
          cs2 <- cs1 %>% filter(voter_precinct == "97")
        }else if(input$loc.type2 == "95"){
          cs2 <- cs1 %>% filter(voter_precinct == "95")
        }else if(input$loc.type2 == "132"){
          cs2 <- cs1 %>% filter(voter_precinct == "132")
        }else if(input$loc.type2 == "128"){
          cs2 <- cs1 %>% filter(voter_precinct == "128")
        }else if(input$loc.type2 == "127"){
          cs2 <- cs1 %>% filter(voter_precinct == "127")
        }else if(input$loc.type2 == "76"){
          cs2 <- cs1 %>% filter(voter_precinct == "76")
        }else if(input$loc.type2 == "130"){
          cs2 <- cs1 %>% filter(voter_precinct == "130")
        }else if(input$loc.type2 == "75"){
          cs2 <- cs1 %>% filter(voter_precinct == "75")
        }else if(input$loc.type2 == "135"){
          cs2 <- cs1 %>% filter(voter_precinct == "135")
        }else if(input$loc.type2 == "141"){
          cs2 <- cs1 %>% filter(voter_precinct == "141")
        }else if(input$loc.type2 == "20"){
          cs2 <- cs1 %>% filter(voter_precinct == "20")
        }else if(input$loc.type2 == "22"){
          cs2 <- cs1 %>% filter(voter_precinct == "22")
        }else if(input$loc.type2 == "72"){
          cs2 <- cs1 %>% filter(voter_precinct == "72")
        }else if(input$loc.type2 == "1"){
          cs2 <- cs1 %>% filter(voter_precinct == "1")
        }else if(input$loc.type2 == "73"){
          cs2 <- cs1 %>% filter(voter_precinct == "73")
        }else if(input$loc.type2 == "42"){
          cs2 <- cs1 %>% filter(voter_precinct == "42")
        }else if(input$loc.type2 == "19"){
          cs2 <- cs1 %>% filter(voter_precinct == "19")
        }else if(input$loc.type2 == "69"){
          cs2 <- cs1 %>% filter(voter_precinct == "69")
        }else if(input$loc.type2 == "29"){
          cs2 <- cs1 %>% filter(voter_precinct == "29")
        }else if(input$loc.type2 == "82"){
          cs2 <- cs1 %>% filter(voter_precinct == "82")
        }else if(input$loc.type2 == "17"){
          cs2 <- cs1 %>% filter(voter_precinct == "17")
        }else if(input$loc.type2 == "9"){
          cs2 <- cs1 %>% filter(voter_precinct == "9")
        }else if(input$loc.type2 == "26"){
          cs2 <- cs1 %>% filter(voter_precinct == "26")
        }else if(input$loc.type2 == "74"){
          cs2 <- cs1 %>% filter(voter_precinct == "74")
        }else if(input$loc.type2 == "45"){
          cs2 <- cs1 %>% filter(voter_precinct == "45")
        }else if(input$loc.type2 == "30"){
          cs2 <- cs1 %>% filter(voter_precinct == "30")
        }else if(input$loc.type2 == "27"){
          cs2 <- cs1 %>% filter(voter_precinct == "27")
        }else if(input$loc.type2 == "48"){
          cs2 <- cs1 %>% filter(voter_precinct == "48")
        }else if(input$loc.type2 == "67"){
          cs2 <- cs1 %>% filter(voter_precinct == "67")
        }else if(input$loc.type2 == "6"){
          cs2 <- cs1 %>% filter(voter_precinct == "6")
        }else if(input$loc.type2 == "12"){
          cs2 <- cs1 %>% filter(voter_precinct == "12")
        }else if(input$loc.type2 == "88"){
          cs2 <- cs1 %>% filter(voter_precinct == "88")
        }else if(input$loc.type2 == "131"){
          cs2 <- cs1 %>% filter(voter_precinct == "131")
        }else if(input$loc.type2 == "85"){
          cs2 <- cs1 %>% filter(voter_precinct == "85")
        }else if(input$loc.type2 == "89"){
          cs2 <- cs1 %>% filter(voter_precinct == "89")
        }else if(input$loc.type2 == "79"){
          cs2 <- cs1 %>% filter(voter_precinct == "79")
        }else if(input$loc.type2 == "91"){
          cs2 <- cs1 %>% filter(voter_precinct == "91")
        }else if(input$loc.type2 == "133"){
          cs2 <- cs1 %>% filter(voter_precinct == "133")
        }else if(input$loc.type2 == "81"){
          cs2 <- cs1 %>% filter(voter_precinct == "81")
        }else if(input$loc.type2 == "71"){
          cs2 <- cs1 %>% filter(voter_precinct == "71")
        }else if(input$loc.type2 == "86"){
          cs2 <- cs1 %>% filter(voter_precinct == "86")
        }else if(input$loc.type2 == "56"){
          cs2 <- cs1 %>% filter(voter_precinct == "56")
        }else if(input$loc.type2 == "59"){
          cs2 <- cs1 %>% filter(voter_precinct == "59")
        }else if(input$loc.type2 == "50"){
          cs2 <- cs1 %>% filter(voter_precinct == "50")
        }else if(input$loc.type2 == "61"){
          cs2 <- cs1 %>% filter(voter_precinct == "61")
        }else if(input$loc.type2 == "31"){
          cs2 <- cs1 %>% filter(voter_precinct == "31")
        }else if(input$loc.type2 == "138"){
          cs2 <- cs1 %>% filter(voter_precinct == "138")
        }else if(input$loc.type2 == "57"){
          cs2 <- cs1 %>% filter(voter_precinct == "57")
        }else if(input$loc.type2 == "60"){
          cs2 <- cs1 %>% filter(voter_precinct == "60")
        }else if(input$loc.type2 == "53"){
          cs2 <- cs1 %>% filter(voter_precinct == "53")
        }else if(input$loc.type2 == "32"){
          cs2 <- cs1 %>% filter(voter_precinct == "32")
        }else if(input$loc.type2 == "62"){
          cs2 <- cs1 %>% filter(voter_precinct == "62")
        }else if(input$loc.type2 == "66"){
          cs2 <- cs1 %>% filter(voter_precinct == "66")
        }else if(input$loc.type2 == "51"){
          cs2 <- cs1 %>% filter(voter_precinct == "51")
        }
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
        if(input$loc.type2 == "SE"){
          ls2 <- ls1 %>% filter(quadrant == "SE")
        }else if(input$loc.type2 == "SW"){
          ls2 <- ls1 %>% filter(quadrant == "SW")
        }else if(input$loc.type2 == "NE"){
          ls2 <- ls1 %>% filter(quadrant == "NE")
        }else if(input$loc.type2 == "NW"){
          ls2 <- ls1 %>% filter(quadrant == "NW")
        }
      }else if(input$loc.type == "Ward"){
        if(input$loc.type2 == "1"){
          ls2 <- ls1 %>% filter(ward == "1")
        }else if(input$loc.type2 == "2"){
          ls2 <- ls1 %>% filter(ward == "2")
        }else if(input$loc.type2 == "3"){
          ls2 <- ls1 %>% filter(ward == "3")
        }else if(input$loc.type2 == "4"){
          ls2 <- ls1 %>% filter(ward == "4")
        }else if(input$loc.type2 == "5"){
          ls2 <- ls1 %>% filter(ward == "5")
        }else if(input$loc.type2 == "6"){
          ls2 <- ls1 %>% filter(ward == "6")
        }else if(input$loc.type2 == "7"){
          ls2 <- ls1 %>% filter(ward == "7")
        }else if(input$loc.type2 == "8"){
          ls2 <- ls1 %>% filter(ward == "8")
        }
      }else if(input$loc.type == "Zip Code"){
        if(input$loc.type2 == "20227"){
          ls2 <- ls1 %>% filter(zip_code == "20227")
        }else if(input$loc.type2 == "20032"){
          ls2 <- ls1 %>% filter(zip_code == "20032")
        }else if(input$loc.type2 == "20037"){
          ls2 <- ls1 %>% filter(zip_code == "20037")
        }else if(input$loc.type2 == "20019"){
          ls2 <- ls1 %>% filter(zip_code == "20019")
        }else if(input$loc.type2 == "20020"){
          ls2 <- ls1 %>% filter(zip_code == "20020")
        }else if(input$loc.type2 == "20018"){
          ls2 <- ls1 %>% filter(zip_code == "20018")
        }else if(input$loc.type2 == "20024"){
          ls2 <- ls1 %>% filter(zip_code == "20024")
        }else if(input$loc.type2 == "20002"){
          ls2 <- ls1 %>% filter(zip_code == "20002")
        }else if(input$loc.type2 == "20003"){
          ls2 <- ls1 %>% filter(zip_code == "20003")
        }else if(input$loc.type2 == "20001"){
          ls2 <- ls1 %>% filter(zip_code == "20001")
        }else if(input$loc.type2 == "20005"){
          ls2 <- ls1 %>% filter(zip_code == "20005")
        }else if(input$loc.type2 == "20009"){
          ls2 <- ls1 %>% filter(zip_code == "20009")
        }else if(input$loc.type2 == "20017"){
          ls2 <- ls1 %>% filter(zip_code == "20017")
        }else if(input$loc.type2 == "20010"){
          ls2 <- ls1 %>% filter(zip_code == "20010")
        }else if(input$loc.type2 == "20016"){
          ls2 <- ls1 %>% filter(zip_code == "20016")
        }else if(input$loc.type2 == "20008"){
          ls2 <- ls1 %>% filter(zip_code == "20008")
        }else if(input$loc.type2 == "20011"){
          ls2 <- ls1 %>% filter(zip_code == "20011")
        }else if(input$loc.type2 == "20007"){
          ls2 <- ls1 %>% filter(zip_code == "20007")
        }else if(input$loc.type2 == "20374"){
          ls2 <- ls1 %>% filter(zip_code == "20374")
        }else if(input$loc.type2 == "20015"){
          ls2 <- ls1 %>% filter(zip_code == "20015")
        }else if(input$loc.type2 == "20012"){
          ls2 <- ls1 %>% filter(zip_code == "20012")
        }
      }else if(input$loc.type == "Advisory Neighborhood Commission"){
        if(input$loc.type2 == "2A"){
          ls2 <- ls1 %>% filter(anc == "2A")
        }else if(input$loc.type2 == "8D"){
          ls2 <- ls1 %>% filter(anc == "8D")
        }else if(input$loc.type2 == "8E"){
          ls2 <- ls1 %>% filter(anc == "8E")
        }else if(input$loc.type2 == "8C"){
          ls2 <- ls1 %>% filter(anc == "8C")
        }else if(input$loc.type2 == "7F"){
          ls2 <- ls1 %>% filter(anc == "7F")
        }else if(input$loc.type2 == "7C"){
          ls2 <- ls1 %>% filter(anc == "7C")
        }else if(input$loc.type2 == "7E"){
          ls2 <- ls1 %>% filter(anc == "7E")
        }else if(input$loc.type2 == "5C"){
          ls2 <- ls1 %>% filter(anc == "5C")
        }else if(input$loc.type2 == "6D"){
          ls2 <- ls1 %>% filter(anc == "6D")
        }else if(input$loc.type2 == "5D"){
          ls2 <- ls1 %>% filter(anc == "5D")
        }else if(input$loc.type2 == "6B"){
          ls2 <- ls1 %>% filter(anc == "6B")
        }else if(input$loc.type2 == "5E"){
          ls2 <- ls1 %>% filter(anc == "5E")
        }else if(input$loc.type2 == "2F"){
          ls2 <- ls1 %>% filter(anc == "2F")
        }else if(input$loc.type2 == "2B"){
          ls2 <- ls1 %>% filter(anc == "2B")
        }else if(input$loc.type2 == "1B"){
          ls2 <- ls1 %>% filter(anc == "1B")
        }else if(input$loc.type2 == "6E"){
          ls2 <- ls1 %>% filter(anc == "6E")
        }else if(input$loc.type2 == "5B"){
          ls2 <- ls1 %>% filter(anc == "5B")
        }else if(input$loc.type2 == "1A"){
          ls2 <- ls1 %>% filter(anc == "1A")
        }else if(input$loc.type2 == "3C"){
          ls2 <- ls1 %>% filter(anc == "3C")
        }else if(input$loc.type2 == "6A"){
          ls2 <- ls1 %>% filter(anc == "6A")
        }else if(input$loc.type2 == "3D"){
          ls2 <- ls1 %>% filter(anc == "3D")
        }else if(input$loc.type2 == "4C"){
          ls2 <- ls1 %>% filter(anc == "4C")
        }else if(input$loc.type2 == "3E"){
          ls2 <- ls1 %>% filter(anc == "3E")
        }else if(input$loc.type2 == "2E"){
          ls2 <- ls1 %>% filter(anc == "2E")
        }else if(input$loc.type2 == "6C"){
          ls2 <- ls1 %>% filter(anc == "6C")
        }else if(input$loc.type2 == "8A"){
          ls2 <- ls1 %>% filter(anc == "8A")
        }else if(input$loc.type2 == "4D"){
          ls2 <- ls1 %>% filter(anc == "4D")
        }else if(input$loc.type2 == "4B"){
          ls2 <- ls1 %>% filter(anc == "4B")
        }else if(input$loc.type2 == "4A"){
          ls2 <- ls1 %>% filter(anc == "4A")
        }else if(input$loc.type2 == "3G"){
          ls2 <- ls1 %>% filter(anc == "3G")
        }else if(input$loc.type2 == "3F"){
          ls2 <- ls1 %>% filter(anc == "3F")
        }else if(input$loc.type2 == "5A"){
          ls2 <- ls1 %>% filter(anc == "5A")
        }
      }else if(input$loc.type == "Census Tract"){
        if(input$loc.type2 == "006202"){
          ls2 <- ls1 %>% filter(census_tract == "006202")
        }else if(input$loc.type2 == "009811"){
          ls2 <- ls1 %>% filter(census_tract == "009811")
        }else if(input$loc.type2 == "009807"){
          ls2 <- ls1 %>% filter(census_tract == "009807")
        }else if(input$loc.type2 == "009700"){
          ls2 <- ls1 %>% filter(census_tract == "009700")
        }else if(input$loc.type2 == "009804"){
          ls2 <- ls1 %>% filter(census_tract == "009804")
        }else if(input$loc.type2 == "007304"){
          ls2 <- ls1 %>% filter(census_tract == "007304")
        }else if(input$loc.type2 == "007301"){
          ls2 <- ls1 %>% filter(census_tract == "007301")
        }else if(input$loc.type2 == "010400"){
          ls2 <- ls1 %>% filter(census_tract == "010400")
        }else if(input$loc.type2 == "010800"){
          ls2 <- ls1 %>% filter(census_tract == "010800")
        }else if(input$loc.type2 == "005600"){
          ls2 <- ls1 %>% filter(census_tract == "005600")
        }else if(input$loc.type2 == "009603"){
          ls2 <- ls1 %>% filter(census_tract == "009603")
        }else if(input$loc.type2 == "007809"){
          ls2 <- ls1 %>% filter(census_tract == "007809")
        }else if(input$loc.type2 == "009902"){
          ls2 <- ls1 %>% filter(census_tract == "009902")
        }else if(input$loc.type2 == "009000"){
          ls2 <- ls1 %>% filter(census_tract == "009000")
        }else if(input$loc.type2 == "009905"){
          ls2 <- ls1 %>% filter(census_tract == "009905")
        }else if(input$loc.type2 == "007707"){
          ls2 <- ls1 %>% filter(census_tract == "007707")
        }else if(input$loc.type2 == "007806"){
          ls2 <- ls1 %>% filter(census_tract == "007806")
        }else if(input$loc.type2 == "007804"){
          ls2 <- ls1 %>% filter(census_tract == "007804")
        }else if(input$loc.type2 == "007807"){
          ls2 <- ls1 %>% filter(census_tract == "007807")
        }else if(input$loc.type2 == "007708"){
          ls2 <- ls1 %>% filter(census_tract == "007708")
        }else if(input$loc.type2 == "010500"){
          ls2 <- ls1 %>% filter(census_tract == "010500")
        }else if(input$loc.type2 == "006400"){
          ls2 <- ls1 %>% filter(census_tract == "006400")
        }else if(input$loc.type2 == "008803"){
          ls2 <- ls1 %>% filter(census_tract == "008803")
        }else if(input$loc.type2 == "010200"){
          ls2 <- ls1 %>% filter(census_tract == "010200")
        }else if(input$loc.type2 == "006500"){
          ls2 <- ls1 %>% filter(census_tract == "006500")
        }else if(input$loc.type2 == "008702"){
          ls2 <- ls1 %>% filter(census_tract == "008702")
        }else if(input$loc.type2 == "003301"){
          ls2 <- ls1 %>% filter(census_tract == "003301")
        }else if(input$loc.type2 == "004902"){
          ls2 <- ls1 %>% filter(census_tract == "004902")
        }else if(input$loc.type2 == "004201"){
          ls2 <- ls1 %>% filter(census_tract == "004201")
        }else if(input$loc.type2 == "003400"){
          ls2 <- ls1 %>% filter(census_tract == "003400")
        }else if(input$loc.type2 == "004400"){
          ls2 <- ls1 %>% filter(census_tract == "004400")
        }else if(input$loc.type2 == "011100"){
          ls2 <- ls1 %>% filter(census_tract == "011100")
        }else if(input$loc.type2 == "004702"){
          ls2 <- ls1 %>% filter(census_tract == "004702")
        }else if(input$loc.type2 == "009302"){
          ls2 <- ls1 %>% filter(census_tract == "009302")
        }else if(input$loc.type2 == "008701"){
          ls2 <- ls1 %>% filter(census_tract == "008701")
        }else if(input$loc.type2 == "010100"){
          ls2 <- ls1 %>% filter(census_tract == "010100")
        }else if(input$loc.type2 == "002900"){
          ls2 <- ls1 %>% filter(census_tract == "002900")
        }else if(input$loc.type2 == "004600"){
          ls2 <- ls1 %>% filter(census_tract == "004600")
        }else if(input$loc.type2 == "009400"){
          ls2 <- ls1 %>% filter(census_tract == "009400")
        }else if(input$loc.type2 == "001002"){
          ls2 <- ls1 %>% filter(census_tract == "001002")
        }else if(input$loc.type2 == "010600"){
          ls2 <- ls1 %>% filter(census_tract == "010600")
        }else if(input$loc.type2 == "000901"){
          ls2 <- ls1 %>% filter(census_tract == "000901")
        }else if(input$loc.type2 == "000502"){
          ls2 <- ls1 %>% filter(census_tract == "000502")
        }else if(input$loc.type2 == "009102"){
          ls2 <- ls1 %>% filter(census_tract == "009102")
        }else if(input$loc.type2 == "009201"){
          ls2 <- ls1 %>% filter(census_tract == "009201")
        }else if(input$loc.type2 == "002400"){
          ls2 <- ls1 %>% filter(census_tract == "002400")
        }else if(input$loc.type2 == "001001"){
          ls2 <- ls1 %>% filter(census_tract == "001001")
        }else if(input$loc.type2 == "000600"){
          ls2 <- ls1 %>% filter(census_tract == "000600")
        }else if(input$loc.type2 == "002501"){
          ls2 <- ls1 %>% filter(census_tract == "002501")
        }else if(input$loc.type2 == "009503"){
          ls2 <- ls1 %>% filter(census_tract == "009503")
        }else if(input$loc.type2 == "000300"){
          ls2 <- ls1 %>% filter(census_tract == "000300")
        }else if(input$loc.type2 == "000400"){
          ls2 <- ls1 %>% filter(census_tract == "000400")
        }else if(input$loc.type2 == "006700"){
          ls2 <- ls1 %>% filter(census_tract == "006700")
        }else if(input$loc.type2 == "007200"){
          ls2 <- ls1 %>% filter(census_tract == "007200")
        }else if(input$loc.type2 == "008302"){
          ls2 <- ls1 %>% filter(census_tract == "008302")
        }else if(input$loc.type2 == "006600"){
          ls2 <- ls1 %>% filter(census_tract == "006600")
        }else if(input$loc.type2 == "008904"){
          ls2 <- ls1 %>% filter(census_tract == "008904")
        }else if(input$loc.type2 == "008100"){
          ls2 <- ls1 %>% filter(census_tract == "008100")
        }else if(input$loc.type2 == "006802"){
          ls2 <- ls1 %>% filter(census_tract == "006802")
        }else if(input$loc.type2 == "007601"){
          ls2 <- ls1 %>% filter(census_tract == "007601")
        }else if(input$loc.type2 == "007100"){
          ls2 <- ls1 %>% filter(census_tract == "007100")
        }else if(input$loc.type2 == "008200"){
          ls2 <- ls1 %>% filter(census_tract == "008200")
        }else if(input$loc.type2 == "008001"){
          ls2 <- ls1 %>% filter(census_tract == "008001")
        }else if(input$loc.type2 == "008002"){
          ls2 <- ls1 %>% filter(census_tract == "008002")
        }else if(input$loc.type2 == "002101"){
          ls2 <- ls1 %>% filter(census_tract == "002101")
        }else if(input$loc.type2 == "001901"){
          ls2 <- ls1 %>% filter(census_tract == "001901")
        }else if(input$loc.type2 == "002600"){
          ls2 <- ls1 %>% filter(census_tract == "002600")
        }else if(input$loc.type2 == "001401"){
          ls2 <- ls1 %>% filter(census_tract == "001401")
        }else if(input$loc.type2 == "001803"){
          ls2 <- ls1 %>% filter(census_tract == "001803")
        }else if(input$loc.type2 == "001301"){
          ls2 <- ls1 %>% filter(census_tract == "001301")
        }else if(input$loc.type2 == "002102"){
          ls2 <- ls1 %>% filter(census_tract == "002102")
        }else if(input$loc.type2 == "001804"){
          ls2 <- ls1 %>% filter(census_tract == "001804")
        }else if(input$loc.type2 == "002001"){
          ls2 <- ls1 %>% filter(census_tract == "002001")
        }else if(input$loc.type2 == "001902"){
          ls2 <- ls1 %>% filter(census_tract == "001902")
        }else if(input$loc.type2 == "001100"){
          ls2 <- ls1 %>% filter(census_tract == "001100")
        }else if(input$loc.type2 == "001600"){
          ls2 <- ls1 %>% filter(census_tract == "001600")
        }else if(input$loc.type2 == "009509"){
          ls2 <- ls1 %>% filter(census_tract == "009509")
        }else if(input$loc.type2 == "001500"){
          ls2 <- ls1 %>% filter(census_tract == "001500")
        }
      }else if(input$loc.type == "Single Member District"){
        if(input$loc.type2 == "2A01"){
          ls2 <- ls1 %>% filter(single_member_district == "2A01")
        }else if(input$loc.type2 == "8D03"){
          ls2 <- ls1 %>% filter(single_member_district == "8D03")
        }else if(input$loc.type2 == "8D01"){
          ls2 <- ls1 %>% filter(single_member_district == "8D01")
        }else if(input$loc.type2 == "8D06"){
          ls2 <- ls1 %>% filter(single_member_district == "8D06")
        }else if(input$loc.type2 == "8E05"){
          ls2 <- ls1 %>% filter(single_member_district == "8E05")
        }else if(input$loc.type2 == "8C07"){
          ls2 <- ls1 %>% filter(single_member_district == "8C07")
        }else if(input$loc.type2 == "8E04"){
          ls2 <- ls1 %>% filter(single_member_district == "8E04")
        }else if(input$loc.type2 == "8C05"){
          ls2 <- ls1 %>% filter(single_member_district == "8C05")
        }else if(input$loc.type2 == "8C03"){
          ls2 <- ls1 %>% filter(single_member_district == "8C03")
        }else if(input$loc.type2 == "2A07"){
          ls2 <- ls1 %>% filter(single_member_district == "2A07")
        }else if(input$loc.type2 == "2A03"){
          ls2 <- ls1 %>% filter(single_member_district == "2A03")
        }else if(input$loc.type2 == "7F01"){
          ls2 <- ls1 %>% filter(single_member_district == "7F01")
        }else if(input$loc.type2 == "7C04"){
          ls2 <- ls1 %>% filter(single_member_district == "7C04")
        }else if(input$loc.type2 == "7E02"){
          ls2 <- ls1 %>% filter(single_member_district == "7E02")
        }else if(input$loc.type2 == "5C03"){
          ls2 <- ls1 %>% filter(single_member_district == "5C03")
        }else if(input$loc.type2 == "7E06"){
          ls2 <- ls1 %>% filter(single_member_district == "7E06")
        }else if(input$loc.type2 == "7E01"){
          ls2 <- ls1 %>% filter(single_member_district == "7E01")
        }else if(input$loc.type2 == "7C07"){
          ls2 <- ls1 %>% filter(single_member_district == "7C07")
        }else if(input$loc.type2 == "7C03"){
          ls2 <- ls1 %>% filter(single_member_district == "7C03")
        }else if(input$loc.type2 == "7C06"){
          ls2 <- ls1 %>% filter(single_member_district == "7C06")
        }else if(input$loc.type2 == "7F06"){
          ls2 <- ls1 %>% filter(single_member_district == "7F06")
        }else if(input$loc.type2 == "6D03"){
          ls2 <- ls1 %>% filter(single_member_district == "6D03")
        }else if(input$loc.type2 == "6D06"){
          ls2 <- ls1 %>% filter(single_member_district == "6D06")
        }else if(input$loc.type2 == "5D01"){
          ls2 <- ls1 %>% filter(single_member_district == "5D01")
        }else if(input$loc.type2 == "6D05"){
          ls2 <- ls1 %>% filter(single_member_district == "6D05")
        }else if(input$loc.type2 == "6B01"){
          ls2 <- ls1 %>% filter(single_member_district == "6B01")
        }else if(input$loc.type2 == "5E03"){
          ls2 <- ls1 %>% filter(single_member_district == "5E03")
        }else if(input$loc.type2 == "5E08"){
          ls2 <- ls1 %>% filter(single_member_district == "5E08")
        }else if(input$loc.type2 == "2F06"){
          ls2 <- ls1 %>% filter(single_member_district == "2F06")
        }else if(input$loc.type2 == "2B09"){
          ls2 <- ls1 %>% filter(single_member_district == "2B09")
        }else if(input$loc.type2 == "1B01"){
          ls2 <- ls1 %>% filter(single_member_district == "1B01")
        }else if(input$loc.type2 == "1B02"){
          ls2 <- ls1 %>% filter(single_member_district == "1B02")
        }else if(input$loc.type2 == "5C02"){
          ls2 <- ls1 %>% filter(single_member_district == "5C02")
        }else if(input$loc.type2 == "6E07"){
          ls2 <- ls1 %>% filter(single_member_district == "6E07")
        }else if(input$loc.type2 == "5B03"){
          ls2 <- ls1 %>% filter(single_member_district == "5B03")
        }else if(input$loc.type2 == "5E04"){
          ls2 <- ls1 %>% filter(single_member_district == "5E04")
        }else if(input$loc.type2 == "2F08"){
          ls2 <- ls1 %>% filter(single_member_district == "2F08")
        }else if(input$loc.type2 == "1A04"){
          ls2 <- ls1 %>% filter(single_member_district == "1A04")
        }else if(input$loc.type2 == "5E05"){
          ls2 <- ls1 %>% filter(single_member_district == "5E05")
        }else if(input$loc.type2 == "5C01"){
          ls2 <- ls1 %>% filter(single_member_district == "5C01")
        }else if(input$loc.type2 == "3C06"){
          ls2 <- ls1 %>% filter(single_member_district == "3C06")
        }else if(input$loc.type2 == "6A01"){
          ls2 <- ls1 %>% filter(single_member_district == "6A01")
        }else if(input$loc.type2 == "2B05"){
          ls2 <- ls1 %>% filter(single_member_district == "2B05")
        }else if(input$loc.type2 == "3D02"){
          ls2 <- ls1 %>% filter(single_member_district == "3D02")
        }else if(input$loc.type2 == "3C03"){
          ls2 <- ls1 %>% filter(single_member_district == "3C03")
        }else if(input$loc.type2 == "5B01"){
          ls2 <- ls1 %>% filter(single_member_district == "5B01")
        }else if(input$loc.type2 == "5C06"){
          ls2 <- ls1 %>% filter(single_member_district == "5C06")
        }else if(input$loc.type2 == "5E01"){
          ls2 <- ls1 %>% filter(single_member_district == "5E01")
        }else if(input$loc.type2 == "4C08"){
          ls2 <- ls1 %>% filter(single_member_district == "4C08")
        }else if(input$loc.type2 == "3E05"){
          ls2 <- ls1 %>% filter(single_member_district == "3E05")
        }else if(input$loc.type2 == "3C09"){
          ls2 <- ls1 %>% filter(single_member_district == "3C09")
        }else if(input$loc.type2 == "4C03"){
          ls2 <- ls1 %>% filter(single_member_district == "4C03")
        }else if(input$loc.type2 == "2E01"){
          ls2 <- ls1 %>% filter(single_member_district == "2E01")
        }else if(input$loc.type2 == "3C08"){
          ls2 <- ls1 %>% filter(single_member_district == "3C08")
        }else if(input$loc.type2 == "6B05"){
          ls2 <- ls1 %>% filter(single_member_district == "6B05")
        }else if(input$loc.type2 == "5C04"){
          ls2 <- ls1 %>% filter(single_member_district == "5C04")
        }else if(input$loc.type2 == "6D07"){
          ls2 <- ls1 %>% filter(single_member_district == "6D07")
        }else if(input$loc.type2 == "6C03"){
          ls2 <- ls1 %>% filter(single_member_district == "6C03")
        }else if(input$loc.type2 == "6B02"){
          ls2 <- ls1 %>% filter(single_member_district == "6B02")
        }else if(input$loc.type2 == "5D05"){
          ls2 <- ls1 %>% filter(single_member_district == "5D05")
        }else if(input$loc.type2 == "6A03"){
          ls2 <- ls1 %>% filter(single_member_district == "6A03")
        }else if(input$loc.type2 == "6B09"){
          ls2 <- ls1 %>% filter(single_member_district == "6B09")
        }else if(input$loc.type2 == "8A03"){
          ls2 <- ls1 %>% filter(single_member_district == "8A03")
        }else if(input$loc.type2 == "6B07"){
          ls2 <- ls1 %>% filter(single_member_district == "6B07")
        }else if(input$loc.type2 == "6B06"){
          ls2 <- ls1 %>% filter(single_member_district == "6B06")
        }else if(input$loc.type2 == "6A02"){
          ls2 <- ls1 %>% filter(single_member_district == "6A02")
        }else if(input$loc.type2 == "6A08"){
          ls2 <- ls1 %>% filter(single_member_district == "6A08")
        }else if(input$loc.type2 == "4D03"){
          ls2 <- ls1 %>% filter(single_member_district == "4D03")
        }else if(input$loc.type2 == "4B04"){
          ls2 <- ls1 %>% filter(single_member_district == "4B04")
        }else if(input$loc.type2 == "4A08"){
          ls2 <- ls1 %>% filter(single_member_district == "4A08")
        }else if(input$loc.type2 == "3D03"){
          ls2 <- ls1 %>% filter(single_member_district == "3D03")
        }else if(input$loc.type2 == "3G05"){
          ls2 <- ls1 %>% filter(single_member_district == "3G05")
        }else if(input$loc.type2 == "4A07"){
          ls2 <- ls1 %>% filter(single_member_district == "4A07")
        }else if(input$loc.type2 == "3E02"){
          ls2 <- ls1 %>% filter(single_member_district == "3E02")
        }else if(input$loc.type2 == "3F03"){
          ls2 <- ls1 %>% filter(single_member_district == "3F03")
        }else if(input$loc.type2 == "4B06"){
          ls2 <- ls1 %>% filter(single_member_district == "4B06")
        }else if(input$loc.type2 == "4A04"){
          ls2 <- ls1 %>% filter(single_member_district == "4A04")
        }else if(input$loc.type2 == "4A06"){
          ls2 <- ls1 %>% filter(single_member_district == "4A06")
        }else if(input$loc.type2 == "3E04"){
          ls2 <- ls1 %>% filter(single_member_district == "3E04")
        }else if(input$loc.type2 == "4A02"){
          ls2 <- ls1 %>% filter(single_member_district == "4A02")
        }else if(input$loc.type2 == "5A08"){
          ls2 <- ls1 %>% filter(single_member_district == "5A08")
        }else if(input$loc.type2 == "3G04"){
          ls2 <- ls1 %>% filter(single_member_district == "3G04")
        }
      }else if(input$loc.type == "Voter Precinct"){
        if(input$loc.type2 == "129"){
          ls2 <- ls1 %>% filter(voter_precinct == "129")
        }else if(input$loc.type2 == "125"){
          ls2 <- ls1 %>% filter(voter_precinct == "125")
        }else if(input$loc.type2 == "126"){
          ls2 <- ls1 %>% filter(voter_precinct == "126")
        }else if(input$loc.type2 == "121"){
          ls2 <- ls1 %>% filter(voter_precinct == "121")
        }else if(input$loc.type2 == "122"){
          ls2 <- ls1 %>% filter(voter_precinct == "122")
        }else if(input$loc.type2 == "120"){
          ls2 <- ls1 %>% filter(voter_precinct == "120")
        }else if(input$loc.type2 == "123"){
          ls2 <- ls1 %>% filter(voter_precinct == "123")
        }else if(input$loc.type2 == "2"){
          ls2 <- ls1 %>% filter(voter_precinct == "2")
        }else if(input$loc.type2 == "3"){
          ls2 <- ls1 %>% filter(voter_precinct == "3")
        }else if(input$loc.type2 == "102"){
          ls2 <- ls1 %>% filter(voter_precinct == "102")
        }else if(input$loc.type2 == "94"){
          ls2 <- ls1 %>% filter(voter_precinct == "94")
        }else if(input$loc.type2 == "110"){
          ls2 <- ls1 %>% filter(voter_precinct == "110")
        }else if(input$loc.type2 == "139"){
          ls2 <- ls1 %>% filter(voter_precinct == "139")
        }else if(input$loc.type2 == "105"){
          ls2 <- ls1 %>% filter(voter_precinct == "105")
        }else if(input$loc.type2 == "106"){
          ls2 <- ls1 %>% filter(voter_precinct == "106")
        }else if(input$loc.type2 == "93"){
          ls2 <- ls1 %>% filter(voter_precinct == "93")
        }else if(input$loc.type2 == "97"){
          ls2 <- ls1 %>% filter(voter_precinct == "97")
        }else if(input$loc.type2 == "95"){
          ls2 <- ls1 %>% filter(voter_precinct == "95")
        }else if(input$loc.type2 == "132"){
          ls2 <- ls1 %>% filter(voter_precinct == "132")
        }else if(input$loc.type2 == "128"){
          ls2 <- ls1 %>% filter(voter_precinct == "128")
        }else if(input$loc.type2 == "127"){
          ls2 <- ls1 %>% filter(voter_precinct == "127")
        }else if(input$loc.type2 == "76"){
          ls2 <- ls1 %>% filter(voter_precinct == "76")
        }else if(input$loc.type2 == "130"){
          ls2 <- ls1 %>% filter(voter_precinct == "130")
        }else if(input$loc.type2 == "75"){
          ls2 <- ls1 %>% filter(voter_precinct == "75")
        }else if(input$loc.type2 == "135"){
          ls2 <- ls1 %>% filter(voter_precinct == "135")
        }else if(input$loc.type2 == "141"){
          ls2 <- ls1 %>% filter(voter_precinct == "141")
        }else if(input$loc.type2 == "20"){
          ls2 <- ls1 %>% filter(voter_precinct == "20")
        }else if(input$loc.type2 == "22"){
          ls2 <- ls1 %>% filter(voter_precinct == "22")
        }else if(input$loc.type2 == "72"){
          ls2 <- ls1 %>% filter(voter_precinct == "72")
        }else if(input$loc.type2 == "1"){
          ls2 <- ls1 %>% filter(voter_precinct == "1")
        }else if(input$loc.type2 == "73"){
          ls2 <- ls1 %>% filter(voter_precinct == "73")
        }else if(input$loc.type2 == "42"){
          ls2 <- ls1 %>% filter(voter_precinct == "42")
        }else if(input$loc.type2 == "19"){
          ls2 <- ls1 %>% filter(voter_precinct == "19")
        }else if(input$loc.type2 == "69"){
          ls2 <- ls1 %>% filter(voter_precinct == "69")
        }else if(input$loc.type2 == "29"){
          ls2 <- ls1 %>% filter(voter_precinct == "29")
        }else if(input$loc.type2 == "82"){
          ls2 <- ls1 %>% filter(voter_precinct == "82")
        }else if(input$loc.type2 == "17"){
          ls2 <- ls1 %>% filter(voter_precinct == "17")
        }else if(input$loc.type2 == "9"){
          ls2 <- ls1 %>% filter(voter_precinct == "9")
        }else if(input$loc.type2 == "26"){
          ls2 <- ls1 %>% filter(voter_precinct == "26")
        }else if(input$loc.type2 == "74"){
          ls2 <- ls1 %>% filter(voter_precinct == "74")
        }else if(input$loc.type2 == "45"){
          ls2 <- ls1 %>% filter(voter_precinct == "45")
        }else if(input$loc.type2 == "30"){
          ls2 <- ls1 %>% filter(voter_precinct == "30")
        }else if(input$loc.type2 == "27"){
          ls2 <- ls1 %>% filter(voter_precinct == "27")
        }else if(input$loc.type2 == "48"){
          ls2 <- ls1 %>% filter(voter_precinct == "48")
        }else if(input$loc.type2 == "67"){
          ls2 <- ls1 %>% filter(voter_precinct == "67")
        }else if(input$loc.type2 == "6"){
          ls2 <- ls1 %>% filter(voter_precinct == "6")
        }else if(input$loc.type2 == "12"){
          ls2 <- ls1 %>% filter(voter_precinct == "12")
        }else if(input$loc.type2 == "88"){
          ls2 <- ls1 %>% filter(voter_precinct == "88")
        }else if(input$loc.type2 == "131"){
          ls2 <- ls1 %>% filter(voter_precinct == "131")
        }else if(input$loc.type2 == "85"){
          ls2 <- ls1 %>% filter(voter_precinct == "85")
        }else if(input$loc.type2 == "89"){
          ls2 <- ls1 %>% filter(voter_precinct == "89")
        }else if(input$loc.type2 == "79"){
          ls2 <- ls1 %>% filter(voter_precinct == "79")
        }else if(input$loc.type2 == "91"){
          ls2 <- ls1 %>% filter(voter_precinct == "91")
        }else if(input$loc.type2 == "133"){
          ls2 <- ls1 %>% filter(voter_precinct == "133")
        }else if(input$loc.type2 == "81"){
          ls2 <- ls1 %>% filter(voter_precinct == "81")
        }else if(input$loc.type2 == "71"){
          ls2 <- ls1 %>% filter(voter_precinct == "71")
        }else if(input$loc.type2 == "86"){
          ls2 <- ls1 %>% filter(voter_precinct == "86")
        }else if(input$loc.type2 == "56"){
          ls2 <- ls1 %>% filter(voter_precinct == "56")
        }else if(input$loc.type2 == "59"){
          ls2 <- ls1 %>% filter(voter_precinct == "59")
        }else if(input$loc.type2 == "50"){
          ls2 <- ls1 %>% filter(voter_precinct == "50")
        }else if(input$loc.type2 == "61"){
          ls2 <- ls1 %>% filter(voter_precinct == "61")
        }else if(input$loc.type2 == "31"){
          ls2 <- ls1 %>% filter(voter_precinct == "31")
        }else if(input$loc.type2 == "138"){
          ls2 <- ls1 %>% filter(voter_precinct == "138")
        }else if(input$loc.type2 == "57"){
          ls2 <- ls1 %>% filter(voter_precinct == "57")
        }else if(input$loc.type2 == "60"){
          ls2 <- ls1 %>% filter(voter_precinct == "60")
        }else if(input$loc.type2 == "53"){
          ls2 <- ls1 %>% filter(voter_precinct == "53")
        }else if(input$loc.type2 == "32"){
          ls2 <- ls1 %>% filter(voter_precinct == "32")
        }else if(input$loc.type2 == "62"){
          ls2 <- ls1 %>% filter(voter_precinct == "62")
        }else if(input$loc.type2 == "66"){
          ls2 <- ls1 %>% filter(voter_precinct == "66")
        }else if(input$loc.type2 == "51"){
          ls2 <- ls1 %>% filter(voter_precinct == "51")
        }
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
      
      if(input$loc.type == "Quadrant"){
        if(input$loc.type2 == "SE"){
          cs2 <- com.s1 %>% filter(quadrant == "SE")
        }else if(input$loc.type2 == "SW"){
          cs2 <- com.s1 %>% filter(quadrant == "SW")
        }else if(input$loc.type2 == "NE"){
          cs2 <- com.s1 %>% filter(quadrant == "NE")
        }else if(input$loc.type2 == "NW"){
          cs2 <- com.s1 %>% filter(quadrant == "NW")
        }
      }else if(input$loc.type == "Ward"){
        if(input$loc.type2 == "1"){
          cs2 <- com.s1 %>% filter(ward == "1")
        }else if(input$loc.type2 == "2"){
          cs2 <- com.s1 %>% filter(ward == "2")
        }else if(input$loc.type2 == "3"){
          cs2 <- com.s1 %>% filter(ward == "3")
        }else if(input$loc.type2 == "4"){
          cs2 <- com.s1 %>% filter(ward == "4")
        }else if(input$loc.type2 == "5"){
          cs2 <- com.s1 %>% filter(ward == "5")
        }else if(input$loc.type2 == "6"){
          cs2 <- com.s1 %>% filter(ward == "6")
        }else if(input$loc.type2 == "7"){
          cs2 <- com.s1 %>% filter(ward == "7")
        }else if(input$loc.type2 == "8"){
          cs2 <- com.s1 %>% filter(ward == "8")
        }
      }else if(input$loc.type == "Zip Code"){
        if(input$loc.type2 == "20227"){
          cs2 <- com.s1 %>% filter(zip_code == "20227")
        }else if(input$loc.type2 == "20032"){
          cs2 <- com.s1 %>% filter(zip_code == "20032")
        }else if(input$loc.type2 == "20037"){
          cs2 <- com.s1 %>% filter(zip_code == "20037")
        }else if(input$loc.type2 == "20019"){
          cs2 <- com.s1 %>% filter(zip_code == "20019")
        }else if(input$loc.type2 == "20020"){
          cs2 <- com.s1 %>% filter(zip_code == "20020")
        }else if(input$loc.type2 == "20018"){
          cs2 <- com.s1 %>% filter(zip_code == "20018")
        }else if(input$loc.type2 == "20024"){
          cs2 <- com.s1 %>% filter(zip_code == "20024")
        }else if(input$loc.type2 == "20002"){
          cs2 <- com.s1 %>% filter(zip_code == "20002")
        }else if(input$loc.type2 == "20003"){
          cs2 <- com.s1 %>% filter(zip_code == "20003")
        }else if(input$loc.type2 == "20001"){
          cs2 <- com.s1 %>% filter(zip_code == "20001")
        }else if(input$loc.type2 == "20005"){
          cs2 <- com.s1 %>% filter(zip_code == "20005")
        }else if(input$loc.type2 == "20009"){
          cs2 <- com.s1 %>% filter(zip_code == "20009")
        }else if(input$loc.type2 == "20017"){
          cs2 <- com.s1 %>% filter(zip_code == "20017")
        }else if(input$loc.type2 == "20010"){
          cs2 <- com.s1 %>% filter(zip_code == "20010")
        }else if(input$loc.type2 == "20016"){
          cs2 <- com.s1 %>% filter(zip_code == "20016")
        }else if(input$loc.type2 == "20008"){
          cs2 <- com.s1 %>% filter(zip_code == "20008")
        }else if(input$loc.type2 == "20011"){
          cs2 <- com.s1 %>% filter(zip_code == "20011")
        }else if(input$loc.type2 == "20007"){
          cs2 <- com.s1 %>% filter(zip_code == "20007")
        }else if(input$loc.type2 == "20374"){
          cs2 <- com.s1 %>% filter(zip_code == "20374")
        }else if(input$loc.type2 == "20015"){
          cs2 <- com.s1 %>% filter(zip_code == "20015")
        }else if(input$loc.type2 == "20012"){
          cs2 <- com.s1 %>% filter(zip_code == "20012")
        }
      }else if(input$loc.type == "Advisory Neighborhood Commission"){
        if(input$loc.type2 == "2A"){
          cs2 <- com.s1 %>% filter(anc == "2A")
        }else if(input$loc.type2 == "8D"){
          cs2 <- com.s1 %>% filter(anc == "8D")
        }else if(input$loc.type2 == "8E"){
          cs2 <- com.s1 %>% filter(anc == "8E")
        }else if(input$loc.type2 == "8C"){
          cs2 <- com.s1 %>% filter(anc == "8C")
        }else if(input$loc.type2 == "7F"){
          cs2 <- com.s1 %>% filter(anc == "7F")
        }else if(input$loc.type2 == "7C"){
          cs2 <- com.s1 %>% filter(anc == "7C")
        }else if(input$loc.type2 == "7E"){
          cs2 <- com.s1 %>% filter(anc == "7E")
        }else if(input$loc.type2 == "5C"){
          cs2 <- com.s1 %>% filter(anc == "5C")
        }else if(input$loc.type2 == "6D"){
          cs2 <- com.s1 %>% filter(anc == "6D")
        }else if(input$loc.type2 == "5D"){
          cs2 <- com.s1 %>% filter(anc == "5D")
        }else if(input$loc.type2 == "6B"){
          cs2 <- com.s1 %>% filter(anc == "6B")
        }else if(input$loc.type2 == "5E"){
          cs2 <- com.s1 %>% filter(anc == "5E")
        }else if(input$loc.type2 == "2F"){
          cs2 <- com.s1 %>% filter(anc == "2F")
        }else if(input$loc.type2 == "2B"){
          cs2 <- com.s1 %>% filter(anc == "2B")
        }else if(input$loc.type2 == "1B"){
          cs2 <- com.s1 %>% filter(anc == "1B")
        }else if(input$loc.type2 == "6E"){
          cs2 <- com.s1 %>% filter(anc == "6E")
        }else if(input$loc.type2 == "5B"){
          cs2 <- com.s1 %>% filter(anc == "5B")
        }else if(input$loc.type2 == "1A"){
          cs2 <- com.s1 %>% filter(anc == "1A")
        }else if(input$loc.type2 == "3C"){
          cs2 <- com.s1 %>% filter(anc == "3C")
        }else if(input$loc.type2 == "6A"){
          cs2 <- com.s1 %>% filter(anc == "6A")
        }else if(input$loc.type2 == "3D"){
          cs2 <- com.s1 %>% filter(anc == "3D")
        }else if(input$loc.type2 == "4C"){
          cs2 <- com.s1 %>% filter(anc == "4C")
        }else if(input$loc.type2 == "3E"){
          cs2 <- com.s1 %>% filter(anc == "3E")
        }else if(input$loc.type2 == "2E"){
          cs2 <- com.s1 %>% filter(anc == "2E")
        }else if(input$loc.type2 == "6C"){
          cs2 <- com.s1 %>% filter(anc == "6C")
        }else if(input$loc.type2 == "8A"){
          cs2 <- com.s1 %>% filter(anc == "8A")
        }else if(input$loc.type2 == "4D"){
          cs2 <- com.s1 %>% filter(anc == "4D")
        }else if(input$loc.type2 == "4B"){
          cs2 <- com.s1 %>% filter(anc == "4B")
        }else if(input$loc.type2 == "4A"){
          cs2 <- com.s1 %>% filter(anc == "4A")
        }else if(input$loc.type2 == "3G"){
          cs2 <- com.s1 %>% filter(anc == "3G")
        }else if(input$loc.type2 == "3F"){
          cs2 <- com.s1 %>% filter(anc == "3F")
        }else if(input$loc.type2 == "5A"){
          cs2 <- com.s1 %>% filter(anc == "5A")
        }
      }else if(input$loc.type == "Census Tract"){
        if(input$loc.type2 == "006202"){
          cs2 <- com.s1 %>% filter(census_tract == "006202")
        }else if(input$loc.type2 == "009811"){
          cs2 <- com.s1 %>% filter(census_tract == "009811")
        }else if(input$loc.type2 == "009807"){
          cs2 <- com.s1 %>% filter(census_tract == "009807")
        }else if(input$loc.type2 == "009700"){
          cs2 <- com.s1 %>% filter(census_tract == "009700")
        }else if(input$loc.type2 == "009804"){
          cs2 <- com.s1 %>% filter(census_tract == "009804")
        }else if(input$loc.type2 == "007304"){
          cs2 <- com.s1 %>% filter(census_tract == "007304")
        }else if(input$loc.type2 == "007301"){
          cs2 <- com.s1 %>% filter(census_tract == "007301")
        }else if(input$loc.type2 == "010400"){
          cs2 <- com.s1 %>% filter(census_tract == "010400")
        }else if(input$loc.type2 == "010800"){
          cs2 <- com.s1 %>% filter(census_tract == "010800")
        }else if(input$loc.type2 == "005600"){
          cs2 <- com.s1 %>% filter(census_tract == "005600")
        }else if(input$loc.type2 == "009603"){
          cs2 <- com.s1 %>% filter(census_tract == "009603")
        }else if(input$loc.type2 == "007809"){
          cs2 <- com.s1 %>% filter(census_tract == "007809")
        }else if(input$loc.type2 == "009902"){
          cs2 <- com.s1 %>% filter(census_tract == "009902")
        }else if(input$loc.type2 == "009000"){
          cs2 <- com.s1 %>% filter(census_tract == "009000")
        }else if(input$loc.type2 == "009905"){
          cs2 <- com.s1 %>% filter(census_tract == "009905")
        }else if(input$loc.type2 == "007707"){
          cs2 <- com.s1 %>% filter(census_tract == "007707")
        }else if(input$loc.type2 == "007806"){
          cs2 <- com.s1 %>% filter(census_tract == "007806")
        }else if(input$loc.type2 == "007804"){
          cs2 <- com.s1 %>% filter(census_tract == "007804")
        }else if(input$loc.type2 == "007807"){
          cs2 <- com.s1 %>% filter(census_tract == "007807")
        }else if(input$loc.type2 == "007708"){
          cs2 <- com.s1 %>% filter(census_tract == "007708")
        }else if(input$loc.type2 == "010500"){
          cs2 <- com.s1 %>% filter(census_tract == "010500")
        }else if(input$loc.type2 == "006400"){
          cs2 <- com.s1 %>% filter(census_tract == "006400")
        }else if(input$loc.type2 == "008803"){
          cs2 <- com.s1 %>% filter(census_tract == "008803")
        }else if(input$loc.type2 == "010200"){
          cs2 <- com.s1 %>% filter(census_tract == "010200")
        }else if(input$loc.type2 == "006500"){
          cs2 <- com.s1 %>% filter(census_tract == "006500")
        }else if(input$loc.type2 == "008702"){
          cs2 <- com.s1 %>% filter(census_tract == "008702")
        }else if(input$loc.type2 == "003301"){
          cs2 <- com.s1 %>% filter(census_tract == "003301")
        }else if(input$loc.type2 == "004902"){
          cs2 <- com.s1 %>% filter(census_tract == "004902")
        }else if(input$loc.type2 == "004201"){
          cs2 <- com.s1 %>% filter(census_tract == "004201")
        }else if(input$loc.type2 == "003400"){
          cs2 <- com.s1 %>% filter(census_tract == "003400")
        }else if(input$loc.type2 == "004400"){
          cs2 <- com.s1 %>% filter(census_tract == "004400")
        }else if(input$loc.type2 == "011100"){
          cs2 <- com.s1 %>% filter(census_tract == "011100")
        }else if(input$loc.type2 == "004702"){
          cs2 <- com.s1 %>% filter(census_tract == "004702")
        }else if(input$loc.type2 == "009302"){
          cs2 <- com.s1 %>% filter(census_tract == "009302")
        }else if(input$loc.type2 == "008701"){
          cs2 <- com.s1 %>% filter(census_tract == "008701")
        }else if(input$loc.type2 == "010100"){
          cs2 <- com.s1 %>% filter(census_tract == "010100")
        }else if(input$loc.type2 == "002900"){
          cs2 <- com.s1 %>% filter(census_tract == "002900")
        }else if(input$loc.type2 == "004600"){
          cs2 <- com.s1 %>% filter(census_tract == "004600")
        }else if(input$loc.type2 == "009400"){
          cs2 <- com.s1 %>% filter(census_tract == "009400")
        }else if(input$loc.type2 == "001002"){
          cs2 <- com.s1 %>% filter(census_tract == "001002")
        }else if(input$loc.type2 == "010600"){
          cs2 <- com.s1 %>% filter(census_tract == "010600")
        }else if(input$loc.type2 == "000901"){
          cs2 <- com.s1 %>% filter(census_tract == "000901")
        }else if(input$loc.type2 == "000502"){
          cs2 <- com.s1 %>% filter(census_tract == "000502")
        }else if(input$loc.type2 == "009102"){
          cs2 <- com.s1 %>% filter(census_tract == "009102")
        }else if(input$loc.type2 == "009201"){
          cs2 <- com.s1 %>% filter(census_tract == "009201")
        }else if(input$loc.type2 == "002400"){
          cs2 <- com.s1 %>% filter(census_tract == "002400")
        }else if(input$loc.type2 == "001001"){
          cs2 <- com.s1 %>% filter(census_tract == "001001")
        }else if(input$loc.type2 == "000600"){
          cs2 <- com.s1 %>% filter(census_tract == "000600")
        }else if(input$loc.type2 == "002501"){
          cs2 <- com.s1 %>% filter(census_tract == "002501")
        }else if(input$loc.type2 == "009503"){
          cs2 <- com.s1 %>% filter(census_tract == "009503")
        }else if(input$loc.type2 == "000300"){
          cs2 <- com.s1 %>% filter(census_tract == "000300")
        }else if(input$loc.type2 == "000400"){
          cs2 <- com.s1 %>% filter(census_tract == "000400")
        }else if(input$loc.type2 == "006700"){
          cs2 <- com.s1 %>% filter(census_tract == "006700")
        }else if(input$loc.type2 == "007200"){
          cs2 <- com.s1 %>% filter(census_tract == "007200")
        }else if(input$loc.type2 == "008302"){
          cs2 <- com.s1 %>% filter(census_tract == "008302")
        }else if(input$loc.type2 == "006600"){
          cs2 <- com.s1 %>% filter(census_tract == "006600")
        }else if(input$loc.type2 == "008904"){
          cs2 <- com.s1 %>% filter(census_tract == "008904")
        }else if(input$loc.type2 == "008100"){
          cs2 <- com.s1 %>% filter(census_tract == "008100")
        }else if(input$loc.type2 == "006802"){
          cs2 <- com.s1 %>% filter(census_tract == "006802")
        }else if(input$loc.type2 == "007601"){
          cs2 <- com.s1 %>% filter(census_tract == "007601")
        }else if(input$loc.type2 == "007100"){
          cs2 <- com.s1 %>% filter(census_tract == "007100")
        }else if(input$loc.type2 == "008200"){
          cs2 <- com.s1 %>% filter(census_tract == "008200")
        }else if(input$loc.type2 == "008001"){
          cs2 <- com.s1 %>% filter(census_tract == "008001")
        }else if(input$loc.type2 == "008002"){
          cs2 <- com.s1 %>% filter(census_tract == "008002")
        }else if(input$loc.type2 == "002101"){
          cs2 <- com.s1 %>% filter(census_tract == "002101")
        }else if(input$loc.type2 == "001901"){
          cs2 <- com.s1 %>% filter(census_tract == "001901")
        }else if(input$loc.type2 == "002600"){
          cs2 <- com.s1 %>% filter(census_tract == "002600")
        }else if(input$loc.type2 == "001401"){
          cs2 <- com.s1 %>% filter(census_tract == "001401")
        }else if(input$loc.type2 == "001803"){
          cs2 <- com.s1 %>% filter(census_tract == "001803")
        }else if(input$loc.type2 == "001301"){
          cs2 <- com.s1 %>% filter(census_tract == "001301")
        }else if(input$loc.type2 == "002102"){
          cs2 <- com.s1 %>% filter(census_tract == "002102")
        }else if(input$loc.type2 == "001804"){
          cs2 <- com.s1 %>% filter(census_tract == "001804")
        }else if(input$loc.type2 == "002001"){
          cs2 <- com.s1 %>% filter(census_tract == "002001")
        }else if(input$loc.type2 == "001902"){
          cs2 <- com.s1 %>% filter(census_tract == "001902")
        }else if(input$loc.type2 == "001100"){
          cs2 <- com.s1 %>% filter(census_tract == "001100")
        }else if(input$loc.type2 == "001600"){
          cs2 <- com.s1 %>% filter(census_tract == "001600")
        }else if(input$loc.type2 == "009509"){
          cs2 <- com.s1 %>% filter(census_tract == "009509")
        }else if(input$loc.type2 == "001500"){
          cs2 <- com.s1 %>% filter(census_tract == "001500")
        }
      }else if(input$loc.type == "Single Member District"){
        if(input$loc.type2 == "2A01"){
          cs2 <- com.s1 %>% filter(single_member_district == "2A01")
        }else if(input$loc.type2 == "8D03"){
          cs2 <- com.s1 %>% filter(single_member_district == "8D03")
        }else if(input$loc.type2 == "8D01"){
          cs2 <- com.s1 %>% filter(single_member_district == "8D01")
        }else if(input$loc.type2 == "8D06"){
          cs2 <- com.s1 %>% filter(single_member_district == "8D06")
        }else if(input$loc.type2 == "8E05"){
          cs2 <- com.s1 %>% filter(single_member_district == "8E05")
        }else if(input$loc.type2 == "8C07"){
          cs2 <- com.s1 %>% filter(single_member_district == "8C07")
        }else if(input$loc.type2 == "8E04"){
          cs2 <- com.s1 %>% filter(single_member_district == "8E04")
        }else if(input$loc.type2 == "8C05"){
          cs2 <- com.s1 %>% filter(single_member_district == "8C05")
        }else if(input$loc.type2 == "8C03"){
          cs2 <- com.s1 %>% filter(single_member_district == "8C03")
        }else if(input$loc.type2 == "2A07"){
          cs2 <- com.s1 %>% filter(single_member_district == "2A07")
        }else if(input$loc.type2 == "2A03"){
          cs2 <- com.s1 %>% filter(single_member_district == "2A03")
        }else if(input$loc.type2 == "7F01"){
          cs2 <- com.s1 %>% filter(single_member_district == "7F01")
        }else if(input$loc.type2 == "7C04"){
          cs2 <- com.s1 %>% filter(single_member_district == "7C04")
        }else if(input$loc.type2 == "7E02"){
          cs2 <- com.s1 %>% filter(single_member_district == "7E02")
        }else if(input$loc.type2 == "5C03"){
          cs2 <- com.s1 %>% filter(single_member_district == "5C03")
        }else if(input$loc.type2 == "7E06"){
          cs2 <- com.s1 %>% filter(single_member_district == "7E06")
        }else if(input$loc.type2 == "7E01"){
          cs2 <- com.s1 %>% filter(single_member_district == "7E01")
        }else if(input$loc.type2 == "7C07"){
          cs2 <- com.s1 %>% filter(single_member_district == "7C07")
        }else if(input$loc.type2 == "7C03"){
          cs2 <- com.s1 %>% filter(single_member_district == "7C03")
        }else if(input$loc.type2 == "7C06"){
          cs2 <- com.s1 %>% filter(single_member_district == "7C06")
        }else if(input$loc.type2 == "7F06"){
          cs2 <- com.s1 %>% filter(single_member_district == "7F06")
        }else if(input$loc.type2 == "6D03"){
          cs2 <- com.s1 %>% filter(single_member_district == "6D03")
        }else if(input$loc.type2 == "6D06"){
          cs2 <- com.s1 %>% filter(single_member_district == "6D06")
        }else if(input$loc.type2 == "5D01"){
          cs2 <- com.s1 %>% filter(single_member_district == "5D01")
        }else if(input$loc.type2 == "6D05"){
          cs2 <- com.s1 %>% filter(single_member_district == "6D05")
        }else if(input$loc.type2 == "6B01"){
          cs2 <- com.s1 %>% filter(single_member_district == "6B01")
        }else if(input$loc.type2 == "5E03"){
          cs2 <- com.s1 %>% filter(single_member_district == "5E03")
        }else if(input$loc.type2 == "5E08"){
          cs2 <- com.s1 %>% filter(single_member_district == "5E08")
        }else if(input$loc.type2 == "2F06"){
          cs2 <- com.s1 %>% filter(single_member_district == "2F06")
        }else if(input$loc.type2 == "2B09"){
          cs2 <- com.s1 %>% filter(single_member_district == "2B09")
        }else if(input$loc.type2 == "1B01"){
          cs2 <- com.s1 %>% filter(single_member_district == "1B01")
        }else if(input$loc.type2 == "1B02"){
          cs2 <- com.s1 %>% filter(single_member_district == "1B02")
        }else if(input$loc.type2 == "5C02"){
          cs2 <- com.s1 %>% filter(single_member_district == "5C02")
        }else if(input$loc.type2 == "6E07"){
          cs2 <- com.s1 %>% filter(single_member_district == "6E07")
        }else if(input$loc.type2 == "5B03"){
          cs2 <- com.s1 %>% filter(single_member_district == "5B03")
        }else if(input$loc.type2 == "5E04"){
          cs2 <- com.s1 %>% filter(single_member_district == "5E04")
        }else if(input$loc.type2 == "2F08"){
          cs2 <- com.s1 %>% filter(single_member_district == "2F08")
        }else if(input$loc.type2 == "1A04"){
          cs2 <- com.s1 %>% filter(single_member_district == "1A04")
        }else if(input$loc.type2 == "5E05"){
          cs2 <- com.s1 %>% filter(single_member_district == "5E05")
        }else if(input$loc.type2 == "5C01"){
          cs2 <- com.s1 %>% filter(single_member_district == "5C01")
        }else if(input$loc.type2 == "3C06"){
          cs2 <- com.s1 %>% filter(single_member_district == "3C06")
        }else if(input$loc.type2 == "6A01"){
          cs2 <- com.s1 %>% filter(single_member_district == "6A01")
        }else if(input$loc.type2 == "2B05"){
          cs2 <- com.s1 %>% filter(single_member_district == "2B05")
        }else if(input$loc.type2 == "3D02"){
          cs2 <- com.s1 %>% filter(single_member_district == "3D02")
        }else if(input$loc.type2 == "3C03"){
          cs2 <- com.s1 %>% filter(single_member_district == "3C03")
        }else if(input$loc.type2 == "5B01"){
          cs2 <- com.s1 %>% filter(single_member_district == "5B01")
        }else if(input$loc.type2 == "5C06"){
          cs2 <- com.s1 %>% filter(single_member_district == "5C06")
        }else if(input$loc.type2 == "5E01"){
          cs2 <- com.s1 %>% filter(single_member_district == "5E01")
        }else if(input$loc.type2 == "4C08"){
          cs2 <- com.s1 %>% filter(single_member_district == "4C08")
        }else if(input$loc.type2 == "3E05"){
          cs2 <- com.s1 %>% filter(single_member_district == "3E05")
        }else if(input$loc.type2 == "3C09"){
          cs2 <- com.s1 %>% filter(single_member_district == "3C09")
        }else if(input$loc.type2 == "4C03"){
          cs2 <- com.s1 %>% filter(single_member_district == "4C03")
        }else if(input$loc.type2 == "2E01"){
          cs2 <- com.s1 %>% filter(single_member_district == "2E01")
        }else if(input$loc.type2 == "3C08"){
          cs2 <- com.s1 %>% filter(single_member_district == "3C08")
        }else if(input$loc.type2 == "6B05"){
          cs2 <- com.s1 %>% filter(single_member_district == "6B05")
        }else if(input$loc.type2 == "5C04"){
          cs2 <- com.s1 %>% filter(single_member_district == "5C04")
        }else if(input$loc.type2 == "6D07"){
          cs2 <- com.s1 %>% filter(single_member_district == "6D07")
        }else if(input$loc.type2 == "6C03"){
          cs2 <- com.s1 %>% filter(single_member_district == "6C03")
        }else if(input$loc.type2 == "6B02"){
          cs2 <- com.s1 %>% filter(single_member_district == "6B02")
        }else if(input$loc.type2 == "5D05"){
          cs2 <- com.s1 %>% filter(single_member_district == "5D05")
        }else if(input$loc.type2 == "6A03"){
          cs2 <- com.s1 %>% filter(single_member_district == "6A03")
        }else if(input$loc.type2 == "6B09"){
          cs2 <- com.s1 %>% filter(single_member_district == "6B09")
        }else if(input$loc.type2 == "8A03"){
          cs2 <- com.s1 %>% filter(single_member_district == "8A03")
        }else if(input$loc.type2 == "6B07"){
          cs2 <- com.s1 %>% filter(single_member_district == "6B07")
        }else if(input$loc.type2 == "6B06"){
          cs2 <- com.s1 %>% filter(single_member_district == "6B06")
        }else if(input$loc.type2 == "6A02"){
          cs2 <- com.s1 %>% filter(single_member_district == "6A02")
        }else if(input$loc.type2 == "6A08"){
          cs2 <- com.s1 %>% filter(single_member_district == "6A08")
        }else if(input$loc.type2 == "4D03"){
          cs2 <- com.s1 %>% filter(single_member_district == "4D03")
        }else if(input$loc.type2 == "4B04"){
          cs2 <- com.s1 %>% filter(single_member_district == "4B04")
        }else if(input$loc.type2 == "4A08"){
          cs2 <- com.s1 %>% filter(single_member_district == "4A08")
        }else if(input$loc.type2 == "3D03"){
          cs2 <- com.s1 %>% filter(single_member_district == "3D03")
        }else if(input$loc.type2 == "3G05"){
          cs2 <- com.s1 %>% filter(single_member_district == "3G05")
        }else if(input$loc.type2 == "4A07"){
          cs2 <- com.s1 %>% filter(single_member_district == "4A07")
        }else if(input$loc.type2 == "3E02"){
          cs2 <- com.s1 %>% filter(single_member_district == "3E02")
        }else if(input$loc.type2 == "3F03"){
          cs2 <- com.s1 %>% filter(single_member_district == "3F03")
        }else if(input$loc.type2 == "4B06"){
          cs2 <- com.s1 %>% filter(single_member_district == "4B06")
        }else if(input$loc.type2 == "4A04"){
          cs2 <- com.s1 %>% filter(single_member_district == "4A04")
        }else if(input$loc.type2 == "4A06"){
          cs2 <- com.s1 %>% filter(single_member_district == "4A06")
        }else if(input$loc.type2 == "3E04"){
          cs2 <- com.s1 %>% filter(single_member_district == "3E04")
        }else if(input$loc.type2 == "4A02"){
          cs2 <- com.s1 %>% filter(single_member_district == "4A02")
        }else if(input$loc.type2 == "5A08"){
          cs2 <- com.s1 %>% filter(single_member_district == "5A08")
        }else if(input$loc.type2 == "3G04"){
          cs2 <- com.s1 %>% filter(single_member_district == "3G04")
        }
      }else if(input$loc.type == "Voter Precinct"){
        if(input$loc.type2 == "129"){
          cs2 <- com.s1 %>% filter(voter_precinct == "129")
        }else if(input$loc.type2 == "125"){
          cs2 <- com.s1 %>% filter(voter_precinct == "125")
        }else if(input$loc.type2 == "126"){
          cs2 <- com.s1 %>% filter(voter_precinct == "126")
        }else if(input$loc.type2 == "121"){
          cs2 <- com.s1 %>% filter(voter_precinct == "121")
        }else if(input$loc.type2 == "122"){
          cs2 <- com.s1 %>% filter(voter_precinct == "122")
        }else if(input$loc.type2 == "120"){
          cs2 <- com.s1 %>% filter(voter_precinct == "120")
        }else if(input$loc.type2 == "123"){
          cs2 <- com.s1 %>% filter(voter_precinct == "123")
        }else if(input$loc.type2 == "2"){
          cs2 <- com.s1 %>% filter(voter_precinct == "2")
        }else if(input$loc.type2 == "3"){
          cs2 <- com.s1 %>% filter(voter_precinct == "3")
        }else if(input$loc.type2 == "102"){
          cs2 <- com.s1 %>% filter(voter_precinct == "102")
        }else if(input$loc.type2 == "94"){
          cs2 <- com.s1 %>% filter(voter_precinct == "94")
        }else if(input$loc.type2 == "110"){
          cs2 <- com.s1 %>% filter(voter_precinct == "110")
        }else if(input$loc.type2 == "139"){
          cs2 <- com.s1 %>% filter(voter_precinct == "139")
        }else if(input$loc.type2 == "105"){
          cs2 <- com.s1 %>% filter(voter_precinct == "105")
        }else if(input$loc.type2 == "106"){
          cs2 <- com.s1 %>% filter(voter_precinct == "106")
        }else if(input$loc.type2 == "93"){
          cs2 <- com.s1 %>% filter(voter_precinct == "93")
        }else if(input$loc.type2 == "97"){
          cs2 <- com.s1 %>% filter(voter_precinct == "97")
        }else if(input$loc.type2 == "95"){
          cs2 <- com.s1 %>% filter(voter_precinct == "95")
        }else if(input$loc.type2 == "132"){
          cs2 <- com.s1 %>% filter(voter_precinct == "132")
        }else if(input$loc.type2 == "128"){
          cs2 <- com.s1 %>% filter(voter_precinct == "128")
        }else if(input$loc.type2 == "127"){
          cs2 <- com.s1 %>% filter(voter_precinct == "127")
        }else if(input$loc.type2 == "76"){
          cs2 <- com.s1 %>% filter(voter_precinct == "76")
        }else if(input$loc.type2 == "130"){
          cs2 <- com.s1 %>% filter(voter_precinct == "130")
        }else if(input$loc.type2 == "75"){
          cs2 <- com.s1 %>% filter(voter_precinct == "75")
        }else if(input$loc.type2 == "135"){
          cs2 <- com.s1 %>% filter(voter_precinct == "135")
        }else if(input$loc.type2 == "141"){
          cs2 <- com.s1 %>% filter(voter_precinct == "141")
        }else if(input$loc.type2 == "20"){
          cs2 <- com.s1 %>% filter(voter_precinct == "20")
        }else if(input$loc.type2 == "22"){
          cs2 <- com.s1 %>% filter(voter_precinct == "22")
        }else if(input$loc.type2 == "72"){
          cs2 <- com.s1 %>% filter(voter_precinct == "72")
        }else if(input$loc.type2 == "1"){
          cs2 <- com.s1 %>% filter(voter_precinct == "1")
        }else if(input$loc.type2 == "73"){
          cs2 <- com.s1 %>% filter(voter_precinct == "73")
        }else if(input$loc.type2 == "42"){
          cs2 <- com.s1 %>% filter(voter_precinct == "42")
        }else if(input$loc.type2 == "19"){
          cs2 <- com.s1 %>% filter(voter_precinct == "19")
        }else if(input$loc.type2 == "69"){
          cs2 <- com.s1 %>% filter(voter_precinct == "69")
        }else if(input$loc.type2 == "29"){
          cs2 <- com.s1 %>% filter(voter_precinct == "29")
        }else if(input$loc.type2 == "82"){
          cs2 <- com.s1 %>% filter(voter_precinct == "82")
        }else if(input$loc.type2 == "17"){
          cs2 <- com.s1 %>% filter(voter_precinct == "17")
        }else if(input$loc.type2 == "9"){
          cs2 <- com.s1 %>% filter(voter_precinct == "9")
        }else if(input$loc.type2 == "26"){
          cs2 <- com.s1 %>% filter(voter_precinct == "26")
        }else if(input$loc.type2 == "74"){
          cs2 <- com.s1 %>% filter(voter_precinct == "74")
        }else if(input$loc.type2 == "45"){
          cs2 <- com.s1 %>% filter(voter_precinct == "45")
        }else if(input$loc.type2 == "30"){
          cs2 <- com.s1 %>% filter(voter_precinct == "30")
        }else if(input$loc.type2 == "27"){
          cs2 <- com.s1 %>% filter(voter_precinct == "27")
        }else if(input$loc.type2 == "48"){
          cs2 <- com.s1 %>% filter(voter_precinct == "48")
        }else if(input$loc.type2 == "67"){
          cs2 <- com.s1 %>% filter(voter_precinct == "67")
        }else if(input$loc.type2 == "6"){
          cs2 <- com.s1 %>% filter(voter_precinct == "6")
        }else if(input$loc.type2 == "12"){
          cs2 <- com.s1 %>% filter(voter_precinct == "12")
        }else if(input$loc.type2 == "88"){
          cs2 <- com.s1 %>% filter(voter_precinct == "88")
        }else if(input$loc.type2 == "131"){
          cs2 <- com.s1 %>% filter(voter_precinct == "131")
        }else if(input$loc.type2 == "85"){
          cs2 <- com.s1 %>% filter(voter_precinct == "85")
        }else if(input$loc.type2 == "89"){
          cs2 <- com.s1 %>% filter(voter_precinct == "89")
        }else if(input$loc.type2 == "79"){
          cs2 <- com.s1 %>% filter(voter_precinct == "79")
        }else if(input$loc.type2 == "91"){
          cs2 <- com.s1 %>% filter(voter_precinct == "91")
        }else if(input$loc.type2 == "133"){
          cs2 <- com.s1 %>% filter(voter_precinct == "133")
        }else if(input$loc.type2 == "81"){
          cs2 <- com.s1 %>% filter(voter_precinct == "81")
        }else if(input$loc.type2 == "71"){
          cs2 <- com.s1 %>% filter(voter_precinct == "71")
        }else if(input$loc.type2 == "86"){
          cs2 <- com.s1 %>% filter(voter_precinct == "86")
        }else if(input$loc.type2 == "56"){
          cs2 <- com.s1 %>% filter(voter_precinct == "56")
        }else if(input$loc.type2 == "59"){
          cs2 <- com.s1 %>% filter(voter_precinct == "59")
        }else if(input$loc.type2 == "50"){
          cs2 <- com.s1 %>% filter(voter_precinct == "50")
        }else if(input$loc.type2 == "61"){
          cs2 <- com.s1 %>% filter(voter_precinct == "61")
        }else if(input$loc.type2 == "31"){
          cs2 <- com.s1 %>% filter(voter_precinct == "31")
        }else if(input$loc.type2 == "138"){
          cs2 <- com.s1 %>% filter(voter_precinct == "138")
        }else if(input$loc.type2 == "57"){
          cs2 <- com.s1 %>% filter(voter_precinct == "57")
        }else if(input$loc.type2 == "60"){
          cs2 <- com.s1 %>% filter(voter_precinct == "60")
        }else if(input$loc.type2 == "53"){
          cs2 <- com.s1 %>% filter(voter_precinct == "53")
        }else if(input$loc.type2 == "32"){
          cs2 <- com.s1 %>% filter(voter_precinct == "32")
        }else if(input$loc.type2 == "62"){
          cs2 <- com.s1 %>% filter(voter_precinct == "62")
        }else if(input$loc.type2 == "66"){
          cs2 <- com.s1 %>% filter(voter_precinct == "66")
        }else if(input$loc.type2 == "51"){
          cs2 <- com.s1 %>% filter(voter_precinct == "51")
        }
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
        labs(x = "Air Quality Parameters", y = "AQI", fill = "Date") +
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
        geom_bar(stat = "identity", , position = "dodge") +
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
      if(input$loc.type2 == "SE"){
        cs2 <- mean(cs1$current_speed[cs1$quadrant == "SE"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$quadrant == "SE"], na.rm = TRUE)
      }else if(input$loc.type2 == "SW"){
        cs2 <- mean(cs1$current_speed[cs1$quadrant == "SW"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$quadrant == "SW"], na.rm = TRUE)
      }else if(input$loc.type2 == "NE"){
        cs2 <- mean(cs1$current_speed[cs1$quadrant == "NE"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$quadrant == "NE"], na.rm = TRUE)
      }else if(input$loc.type2 == "NW"){
        cs2 <- mean(cs1$current_speed[cs1$quadrant == "NW"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$quadrant == "NW"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Ward"){
      if(input$loc.type2 == "1"){
        cs2 <- mean(cs1$current_speed[cs1$ward == "1"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$ward == "1"], na.rm = TRUE)
      }else if(input$loc.type2 == "2"){
        cs2 <- mean(cs1$current_speed[cs1$ward == "2"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$ward == "2"], na.rm = TRUE)
      }else if(input$loc.type2 == "3"){
        cs2 <- mean(cs1$current_speed[cs1$ward == "3"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$ward == "3"], na.rm = TRUE)
      }else if(input$loc.type2 == "4"){
        cs2 <- mean(cs1$current_speed[cs1$ward == "4"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$ward == "4"], na.rm = TRUE)
      }else if(input$loc.type2 == "5"){
        cs2 <- mean(cs1$current_speed[cs1$ward == "5"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$ward == "5"], na.rm = TRUE)
      }else if(input$loc.type2 == "6"){
        cs2 <- mean(cs1$current_speed[cs1$ward == "6"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$ward == "6"], na.rm = TRUE)
      }else if(input$loc.type2 == "7"){
        cs2 <- mean(cs1$current_speed[cs1$ward == "7"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$ward == "7"], na.rm = TRUE)
      }else if(input$loc.type2 == "8"){
        cs2 <- mean(cs1$current_speed[cs1$ward == "8"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$ward == "8"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Zip Code"){
      if(input$loc.type2 == "20227"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20227"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20227"], na.rm = TRUE)
      }else if(input$loc.type2 == "20032"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20032"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20032"], na.rm = TRUE)
      }else if(input$loc.type2 == "20037"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20037"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20037"], na.rm = TRUE)
      }else if(input$loc.type2 == "20019"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20019"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20019"], na.rm = TRUE)
      }else if(input$loc.type2 == "20020"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20020"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20020"], na.rm = TRUE)
      }else if(input$loc.type2 == "20018"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20018"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20018"], na.rm = TRUE)
      }else if(input$loc.type2 == "20024"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20024"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20024"], na.rm = TRUE)
      }else if(input$loc.type2 == "20002"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20002"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20002"], na.rm = TRUE)
      }else if(input$loc.type2 == "20003"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20003"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20003"], na.rm = TRUE)
      }else if(input$loc.type2 == "20001"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20001"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20001"], na.rm = TRUE)
      }else if(input$loc.type2 == "20005"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20005"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20005"], na.rm = TRUE)
      }else if(input$loc.type2 == "20009"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20009"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20009"], na.rm = TRUE)
      }else if(input$loc.type2 == "20017"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20017"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20017"], na.rm = TRUE)
      }else if(input$loc.type2 == "20010"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20010"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20010"], na.rm = TRUE)
      }else if(input$loc.type2 == "20016"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20016"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20016"], na.rm = TRUE)
      }else if(input$loc.type2 == "20008"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20008"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20008"], na.rm = TRUE)
      }else if(input$loc.type2 == "20011"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20011"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20011"], na.rm = TRUE)
      }else if(input$loc.type2 == "20007"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20007"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20007"], na.rm = TRUE)
      }else if(input$loc.type2 == "20374"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20374"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20374"], na.rm = TRUE)
      }else if(input$loc.type2 == "20015"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20015"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20015"], na.rm = TRUE)
      }else if(input$loc.type2 == "20012"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20012"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20012"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Advisory Neighborhood Commission"){
      if(input$loc.type2 == "2A"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "2A"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "2A"], na.rm = TRUE)
      }else if(input$loc.type2 == "8D"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "8D"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "8D"], na.rm = TRUE)
      }else if(input$loc.type2 == "8E"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "8E"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "8E"], na.rm = TRUE)
      }else if(input$loc.type2 == "8C"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "8C"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "8C"], na.rm = TRUE)
      }else if(input$loc.type2 == "7F"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "7F"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "7F"], na.rm = TRUE)
      }else if(input$loc.type2 == "7C"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "7C"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "7C"], na.rm = TRUE)
      }else if(input$loc.type2 == "7E"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "7E"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "7E"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "5C"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "5C"], na.rm = TRUE)
      }else if(input$loc.type2 == "6D"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "6D"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "6D"], na.rm = TRUE)
      }else if(input$loc.type2 == "5D"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "5D"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "5D"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "6B"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "6B"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "5E"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "5E"], na.rm = TRUE)
      }else if(input$loc.type2 == "2F"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "2F"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "2F"], na.rm = TRUE)
      }else if(input$loc.type2 == "2B"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "2B"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "2B"], na.rm = TRUE)
      }else if(input$loc.type2 == "1B"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "1B"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "1B"], na.rm = TRUE)
      }else if(input$loc.type2 == "6E"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "6E"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "6E"], na.rm = TRUE)
      }else if(input$loc.type2 == "5B"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "5B"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "5B"], na.rm = TRUE)
      }else if(input$loc.type2 == "1A"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "1A"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "1A"], na.rm = TRUE)
      }else if(input$loc.type2 == "3C"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "3C"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "3C"], na.rm = TRUE)
      }else if(input$loc.type2 == "6A"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "6A"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "6A"], na.rm = TRUE)
      }else if(input$loc.type2 == "3D"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "3D"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "3D"], na.rm = TRUE)
      }else if(input$loc.type2 == "4C"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "4C"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "4C"], na.rm = TRUE)
      }else if(input$loc.type2 == "3E"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "3E"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "3E"], na.rm = TRUE)
      }else if(input$loc.type2 == "2E"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "2E"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "2E"], na.rm = TRUE)
      }else if(input$loc.type2 == "6C"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "6C"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "6C"], na.rm = TRUE)
      }else if(input$loc.type2 == "8A"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "8A"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "8A"], na.rm = TRUE)
      }else if(input$loc.type2 == "4D"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "4D"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "4D"], na.rm = TRUE)
      }else if(input$loc.type2 == "4B"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "4B"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "4B"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "4A"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "4A"], na.rm = TRUE)
      }else if(input$loc.type2 == "3G"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "3G"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "3G"], na.rm = TRUE)
      }else if(input$loc.type2 == "3F"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "3F"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "3F"], na.rm = TRUE)
      }else if(input$loc.type2 == "5A"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "5A"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "5A"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Census Tract"){
      if(input$loc.type2 == "006202"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "006202"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "006202"], na.rm = TRUE)
      }else if(input$loc.type2 == "009811"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "009811"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "009811"], na.rm = TRUE)
      }else if(input$loc.type2 == "009807"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "009807"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "009807"], na.rm = TRUE)
      }else if(input$loc.type2 == "009700"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "009700"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "009700"], na.rm = TRUE)
      }else if(input$loc.type2 == "009804"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "009804"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "009804"], na.rm = TRUE)
      }else if(input$loc.type2 == "007304"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "007304"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "007304"], na.rm = TRUE)
      }else if(input$loc.type2 == "007301"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "007301"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "007301"], na.rm = TRUE)
      }else if(input$loc.type2 == "010400"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "010400"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "010400"], na.rm = TRUE)
      }else if(input$loc.type2 == "010800"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "010800"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "010800"], na.rm = TRUE)
      }else if(input$loc.type2 == "005600"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "005600"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "005600"], na.rm = TRUE)
      }else if(input$loc.type2 == "009603"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "009603"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "009603"], na.rm = TRUE)
      }else if(input$loc.type2 == "007809"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "007809"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "007809"], na.rm = TRUE)
      }else if(input$loc.type2 == "009902"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "009902"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "009902"], na.rm = TRUE)
      }else if(input$loc.type2 == "009000"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "009000"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "009000"], na.rm = TRUE)
      }else if(input$loc.type2 == "009905"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "009905"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "009905"], na.rm = TRUE)
      }else if(input$loc.type2 == "007707"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "007707"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "007707"], na.rm = TRUE)
      }else if(input$loc.type2 == "007806"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "007806"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "007806"], na.rm = TRUE)
      }else if(input$loc.type2 == "007804"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "007804"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "007804"], na.rm = TRUE)
      }else if(input$loc.type2 == "007807"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "007807"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "007807"], na.rm = TRUE)
      }else if(input$loc.type2 == "007708"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "007708"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "007708"], na.rm = TRUE)
      }else if(input$loc.type2 == "010500"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "010500"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "010500"], na.rm = TRUE)
      }else if(input$loc.type2 == "006400"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "006400"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "006400"], na.rm = TRUE)
      }else if(input$loc.type2 == "008803"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "008803"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "008803"], na.rm = TRUE)
      }else if(input$loc.type2 == "010200"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "010200"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "010200"], na.rm = TRUE)
      }else if(input$loc.type2 == "006500"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "006500"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "006500"], na.rm = TRUE)
      }else if(input$loc.type2 == "008702"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "008702"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "008702"], na.rm = TRUE)
      }else if(input$loc.type2 == "003301"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "003301"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "003301"], na.rm = TRUE)
      }else if(input$loc.type2 == "004902"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "004902"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "004902"], na.rm = TRUE)
      }else if(input$loc.type2 == "004201"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "004201"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "004201"], na.rm = TRUE)
      }else if(input$loc.type2 == "003400"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "003400"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "003400"], na.rm = TRUE)
      }else if(input$loc.type2 == "004400"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "004400"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "004400"], na.rm = TRUE)
      }else if(input$loc.type2 == "011100"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "011100"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "011100"], na.rm = TRUE)
      }else if(input$loc.type2 == "004702"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "004702"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "004702"], na.rm = TRUE)
      }else if(input$loc.type2 == "009302"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "009302"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "009302"], na.rm = TRUE)
      }else if(input$loc.type2 == "008701"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "008701"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "008701"], na.rm = TRUE)
      }else if(input$loc.type2 == "010100"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "010100"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "010100"], na.rm = TRUE)
      }else if(input$loc.type2 == "002900"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "002900"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "002900"], na.rm = TRUE)
      }else if(input$loc.type2 == "004600"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "004600"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "004600"], na.rm = TRUE)
      }else if(input$loc.type2 == "009400"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "009400"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "009400"], na.rm = TRUE)
      }else if(input$loc.type2 == "001002"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "001002"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "001002"], na.rm = TRUE)
      }else if(input$loc.type2 == "010600"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "010600"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "010600"], na.rm = TRUE)
      }else if(input$loc.type2 == "000901"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "000901"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "000901"], na.rm = TRUE)
      }else if(input$loc.type2 == "000502"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "000502"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "000502"], na.rm = TRUE)
      }else if(input$loc.type2 == "009102"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "009102"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "009102"], na.rm = TRUE)
      }else if(input$loc.type2 == "009201"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "009201"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "009201"], na.rm = TRUE)
      }else if(input$loc.type2 == "002400"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "002400"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "002400"], na.rm = TRUE)
      }else if(input$loc.type2 == "001001"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "001001"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "001001"], na.rm = TRUE)
      }else if(input$loc.type2 == "000600"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "000600"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "000600"], na.rm = TRUE)
      }else if(input$loc.type2 == "002501"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "002501"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "002501"], na.rm = TRUE)
      }else if(input$loc.type2 == "009503"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "009503"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "009503"], na.rm = TRUE)
      }else if(input$loc.type2 == "000300"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "000300"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "000300"], na.rm = TRUE)
      }else if(input$loc.type2 == "000400"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "000400"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "000400"], na.rm = TRUE)
      }else if(input$loc.type2 == "006700"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "006700"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "006700"], na.rm = TRUE)
      }else if(input$loc.type2 == "007200"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "007200"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "007200"], na.rm = TRUE)
      }else if(input$loc.type2 == "008302"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "008302"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "008302"], na.rm = TRUE)
      }else if(input$loc.type2 == "006600"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "006600"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "006600"], na.rm = TRUE)
      }else if(input$loc.type2 == "008904"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "008904"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "008904"], na.rm = TRUE)
      }else if(input$loc.type2 == "008100"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "008100"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "008100"], na.rm = TRUE)
      }else if(input$loc.type2 == "006802"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "006802"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "006802"], na.rm = TRUE)
      }else if(input$loc.type2 == "007601"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "007601"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "007601"], na.rm = TRUE)
      }else if(input$loc.type2 == "007100"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "007100"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "007100"], na.rm = TRUE)
      }else if(input$loc.type2 == "008200"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "008200"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "008200"], na.rm = TRUE)
      }else if(input$loc.type2 == "008001"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "008001"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "008001"], na.rm = TRUE)
      }else if(input$loc.type2 == "008002"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "008002"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "008002"], na.rm = TRUE)
      }else if(input$loc.type2 == "002101"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "002101"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "002101"], na.rm = TRUE)
      }else if(input$loc.type2 == "001901"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "001901"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "001901"], na.rm = TRUE)
      }else if(input$loc.type2 == "002600"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "002600"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "002600"], na.rm = TRUE)
      }else if(input$loc.type2 == "001401"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "001401"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "001401"], na.rm = TRUE)
      }else if(input$loc.type2 == "001803"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "001803"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "001803"], na.rm = TRUE)
      }else if(input$loc.type2 == "001301"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "001301"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "001301"], na.rm = TRUE)
      }else if(input$loc.type2 == "002102"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "002102"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "002102"], na.rm = TRUE)
      }else if(input$loc.type2 == "001804"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "001804"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "001804"], na.rm = TRUE)
      }else if(input$loc.type2 == "002001"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "002001"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "002001"], na.rm = TRUE)
      }else if(input$loc.type2 == "001902"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "001902"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "001902"], na.rm = TRUE)
      }else if(input$loc.type2 == "001100"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "001100"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "001100"], na.rm = TRUE)
      }else if(input$loc.type2 == "001600"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "001600"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "001600"], na.rm = TRUE)
      }else if(input$loc.type2 == "009509"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "009509"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "009509"], na.rm = TRUE)
      }else if(input$loc.type2 == "001500"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "001500"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "001500"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Single Member District"){
      if(input$loc.type2 == "2A01"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "2A01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "2A01"], na.rm = TRUE)
      }else if(input$loc.type2 == "8D03"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "8D03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "8D03"], na.rm = TRUE)
      }else if(input$loc.type2 == "8D01"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "8D01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "8D01"], na.rm = TRUE)
      }else if(input$loc.type2 == "8D06"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "8D06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "8D06"], na.rm = TRUE)
      }else if(input$loc.type2 == "8E05"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "8E05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "8E05"], na.rm = TRUE)
      }else if(input$loc.type2 == "8C07"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "8C07"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "8C07"], na.rm = TRUE)
      }else if(input$loc.type2 == "8E04"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "8E04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "8E04"], na.rm = TRUE)
      }else if(input$loc.type2 == "8C05"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "8C05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "8C05"], na.rm = TRUE)
      }else if(input$loc.type2 == "8C03"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "8C03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "8C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "2A07"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "2A07"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "2A07"], na.rm = TRUE)
      }else if(input$loc.type2 == "2A03"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "2A03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "2A03"], na.rm = TRUE)
      }else if(input$loc.type2 == "7F01"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "7F01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "7F01"], na.rm = TRUE)
      }else if(input$loc.type2 == "7C04"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "7C04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "7C04"], na.rm = TRUE)
      }else if(input$loc.type2 == "7E02"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "7E02"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "7E02"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C03"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "5C03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "5C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "7E06"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "7E06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "7E06"], na.rm = TRUE)
      }else if(input$loc.type2 == "7E01"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "7E01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "7E01"], na.rm = TRUE)
      }else if(input$loc.type2 == "7C07"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "7C07"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "7C07"], na.rm = TRUE)
      }else if(input$loc.type2 == "7C03"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "7C03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "7C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "7C06"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "7C06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "7C06"], na.rm = TRUE)
      }else if(input$loc.type2 == "7F06"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "7F06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "7F06"], na.rm = TRUE)
      }else if(input$loc.type2 == "6D03"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "6D03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "6D03"], na.rm = TRUE)
      }else if(input$loc.type2 == "6D06"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "6D06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "6D06"], na.rm = TRUE)
      }else if(input$loc.type2 == "5D01"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "5D01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "5D01"], na.rm = TRUE)
      }else if(input$loc.type2 == "6D05"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "6D05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "6D05"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B01"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "6B01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "6B01"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E03"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "5E03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "5E03"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E08"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "5E08"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "5E08"], na.rm = TRUE)
      }else if(input$loc.type2 == "2F06"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "2F06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "2F06"], na.rm = TRUE)
      }else if(input$loc.type2 == "2B09"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "2B09"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "2B09"], na.rm = TRUE)
      }else if(input$loc.type2 == "1B01"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "1B01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "1B01"], na.rm = TRUE)
      }else if(input$loc.type2 == "1B02"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "1B02"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "1B02"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C02"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "5C02"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "5C02"], na.rm = TRUE)
      }else if(input$loc.type2 == "6E07"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "6E07"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "6E07"], na.rm = TRUE)
      }else if(input$loc.type2 == "5B03"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "5B03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "5B03"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E04"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "5E04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "5E04"], na.rm = TRUE)
      }else if(input$loc.type2 == "2F08"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "2F08"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "2F08"], na.rm = TRUE)
      }else if(input$loc.type2 == "1A04"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "1A04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "1A04"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E05"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "5E05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "5E05"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C01"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "5C01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "5C01"], na.rm = TRUE)
      }else if(input$loc.type2 == "3C06"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "3C06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "3C06"], na.rm = TRUE)
      }else if(input$loc.type2 == "6A01"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "6A01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "6A01"], na.rm = TRUE)
      }else if(input$loc.type2 == "2B05"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "2B05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "2B05"], na.rm = TRUE)
      }else if(input$loc.type2 == "3D02"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "3D02"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "3D02"], na.rm = TRUE)
      }else if(input$loc.type2 == "3C03"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "3C03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "3C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "5B01"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "5B01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "5B01"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C06"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "5C06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "5C06"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E01"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "5E01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "5E01"], na.rm = TRUE)
      }else if(input$loc.type2 == "4C08"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "4C08"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "4C08"], na.rm = TRUE)
      }else if(input$loc.type2 == "3E05"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "3E05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "3E05"], na.rm = TRUE)
      }else if(input$loc.type2 == "3C09"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "3C09"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "3C09"], na.rm = TRUE)
      }else if(input$loc.type2 == "4C03"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "4C03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "4C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "2E01"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "2E01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "2E01"], na.rm = TRUE)
      }else if(input$loc.type2 == "3C08"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "3C08"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "3C08"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B05"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "6B05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "6B05"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C04"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "5C04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "5C04"], na.rm = TRUE)
      }else if(input$loc.type2 == "6D07"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "6D07"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "6D07"], na.rm = TRUE)
      }else if(input$loc.type2 == "6C03"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "6C03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "6C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B02"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "6B02"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "6B02"], na.rm = TRUE)
      }else if(input$loc.type2 == "5D05"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "5D05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "5D05"], na.rm = TRUE)
      }else if(input$loc.type2 == "6A03"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "6A03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "6A03"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B09"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "6B09"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "6B09"], na.rm = TRUE)
      }else if(input$loc.type2 == "8A03"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "8A03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "8A03"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B07"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "6B07"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "6B07"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B06"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "6B06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "6B06"], na.rm = TRUE)
      }else if(input$loc.type2 == "6A02"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "6A02"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "6A02"], na.rm = TRUE)
      }else if(input$loc.type2 == "6A08"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "6A08"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "6A08"], na.rm = TRUE)
      }else if(input$loc.type2 == "4D03"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "4D03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "4D03"], na.rm = TRUE)
      }else if(input$loc.type2 == "4B04"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "4B04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "4B04"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A08"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "4A08"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "4A08"], na.rm = TRUE)
      }else if(input$loc.type2 == "3D03"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "3D03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "3D03"], na.rm = TRUE)
      }else if(input$loc.type2 == "3G05"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "3G05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "3G05"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A07"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "4A07"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "4A07"], na.rm = TRUE)
      }else if(input$loc.type2 == "3E02"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "3E02"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "3E02"], na.rm = TRUE)
      }else if(input$loc.type2 == "3F03"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "3F03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "3F03"], na.rm = TRUE)
      }else if(input$loc.type2 == "4B06"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "4B06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "4B06"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A04"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "4A04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "4A04"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A06"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "4A06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "4A06"], na.rm = TRUE)
      }else if(input$loc.type2 == "3E04"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "3E04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "3E04"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A02"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "4A02"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "4A02"], na.rm = TRUE)
      }else if(input$loc.type2 == "5A08"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "5A08"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "5A08"], na.rm = TRUE)
      }else if(input$loc.type2 == "3G04"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "3G04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "3G04"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Voter Precinct"){
      if(input$loc.type2 == "129"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "129"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "129"], na.rm = TRUE)
      }else if(input$loc.type2 == "125"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "125"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "125"], na.rm = TRUE)
      }else if(input$loc.type2 == "126"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "126"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "126"], na.rm = TRUE)
      }else if(input$loc.type2 == "121"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "121"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "121"], na.rm = TRUE)
      }else if(input$loc.type2 == "122"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "122"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "122"], na.rm = TRUE)
      }else if(input$loc.type2 == "120"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "120"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "120"], na.rm = TRUE)
      }else if(input$loc.type2 == "123"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "123"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "123"], na.rm = TRUE)
      }else if(input$loc.type2 == "2"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "2"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "2"], na.rm = TRUE)
      }else if(input$loc.type2 == "3"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "3"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "3"], na.rm = TRUE)
      }else if(input$loc.type2 == "102"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "102"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "102"], na.rm = TRUE)
      }else if(input$loc.type2 == "94"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "94"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "94"], na.rm = TRUE)
      }else if(input$loc.type2 == "110"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "110"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "110"], na.rm = TRUE)
      }else if(input$loc.type2 == "139"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "139"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "139"], na.rm = TRUE)
      }else if(input$loc.type2 == "105"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "105"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "105"], na.rm = TRUE)
      }else if(input$loc.type2 == "106"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "106"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "106"], na.rm = TRUE)
      }else if(input$loc.type2 == "93"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "93"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "93"], na.rm = TRUE)
      }else if(input$loc.type2 == "97"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "97"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "97"], na.rm = TRUE)
      }else if(input$loc.type2 == "95"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "95"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "95"], na.rm = TRUE)
      }else if(input$loc.type2 == "132"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "132"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "132"], na.rm = TRUE)
      }else if(input$loc.type2 == "128"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "128"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "138"], na.rm = TRUE)
      }else if(input$loc.type2 == "127"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "127"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "127"], na.rm = TRUE)
      }else if(input$loc.type2 == "76"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "76"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "76"], na.rm = TRUE)
      }else if(input$loc.type2 == "130"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "130"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "130"], na.rm = TRUE)
      }else if(input$loc.type2 == "75"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "75"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "75"], na.rm = TRUE)
      }else if(input$loc.type2 == "135"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "135"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "135"], na.rm = TRUE)
      }else if(input$loc.type2 == "141"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "141"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "141"], na.rm = TRUE)
      }else if(input$loc.type2 == "20"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "20"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "20"], na.rm = TRUE)
      }else if(input$loc.type2 == "22"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "22"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "22"], na.rm = TRUE)
      }else if(input$loc.type2 == "72"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "72"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "72"], na.rm = TRUE)
      }else if(input$loc.type2 == "1"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "1"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "1"], na.rm = TRUE)
      }else if(input$loc.type2 == "73"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "73"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "73"], na.rm = TRUE)
      }else if(input$loc.type2 == "42"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "42"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "42"], na.rm = TRUE)
      }else if(input$loc.type2 == "19"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "19"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "19"], na.rm = TRUE)
      }else if(input$loc.type2 == "69"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "69"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "69"], na.rm = TRUE)
      }else if(input$loc.type2 == "29"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "29"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "29"], na.rm = TRUE)
      }else if(input$loc.type2 == "82"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "82"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "82"], na.rm = TRUE)
      }else if(input$loc.type2 == "17"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "17"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "17"], na.rm = TRUE)
      }else if(input$loc.type2 == "9"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "9"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "9"], na.rm = TRUE)
      }else if(input$loc.type2 == "26"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "26"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "26"], na.rm = TRUE)
      }else if(input$loc.type2 == "74"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "74"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "74"], na.rm = TRUE)
      }else if(input$loc.type2 == "45"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "45"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "45"], na.rm = TRUE)
      }else if(input$loc.type2 == "30"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "30"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "30"], na.rm = TRUE)
      }else if(input$loc.type2 == "27"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "27"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "27"], na.rm = TRUE)
      }else if(input$loc.type2 == "48"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "48"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "48"], na.rm = TRUE)
      }else if(input$loc.type2 == "67"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "67"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "67"], na.rm = TRUE)
      }else if(input$loc.type2 == "6"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "6"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "6"], na.rm = TRUE)
      }else if(input$loc.type2 == "12"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "12"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "12"], na.rm = TRUE)
      }else if(input$loc.type2 == "88"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "88"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "88"], na.rm = TRUE)
      }else if(input$loc.type2 == "131"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "131"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "131"], na.rm = TRUE)
      }else if(input$loc.type2 == "85"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "85"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "85"], na.rm = TRUE)
      }else if(input$loc.type2 == "89"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "89"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "89"], na.rm = TRUE)
      }else if(input$loc.type2 == "79"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "79"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "79"], na.rm = TRUE)
      }else if(input$loc.type2 == "91"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "91"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "91"], na.rm = TRUE)
      }else if(input$loc.type2 == "133"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "133"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "133"], na.rm = TRUE)
      }else if(input$loc.type2 == "81"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "81"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "81"], na.rm = TRUE)
      }else if(input$loc.type2 == "71"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "71"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "71"], na.rm = TRUE)
      }else if(input$loc.type2 == "86"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "86"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "86"], na.rm = TRUE)
      }else if(input$loc.type2 == "56"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "56"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "56"], na.rm = TRUE)
      }else if(input$loc.type2 == "59"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "59"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "59"], na.rm = TRUE)
      }else if(input$loc.type2 == "50"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "50"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "50"], na.rm = TRUE)
      }else if(input$loc.type2 == "61"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "61"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "61"], na.rm = TRUE)
      }else if(input$loc.type2 == "31"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "31"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "31"], na.rm = TRUE)
      }else if(input$loc.type2 == "138"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "138"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "138"], na.rm = TRUE)
      }else if(input$loc.type2 == "57"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "57"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "57"], na.rm = TRUE)
      }else if(input$loc.type2 == "60"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "60"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "60"], na.rm = TRUE)
      }else if(input$loc.type2 == "53"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "53"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "53"], na.rm = TRUE)
      }else if(input$loc.type2 == "32"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "32"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "32"], na.rm = TRUE)
      }else if(input$loc.type2 == "62"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "62"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "62"], na.rm = TRUE)
      }else if(input$loc.type2 == "66"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "66"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "66"], na.rm = TRUE)
      }else if(input$loc.type2 == "51"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "51"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "51"], na.rm = TRUE)
      }
    }
    
    if(input$loc.type == "Quadrant"){
      if(input$loc.type2 == "SE"){
        ls2 <- mean(ls1$current_speed[ls1$quadrant == "SE"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$quadrant == "SE"], na.rm = TRUE)
      }else if(input$loc.type2 == "SW"){
        ls2 <- mean(ls1$current_speed[ls1$quadrant == "SW"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$quadrant == "SW"], na.rm = TRUE)
      }else if(input$loc.type2 == "NE"){
        ls2 <- mean(ls1$current_speed[ls1$quadrant == "NE"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$quadrant == "NE"], na.rm = TRUE)
      }else if(input$loc.type2 == "NW"){
        ls2 <- mean(ls1$current_speed[ls1$quadrant == "NW"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$quadrant == "NW"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Ward"){
      if(input$loc.type2 == "1"){
        ls2 <- mean(ls1$current_speed[ls1$ward == "1"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$ward == "1"], na.rm = TRUE)
      }else if(input$loc.type2 == "2"){
        ls2 <- mean(ls1$current_speed[ls1$ward == "2"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$ward == "2"], na.rm = TRUE)
      }else if(input$loc.type2 == "3"){
        ls2 <- mean(ls1$current_speed[ls1$ward == "3"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$ward == "3"], na.rm = TRUE)
      }else if(input$loc.type2 == "4"){
        ls2 <- mean(ls1$current_speed[ls1$ward == "4"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$ward == "4"], na.rm = TRUE)
      }else if(input$loc.type2 == "5"){
        ls2 <- mean(ls1$current_speed[ls1$ward == "5"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$ward == "5"], na.rm = TRUE)
      }else if(input$loc.type2 == "6"){
        ls2 <- mean(ls1$current_speed[ls1$ward == "6"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$ward == "6"], na.rm = TRUE)
      }else if(input$loc.type2 == "7"){
        ls2 <- mean(ls1$current_speed[ls1$ward == "7"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$ward == "7"], na.rm = TRUE)
      }else if(input$loc.type2 == "8"){
        ls2 <- mean(ls1$current_speed[ls1$ward == "8"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$ward == "8"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Zip Code"){
      if(input$loc.type2 == "20227"){
        ls2 <- mean(ls1$current_speed[ls1$zip_code == "20227"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$zip_code == "20227"], na.rm = TRUE)
      }else if(input$loc.type2 == "20032"){
        ls2 <- mean(ls1$current_speed[ls1$zip_code == "20032"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$zip_code == "20032"], na.rm = TRUE)
      }else if(input$loc.type2 == "20037"){
        ls2 <- mean(ls1$current_speed[ls1$zip_code == "20037"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$zip_code == "20037"], na.rm = TRUE)
      }else if(input$loc.type2 == "20019"){
        ls2 <- mean(ls1$current_speed[ls1$zip_code == "20019"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$zip_code == "20019"], na.rm = TRUE)
      }else if(input$loc.type2 == "20020"){
        ls2 <- mean(ls1$current_speed[ls1$zip_code == "20020"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$zip_code == "20020"], na.rm = TRUE)
      }else if(input$loc.type2 == "20018"){
        ls2 <- mean(ls1$current_speed[ls1$zip_code == "20018"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$zip_code == "20018"], na.rm = TRUE)
      }else if(input$loc.type2 == "20024"){
        ls2 <- mean(ls1$current_speed[ls1$zip_code == "20024"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$zip_code == "20024"], na.rm = TRUE)
      }else if(input$loc.type2 == "20002"){
        ls2 <- mean(ls1$current_speed[ls1$zip_code == "20002"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$zip_code == "20002"], na.rm = TRUE)
      }else if(input$loc.type2 == "20003"){
        ls2 <- mean(ls1$current_speed[ls1$zip_code == "20003"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$zip_code == "20003"], na.rm = TRUE)
      }else if(input$loc.type2 == "20001"){
        ls2 <- mean(ls1$current_speed[ls1$zip_code == "20001"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$zip_code == "20001"], na.rm = TRUE)
      }else if(input$loc.type2 == "20005"){
        ls2 <- mean(ls1$current_speed[ls1$zip_code == "20005"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$zip_code == "20005"], na.rm = TRUE)
      }else if(input$loc.type2 == "20009"){
        ls2 <- mean(ls1$current_speed[ls1$zip_code == "20009"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$zip_code == "20009"], na.rm = TRUE)
      }else if(input$loc.type2 == "20017"){
        ls2 <- mean(ls1$current_speed[ls1$zip_code == "20017"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$zip_code == "20017"], na.rm = TRUE)
      }else if(input$loc.type2 == "20010"){
        ls2 <- mean(ls1$current_speed[ls1$zip_code == "20010"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$zip_code == "20010"], na.rm = TRUE)
      }else if(input$loc.type2 == "20016"){
        ls2 <- mean(ls1$current_speed[ls1$zip_code == "20016"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$zip_code == "20016"], na.rm = TRUE)
      }else if(input$loc.type2 == "20008"){
        ls2 <- mean(ls1$current_speed[ls1$zip_code == "20008"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$zip_code == "20008"], na.rm = TRUE)
      }else if(input$loc.type2 == "20011"){
        ls2 <- mean(ls1$current_speed[ls1$zip_code == "20011"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$zip_code == "20011"], na.rm = TRUE)
      }else if(input$loc.type2 == "20007"){
        ls2 <- mean(ls1$current_speed[ls1$zip_code == "20007"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$zip_code == "20007"], na.rm = TRUE)
      }else if(input$loc.type2 == "20374"){
        ls2 <- mean(ls1$current_speed[ls1$zip_code == "20374"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$zip_code == "20374"], na.rm = TRUE)
      }else if(input$loc.type2 == "20015"){
        ls2 <- mean(ls1$current_speed[ls1$zip_code == "20015"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$zip_code == "20015"], na.rm = TRUE)
      }else if(input$loc.type2 == "20012"){
        ls2 <- mean(ls1$current_speed[ls1$zip_code == "20012"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$zip_code == "20012"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Advisory Neighborhood Commission"){
      if(input$loc.type2 == "2A"){
        ls2 <- mean(ls1$current_speed[ls1$anc == "2A"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$anc == "2A"], na.rm = TRUE)
      }else if(input$loc.type2 == "8D"){
        ls2 <- mean(ls1$current_speed[ls1$anc == "8D"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$anc == "8D"], na.rm = TRUE)
      }else if(input$loc.type2 == "8E"){
        ls2 <- mean(ls1$current_speed[ls1$anc == "8E"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$anc == "8E"], na.rm = TRUE)
      }else if(input$loc.type2 == "8C"){
        ls2 <- mean(ls1$current_speed[ls1$anc == "8C"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$anc == "8C"], na.rm = TRUE)
      }else if(input$loc.type2 == "7F"){
        ls2 <- mean(ls1$current_speed[ls1$anc == "7F"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$anc == "7F"], na.rm = TRUE)
      }else if(input$loc.type2 == "7C"){
        ls2 <- mean(ls1$current_speed[ls1$anc == "7C"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$anc == "7C"], na.rm = TRUE)
      }else if(input$loc.type2 == "7E"){
        ls2 <- mean(ls1$current_speed[ls1$anc == "7E"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$anc == "7E"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C"){
        ls2 <- mean(ls1$current_speed[ls1$anc == "5C"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$anc == "5C"], na.rm = TRUE)
      }else if(input$loc.type2 == "6D"){
        ls2 <- mean(ls1$current_speed[ls1$anc == "6D"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$anc == "6D"], na.rm = TRUE)
      }else if(input$loc.type2 == "5D"){
        ls2 <- mean(ls1$current_speed[ls1$anc == "5D"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$anc == "5D"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B"){
        ls2 <- mean(ls1$current_speed[ls1$anc == "6B"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$anc == "6B"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E"){
        ls2 <- mean(ls1$current_speed[ls1$anc == "5E"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$anc == "5E"], na.rm = TRUE)
      }else if(input$loc.type2 == "2F"){
        ls2 <- mean(ls1$current_speed[ls1$anc == "2F"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$anc == "2F"], na.rm = TRUE)
      }else if(input$loc.type2 == "2B"){
        ls2 <- mean(ls1$current_speed[ls1$anc == "2B"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$anc == "2B"], na.rm = TRUE)
      }else if(input$loc.type2 == "1B"){
        ls2 <- mean(ls1$current_speed[ls1$anc == "1B"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$anc == "1B"], na.rm = TRUE)
      }else if(input$loc.type2 == "6E"){
        ls2 <- mean(ls1$current_speed[ls1$anc == "6E"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$anc == "6E"], na.rm = TRUE)
      }else if(input$loc.type2 == "5B"){
        ls2 <- mean(ls1$current_speed[ls1$anc == "5B"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$anc == "5B"], na.rm = TRUE)
      }else if(input$loc.type2 == "1A"){
        ls2 <- mean(ls1$current_speed[ls1$anc == "1A"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$anc == "1A"], na.rm = TRUE)
      }else if(input$loc.type2 == "3C"){
        ls2 <- mean(ls1$current_speed[ls1$anc == "3C"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$anc == "3C"], na.rm = TRUE)
      }else if(input$loc.type2 == "6A"){
        ls2 <- mean(ls1$current_speed[ls1$anc == "6A"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$anc == "6A"], na.rm = TRUE)
      }else if(input$loc.type2 == "3D"){
        ls2 <- mean(ls1$current_speed[ls1$anc == "3D"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$anc == "3D"], na.rm = TRUE)
      }else if(input$loc.type2 == "4C"){
        ls2 <- mean(ls1$current_speed[ls1$anc == "4C"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$anc == "4C"], na.rm = TRUE)
      }else if(input$loc.type2 == "3E"){
        ls2 <- mean(ls1$current_speed[ls1$anc == "3E"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$anc == "3E"], na.rm = TRUE)
      }else if(input$loc.type2 == "2E"){
        ls2 <- mean(ls1$current_speed[ls1$anc == "2E"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$anc == "2E"], na.rm = TRUE)
      }else if(input$loc.type2 == "6C"){
        ls2 <- mean(ls1$current_speed[ls1$anc == "6C"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$anc == "6C"], na.rm = TRUE)
      }else if(input$loc.type2 == "8A"){
        ls2 <- mean(ls1$current_speed[ls1$anc == "8A"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$anc == "8A"], na.rm = TRUE)
      }else if(input$loc.type2 == "4D"){
        ls2 <- mean(ls1$current_speed[ls1$anc == "4D"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$anc == "4D"], na.rm = TRUE)
      }else if(input$loc.type2 == "4B"){
        ls2 <- mean(ls1$current_speed[ls1$anc == "4B"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$anc == "4B"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A"){
        ls2 <- mean(ls1$current_speed[ls1$anc == "4A"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$anc == "4A"], na.rm = TRUE)
      }else if(input$loc.type2 == "3G"){
        ls2 <- mean(ls1$current_speed[ls1$anc == "3G"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$anc == "3G"], na.rm = TRUE)
      }else if(input$loc.type2 == "3F"){
        ls2 <- mean(ls1$current_speed[ls1$anc == "3F"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$anc == "3F"], na.rm = TRUE)
      }else if(input$loc.type2 == "5A"){
        ls2 <- mean(ls1$current_speed[ls1$anc == "5A"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$anc == "5A"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Census Tract"){
      if(input$loc.type2 == "006202"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "006202"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "006202"], na.rm = TRUE)
      }else if(input$loc.type2 == "009811"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "009811"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "009811"], na.rm = TRUE)
      }else if(input$loc.type2 == "009807"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "009807"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "009807"], na.rm = TRUE)
      }else if(input$loc.type2 == "009700"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "009700"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "009700"], na.rm = TRUE)
      }else if(input$loc.type2 == "009804"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "009804"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "009804"], na.rm = TRUE)
      }else if(input$loc.type2 == "007304"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "007304"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "007304"], na.rm = TRUE)
      }else if(input$loc.type2 == "007301"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "007301"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "007301"], na.rm = TRUE)
      }else if(input$loc.type2 == "010400"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "010400"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "010400"], na.rm = TRUE)
      }else if(input$loc.type2 == "010800"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "010800"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "010800"], na.rm = TRUE)
      }else if(input$loc.type2 == "005600"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "005600"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "005600"], na.rm = TRUE)
      }else if(input$loc.type2 == "009603"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "009603"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "009603"], na.rm = TRUE)
      }else if(input$loc.type2 == "007809"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "007809"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "007809"], na.rm = TRUE)
      }else if(input$loc.type2 == "009902"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "009902"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "009902"], na.rm = TRUE)
      }else if(input$loc.type2 == "009000"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "009000"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "009000"], na.rm = TRUE)
      }else if(input$loc.type2 == "009905"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "009905"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "009905"], na.rm = TRUE)
      }else if(input$loc.type2 == "007707"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "007707"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "007707"], na.rm = TRUE)
      }else if(input$loc.type2 == "007806"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "007806"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "007806"], na.rm = TRUE)
      }else if(input$loc.type2 == "007804"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "007804"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "007804"], na.rm = TRUE)
      }else if(input$loc.type2 == "007807"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "007807"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "007807"], na.rm = TRUE)
      }else if(input$loc.type2 == "007708"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "007708"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "007708"], na.rm = TRUE)
      }else if(input$loc.type2 == "010500"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "010500"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "010500"], na.rm = TRUE)
      }else if(input$loc.type2 == "006400"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "006400"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "006400"], na.rm = TRUE)
      }else if(input$loc.type2 == "008803"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "008803"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "008803"], na.rm = TRUE)
      }else if(input$loc.type2 == "010200"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "010200"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "010200"], na.rm = TRUE)
      }else if(input$loc.type2 == "006500"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "006500"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "006500"], na.rm = TRUE)
      }else if(input$loc.type2 == "008702"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "008702"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "008702"], na.rm = TRUE)
      }else if(input$loc.type2 == "003301"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "003301"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "003301"], na.rm = TRUE)
      }else if(input$loc.type2 == "004902"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "004902"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "004902"], na.rm = TRUE)
      }else if(input$loc.type2 == "004201"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "004201"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "004201"], na.rm = TRUE)
      }else if(input$loc.type2 == "003400"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "003400"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "003400"], na.rm = TRUE)
      }else if(input$loc.type2 == "004400"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "004400"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "004400"], na.rm = TRUE)
      }else if(input$loc.type2 == "011100"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "011100"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "011100"], na.rm = TRUE)
      }else if(input$loc.type2 == "004702"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "004702"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "004702"], na.rm = TRUE)
      }else if(input$loc.type2 == "009302"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "009302"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "009302"], na.rm = TRUE)
      }else if(input$loc.type2 == "008701"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "008701"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "008701"], na.rm = TRUE)
      }else if(input$loc.type2 == "010100"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "010100"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "010100"], na.rm = TRUE)
      }else if(input$loc.type2 == "002900"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "002900"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "002900"], na.rm = TRUE)
      }else if(input$loc.type2 == "004600"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "004600"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "004600"], na.rm = TRUE)
      }else if(input$loc.type2 == "009400"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "009400"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "009400"], na.rm = TRUE)
      }else if(input$loc.type2 == "001002"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "001002"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "001002"], na.rm = TRUE)
      }else if(input$loc.type2 == "010600"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "010600"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "010600"], na.rm = TRUE)
      }else if(input$loc.type2 == "000901"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "000901"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "000901"], na.rm = TRUE)
      }else if(input$loc.type2 == "000502"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "000502"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "000502"], na.rm = TRUE)
      }else if(input$loc.type2 == "009102"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "009102"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "009102"], na.rm = TRUE)
      }else if(input$loc.type2 == "009201"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "009201"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "009201"], na.rm = TRUE)
      }else if(input$loc.type2 == "002400"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "002400"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "002400"], na.rm = TRUE)
      }else if(input$loc.type2 == "001001"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "001001"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "001001"], na.rm = TRUE)
      }else if(input$loc.type2 == "000600"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "000600"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "000600"], na.rm = TRUE)
      }else if(input$loc.type2 == "002501"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "002501"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "002501"], na.rm = TRUE)
      }else if(input$loc.type2 == "009503"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "009503"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "009503"], na.rm = TRUE)
      }else if(input$loc.type2 == "000300"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "000300"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "000300"], na.rm = TRUE)
      }else if(input$loc.type2 == "000400"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "000400"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "000400"], na.rm = TRUE)
      }else if(input$loc.type2 == "006700"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "006700"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "006700"], na.rm = TRUE)
      }else if(input$loc.type2 == "007200"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "007200"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "007200"], na.rm = TRUE)
      }else if(input$loc.type2 == "008302"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "008302"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "008302"], na.rm = TRUE)
      }else if(input$loc.type2 == "006600"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "006600"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "006600"], na.rm = TRUE)
      }else if(input$loc.type2 == "008904"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "008904"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "008904"], na.rm = TRUE)
      }else if(input$loc.type2 == "008100"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "008100"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "008100"], na.rm = TRUE)
      }else if(input$loc.type2 == "006802"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "006802"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "006802"], na.rm = TRUE)
      }else if(input$loc.type2 == "007601"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "007601"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "007601"], na.rm = TRUE)
      }else if(input$loc.type2 == "007100"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "007100"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "007100"], na.rm = TRUE)
      }else if(input$loc.type2 == "008200"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "008200"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "008200"], na.rm = TRUE)
      }else if(input$loc.type2 == "008001"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "008001"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "008001"], na.rm = TRUE)
      }else if(input$loc.type2 == "008002"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "008002"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "008002"], na.rm = TRUE)
      }else if(input$loc.type2 == "002101"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "002101"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "002101"], na.rm = TRUE)
      }else if(input$loc.type2 == "001901"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "001901"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "001901"], na.rm = TRUE)
      }else if(input$loc.type2 == "002600"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "002600"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "002600"], na.rm = TRUE)
      }else if(input$loc.type2 == "001401"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "001401"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "001401"], na.rm = TRUE)
      }else if(input$loc.type2 == "001803"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "001803"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "001803"], na.rm = TRUE)
      }else if(input$loc.type2 == "001301"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "001301"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "001301"], na.rm = TRUE)
      }else if(input$loc.type2 == "002102"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "002102"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "002102"], na.rm = TRUE)
      }else if(input$loc.type2 == "001804"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "001804"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "001804"], na.rm = TRUE)
      }else if(input$loc.type2 == "002001"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "002001"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "002001"], na.rm = TRUE)
      }else if(input$loc.type2 == "001902"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "001902"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "001902"], na.rm = TRUE)
      }else if(input$loc.type2 == "001100"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "001100"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "001100"], na.rm = TRUE)
      }else if(input$loc.type2 == "001600"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "001600"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "001600"], na.rm = TRUE)
      }else if(input$loc.type2 == "009509"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "009509"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "009509"], na.rm = TRUE)
      }else if(input$loc.type2 == "001500"){
        ls2 <- mean(ls1$current_speed[ls1$census_tract == "001500"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$census_tract == "001500"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Single Member District"){
      if(input$loc.type2 == "2A01"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "2A01"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "2A01"], na.rm = TRUE)
      }else if(input$loc.type2 == "8D03"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "8D03"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "8D03"], na.rm = TRUE)
      }else if(input$loc.type2 == "8D01"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "8D01"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "8D01"], na.rm = TRUE)
      }else if(input$loc.type2 == "8D06"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "8D06"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "8D06"], na.rm = TRUE)
      }else if(input$loc.type2 == "8E05"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "8E05"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "8E05"], na.rm = TRUE)
      }else if(input$loc.type2 == "8C07"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "8C07"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "8C07"], na.rm = TRUE)
      }else if(input$loc.type2 == "8E04"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "8E04"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "8E04"], na.rm = TRUE)
      }else if(input$loc.type2 == "8C05"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "8C05"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "8C05"], na.rm = TRUE)
      }else if(input$loc.type2 == "8C03"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "8C03"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "8C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "2A07"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "2A07"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "2A07"], na.rm = TRUE)
      }else if(input$loc.type2 == "2A03"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "2A03"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "2A03"], na.rm = TRUE)
      }else if(input$loc.type2 == "7F01"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "7F01"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "7F01"], na.rm = TRUE)
      }else if(input$loc.type2 == "7C04"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "7C04"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "7C04"], na.rm = TRUE)
      }else if(input$loc.type2 == "7E02"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "7E02"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "7E02"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C03"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "5C03"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "5C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "7E06"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "7E06"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "7E06"], na.rm = TRUE)
      }else if(input$loc.type2 == "7E01"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "7E01"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "7E01"], na.rm = TRUE)
      }else if(input$loc.type2 == "7C07"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "7C07"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "7C07"], na.rm = TRUE)
      }else if(input$loc.type2 == "7C03"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "7C03"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "7C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "7C06"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "7C06"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "7C06"], na.rm = TRUE)
      }else if(input$loc.type2 == "7F06"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "7F06"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "7F06"], na.rm = TRUE)
      }else if(input$loc.type2 == "6D03"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "6D03"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "6D03"], na.rm = TRUE)
      }else if(input$loc.type2 == "6D06"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "6D06"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "6D06"], na.rm = TRUE)
      }else if(input$loc.type2 == "5D01"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "5D01"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "5D01"], na.rm = TRUE)
      }else if(input$loc.type2 == "6D05"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "6D05"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "6D05"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B01"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "6B01"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "6B01"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E03"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "5E03"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "5E03"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E08"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "5E08"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "5E08"], na.rm = TRUE)
      }else if(input$loc.type2 == "2F06"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "2F06"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "2F06"], na.rm = TRUE)
      }else if(input$loc.type2 == "2B09"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "2B09"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "2B09"], na.rm = TRUE)
      }else if(input$loc.type2 == "1B01"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "1B01"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "1B01"], na.rm = TRUE)
      }else if(input$loc.type2 == "1B02"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "1B02"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "1B02"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C02"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "5C02"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "5C02"], na.rm = TRUE)
      }else if(input$loc.type2 == "6E07"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "6E07"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "6E07"], na.rm = TRUE)
      }else if(input$loc.type2 == "5B03"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "5B03"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "5B03"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E04"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "5E04"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "5E04"], na.rm = TRUE)
      }else if(input$loc.type2 == "2F08"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "2F08"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "2F08"], na.rm = TRUE)
      }else if(input$loc.type2 == "1A04"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "1A04"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "1A04"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E05"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "5E05"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "5E05"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C01"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "5C01"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "5C01"], na.rm = TRUE)
      }else if(input$loc.type2 == "3C06"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "3C06"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "3C06"], na.rm = TRUE)
      }else if(input$loc.type2 == "6A01"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "6A01"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "6A01"], na.rm = TRUE)
      }else if(input$loc.type2 == "2B05"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "2B05"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "2B05"], na.rm = TRUE)
      }else if(input$loc.type2 == "3D02"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "3D02"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "3D02"], na.rm = TRUE)
      }else if(input$loc.type2 == "3C03"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "3C03"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "3C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "5B01"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "5B01"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "5B01"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C06"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "5C06"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "5C06"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E01"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "5E01"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "5E01"], na.rm = TRUE)
      }else if(input$loc.type2 == "4C08"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "4C08"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "4C08"], na.rm = TRUE)
      }else if(input$loc.type2 == "3E05"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "3E05"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "3E05"], na.rm = TRUE)
      }else if(input$loc.type2 == "3C09"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "3C09"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "3C09"], na.rm = TRUE)
      }else if(input$loc.type2 == "4C03"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "4C03"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "4C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "2E01"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "2E01"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "2E01"], na.rm = TRUE)
      }else if(input$loc.type2 == "3C08"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "3C08"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "3C08"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B05"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "6B05"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "6B05"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C04"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "5C04"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "5C04"], na.rm = TRUE)
      }else if(input$loc.type2 == "6D07"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "6D07"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "6D07"], na.rm = TRUE)
      }else if(input$loc.type2 == "6C03"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "6C03"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "6C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B02"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "6B02"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "6B02"], na.rm = TRUE)
      }else if(input$loc.type2 == "5D05"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "5D05"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "5D05"], na.rm = TRUE)
      }else if(input$loc.type2 == "6A03"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "6A03"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "6A03"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B09"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "6B09"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "6B09"], na.rm = TRUE)
      }else if(input$loc.type2 == "8A03"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "8A03"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "8A03"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B07"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "6B07"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "6B07"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B06"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "6B06"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "6B06"], na.rm = TRUE)
      }else if(input$loc.type2 == "6A02"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "6A02"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "6A02"], na.rm = TRUE)
      }else if(input$loc.type2 == "6A08"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "6A08"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "6A08"], na.rm = TRUE)
      }else if(input$loc.type2 == "4D03"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "4D03"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "4D03"], na.rm = TRUE)
      }else if(input$loc.type2 == "4B04"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "4B04"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "4B04"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A08"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "4A08"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "4A08"], na.rm = TRUE)
      }else if(input$loc.type2 == "3D03"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "3D03"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "3D03"], na.rm = TRUE)
      }else if(input$loc.type2 == "3G05"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "3G05"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "3G05"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A07"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "4A07"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "4A07"], na.rm = TRUE)
      }else if(input$loc.type2 == "3E02"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "3E02"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "3E02"], na.rm = TRUE)
      }else if(input$loc.type2 == "3F03"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "3F03"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "3F03"], na.rm = TRUE)
      }else if(input$loc.type2 == "4B06"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "4B06"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "4B06"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A04"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "4A04"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "4A04"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A06"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "4A06"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "4A06"], na.rm = TRUE)
      }else if(input$loc.type2 == "3E04"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "3E04"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "3E04"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A02"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "4A02"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "4A02"], na.rm = TRUE)
      }else if(input$loc.type2 == "5A08"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "5A08"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "5A08"], na.rm = TRUE)
      }else if(input$loc.type2 == "3G04"){
        ls2 <- mean(ls1$current_speed[ls1$single_member_district == "3G04"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$single_member_district == "3G04"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Voter Precinct"){
      if(input$loc.type2 == "129"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "129"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "129"], na.rm = TRUE)
      }else if(input$loc.type2 == "125"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "125"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "125"], na.rm = TRUE)
      }else if(input$loc.type2 == "126"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "126"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "126"], na.rm = TRUE)
      }else if(input$loc.type2 == "121"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "121"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "121"], na.rm = TRUE)
      }else if(input$loc.type2 == "122"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "122"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "122"], na.rm = TRUE)
      }else if(input$loc.type2 == "120"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "120"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "120"], na.rm = TRUE)
      }else if(input$loc.type2 == "123"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "123"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "123"], na.rm = TRUE)
      }else if(input$loc.type2 == "2"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "2"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "2"], na.rm = TRUE)
      }else if(input$loc.type2 == "3"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "3"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "3"], na.rm = TRUE)
      }else if(input$loc.type2 == "102"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "102"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "102"], na.rm = TRUE)
      }else if(input$loc.type2 == "94"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "94"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "94"], na.rm = TRUE)
      }else if(input$loc.type2 == "110"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "110"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "110"], na.rm = TRUE)
      }else if(input$loc.type2 == "139"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "139"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "139"], na.rm = TRUE)
      }else if(input$loc.type2 == "105"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "105"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "105"], na.rm = TRUE)
      }else if(input$loc.type2 == "106"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "106"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "106"], na.rm = TRUE)
      }else if(input$loc.type2 == "93"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "93"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "93"], na.rm = TRUE)
      }else if(input$loc.type2 == "97"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "97"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "97"], na.rm = TRUE)
      }else if(input$loc.type2 == "95"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "95"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "95"], na.rm = TRUE)
      }else if(input$loc.type2 == "132"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "132"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "132"], na.rm = TRUE)
      }else if(input$loc.type2 == "128"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "128"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "138"], na.rm = TRUE)
      }else if(input$loc.type2 == "127"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "127"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "127"], na.rm = TRUE)
      }else if(input$loc.type2 == "76"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "76"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "76"], na.rm = TRUE)
      }else if(input$loc.type2 == "130"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "130"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "130"], na.rm = TRUE)
      }else if(input$loc.type2 == "75"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "75"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "75"], na.rm = TRUE)
      }else if(input$loc.type2 == "135"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "135"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "135"], na.rm = TRUE)
      }else if(input$loc.type2 == "141"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "141"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "141"], na.rm = TRUE)
      }else if(input$loc.type2 == "20"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "20"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "20"], na.rm = TRUE)
      }else if(input$loc.type2 == "22"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "22"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "22"], na.rm = TRUE)
      }else if(input$loc.type2 == "72"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "72"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "72"], na.rm = TRUE)
      }else if(input$loc.type2 == "1"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "1"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "1"], na.rm = TRUE)
      }else if(input$loc.type2 == "73"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "73"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "73"], na.rm = TRUE)
      }else if(input$loc.type2 == "42"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "42"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "42"], na.rm = TRUE)
      }else if(input$loc.type2 == "19"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "19"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "19"], na.rm = TRUE)
      }else if(input$loc.type2 == "69"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "69"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "69"], na.rm = TRUE)
      }else if(input$loc.type2 == "29"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "29"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "29"], na.rm = TRUE)
      }else if(input$loc.type2 == "82"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "82"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "82"], na.rm = TRUE)
      }else if(input$loc.type2 == "17"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "17"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "17"], na.rm = TRUE)
      }else if(input$loc.type2 == "9"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "9"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "9"], na.rm = TRUE)
      }else if(input$loc.type2 == "26"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "26"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "26"], na.rm = TRUE)
      }else if(input$loc.type2 == "74"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "74"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "74"], na.rm = TRUE)
      }else if(input$loc.type2 == "45"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "45"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "45"], na.rm = TRUE)
      }else if(input$loc.type2 == "30"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "30"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "30"], na.rm = TRUE)
      }else if(input$loc.type2 == "27"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "27"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "27"], na.rm = TRUE)
      }else if(input$loc.type2 == "48"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "48"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "48"], na.rm = TRUE)
      }else if(input$loc.type2 == "67"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "67"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "67"], na.rm = TRUE)
      }else if(input$loc.type2 == "6"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "6"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "6"], na.rm = TRUE)
      }else if(input$loc.type2 == "12"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "12"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "12"], na.rm = TRUE)
      }else if(input$loc.type2 == "88"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "88"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "88"], na.rm = TRUE)
      }else if(input$loc.type2 == "131"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "131"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "131"], na.rm = TRUE)
      }else if(input$loc.type2 == "85"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "85"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "85"], na.rm = TRUE)
      }else if(input$loc.type2 == "89"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "89"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "89"], na.rm = TRUE)
      }else if(input$loc.type2 == "79"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "79"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "79"], na.rm = TRUE)
      }else if(input$loc.type2 == "91"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "91"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "91"], na.rm = TRUE)
      }else if(input$loc.type2 == "133"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "133"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "133"], na.rm = TRUE)
      }else if(input$loc.type2 == "81"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "81"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "81"], na.rm = TRUE)
      }else if(input$loc.type2 == "71"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "71"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "71"], na.rm = TRUE)
      }else if(input$loc.type2 == "86"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "86"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "86"], na.rm = TRUE)
      }else if(input$loc.type2 == "56"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "56"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "56"], na.rm = TRUE)
      }else if(input$loc.type2 == "59"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "59"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "59"], na.rm = TRUE)
      }else if(input$loc.type2 == "50"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "50"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "50"], na.rm = TRUE)
      }else if(input$loc.type2 == "61"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "61"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "61"], na.rm = TRUE)
      }else if(input$loc.type2 == "31"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "31"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "31"], na.rm = TRUE)
      }else if(input$loc.type2 == "138"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "138"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "138"], na.rm = TRUE)
      }else if(input$loc.type2 == "57"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "57"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "57"], na.rm = TRUE)
      }else if(input$loc.type2 == "60"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "60"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "60"], na.rm = TRUE)
      }else if(input$loc.type2 == "53"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "53"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "53"], na.rm = TRUE)
      }else if(input$loc.type2 == "32"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "32"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "32"], na.rm = TRUE)
      }else if(input$loc.type2 == "62"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "62"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "62"], na.rm = TRUE)
      }else if(input$loc.type2 == "66"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "66"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "66"], na.rm = TRUE)
      }else if(input$loc.type2 == "51"){
        ls2 <- mean(ls1$current_speed[ls1$voter_precinct == "51"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_speed[ls1$voter_precinct == "51"], na.rm = TRUE)
      }
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
      if(input$loc.type2 == "SE"){
        cs2 <- mean(cs1$current_travel_time[cs1$quadrant == "SE"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$quadrant == "SE"], na.rm = TRUE)
      }else if(input$loc.type2 == "SW"){
        cs2 <- mean(cs1$current_travel_time[cs1$quadrant == "SW"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$quadrant == "SW"], na.rm = TRUE)
      }else if(input$loc.type2 == "NE"){
        cs2 <- mean(cs1$current_travel_time[cs1$quadrant == "NE"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$quadrant == "NE"], na.rm = TRUE)
      }else if(input$loc.type2 == "NW"){
        cs2 <- mean(cs1$current_travel_time[cs1$quadrant == "NW"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$quadrant == "NW"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Ward"){
      if(input$loc.type2 == "1"){
        cs2 <- mean(cs1$current_travel_time[cs1$ward == "1"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$ward == "1"], na.rm = TRUE)
      }else if(input$loc.type2 == "2"){
        cs2 <- mean(cs1$current_travel_time[cs1$ward == "2"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$ward == "2"], na.rm = TRUE)
      }else if(input$loc.type2 == "3"){
        cs2 <- mean(cs1$current_travel_time[cs1$ward == "3"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$ward == "3"], na.rm = TRUE)
      }else if(input$loc.type2 == "4"){
        cs2 <- mean(cs1$current_travel_time[cs1$ward == "4"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$ward == "4"], na.rm = TRUE)
      }else if(input$loc.type2 == "5"){
        cs2 <- mean(cs1$current_travel_time[cs1$ward == "5"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$ward == "5"], na.rm = TRUE)
      }else if(input$loc.type2 == "6"){
        cs2 <- mean(cs1$current_travel_time[cs1$ward == "6"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$ward == "6"], na.rm = TRUE)
      }else if(input$loc.type2 == "7"){
        cs2 <- mean(cs1$current_travel_time[cs1$ward == "7"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$ward == "7"], na.rm = TRUE)
      }else if(input$loc.type2 == "8"){
        cs2 <- mean(cs1$current_travel_time[cs1$ward == "8"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$ward == "8"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Zip Code"){
      if(input$loc.type2 == "20227"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20227"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20227"], na.rm = TRUE)
      }else if(input$loc.type2 == "20032"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20032"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20032"], na.rm = TRUE)
      }else if(input$loc.type2 == "20037"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20037"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20037"], na.rm = TRUE)
      }else if(input$loc.type2 == "20019"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20019"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20019"], na.rm = TRUE)
      }else if(input$loc.type2 == "20020"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20020"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20020"], na.rm = TRUE)
      }else if(input$loc.type2 == "20018"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20018"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20018"], na.rm = TRUE)
      }else if(input$loc.type2 == "20024"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20024"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20024"], na.rm = TRUE)
      }else if(input$loc.type2 == "20002"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20002"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20002"], na.rm = TRUE)
      }else if(input$loc.type2 == "20003"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20003"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20003"], na.rm = TRUE)
      }else if(input$loc.type2 == "20001"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20001"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20001"], na.rm = TRUE)
      }else if(input$loc.type2 == "20005"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20005"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20005"], na.rm = TRUE)
      }else if(input$loc.type2 == "20009"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20009"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20009"], na.rm = TRUE)
      }else if(input$loc.type2 == "20017"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20017"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20017"], na.rm = TRUE)
      }else if(input$loc.type2 == "20010"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20010"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20010"], na.rm = TRUE)
      }else if(input$loc.type2 == "20016"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20016"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20016"], na.rm = TRUE)
      }else if(input$loc.type2 == "20008"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20008"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20008"], na.rm = TRUE)
      }else if(input$loc.type2 == "20011"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20011"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20011"], na.rm = TRUE)
      }else if(input$loc.type2 == "20007"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20007"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20007"], na.rm = TRUE)
      }else if(input$loc.type2 == "20374"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20374"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20374"], na.rm = TRUE)
      }else if(input$loc.type2 == "20015"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20015"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20015"], na.rm = TRUE)
      }else if(input$loc.type2 == "20012"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20012"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20012"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Advisory Neighborhood Commission"){
      if(input$loc.type2 == "2A"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "2A"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "2A"], na.rm = TRUE)
      }else if(input$loc.type2 == "8D"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "8D"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "8D"], na.rm = TRUE)
      }else if(input$loc.type2 == "8E"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "8E"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "8E"], na.rm = TRUE)
      }else if(input$loc.type2 == "8C"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "8C"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "8C"], na.rm = TRUE)
      }else if(input$loc.type2 == "7F"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "7F"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "7F"], na.rm = TRUE)
      }else if(input$loc.type2 == "7C"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "7C"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "7C"], na.rm = TRUE)
      }else if(input$loc.type2 == "7E"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "7E"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "7E"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "5C"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "5C"], na.rm = TRUE)
      }else if(input$loc.type2 == "6D"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "6D"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "6D"], na.rm = TRUE)
      }else if(input$loc.type2 == "5D"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "5D"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "5D"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "6B"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "6B"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "5E"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "5E"], na.rm = TRUE)
      }else if(input$loc.type2 == "2F"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "2F"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "2F"], na.rm = TRUE)
      }else if(input$loc.type2 == "2B"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "2B"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "2B"], na.rm = TRUE)
      }else if(input$loc.type2 == "1B"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "1B"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "1B"], na.rm = TRUE)
      }else if(input$loc.type2 == "6E"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "6E"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "6E"], na.rm = TRUE)
      }else if(input$loc.type2 == "5B"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "5B"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "5B"], na.rm = TRUE)
      }else if(input$loc.type2 == "1A"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "1A"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "1A"], na.rm = TRUE)
      }else if(input$loc.type2 == "3C"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "3C"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "3C"], na.rm = TRUE)
      }else if(input$loc.type2 == "6A"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "6A"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "6A"], na.rm = TRUE)
      }else if(input$loc.type2 == "3D"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "3D"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "3D"], na.rm = TRUE)
      }else if(input$loc.type2 == "4C"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "4C"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "4C"], na.rm = TRUE)
      }else if(input$loc.type2 == "3E"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "3E"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "3E"], na.rm = TRUE)
      }else if(input$loc.type2 == "2E"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "2E"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "2E"], na.rm = TRUE)
      }else if(input$loc.type2 == "6C"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "6C"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "6C"], na.rm = TRUE)
      }else if(input$loc.type2 == "8A"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "8A"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "8A"], na.rm = TRUE)
      }else if(input$loc.type2 == "4D"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "4D"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "4D"], na.rm = TRUE)
      }else if(input$loc.type2 == "4B"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "4B"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "4B"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "4A"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "4A"], na.rm = TRUE)
      }else if(input$loc.type2 == "3G"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "3G"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "3G"], na.rm = TRUE)
      }else if(input$loc.type2 == "3F"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "3F"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "3F"], na.rm = TRUE)
      }else if(input$loc.type2 == "5A"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "5A"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "5A"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Census Tract"){
      if(input$loc.type2 == "006202"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "006202"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "006202"], na.rm = TRUE)
      }else if(input$loc.type2 == "009811"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "009811"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "009811"], na.rm = TRUE)
      }else if(input$loc.type2 == "009807"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "009807"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "009807"], na.rm = TRUE)
      }else if(input$loc.type2 == "009700"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "009700"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "009700"], na.rm = TRUE)
      }else if(input$loc.type2 == "009804"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "009804"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "009804"], na.rm = TRUE)
      }else if(input$loc.type2 == "007304"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "007304"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "007304"], na.rm = TRUE)
      }else if(input$loc.type2 == "007301"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "007301"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "007301"], na.rm = TRUE)
      }else if(input$loc.type2 == "010400"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "010400"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "010400"], na.rm = TRUE)
      }else if(input$loc.type2 == "010800"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "010800"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "010800"], na.rm = TRUE)
      }else if(input$loc.type2 == "005600"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "005600"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "005600"], na.rm = TRUE)
      }else if(input$loc.type2 == "009603"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "009603"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "009603"], na.rm = TRUE)
      }else if(input$loc.type2 == "007809"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "007809"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "007809"], na.rm = TRUE)
      }else if(input$loc.type2 == "009902"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "009902"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "009902"], na.rm = TRUE)
      }else if(input$loc.type2 == "009000"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "009000"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "009000"], na.rm = TRUE)
      }else if(input$loc.type2 == "009905"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "009905"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "009905"], na.rm = TRUE)
      }else if(input$loc.type2 == "007707"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "007707"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "007707"], na.rm = TRUE)
      }else if(input$loc.type2 == "007806"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "007806"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "007806"], na.rm = TRUE)
      }else if(input$loc.type2 == "007804"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "007804"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "007804"], na.rm = TRUE)
      }else if(input$loc.type2 == "007807"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "007807"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "007807"], na.rm = TRUE)
      }else if(input$loc.type2 == "007708"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "007708"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "007708"], na.rm = TRUE)
      }else if(input$loc.type2 == "010500"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "010500"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "010500"], na.rm = TRUE)
      }else if(input$loc.type2 == "006400"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "006400"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "006400"], na.rm = TRUE)
      }else if(input$loc.type2 == "008803"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "008803"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "008803"], na.rm = TRUE)
      }else if(input$loc.type2 == "010200"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "010200"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "010200"], na.rm = TRUE)
      }else if(input$loc.type2 == "006500"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "006500"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "006500"], na.rm = TRUE)
      }else if(input$loc.type2 == "008702"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "008702"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "008702"], na.rm = TRUE)
      }else if(input$loc.type2 == "003301"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "003301"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "003301"], na.rm = TRUE)
      }else if(input$loc.type2 == "004902"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "004902"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "004902"], na.rm = TRUE)
      }else if(input$loc.type2 == "004201"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "004201"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "004201"], na.rm = TRUE)
      }else if(input$loc.type2 == "003400"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "003400"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "003400"], na.rm = TRUE)
      }else if(input$loc.type2 == "004400"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "004400"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "004400"], na.rm = TRUE)
      }else if(input$loc.type2 == "011100"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "011100"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "011100"], na.rm = TRUE)
      }else if(input$loc.type2 == "004702"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "004702"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "004702"], na.rm = TRUE)
      }else if(input$loc.type2 == "009302"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "009302"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "009302"], na.rm = TRUE)
      }else if(input$loc.type2 == "008701"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "008701"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "008701"], na.rm = TRUE)
      }else if(input$loc.type2 == "010100"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "010100"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "010100"], na.rm = TRUE)
      }else if(input$loc.type2 == "002900"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "002900"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "002900"], na.rm = TRUE)
      }else if(input$loc.type2 == "004600"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "004600"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "004600"], na.rm = TRUE)
      }else if(input$loc.type2 == "009400"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "009400"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "009400"], na.rm = TRUE)
      }else if(input$loc.type2 == "001002"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "001002"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "001002"], na.rm = TRUE)
      }else if(input$loc.type2 == "010600"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "010600"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "010600"], na.rm = TRUE)
      }else if(input$loc.type2 == "000901"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "000901"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "000901"], na.rm = TRUE)
      }else if(input$loc.type2 == "000502"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "000502"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "000502"], na.rm = TRUE)
      }else if(input$loc.type2 == "009102"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "009102"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "009102"], na.rm = TRUE)
      }else if(input$loc.type2 == "009201"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "009201"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "009201"], na.rm = TRUE)
      }else if(input$loc.type2 == "002400"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "002400"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "002400"], na.rm = TRUE)
      }else if(input$loc.type2 == "001001"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "001001"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "001001"], na.rm = TRUE)
      }else if(input$loc.type2 == "000600"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "000600"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "000600"], na.rm = TRUE)
      }else if(input$loc.type2 == "002501"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "002501"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "002501"], na.rm = TRUE)
      }else if(input$loc.type2 == "009503"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "009503"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "009503"], na.rm = TRUE)
      }else if(input$loc.type2 == "000300"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "000300"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "000300"], na.rm = TRUE)
      }else if(input$loc.type2 == "000400"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "000400"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "000400"], na.rm = TRUE)
      }else if(input$loc.type2 == "006700"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "006700"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "006700"], na.rm = TRUE)
      }else if(input$loc.type2 == "007200"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "007200"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "007200"], na.rm = TRUE)
      }else if(input$loc.type2 == "008302"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "008302"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "008302"], na.rm = TRUE)
      }else if(input$loc.type2 == "006600"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "006600"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "006600"], na.rm = TRUE)
      }else if(input$loc.type2 == "008904"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "008904"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "008904"], na.rm = TRUE)
      }else if(input$loc.type2 == "008100"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "008100"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "008100"], na.rm = TRUE)
      }else if(input$loc.type2 == "006802"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "006802"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "006802"], na.rm = TRUE)
      }else if(input$loc.type2 == "007601"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "007601"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "007601"], na.rm = TRUE)
      }else if(input$loc.type2 == "007100"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "007100"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "007100"], na.rm = TRUE)
      }else if(input$loc.type2 == "008200"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "008200"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "008200"], na.rm = TRUE)
      }else if(input$loc.type2 == "008001"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "008001"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "008001"], na.rm = TRUE)
      }else if(input$loc.type2 == "008002"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "008002"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "008002"], na.rm = TRUE)
      }else if(input$loc.type2 == "002101"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "002101"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "002101"], na.rm = TRUE)
      }else if(input$loc.type2 == "001901"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "001901"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "001901"], na.rm = TRUE)
      }else if(input$loc.type2 == "002600"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "002600"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "002600"], na.rm = TRUE)
      }else if(input$loc.type2 == "001401"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "001401"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "001401"], na.rm = TRUE)
      }else if(input$loc.type2 == "001803"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "001803"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "001803"], na.rm = TRUE)
      }else if(input$loc.type2 == "001301"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "001301"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "001301"], na.rm = TRUE)
      }else if(input$loc.type2 == "002102"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "002102"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "002102"], na.rm = TRUE)
      }else if(input$loc.type2 == "001804"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "001804"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "001804"], na.rm = TRUE)
      }else if(input$loc.type2 == "002001"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "002001"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "002001"], na.rm = TRUE)
      }else if(input$loc.type2 == "001902"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "001902"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "001902"], na.rm = TRUE)
      }else if(input$loc.type2 == "001100"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "001100"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "001100"], na.rm = TRUE)
      }else if(input$loc.type2 == "001600"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "001600"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "001600"], na.rm = TRUE)
      }else if(input$loc.type2 == "009509"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "009509"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "009509"], na.rm = TRUE)
      }else if(input$loc.type2 == "001500"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "001500"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "001500"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Single Member District"){
      if(input$loc.type2 == "2A01"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "2A01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "2A01"], na.rm = TRUE)
      }else if(input$loc.type2 == "8D03"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "8D03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "8D03"], na.rm = TRUE)
      }else if(input$loc.type2 == "8D01"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "8D01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "8D01"], na.rm = TRUE)
      }else if(input$loc.type2 == "8D06"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "8D06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "8D06"], na.rm = TRUE)
      }else if(input$loc.type2 == "8E05"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "8E05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "8E05"], na.rm = TRUE)
      }else if(input$loc.type2 == "8C07"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "8C07"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "8C07"], na.rm = TRUE)
      }else if(input$loc.type2 == "8E04"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "8E04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "8E04"], na.rm = TRUE)
      }else if(input$loc.type2 == "8C05"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "8C05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "8C05"], na.rm = TRUE)
      }else if(input$loc.type2 == "8C03"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "8C03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "8C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "2A07"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "2A07"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "2A07"], na.rm = TRUE)
      }else if(input$loc.type2 == "2A03"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "2A03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "2A03"], na.rm = TRUE)
      }else if(input$loc.type2 == "7F01"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "7F01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "7F01"], na.rm = TRUE)
      }else if(input$loc.type2 == "7C04"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "7C04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "7C04"], na.rm = TRUE)
      }else if(input$loc.type2 == "7E02"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "7E02"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "7E02"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C03"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "5C03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "5C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "7E06"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "7E06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "7E06"], na.rm = TRUE)
      }else if(input$loc.type2 == "7E01"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "7E01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "7E01"], na.rm = TRUE)
      }else if(input$loc.type2 == "7C07"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "7C07"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "7C07"], na.rm = TRUE)
      }else if(input$loc.type2 == "7C03"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "7C03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "7C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "7C06"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "7C06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "7C06"], na.rm = TRUE)
      }else if(input$loc.type2 == "7F06"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "7F06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "7F06"], na.rm = TRUE)
      }else if(input$loc.type2 == "6D03"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "6D03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "6D03"], na.rm = TRUE)
      }else if(input$loc.type2 == "6D06"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "6D06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "6D06"], na.rm = TRUE)
      }else if(input$loc.type2 == "5D01"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "5D01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "5D01"], na.rm = TRUE)
      }else if(input$loc.type2 == "6D05"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "6D05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "6D05"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B01"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "6B01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "6B01"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E03"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "5E03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "5E03"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E08"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "5E08"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "5E08"], na.rm = TRUE)
      }else if(input$loc.type2 == "2F06"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "2F06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "2F06"], na.rm = TRUE)
      }else if(input$loc.type2 == "2B09"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "2B09"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "2B09"], na.rm = TRUE)
      }else if(input$loc.type2 == "1B01"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "1B01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "1B01"], na.rm = TRUE)
      }else if(input$loc.type2 == "1B02"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "1B02"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "1B02"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C02"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "5C02"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "5C02"], na.rm = TRUE)
      }else if(input$loc.type2 == "6E07"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "6E07"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "6E07"], na.rm = TRUE)
      }else if(input$loc.type2 == "5B03"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "5B03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "5B03"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E04"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "5E04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "5E04"], na.rm = TRUE)
      }else if(input$loc.type2 == "2F08"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "2F08"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "2F08"], na.rm = TRUE)
      }else if(input$loc.type2 == "1A04"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "1A04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "1A04"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E05"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "5E05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "5E05"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C01"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "5C01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "5C01"], na.rm = TRUE)
      }else if(input$loc.type2 == "3C06"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "3C06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "3C06"], na.rm = TRUE)
      }else if(input$loc.type2 == "6A01"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "6A01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "6A01"], na.rm = TRUE)
      }else if(input$loc.type2 == "2B05"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "2B05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "2B05"], na.rm = TRUE)
      }else if(input$loc.type2 == "3D02"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "3D02"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "3D02"], na.rm = TRUE)
      }else if(input$loc.type2 == "3C03"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "3C03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "3C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "5B01"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "5B01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "5B01"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C06"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "5C06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "5C06"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E01"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "5E01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "5E01"], na.rm = TRUE)
      }else if(input$loc.type2 == "4C08"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "4C08"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "4C08"], na.rm = TRUE)
      }else if(input$loc.type2 == "3E05"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "3E05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "3E05"], na.rm = TRUE)
      }else if(input$loc.type2 == "3C09"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "3C09"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "3C09"], na.rm = TRUE)
      }else if(input$loc.type2 == "4C03"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "4C03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "4C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "2E01"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "2E01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "2E01"], na.rm = TRUE)
      }else if(input$loc.type2 == "3C08"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "3C08"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "3C08"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B05"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "6B05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "6B05"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C04"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "5C04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "5C04"], na.rm = TRUE)
      }else if(input$loc.type2 == "6D07"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "6D07"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "6D07"], na.rm = TRUE)
      }else if(input$loc.type2 == "6C03"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "6C03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "6C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B02"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "6B02"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "6B02"], na.rm = TRUE)
      }else if(input$loc.type2 == "5D05"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "5D05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "5D05"], na.rm = TRUE)
      }else if(input$loc.type2 == "6A03"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "6A03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "6A03"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B09"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "6B09"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "6B09"], na.rm = TRUE)
      }else if(input$loc.type2 == "8A03"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "8A03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "8A03"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B07"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "6B07"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "6B07"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B06"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "6B06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "6B06"], na.rm = TRUE)
      }else if(input$loc.type2 == "6A02"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "6A02"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "6A02"], na.rm = TRUE)
      }else if(input$loc.type2 == "6A08"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "6A08"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "6A08"], na.rm = TRUE)
      }else if(input$loc.type2 == "4D03"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "4D03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "4D03"], na.rm = TRUE)
      }else if(input$loc.type2 == "4B04"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "4B04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "4B04"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A08"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "4A08"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "4A08"], na.rm = TRUE)
      }else if(input$loc.type2 == "3D03"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "3D03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "3D03"], na.rm = TRUE)
      }else if(input$loc.type2 == "3G05"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "3G05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "3G05"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A07"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "4A07"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "4A07"], na.rm = TRUE)
      }else if(input$loc.type2 == "3E02"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "3E02"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "3E02"], na.rm = TRUE)
      }else if(input$loc.type2 == "3F03"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "3F03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "3F03"], na.rm = TRUE)
      }else if(input$loc.type2 == "4B06"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "4B06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "4B06"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A04"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "4A04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "4A04"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A06"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "4A06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "4A06"], na.rm = TRUE)
      }else if(input$loc.type2 == "3E04"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "3E04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "3E04"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A02"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "4A02"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "4A02"], na.rm = TRUE)
      }else if(input$loc.type2 == "5A08"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "5A08"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "5A08"], na.rm = TRUE)
      }else if(input$loc.type2 == "3G04"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "3G04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "3G04"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Voter Precinct"){
      if(input$loc.type2 == "129"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "129"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "129"], na.rm = TRUE)
      }else if(input$loc.type2 == "125"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "125"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "125"], na.rm = TRUE)
      }else if(input$loc.type2 == "126"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "126"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "126"], na.rm = TRUE)
      }else if(input$loc.type2 == "121"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "121"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "121"], na.rm = TRUE)
      }else if(input$loc.type2 == "122"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "122"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "122"], na.rm = TRUE)
      }else if(input$loc.type2 == "120"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "120"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "120"], na.rm = TRUE)
      }else if(input$loc.type2 == "123"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "123"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "123"], na.rm = TRUE)
      }else if(input$loc.type2 == "2"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "2"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "2"], na.rm = TRUE)
      }else if(input$loc.type2 == "3"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "3"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "3"], na.rm = TRUE)
      }else if(input$loc.type2 == "102"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "102"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "102"], na.rm = TRUE)
      }else if(input$loc.type2 == "94"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "94"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "94"], na.rm = TRUE)
      }else if(input$loc.type2 == "110"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "110"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "110"], na.rm = TRUE)
      }else if(input$loc.type2 == "139"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "139"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "139"], na.rm = TRUE)
      }else if(input$loc.type2 == "105"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "105"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "105"], na.rm = TRUE)
      }else if(input$loc.type2 == "106"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "106"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "106"], na.rm = TRUE)
      }else if(input$loc.type2 == "93"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "93"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "93"], na.rm = TRUE)
      }else if(input$loc.type2 == "97"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "97"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "97"], na.rm = TRUE)
      }else if(input$loc.type2 == "95"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "95"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "95"], na.rm = TRUE)
      }else if(input$loc.type2 == "132"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "132"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "132"], na.rm = TRUE)
      }else if(input$loc.type2 == "128"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "128"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "138"], na.rm = TRUE)
      }else if(input$loc.type2 == "127"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "127"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "127"], na.rm = TRUE)
      }else if(input$loc.type2 == "76"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "76"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "76"], na.rm = TRUE)
      }else if(input$loc.type2 == "130"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "130"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "130"], na.rm = TRUE)
      }else if(input$loc.type2 == "75"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "75"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "75"], na.rm = TRUE)
      }else if(input$loc.type2 == "135"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "135"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "135"], na.rm = TRUE)
      }else if(input$loc.type2 == "141"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "141"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "141"], na.rm = TRUE)
      }else if(input$loc.type2 == "20"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "20"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "20"], na.rm = TRUE)
      }else if(input$loc.type2 == "22"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "22"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "22"], na.rm = TRUE)
      }else if(input$loc.type2 == "72"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "72"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "72"], na.rm = TRUE)
      }else if(input$loc.type2 == "1"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "1"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "1"], na.rm = TRUE)
      }else if(input$loc.type2 == "73"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "73"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "73"], na.rm = TRUE)
      }else if(input$loc.type2 == "42"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "42"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "42"], na.rm = TRUE)
      }else if(input$loc.type2 == "19"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "19"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "19"], na.rm = TRUE)
      }else if(input$loc.type2 == "69"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "69"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "69"], na.rm = TRUE)
      }else if(input$loc.type2 == "29"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "29"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "29"], na.rm = TRUE)
      }else if(input$loc.type2 == "82"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "82"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "82"], na.rm = TRUE)
      }else if(input$loc.type2 == "17"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "17"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "17"], na.rm = TRUE)
      }else if(input$loc.type2 == "9"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "9"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "9"], na.rm = TRUE)
      }else if(input$loc.type2 == "26"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "26"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "26"], na.rm = TRUE)
      }else if(input$loc.type2 == "74"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "74"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "74"], na.rm = TRUE)
      }else if(input$loc.type2 == "45"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "45"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "45"], na.rm = TRUE)
      }else if(input$loc.type2 == "30"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "30"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "30"], na.rm = TRUE)
      }else if(input$loc.type2 == "27"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "27"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "27"], na.rm = TRUE)
      }else if(input$loc.type2 == "48"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "48"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "48"], na.rm = TRUE)
      }else if(input$loc.type2 == "67"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "67"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "67"], na.rm = TRUE)
      }else if(input$loc.type2 == "6"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "6"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "6"], na.rm = TRUE)
      }else if(input$loc.type2 == "12"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "12"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "12"], na.rm = TRUE)
      }else if(input$loc.type2 == "88"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "88"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "88"], na.rm = TRUE)
      }else if(input$loc.type2 == "131"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "131"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "131"], na.rm = TRUE)
      }else if(input$loc.type2 == "85"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "85"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "85"], na.rm = TRUE)
      }else if(input$loc.type2 == "89"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "89"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "89"], na.rm = TRUE)
      }else if(input$loc.type2 == "79"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "79"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "79"], na.rm = TRUE)
      }else if(input$loc.type2 == "91"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "91"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "91"], na.rm = TRUE)
      }else if(input$loc.type2 == "133"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "133"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "133"], na.rm = TRUE)
      }else if(input$loc.type2 == "81"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "81"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "81"], na.rm = TRUE)
      }else if(input$loc.type2 == "71"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "71"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "71"], na.rm = TRUE)
      }else if(input$loc.type2 == "86"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "86"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "86"], na.rm = TRUE)
      }else if(input$loc.type2 == "56"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "56"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "56"], na.rm = TRUE)
      }else if(input$loc.type2 == "59"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "59"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "59"], na.rm = TRUE)
      }else if(input$loc.type2 == "50"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "50"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "50"], na.rm = TRUE)
      }else if(input$loc.type2 == "61"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "61"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "61"], na.rm = TRUE)
      }else if(input$loc.type2 == "31"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "31"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "31"], na.rm = TRUE)
      }else if(input$loc.type2 == "138"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "138"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "138"], na.rm = TRUE)
      }else if(input$loc.type2 == "57"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "57"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "57"], na.rm = TRUE)
      }else if(input$loc.type2 == "60"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "60"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "60"], na.rm = TRUE)
      }else if(input$loc.type2 == "53"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "53"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "53"], na.rm = TRUE)
      }else if(input$loc.type2 == "32"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "32"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "32"], na.rm = TRUE)
      }else if(input$loc.type2 == "62"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "62"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "62"], na.rm = TRUE)
      }else if(input$loc.type2 == "66"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "66"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "66"], na.rm = TRUE)
      }else if(input$loc.type2 == "51"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "51"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "51"], na.rm = TRUE)
      }
    }
    
    if(input$loc.type == "Quadrant"){
      if(input$loc.type2 == "SE"){
        ls2 <- mean(ls1$current_travel_time[ls1$quadrant == "SE"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$quadrant == "SE"], na.rm = TRUE)
      }else if(input$loc.type2 == "SW"){
        ls2 <- mean(ls1$current_travel_time[ls1$quadrant == "SW"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$quadrant == "SW"], na.rm = TRUE)
      }else if(input$loc.type2 == "NE"){
        ls2 <- mean(ls1$current_travel_time[ls1$quadrant == "NE"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$quadrant == "NE"], na.rm = TRUE)
      }else if(input$loc.type2 == "NW"){
        ls2 <- mean(ls1$current_travel_time[ls1$quadrant == "NW"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$quadrant == "NW"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Ward"){
      if(input$loc.type2 == "1"){
        ls2 <- mean(ls1$current_travel_time[ls1$ward == "1"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$ward == "1"], na.rm = TRUE)
      }else if(input$loc.type2 == "2"){
        ls2 <- mean(ls1$current_travel_time[ls1$ward == "2"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$ward == "2"], na.rm = TRUE)
      }else if(input$loc.type2 == "3"){
        ls2 <- mean(ls1$current_travel_time[ls1$ward == "3"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$ward == "3"], na.rm = TRUE)
      }else if(input$loc.type2 == "4"){
        ls2 <- mean(ls1$current_travel_time[ls1$ward == "4"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$ward == "4"], na.rm = TRUE)
      }else if(input$loc.type2 == "5"){
        ls2 <- mean(ls1$current_travel_time[ls1$ward == "5"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$ward == "5"], na.rm = TRUE)
      }else if(input$loc.type2 == "6"){
        ls2 <- mean(ls1$current_travel_time[ls1$ward == "6"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$ward == "6"], na.rm = TRUE)
      }else if(input$loc.type2 == "7"){
        ls2 <- mean(ls1$current_travel_time[ls1$ward == "7"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$ward == "7"], na.rm = TRUE)
      }else if(input$loc.type2 == "8"){
        ls2 <- mean(ls1$current_travel_time[ls1$ward == "8"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$ward == "8"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Zip Code"){
      if(input$loc.type2 == "20227"){
        ls2 <- mean(ls1$current_travel_time[ls1$zip_code == "20227"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$zip_code == "20227"], na.rm = TRUE)
      }else if(input$loc.type2 == "20032"){
        ls2 <- mean(ls1$current_travel_time[ls1$zip_code == "20032"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$zip_code == "20032"], na.rm = TRUE)
      }else if(input$loc.type2 == "20037"){
        ls2 <- mean(ls1$current_travel_time[ls1$zip_code == "20037"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$zip_code == "20037"], na.rm = TRUE)
      }else if(input$loc.type2 == "20019"){
        ls2 <- mean(ls1$current_travel_time[ls1$zip_code == "20019"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$zip_code == "20019"], na.rm = TRUE)
      }else if(input$loc.type2 == "20020"){
        ls2 <- mean(ls1$current_travel_time[ls1$zip_code == "20020"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$zip_code == "20020"], na.rm = TRUE)
      }else if(input$loc.type2 == "20018"){
        ls2 <- mean(ls1$current_travel_time[ls1$zip_code == "20018"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$zip_code == "20018"], na.rm = TRUE)
      }else if(input$loc.type2 == "20024"){
        ls2 <- mean(ls1$current_travel_time[ls1$zip_code == "20024"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$zip_code == "20024"], na.rm = TRUE)
      }else if(input$loc.type2 == "20002"){
        ls2 <- mean(ls1$current_travel_time[ls1$zip_code == "20002"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$zip_code == "20002"], na.rm = TRUE)
      }else if(input$loc.type2 == "20003"){
        ls2 <- mean(ls1$current_travel_time[ls1$zip_code == "20003"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$zip_code == "20003"], na.rm = TRUE)
      }else if(input$loc.type2 == "20001"){
        ls2 <- mean(ls1$current_travel_time[ls1$zip_code == "20001"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$zip_code == "20001"], na.rm = TRUE)
      }else if(input$loc.type2 == "20005"){
        ls2 <- mean(ls1$current_travel_time[ls1$zip_code == "20005"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$zip_code == "20005"], na.rm = TRUE)
      }else if(input$loc.type2 == "20009"){
        ls2 <- mean(ls1$current_travel_time[ls1$zip_code == "20009"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$zip_code == "20009"], na.rm = TRUE)
      }else if(input$loc.type2 == "20017"){
        ls2 <- mean(ls1$current_travel_time[ls1$zip_code == "20017"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$zip_code == "20017"], na.rm = TRUE)
      }else if(input$loc.type2 == "20010"){
        ls2 <- mean(ls1$current_travel_time[ls1$zip_code == "20010"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$zip_code == "20010"], na.rm = TRUE)
      }else if(input$loc.type2 == "20016"){
        ls2 <- mean(ls1$current_travel_time[ls1$zip_code == "20016"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$zip_code == "20016"], na.rm = TRUE)
      }else if(input$loc.type2 == "20008"){
        ls2 <- mean(ls1$current_travel_time[ls1$zip_code == "20008"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$zip_code == "20008"], na.rm = TRUE)
      }else if(input$loc.type2 == "20011"){
        ls2 <- mean(ls1$current_travel_time[ls1$zip_code == "20011"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$zip_code == "20011"], na.rm = TRUE)
      }else if(input$loc.type2 == "20007"){
        ls2 <- mean(ls1$current_travel_time[ls1$zip_code == "20007"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$zip_code == "20007"], na.rm = TRUE)
      }else if(input$loc.type2 == "20374"){
        ls2 <- mean(ls1$current_travel_time[ls1$zip_code == "20374"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$zip_code == "20374"], na.rm = TRUE)
      }else if(input$loc.type2 == "20015"){
        ls2 <- mean(ls1$current_travel_time[ls1$zip_code == "20015"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$zip_code == "20015"], na.rm = TRUE)
      }else if(input$loc.type2 == "20012"){
        ls2 <- mean(ls1$current_travel_time[ls1$zip_code == "20012"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$zip_code == "20012"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Advisory Neighborhood Commission"){
      if(input$loc.type2 == "2A"){
        ls2 <- mean(ls1$current_travel_time[ls1$anc == "2A"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$anc == "2A"], na.rm = TRUE)
      }else if(input$loc.type2 == "8D"){
        ls2 <- mean(ls1$current_travel_time[ls1$anc == "8D"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$anc == "8D"], na.rm = TRUE)
      }else if(input$loc.type2 == "8E"){
        ls2 <- mean(ls1$current_travel_time[ls1$anc == "8E"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$anc == "8E"], na.rm = TRUE)
      }else if(input$loc.type2 == "8C"){
        ls2 <- mean(ls1$current_travel_time[ls1$anc == "8C"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$anc == "8C"], na.rm = TRUE)
      }else if(input$loc.type2 == "7F"){
        ls2 <- mean(ls1$current_travel_time[ls1$anc == "7F"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$anc == "7F"], na.rm = TRUE)
      }else if(input$loc.type2 == "7C"){
        ls2 <- mean(ls1$current_travel_time[ls1$anc == "7C"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$anc == "7C"], na.rm = TRUE)
      }else if(input$loc.type2 == "7E"){
        ls2 <- mean(ls1$current_travel_time[ls1$anc == "7E"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$anc == "7E"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C"){
        ls2 <- mean(ls1$current_travel_time[ls1$anc == "5C"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$anc == "5C"], na.rm = TRUE)
      }else if(input$loc.type2 == "6D"){
        ls2 <- mean(ls1$current_travel_time[ls1$anc == "6D"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$anc == "6D"], na.rm = TRUE)
      }else if(input$loc.type2 == "5D"){
        ls2 <- mean(ls1$current_travel_time[ls1$anc == "5D"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$anc == "5D"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B"){
        ls2 <- mean(ls1$current_travel_time[ls1$anc == "6B"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$anc == "6B"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E"){
        ls2 <- mean(ls1$current_travel_time[ls1$anc == "5E"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$anc == "5E"], na.rm = TRUE)
      }else if(input$loc.type2 == "2F"){
        ls2 <- mean(ls1$current_travel_time[ls1$anc == "2F"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$anc == "2F"], na.rm = TRUE)
      }else if(input$loc.type2 == "2B"){
        ls2 <- mean(ls1$current_travel_time[ls1$anc == "2B"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$anc == "2B"], na.rm = TRUE)
      }else if(input$loc.type2 == "1B"){
        ls2 <- mean(ls1$current_travel_time[ls1$anc == "1B"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$anc == "1B"], na.rm = TRUE)
      }else if(input$loc.type2 == "6E"){
        ls2 <- mean(ls1$current_travel_time[ls1$anc == "6E"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$anc == "6E"], na.rm = TRUE)
      }else if(input$loc.type2 == "5B"){
        ls2 <- mean(ls1$current_travel_time[ls1$anc == "5B"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$anc == "5B"], na.rm = TRUE)
      }else if(input$loc.type2 == "1A"){
        ls2 <- mean(ls1$current_travel_time[ls1$anc == "1A"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$anc == "1A"], na.rm = TRUE)
      }else if(input$loc.type2 == "3C"){
        ls2 <- mean(ls1$current_travel_time[ls1$anc == "3C"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$anc == "3C"], na.rm = TRUE)
      }else if(input$loc.type2 == "6A"){
        ls2 <- mean(ls1$current_travel_time[ls1$anc == "6A"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$anc == "6A"], na.rm = TRUE)
      }else if(input$loc.type2 == "3D"){
        ls2 <- mean(ls1$current_travel_time[ls1$anc == "3D"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$anc == "3D"], na.rm = TRUE)
      }else if(input$loc.type2 == "4C"){
        ls2 <- mean(ls1$current_travel_time[ls1$anc == "4C"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$anc == "4C"], na.rm = TRUE)
      }else if(input$loc.type2 == "3E"){
        ls2 <- mean(ls1$current_travel_time[ls1$anc == "3E"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$anc == "3E"], na.rm = TRUE)
      }else if(input$loc.type2 == "2E"){
        ls2 <- mean(ls1$current_travel_time[ls1$anc == "2E"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$anc == "2E"], na.rm = TRUE)
      }else if(input$loc.type2 == "6C"){
        ls2 <- mean(ls1$current_travel_time[ls1$anc == "6C"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$anc == "6C"], na.rm = TRUE)
      }else if(input$loc.type2 == "8A"){
        ls2 <- mean(ls1$current_travel_time[ls1$anc == "8A"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$anc == "8A"], na.rm = TRUE)
      }else if(input$loc.type2 == "4D"){
        ls2 <- mean(ls1$current_travel_time[ls1$anc == "4D"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$anc == "4D"], na.rm = TRUE)
      }else if(input$loc.type2 == "4B"){
        ls2 <- mean(ls1$current_travel_time[ls1$anc == "4B"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$anc == "4B"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A"){
        ls2 <- mean(ls1$current_travel_time[ls1$anc == "4A"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$anc == "4A"], na.rm = TRUE)
      }else if(input$loc.type2 == "3G"){
        ls2 <- mean(ls1$current_travel_time[ls1$anc == "3G"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$anc == "3G"], na.rm = TRUE)
      }else if(input$loc.type2 == "3F"){
        ls2 <- mean(ls1$current_travel_time[ls1$anc == "3F"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$anc == "3F"], na.rm = TRUE)
      }else if(input$loc.type2 == "5A"){
        ls2 <- mean(ls1$current_travel_time[ls1$anc == "5A"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$anc == "5A"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Census Tract"){
      if(input$loc.type2 == "006202"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "006202"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "006202"], na.rm = TRUE)
      }else if(input$loc.type2 == "009811"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "009811"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "009811"], na.rm = TRUE)
      }else if(input$loc.type2 == "009807"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "009807"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "009807"], na.rm = TRUE)
      }else if(input$loc.type2 == "009700"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "009700"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "009700"], na.rm = TRUE)
      }else if(input$loc.type2 == "009804"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "009804"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "009804"], na.rm = TRUE)
      }else if(input$loc.type2 == "007304"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "007304"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "007304"], na.rm = TRUE)
      }else if(input$loc.type2 == "007301"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "007301"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "007301"], na.rm = TRUE)
      }else if(input$loc.type2 == "010400"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "010400"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "010400"], na.rm = TRUE)
      }else if(input$loc.type2 == "010800"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "010800"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "010800"], na.rm = TRUE)
      }else if(input$loc.type2 == "005600"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "005600"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "005600"], na.rm = TRUE)
      }else if(input$loc.type2 == "009603"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "009603"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "009603"], na.rm = TRUE)
      }else if(input$loc.type2 == "007809"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "007809"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "007809"], na.rm = TRUE)
      }else if(input$loc.type2 == "009902"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "009902"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "009902"], na.rm = TRUE)
      }else if(input$loc.type2 == "009000"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "009000"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "009000"], na.rm = TRUE)
      }else if(input$loc.type2 == "009905"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "009905"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "009905"], na.rm = TRUE)
      }else if(input$loc.type2 == "007707"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "007707"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "007707"], na.rm = TRUE)
      }else if(input$loc.type2 == "007806"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "007806"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "007806"], na.rm = TRUE)
      }else if(input$loc.type2 == "007804"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "007804"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "007804"], na.rm = TRUE)
      }else if(input$loc.type2 == "007807"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "007807"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "007807"], na.rm = TRUE)
      }else if(input$loc.type2 == "007708"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "007708"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "007708"], na.rm = TRUE)
      }else if(input$loc.type2 == "010500"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "010500"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "010500"], na.rm = TRUE)
      }else if(input$loc.type2 == "006400"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "006400"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "006400"], na.rm = TRUE)
      }else if(input$loc.type2 == "008803"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "008803"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "008803"], na.rm = TRUE)
      }else if(input$loc.type2 == "010200"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "010200"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "010200"], na.rm = TRUE)
      }else if(input$loc.type2 == "006500"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "006500"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "006500"], na.rm = TRUE)
      }else if(input$loc.type2 == "008702"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "008702"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "008702"], na.rm = TRUE)
      }else if(input$loc.type2 == "003301"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "003301"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "003301"], na.rm = TRUE)
      }else if(input$loc.type2 == "004902"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "004902"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "004902"], na.rm = TRUE)
      }else if(input$loc.type2 == "004201"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "004201"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "004201"], na.rm = TRUE)
      }else if(input$loc.type2 == "003400"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "003400"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "003400"], na.rm = TRUE)
      }else if(input$loc.type2 == "004400"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "004400"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "004400"], na.rm = TRUE)
      }else if(input$loc.type2 == "011100"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "011100"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "011100"], na.rm = TRUE)
      }else if(input$loc.type2 == "004702"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "004702"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "004702"], na.rm = TRUE)
      }else if(input$loc.type2 == "009302"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "009302"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "009302"], na.rm = TRUE)
      }else if(input$loc.type2 == "008701"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "008701"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "008701"], na.rm = TRUE)
      }else if(input$loc.type2 == "010100"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "010100"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "010100"], na.rm = TRUE)
      }else if(input$loc.type2 == "002900"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "002900"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "002900"], na.rm = TRUE)
      }else if(input$loc.type2 == "004600"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "004600"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "004600"], na.rm = TRUE)
      }else if(input$loc.type2 == "009400"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "009400"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "009400"], na.rm = TRUE)
      }else if(input$loc.type2 == "001002"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "001002"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "001002"], na.rm = TRUE)
      }else if(input$loc.type2 == "010600"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "010600"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "010600"], na.rm = TRUE)
      }else if(input$loc.type2 == "000901"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "000901"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "000901"], na.rm = TRUE)
      }else if(input$loc.type2 == "000502"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "000502"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "000502"], na.rm = TRUE)
      }else if(input$loc.type2 == "009102"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "009102"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "009102"], na.rm = TRUE)
      }else if(input$loc.type2 == "009201"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "009201"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "009201"], na.rm = TRUE)
      }else if(input$loc.type2 == "002400"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "002400"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "002400"], na.rm = TRUE)
      }else if(input$loc.type2 == "001001"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "001001"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "001001"], na.rm = TRUE)
      }else if(input$loc.type2 == "000600"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "000600"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "000600"], na.rm = TRUE)
      }else if(input$loc.type2 == "002501"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "002501"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "002501"], na.rm = TRUE)
      }else if(input$loc.type2 == "009503"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "009503"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "009503"], na.rm = TRUE)
      }else if(input$loc.type2 == "000300"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "000300"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "000300"], na.rm = TRUE)
      }else if(input$loc.type2 == "000400"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "000400"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "000400"], na.rm = TRUE)
      }else if(input$loc.type2 == "006700"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "006700"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "006700"], na.rm = TRUE)
      }else if(input$loc.type2 == "007200"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "007200"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "007200"], na.rm = TRUE)
      }else if(input$loc.type2 == "008302"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "008302"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "008302"], na.rm = TRUE)
      }else if(input$loc.type2 == "006600"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "006600"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "006600"], na.rm = TRUE)
      }else if(input$loc.type2 == "008904"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "008904"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "008904"], na.rm = TRUE)
      }else if(input$loc.type2 == "008100"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "008100"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "008100"], na.rm = TRUE)
      }else if(input$loc.type2 == "006802"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "006802"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "006802"], na.rm = TRUE)
      }else if(input$loc.type2 == "007601"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "007601"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "007601"], na.rm = TRUE)
      }else if(input$loc.type2 == "007100"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "007100"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "007100"], na.rm = TRUE)
      }else if(input$loc.type2 == "008200"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "008200"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "008200"], na.rm = TRUE)
      }else if(input$loc.type2 == "008001"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "008001"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "008001"], na.rm = TRUE)
      }else if(input$loc.type2 == "008002"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "008002"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "008002"], na.rm = TRUE)
      }else if(input$loc.type2 == "002101"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "002101"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "002101"], na.rm = TRUE)
      }else if(input$loc.type2 == "001901"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "001901"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "001901"], na.rm = TRUE)
      }else if(input$loc.type2 == "002600"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "002600"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "002600"], na.rm = TRUE)
      }else if(input$loc.type2 == "001401"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "001401"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "001401"], na.rm = TRUE)
      }else if(input$loc.type2 == "001803"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "001803"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "001803"], na.rm = TRUE)
      }else if(input$loc.type2 == "001301"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "001301"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "001301"], na.rm = TRUE)
      }else if(input$loc.type2 == "002102"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "002102"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "002102"], na.rm = TRUE)
      }else if(input$loc.type2 == "001804"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "001804"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "001804"], na.rm = TRUE)
      }else if(input$loc.type2 == "002001"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "002001"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "002001"], na.rm = TRUE)
      }else if(input$loc.type2 == "001902"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "001902"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "001902"], na.rm = TRUE)
      }else if(input$loc.type2 == "001100"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "001100"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "001100"], na.rm = TRUE)
      }else if(input$loc.type2 == "001600"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "001600"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "001600"], na.rm = TRUE)
      }else if(input$loc.type2 == "009509"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "009509"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "009509"], na.rm = TRUE)
      }else if(input$loc.type2 == "001500"){
        ls2 <- mean(ls1$current_travel_time[ls1$census_tract == "001500"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$census_tract == "001500"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Single Member District"){
      if(input$loc.type2 == "2A01"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "2A01"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "2A01"], na.rm = TRUE)
      }else if(input$loc.type2 == "8D03"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "8D03"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "8D03"], na.rm = TRUE)
      }else if(input$loc.type2 == "8D01"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "8D01"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "8D01"], na.rm = TRUE)
      }else if(input$loc.type2 == "8D06"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "8D06"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "8D06"], na.rm = TRUE)
      }else if(input$loc.type2 == "8E05"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "8E05"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "8E05"], na.rm = TRUE)
      }else if(input$loc.type2 == "8C07"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "8C07"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "8C07"], na.rm = TRUE)
      }else if(input$loc.type2 == "8E04"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "8E04"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "8E04"], na.rm = TRUE)
      }else if(input$loc.type2 == "8C05"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "8C05"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "8C05"], na.rm = TRUE)
      }else if(input$loc.type2 == "8C03"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "8C03"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "8C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "2A07"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "2A07"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "2A07"], na.rm = TRUE)
      }else if(input$loc.type2 == "2A03"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "2A03"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "2A03"], na.rm = TRUE)
      }else if(input$loc.type2 == "7F01"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "7F01"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "7F01"], na.rm = TRUE)
      }else if(input$loc.type2 == "7C04"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "7C04"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "7C04"], na.rm = TRUE)
      }else if(input$loc.type2 == "7E02"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "7E02"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "7E02"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C03"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "5C03"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "5C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "7E06"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "7E06"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "7E06"], na.rm = TRUE)
      }else if(input$loc.type2 == "7E01"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "7E01"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "7E01"], na.rm = TRUE)
      }else if(input$loc.type2 == "7C07"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "7C07"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "7C07"], na.rm = TRUE)
      }else if(input$loc.type2 == "7C03"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "7C03"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "7C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "7C06"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "7C06"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "7C06"], na.rm = TRUE)
      }else if(input$loc.type2 == "7F06"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "7F06"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "7F06"], na.rm = TRUE)
      }else if(input$loc.type2 == "6D03"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "6D03"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "6D03"], na.rm = TRUE)
      }else if(input$loc.type2 == "6D06"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "6D06"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "6D06"], na.rm = TRUE)
      }else if(input$loc.type2 == "5D01"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "5D01"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "5D01"], na.rm = TRUE)
      }else if(input$loc.type2 == "6D05"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "6D05"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "6D05"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B01"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "6B01"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "6B01"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E03"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "5E03"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "5E03"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E08"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "5E08"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "5E08"], na.rm = TRUE)
      }else if(input$loc.type2 == "2F06"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "2F06"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "2F06"], na.rm = TRUE)
      }else if(input$loc.type2 == "2B09"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "2B09"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "2B09"], na.rm = TRUE)
      }else if(input$loc.type2 == "1B01"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "1B01"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "1B01"], na.rm = TRUE)
      }else if(input$loc.type2 == "1B02"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "1B02"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "1B02"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C02"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "5C02"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "5C02"], na.rm = TRUE)
      }else if(input$loc.type2 == "6E07"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "6E07"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "6E07"], na.rm = TRUE)
      }else if(input$loc.type2 == "5B03"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "5B03"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "5B03"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E04"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "5E04"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "5E04"], na.rm = TRUE)
      }else if(input$loc.type2 == "2F08"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "2F08"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "2F08"], na.rm = TRUE)
      }else if(input$loc.type2 == "1A04"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "1A04"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "1A04"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E05"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "5E05"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "5E05"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C01"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "5C01"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "5C01"], na.rm = TRUE)
      }else if(input$loc.type2 == "3C06"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "3C06"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "3C06"], na.rm = TRUE)
      }else if(input$loc.type2 == "6A01"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "6A01"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "6A01"], na.rm = TRUE)
      }else if(input$loc.type2 == "2B05"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "2B05"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "2B05"], na.rm = TRUE)
      }else if(input$loc.type2 == "3D02"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "3D02"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "3D02"], na.rm = TRUE)
      }else if(input$loc.type2 == "3C03"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "3C03"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "3C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "5B01"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "5B01"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "5B01"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C06"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "5C06"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "5C06"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E01"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "5E01"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "5E01"], na.rm = TRUE)
      }else if(input$loc.type2 == "4C08"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "4C08"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "4C08"], na.rm = TRUE)
      }else if(input$loc.type2 == "3E05"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "3E05"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "3E05"], na.rm = TRUE)
      }else if(input$loc.type2 == "3C09"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "3C09"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "3C09"], na.rm = TRUE)
      }else if(input$loc.type2 == "4C03"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "4C03"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "4C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "2E01"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "2E01"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "2E01"], na.rm = TRUE)
      }else if(input$loc.type2 == "3C08"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "3C08"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "3C08"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B05"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "6B05"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "6B05"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C04"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "5C04"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "5C04"], na.rm = TRUE)
      }else if(input$loc.type2 == "6D07"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "6D07"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "6D07"], na.rm = TRUE)
      }else if(input$loc.type2 == "6C03"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "6C03"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "6C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B02"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "6B02"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "6B02"], na.rm = TRUE)
      }else if(input$loc.type2 == "5D05"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "5D05"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "5D05"], na.rm = TRUE)
      }else if(input$loc.type2 == "6A03"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "6A03"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "6A03"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B09"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "6B09"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "6B09"], na.rm = TRUE)
      }else if(input$loc.type2 == "8A03"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "8A03"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "8A03"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B07"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "6B07"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "6B07"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B06"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "6B06"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "6B06"], na.rm = TRUE)
      }else if(input$loc.type2 == "6A02"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "6A02"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "6A02"], na.rm = TRUE)
      }else if(input$loc.type2 == "6A08"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "6A08"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "6A08"], na.rm = TRUE)
      }else if(input$loc.type2 == "4D03"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "4D03"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "4D03"], na.rm = TRUE)
      }else if(input$loc.type2 == "4B04"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "4B04"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "4B04"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A08"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "4A08"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "4A08"], na.rm = TRUE)
      }else if(input$loc.type2 == "3D03"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "3D03"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "3D03"], na.rm = TRUE)
      }else if(input$loc.type2 == "3G05"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "3G05"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "3G05"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A07"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "4A07"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "4A07"], na.rm = TRUE)
      }else if(input$loc.type2 == "3E02"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "3E02"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "3E02"], na.rm = TRUE)
      }else if(input$loc.type2 == "3F03"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "3F03"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "3F03"], na.rm = TRUE)
      }else if(input$loc.type2 == "4B06"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "4B06"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "4B06"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A04"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "4A04"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "4A04"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A06"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "4A06"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "4A06"], na.rm = TRUE)
      }else if(input$loc.type2 == "3E04"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "3E04"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "3E04"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A02"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "4A02"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "4A02"], na.rm = TRUE)
      }else if(input$loc.type2 == "5A08"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "5A08"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "5A08"], na.rm = TRUE)
      }else if(input$loc.type2 == "3G04"){
        ls2 <- mean(ls1$current_travel_time[ls1$single_member_district == "3G04"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$single_member_district == "3G04"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Voter Precinct"){
      if(input$loc.type2 == "129"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "129"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "129"], na.rm = TRUE)
      }else if(input$loc.type2 == "125"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "125"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "125"], na.rm = TRUE)
      }else if(input$loc.type2 == "126"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "126"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "126"], na.rm = TRUE)
      }else if(input$loc.type2 == "121"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "121"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "121"], na.rm = TRUE)
      }else if(input$loc.type2 == "122"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "122"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "122"], na.rm = TRUE)
      }else if(input$loc.type2 == "120"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "120"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "120"], na.rm = TRUE)
      }else if(input$loc.type2 == "123"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "123"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "123"], na.rm = TRUE)
      }else if(input$loc.type2 == "2"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "2"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "2"], na.rm = TRUE)
      }else if(input$loc.type2 == "3"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "3"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "3"], na.rm = TRUE)
      }else if(input$loc.type2 == "102"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "102"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "102"], na.rm = TRUE)
      }else if(input$loc.type2 == "94"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "94"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "94"], na.rm = TRUE)
      }else if(input$loc.type2 == "110"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "110"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "110"], na.rm = TRUE)
      }else if(input$loc.type2 == "139"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "139"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "139"], na.rm = TRUE)
      }else if(input$loc.type2 == "105"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "105"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "105"], na.rm = TRUE)
      }else if(input$loc.type2 == "106"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "106"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "106"], na.rm = TRUE)
      }else if(input$loc.type2 == "93"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "93"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "93"], na.rm = TRUE)
      }else if(input$loc.type2 == "97"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "97"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "97"], na.rm = TRUE)
      }else if(input$loc.type2 == "95"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "95"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "95"], na.rm = TRUE)
      }else if(input$loc.type2 == "132"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "132"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "132"], na.rm = TRUE)
      }else if(input$loc.type2 == "128"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "128"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "138"], na.rm = TRUE)
      }else if(input$loc.type2 == "127"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "127"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "127"], na.rm = TRUE)
      }else if(input$loc.type2 == "76"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "76"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "76"], na.rm = TRUE)
      }else if(input$loc.type2 == "130"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "130"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "130"], na.rm = TRUE)
      }else if(input$loc.type2 == "75"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "75"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "75"], na.rm = TRUE)
      }else if(input$loc.type2 == "135"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "135"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "135"], na.rm = TRUE)
      }else if(input$loc.type2 == "141"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "141"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "141"], na.rm = TRUE)
      }else if(input$loc.type2 == "20"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "20"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "20"], na.rm = TRUE)
      }else if(input$loc.type2 == "22"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "22"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "22"], na.rm = TRUE)
      }else if(input$loc.type2 == "72"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "72"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "72"], na.rm = TRUE)
      }else if(input$loc.type2 == "1"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "1"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "1"], na.rm = TRUE)
      }else if(input$loc.type2 == "73"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "73"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "73"], na.rm = TRUE)
      }else if(input$loc.type2 == "42"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "42"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "42"], na.rm = TRUE)
      }else if(input$loc.type2 == "19"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "19"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "19"], na.rm = TRUE)
      }else if(input$loc.type2 == "69"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "69"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "69"], na.rm = TRUE)
      }else if(input$loc.type2 == "29"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "29"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "29"], na.rm = TRUE)
      }else if(input$loc.type2 == "82"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "82"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "82"], na.rm = TRUE)
      }else if(input$loc.type2 == "17"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "17"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "17"], na.rm = TRUE)
      }else if(input$loc.type2 == "9"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "9"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "9"], na.rm = TRUE)
      }else if(input$loc.type2 == "26"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "26"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "26"], na.rm = TRUE)
      }else if(input$loc.type2 == "74"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "74"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "74"], na.rm = TRUE)
      }else if(input$loc.type2 == "45"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "45"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "45"], na.rm = TRUE)
      }else if(input$loc.type2 == "30"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "30"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "30"], na.rm = TRUE)
      }else if(input$loc.type2 == "27"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "27"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "27"], na.rm = TRUE)
      }else if(input$loc.type2 == "48"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "48"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "48"], na.rm = TRUE)
      }else if(input$loc.type2 == "67"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "67"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "67"], na.rm = TRUE)
      }else if(input$loc.type2 == "6"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "6"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "6"], na.rm = TRUE)
      }else if(input$loc.type2 == "12"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "12"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "12"], na.rm = TRUE)
      }else if(input$loc.type2 == "88"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "88"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "88"], na.rm = TRUE)
      }else if(input$loc.type2 == "131"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "131"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "131"], na.rm = TRUE)
      }else if(input$loc.type2 == "85"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "85"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "85"], na.rm = TRUE)
      }else if(input$loc.type2 == "89"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "89"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "89"], na.rm = TRUE)
      }else if(input$loc.type2 == "79"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "79"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "79"], na.rm = TRUE)
      }else if(input$loc.type2 == "91"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "91"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "91"], na.rm = TRUE)
      }else if(input$loc.type2 == "133"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "133"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "133"], na.rm = TRUE)
      }else if(input$loc.type2 == "81"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "81"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "81"], na.rm = TRUE)
      }else if(input$loc.type2 == "71"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "71"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "71"], na.rm = TRUE)
      }else if(input$loc.type2 == "86"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "86"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "86"], na.rm = TRUE)
      }else if(input$loc.type2 == "56"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "56"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "56"], na.rm = TRUE)
      }else if(input$loc.type2 == "59"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "59"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "59"], na.rm = TRUE)
      }else if(input$loc.type2 == "50"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "50"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "50"], na.rm = TRUE)
      }else if(input$loc.type2 == "61"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "61"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "61"], na.rm = TRUE)
      }else if(input$loc.type2 == "31"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "31"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "31"], na.rm = TRUE)
      }else if(input$loc.type2 == "138"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "138"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "138"], na.rm = TRUE)
      }else if(input$loc.type2 == "57"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "57"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "57"], na.rm = TRUE)
      }else if(input$loc.type2 == "60"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "60"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "60"], na.rm = TRUE)
      }else if(input$loc.type2 == "53"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "53"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "53"], na.rm = TRUE)
      }else if(input$loc.type2 == "32"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "32"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "32"], na.rm = TRUE)
      }else if(input$loc.type2 == "62"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "62"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "62"], na.rm = TRUE)
      }else if(input$loc.type2 == "66"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "66"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "66"], na.rm = TRUE)
      }else if(input$loc.type2 == "51"){
        ls2 <- mean(ls1$current_travel_time[ls1$voter_precinct == "51"], na.rm = TRUE)
        ls3 <- mean(ls1$free_flow_travel_time[ls1$voter_precinct == "51"], na.rm = TRUE)
      }
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
      if(input$loc.type2 == "SE"){
        cs2 <- mean(cs1$cat.average.num[cs1$quadrant == "SE"], na.rm = TRUE)
      }else if(input$loc.type2 == "SW"){
        cs2 <- mean(cs1$cat.average.num[cs1$quadrant == "SW"], na.rm = TRUE)
      }else if(input$loc.type2 == "NE"){
        cs2 <- mean(cs1$cat.average.num[cs1$quadrant == "NE"], na.rm = TRUE)
      }else if(input$loc.type2 == "NW"){
        cs2 <- mean(cs1$cat.average.num[cs1$quadrant == "NW"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Ward"){
      if(input$loc.type2 == "1"){
        cs2 <- mean(cs1$cat.average.num[cs1$ward == "1"], na.rm = TRUE)
      }else if(input$loc.type2 == "2"){
        cs2 <- mean(cs1$cat.average.num[cs1$ward == "2"], na.rm = TRUE)
      }else if(input$loc.type2 == "3"){
        cs2 <- mean(cs1$cat.average.num[cs1$ward == "3"], na.rm = TRUE)
      }else if(input$loc.type2 == "4"){
        cs2 <- mean(cs1$cat.average.num[cs1$ward == "4"], na.rm = TRUE)
      }else if(input$loc.type2 == "5"){
        cs2 <- mean(cs1$cat.average.num[cs1$ward == "5"], na.rm = TRUE)
      }else if(input$loc.type2 == "6"){
        cs2 <- mean(cs1$cat.average.num[cs1$ward == "6"], na.rm = TRUE)
      }else if(input$loc.type2 == "7"){
        cs2 <- mean(cs1$cat.average.num[cs1$ward == "7"], na.rm = TRUE)
      }else if(input$loc.type2 == "8"){
        cs2 <- mean(cs1$cat.average.num[cs1$ward == "8"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Zip Code"){
      if(input$loc.type2 == "20227"){
        cs2 <- mean(cs1$cat.average.num[cs1$zip_code == "20227"], na.rm = TRUE)
      }else if(input$loc.type2 == "20032"){
        cs2 <- mean(cs1$cat.average.num[cs1$zip_code == "20032"], na.rm = TRUE)
      }else if(input$loc.type2 == "20037"){
        cs2 <- mean(cs1$cat.average.num[cs1$zip_code == "20037"], na.rm = TRUE)
      }else if(input$loc.type2 == "20019"){
        cs2 <- mean(cs1$cat.average.num[cs1$zip_code == "20019"], na.rm = TRUE)
      }else if(input$loc.type2 == "20020"){
        cs2 <- mean(cs1$cat.average.num[cs1$zip_code == "20020"], na.rm = TRUE)
      }else if(input$loc.type2 == "20018"){
        cs2 <- mean(cs1$cat.average.num[cs1$zip_code == "20018"], na.rm = TRUE)
      }else if(input$loc.type2 == "20024"){
        cs2 <- mean(cs1$cat.average.num[cs1$zip_code == "20024"], na.rm = TRUE)
      }else if(input$loc.type2 == "20002"){
        cs2 <- mean(cs1$cat.average.num[cs1$zip_code == "20002"], na.rm = TRUE)
      }else if(input$loc.type2 == "20003"){
        cs2 <- mean(cs1$cat.average.num[cs1$zip_code == "20003"], na.rm = TRUE)
      }else if(input$loc.type2 == "20001"){
        cs2 <- mean(cs1$cat.average.num[cs1$zip_code == "20001"], na.rm = TRUE)
      }else if(input$loc.type2 == "20005"){
        cs2 <- mean(cs1$cat.average.num[cs1$zip_code == "20005"], na.rm = TRUE)
      }else if(input$loc.type2 == "20009"){
        cs2 <- mean(cs1$cat.average.num[cs1$zip_code == "20009"], na.rm = TRUE)
      }else if(input$loc.type2 == "20017"){
        cs2 <- mean(cs1$cat.average.num[cs1$zip_code == "20017"], na.rm = TRUE)
      }else if(input$loc.type2 == "20010"){
        cs2 <- mean(cs1$cat.average.num[cs1$zip_code == "20010"], na.rm = TRUE)
      }else if(input$loc.type2 == "20016"){
        cs2 <- mean(cs1$cat.average.num[cs1$zip_code == "20016"], na.rm = TRUE)
      }else if(input$loc.type2 == "20008"){
        cs2 <- mean(cs1$cat.average.num[cs1$zip_code == "20008"], na.rm = TRUE)
      }else if(input$loc.type2 == "20011"){
        cs2 <- mean(cs1$cat.average.num[cs1$zip_code == "20011"], na.rm = TRUE)
      }else if(input$loc.type2 == "20007"){
        cs2 <- mean(cs1$cat.average.num[cs1$zip_code == "20007"], na.rm = TRUE)
      }else if(input$loc.type2 == "20374"){
        cs2 <- mean(cs1$cat.average.num[cs1$zip_code == "20374"], na.rm = TRUE)
      }else if(input$loc.type2 == "20015"){
        cs2 <- mean(cs1$cat.average.num[cs1$zip_code == "20015"], na.rm = TRUE)
      }else if(input$loc.type2 == "20012"){
        cs2 <- mean(cs1$cat.average.num[cs1$zip_code == "20012"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Advisory Neighborhood Commission"){
      if(input$loc.type2 == "2A"){
        cs2 <- mean(cs1$cat.average.num[cs1$anc == "2A"], na.rm = TRUE)
      }else if(input$loc.type2 == "8D"){
        cs2 <- mean(cs1$cat.average.num[cs1$anc == "8D"], na.rm = TRUE)
      }else if(input$loc.type2 == "8E"){
        cs2 <- mean(cs1$cat.average.num[cs1$anc == "8E"], na.rm = TRUE)
      }else if(input$loc.type2 == "8C"){
        cs2 <- mean(cs1$cat.average.num[cs1$anc == "8C"], na.rm = TRUE)
      }else if(input$loc.type2 == "7F"){
        cs2 <- mean(cs1$cat.average.num[cs1$anc == "7F"], na.rm = TRUE)
      }else if(input$loc.type2 == "7C"){
        cs2 <- mean(cs1$cat.average.num[cs1$anc == "7C"], na.rm = TRUE)
      }else if(input$loc.type2 == "7E"){
        cs2 <- mean(cs1$cat.average.num[cs1$anc == "7E"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C"){
        cs2 <- mean(cs1$cat.average.num[cs1$anc == "5C"], na.rm = TRUE)
      }else if(input$loc.type2 == "6D"){
        cs2 <- mean(cs1$cat.average.num[cs1$anc == "6D"], na.rm = TRUE)
      }else if(input$loc.type2 == "5D"){
        cs2 <- mean(cs1$cat.average.num[cs1$anc == "5D"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B"){
        cs2 <- mean(cs1$cat.average.num[cs1$anc == "6B"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E"){
        cs2 <- mean(cs1$cat.average.num[cs1$anc == "5E"], na.rm = TRUE)
      }else if(input$loc.type2 == "2F"){
        cs2 <- mean(cs1$cat.average.num[cs1$anc == "2F"], na.rm = TRUE)
      }else if(input$loc.type2 == "2B"){
        cs2 <- mean(cs1$cat.average.num[cs1$anc == "2B"], na.rm = TRUE)
      }else if(input$loc.type2 == "1B"){
        cs2 <- mean(cs1$cat.average.num[cs1$anc == "1B"], na.rm = TRUE)
      }else if(input$loc.type2 == "6E"){
        cs2 <- mean(cs1$cat.average.num[cs1$anc == "6E"], na.rm = TRUE)
      }else if(input$loc.type2 == "5B"){
        cs2 <- mean(cs1$cat.average.num[cs1$anc == "5B"], na.rm = TRUE)
      }else if(input$loc.type2 == "1A"){
        cs2 <- mean(cs1$cat.average.num[cs1$anc == "1A"], na.rm = TRUE)
      }else if(input$loc.type2 == "3C"){
        cs2 <- mean(cs1$cat.average.num[cs1$anc == "3C"], na.rm = TRUE)
      }else if(input$loc.type2 == "6A"){
        cs2 <- mean(cs1$cat.average.num[cs1$anc == "6A"], na.rm = TRUE)
      }else if(input$loc.type2 == "3D"){
        cs2 <- mean(cs1$cat.average.num[cs1$anc == "3D"], na.rm = TRUE)
      }else if(input$loc.type2 == "4C"){
        cs2 <- mean(cs1$cat.average.num[cs1$anc == "4C"], na.rm = TRUE)
      }else if(input$loc.type2 == "3E"){
        cs2 <- mean(cs1$cat.average.num[cs1$anc == "3E"], na.rm = TRUE)
      }else if(input$loc.type2 == "2E"){
        cs2 <- mean(cs1$cat.average.num[cs1$anc == "2E"], na.rm = TRUE)
      }else if(input$loc.type2 == "6C"){
        cs2 <- mean(cs1$cat.average.num[cs1$anc == "6C"], na.rm = TRUE)
      }else if(input$loc.type2 == "8A"){
        cs2 <- mean(cs1$cat.average.num[cs1$anc == "8A"], na.rm = TRUE)
      }else if(input$loc.type2 == "4D"){
        cs2 <- mean(cs1$cat.average.num[cs1$anc == "4D"], na.rm = TRUE)
      }else if(input$loc.type2 == "4B"){
        cs2 <- mean(cs1$cat.average.num[cs1$anc == "4B"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A"){
        cs2 <- mean(cs1$cat.average.num[cs1$anc == "4A"], na.rm = TRUE)
      }else if(input$loc.type2 == "3G"){
        cs2 <- mean(cs1$cat.average.num[cs1$anc == "3G"], na.rm = TRUE)
      }else if(input$loc.type2 == "3F"){
        cs2 <- mean(cs1$cat.average.num[cs1$anc == "3F"], na.rm = TRUE)
      }else if(input$loc.type2 == "5A"){
        cs2 <- mean(cs1$cat.average.num[cs1$anc == "5A"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Census Tract"){
      if(input$loc.type2 == "006202"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "006202"], na.rm = TRUE)
      }else if(input$loc.type2 == "009811"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "009811"], na.rm = TRUE)
      }else if(input$loc.type2 == "009807"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "009807"], na.rm = TRUE)
      }else if(input$loc.type2 == "009700"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "009700"], na.rm = TRUE)
      }else if(input$loc.type2 == "009804"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "009804"], na.rm = TRUE)
      }else if(input$loc.type2 == "007304"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "007304"], na.rm = TRUE)
      }else if(input$loc.type2 == "007301"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "007301"], na.rm = TRUE)
      }else if(input$loc.type2 == "010400"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "010400"], na.rm = TRUE)
      }else if(input$loc.type2 == "010800"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "010800"], na.rm = TRUE)
      }else if(input$loc.type2 == "005600"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "005600"], na.rm = TRUE)
      }else if(input$loc.type2 == "009603"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "009603"], na.rm = TRUE)
      }else if(input$loc.type2 == "007809"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "007809"], na.rm = TRUE)
      }else if(input$loc.type2 == "009902"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "009902"], na.rm = TRUE)
      }else if(input$loc.type2 == "009000"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "009000"], na.rm = TRUE)
      }else if(input$loc.type2 == "009905"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "009905"], na.rm = TRUE)
      }else if(input$loc.type2 == "007707"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "007707"], na.rm = TRUE)
      }else if(input$loc.type2 == "007806"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "007806"], na.rm = TRUE)
      }else if(input$loc.type2 == "007804"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "007804"], na.rm = TRUE)
      }else if(input$loc.type2 == "007807"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "007807"], na.rm = TRUE)
      }else if(input$loc.type2 == "007708"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "007708"], na.rm = TRUE)
      }else if(input$loc.type2 == "010500"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "010500"], na.rm = TRUE)
      }else if(input$loc.type2 == "006400"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "006400"], na.rm = TRUE)
      }else if(input$loc.type2 == "008803"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "008803"], na.rm = TRUE)
      }else if(input$loc.type2 == "010200"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "010200"], na.rm = TRUE)
      }else if(input$loc.type2 == "006500"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "006500"], na.rm = TRUE)
      }else if(input$loc.type2 == "008702"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "008702"], na.rm = TRUE)
      }else if(input$loc.type2 == "003301"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "003301"], na.rm = TRUE)
      }else if(input$loc.type2 == "004902"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "004902"], na.rm = TRUE)
      }else if(input$loc.type2 == "004201"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "004201"], na.rm = TRUE)
      }else if(input$loc.type2 == "003400"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "003400"], na.rm = TRUE)
      }else if(input$loc.type2 == "004400"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "004400"], na.rm = TRUE)
      }else if(input$loc.type2 == "011100"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "011100"], na.rm = TRUE)
      }else if(input$loc.type2 == "004702"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "004702"], na.rm = TRUE)
      }else if(input$loc.type2 == "009302"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "009302"], na.rm = TRUE)
      }else if(input$loc.type2 == "008701"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "008701"], na.rm = TRUE)
      }else if(input$loc.type2 == "010100"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "010100"], na.rm = TRUE)
      }else if(input$loc.type2 == "002900"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "002900"], na.rm = TRUE)
      }else if(input$loc.type2 == "004600"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "004600"], na.rm = TRUE)
      }else if(input$loc.type2 == "009400"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "009400"], na.rm = TRUE)
      }else if(input$loc.type2 == "001002"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "001002"], na.rm = TRUE)
      }else if(input$loc.type2 == "010600"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "010600"], na.rm = TRUE)
      }else if(input$loc.type2 == "000901"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "000901"], na.rm = TRUE)
      }else if(input$loc.type2 == "000502"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "000502"], na.rm = TRUE)
      }else if(input$loc.type2 == "009102"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "009102"], na.rm = TRUE)
      }else if(input$loc.type2 == "009201"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "009201"], na.rm = TRUE)
      }else if(input$loc.type2 == "002400"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "002400"], na.rm = TRUE)
      }else if(input$loc.type2 == "001001"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "001001"], na.rm = TRUE)
      }else if(input$loc.type2 == "000600"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "000600"], na.rm = TRUE)
      }else if(input$loc.type2 == "002501"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "002501"], na.rm = TRUE)
      }else if(input$loc.type2 == "009503"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "009503"], na.rm = TRUE)
      }else if(input$loc.type2 == "000300"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "000300"], na.rm = TRUE)
      }else if(input$loc.type2 == "000400"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "000400"], na.rm = TRUE)
      }else if(input$loc.type2 == "006700"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "006700"], na.rm = TRUE)
      }else if(input$loc.type2 == "007200"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "007200"], na.rm = TRUE)
      }else if(input$loc.type2 == "008302"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "008302"], na.rm = TRUE)
      }else if(input$loc.type2 == "006600"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "006600"], na.rm = TRUE)
      }else if(input$loc.type2 == "008904"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "008904"], na.rm = TRUE)
      }else if(input$loc.type2 == "008100"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "008100"], na.rm = TRUE)
      }else if(input$loc.type2 == "006802"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "006802"], na.rm = TRUE)
      }else if(input$loc.type2 == "007601"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "007601"], na.rm = TRUE)
      }else if(input$loc.type2 == "007100"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "007100"], na.rm = TRUE)
      }else if(input$loc.type2 == "008200"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "008200"], na.rm = TRUE)
      }else if(input$loc.type2 == "008001"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "008001"], na.rm = TRUE)
      }else if(input$loc.type2 == "008002"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "008002"], na.rm = TRUE)
      }else if(input$loc.type2 == "002101"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "002101"], na.rm = TRUE)
      }else if(input$loc.type2 == "001901"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "001901"], na.rm = TRUE)
      }else if(input$loc.type2 == "002600"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "002600"], na.rm = TRUE)
      }else if(input$loc.type2 == "001401"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "001401"], na.rm = TRUE)
      }else if(input$loc.type2 == "001803"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "001803"], na.rm = TRUE)
      }else if(input$loc.type2 == "001301"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "001301"], na.rm = TRUE)
      }else if(input$loc.type2 == "002102"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "002102"], na.rm = TRUE)
      }else if(input$loc.type2 == "001804"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "001804"], na.rm = TRUE)
      }else if(input$loc.type2 == "002001"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "002001"], na.rm = TRUE)
      }else if(input$loc.type2 == "001902"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "001902"], na.rm = TRUE)
      }else if(input$loc.type2 == "001100"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "001100"], na.rm = TRUE)
      }else if(input$loc.type2 == "001600"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "001600"], na.rm = TRUE)
      }else if(input$loc.type2 == "009509"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "009509"], na.rm = TRUE)
      }else if(input$loc.type2 == "001500"){
        cs2 <- mean(cs1$cat.average.num[cs1$census_tract == "001500"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Single Member District"){
      if(input$loc.type2 == "2A01"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "2A01"], na.rm = TRUE)
      }else if(input$loc.type2 == "8D03"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "8D03"], na.rm = TRUE)
      }else if(input$loc.type2 == "8D01"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "8D01"], na.rm = TRUE)
      }else if(input$loc.type2 == "8D06"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "8D06"], na.rm = TRUE)
      }else if(input$loc.type2 == "8E05"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "8E05"], na.rm = TRUE)
      }else if(input$loc.type2 == "8C07"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "8C07"], na.rm = TRUE)
      }else if(input$loc.type2 == "8E04"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "8E04"], na.rm = TRUE)
      }else if(input$loc.type2 == "8C05"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "8C05"], na.rm = TRUE)
      }else if(input$loc.type2 == "8C03"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "8C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "2A07"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "2A07"], na.rm = TRUE)
      }else if(input$loc.type2 == "2A03"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "2A03"], na.rm = TRUE)
      }else if(input$loc.type2 == "7F01"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "7F01"], na.rm = TRUE)
      }else if(input$loc.type2 == "7C04"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "7C04"], na.rm = TRUE)
      }else if(input$loc.type2 == "7E02"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "7E02"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C03"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "5C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "7E06"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "7E06"], na.rm = TRUE)
      }else if(input$loc.type2 == "7E01"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "7E01"], na.rm = TRUE)
      }else if(input$loc.type2 == "7C07"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "7C07"], na.rm = TRUE)
      }else if(input$loc.type2 == "7C03"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "7C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "7C06"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "7C06"], na.rm = TRUE)
      }else if(input$loc.type2 == "7F06"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "7F06"], na.rm = TRUE)
      }else if(input$loc.type2 == "6D03"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "6D03"], na.rm = TRUE)
      }else if(input$loc.type2 == "6D06"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "6D06"], na.rm = TRUE)
      }else if(input$loc.type2 == "5D01"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "5D01"], na.rm = TRUE)
      }else if(input$loc.type2 == "6D05"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "6D05"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B01"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "6B01"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E03"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "5E03"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E08"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "5E08"], na.rm = TRUE)
      }else if(input$loc.type2 == "2F06"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "2F06"], na.rm = TRUE)
      }else if(input$loc.type2 == "2B09"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "2B09"], na.rm = TRUE)
      }else if(input$loc.type2 == "1B01"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "1B01"], na.rm = TRUE)
      }else if(input$loc.type2 == "1B02"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "1B02"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C02"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "5C02"], na.rm = TRUE)
      }else if(input$loc.type2 == "6E07"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "6E07"], na.rm = TRUE)
      }else if(input$loc.type2 == "5B03"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "5B03"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E04"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "5E04"], na.rm = TRUE)
      }else if(input$loc.type2 == "2F08"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "2F08"], na.rm = TRUE)
      }else if(input$loc.type2 == "1A04"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "1A04"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E05"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "5E05"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C01"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "5C01"], na.rm = TRUE)
      }else if(input$loc.type2 == "3C06"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "3C06"], na.rm = TRUE)
      }else if(input$loc.type2 == "6A01"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "6A01"], na.rm = TRUE)
      }else if(input$loc.type2 == "2B05"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "2B05"], na.rm = TRUE)
      }else if(input$loc.type2 == "3D02"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "3D02"], na.rm = TRUE)
      }else if(input$loc.type2 == "3C03"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "3C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "5B01"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "5B01"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C06"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "5C06"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E01"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "5E01"], na.rm = TRUE)
      }else if(input$loc.type2 == "4C08"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "4C08"], na.rm = TRUE)
      }else if(input$loc.type2 == "3E05"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "3E05"], na.rm = TRUE)
      }else if(input$loc.type2 == "3C09"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "3C09"], na.rm = TRUE)
      }else if(input$loc.type2 == "4C03"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "4C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "2E01"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "2E01"], na.rm = TRUE)
      }else if(input$loc.type2 == "3C08"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "3C08"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B05"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "6B05"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C04"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "5C04"], na.rm = TRUE)
      }else if(input$loc.type2 == "6D07"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "6D07"], na.rm = TRUE)
      }else if(input$loc.type2 == "6C03"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "6C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B02"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "6B02"], na.rm = TRUE)
      }else if(input$loc.type2 == "5D05"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "5D05"], na.rm = TRUE)
      }else if(input$loc.type2 == "6A03"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "6A03"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B09"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "6B09"], na.rm = TRUE)
      }else if(input$loc.type2 == "8A03"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "8A03"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B07"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "6B07"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B06"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "6B06"], na.rm = TRUE)
      }else if(input$loc.type2 == "6A02"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "6A02"], na.rm = TRUE)
      }else if(input$loc.type2 == "6A08"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "6A08"], na.rm = TRUE)
      }else if(input$loc.type2 == "4D03"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "4D03"], na.rm = TRUE)
      }else if(input$loc.type2 == "4B04"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "4B04"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A08"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "4A08"], na.rm = TRUE)
      }else if(input$loc.type2 == "3D03"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "3D03"], na.rm = TRUE)
      }else if(input$loc.type2 == "3G05"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "3G05"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A07"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "4A07"], na.rm = TRUE)
      }else if(input$loc.type2 == "3E02"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "3E02"], na.rm = TRUE)
      }else if(input$loc.type2 == "3F03"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "3F03"], na.rm = TRUE)
      }else if(input$loc.type2 == "4B06"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "4B06"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A04"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "4A04"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A06"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "4A06"], na.rm = TRUE)
      }else if(input$loc.type2 == "3E04"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "3E04"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A02"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "4A02"], na.rm = TRUE)
      }else if(input$loc.type2 == "5A08"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "5A08"], na.rm = TRUE)
      }else if(input$loc.type2 == "3G04"){
        cs2 <- mean(cs1$cat.average.num[cs1$single_member_district == "3G04"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Voter Precinct"){
      if(input$loc.type2 == "129"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "129"], na.rm = TRUE)
      }else if(input$loc.type2 == "125"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "125"], na.rm = TRUE)
      }else if(input$loc.type2 == "126"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "126"], na.rm = TRUE)
      }else if(input$loc.type2 == "121"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "121"], na.rm = TRUE)
      }else if(input$loc.type2 == "122"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "122"], na.rm = TRUE)
      }else if(input$loc.type2 == "120"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "120"], na.rm = TRUE)
      }else if(input$loc.type2 == "123"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "123"], na.rm = TRUE)
      }else if(input$loc.type2 == "2"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "2"], na.rm = TRUE)
      }else if(input$loc.type2 == "3"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "3"], na.rm = TRUE)
      }else if(input$loc.type2 == "102"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "102"], na.rm = TRUE)
      }else if(input$loc.type2 == "94"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "94"], na.rm = TRUE)
      }else if(input$loc.type2 == "110"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "110"], na.rm = TRUE)
      }else if(input$loc.type2 == "139"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "139"], na.rm = TRUE)
      }else if(input$loc.type2 == "105"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "105"], na.rm = TRUE)
      }else if(input$loc.type2 == "106"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "106"], na.rm = TRUE)
      }else if(input$loc.type2 == "93"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "93"], na.rm = TRUE)
      }else if(input$loc.type2 == "97"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "97"], na.rm = TRUE)
      }else if(input$loc.type2 == "95"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "95"], na.rm = TRUE)
      }else if(input$loc.type2 == "132"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "132"], na.rm = TRUE)
      }else if(input$loc.type2 == "128"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "128"], na.rm = TRUE)
      }else if(input$loc.type2 == "127"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "127"], na.rm = TRUE)
      }else if(input$loc.type2 == "76"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "76"], na.rm = TRUE)
      }else if(input$loc.type2 == "130"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "130"], na.rm = TRUE)
      }else if(input$loc.type2 == "75"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "75"], na.rm = TRUE)
      }else if(input$loc.type2 == "135"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "135"], na.rm = TRUE)
      }else if(input$loc.type2 == "141"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "141"], na.rm = TRUE)
      }else if(input$loc.type2 == "20"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "20"], na.rm = TRUE)
      }else if(input$loc.type2 == "22"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "22"], na.rm = TRUE)
      }else if(input$loc.type2 == "72"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "72"], na.rm = TRUE)
      }else if(input$loc.type2 == "1"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "1"], na.rm = TRUE)
      }else if(input$loc.type2 == "73"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "73"], na.rm = TRUE)
      }else if(input$loc.type2 == "42"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "42"], na.rm = TRUE)
      }else if(input$loc.type2 == "19"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "19"], na.rm = TRUE)
      }else if(input$loc.type2 == "69"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "69"], na.rm = TRUE)
      }else if(input$loc.type2 == "29"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "29"], na.rm = TRUE)
      }else if(input$loc.type2 == "82"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "82"], na.rm = TRUE)
      }else if(input$loc.type2 == "17"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "17"], na.rm = TRUE)
      }else if(input$loc.type2 == "9"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "9"], na.rm = TRUE)
      }else if(input$loc.type2 == "26"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "26"], na.rm = TRUE)
      }else if(input$loc.type2 == "74"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "74"], na.rm = TRUE)
      }else if(input$loc.type2 == "45"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "45"], na.rm = TRUE)
      }else if(input$loc.type2 == "30"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "30"], na.rm = TRUE)
      }else if(input$loc.type2 == "27"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "27"], na.rm = TRUE)
      }else if(input$loc.type2 == "48"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "48"], na.rm = TRUE)
      }else if(input$loc.type2 == "67"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "67"], na.rm = TRUE)
      }else if(input$loc.type2 == "6"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "6"], na.rm = TRUE)
      }else if(input$loc.type2 == "12"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "12"], na.rm = TRUE)
      }else if(input$loc.type2 == "88"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "88"], na.rm = TRUE)
      }else if(input$loc.type2 == "131"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "131"], na.rm = TRUE)
      }else if(input$loc.type2 == "85"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "85"], na.rm = TRUE)
      }else if(input$loc.type2 == "89"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "89"], na.rm = TRUE)
      }else if(input$loc.type2 == "79"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "79"], na.rm = TRUE)
      }else if(input$loc.type2 == "91"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "91"], na.rm = TRUE)
      }else if(input$loc.type2 == "133"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "133"], na.rm = TRUE)
      }else if(input$loc.type2 == "81"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "81"], na.rm = TRUE)
      }else if(input$loc.type2 == "71"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "71"], na.rm = TRUE)
      }else if(input$loc.type2 == "86"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "86"], na.rm = TRUE)
      }else if(input$loc.type2 == "56"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "56"], na.rm = TRUE)
      }else if(input$loc.type2 == "59"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "59"], na.rm = TRUE)
      }else if(input$loc.type2 == "50"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "50"], na.rm = TRUE)
      }else if(input$loc.type2 == "61"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "61"], na.rm = TRUE)
      }else if(input$loc.type2 == "31"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "31"], na.rm = TRUE)
      }else if(input$loc.type2 == "138"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "138"], na.rm = TRUE)
      }else if(input$loc.type2 == "57"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "57"], na.rm = TRUE)
      }else if(input$loc.type2 == "60"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "60"], na.rm = TRUE)
      }else if(input$loc.type2 == "53"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "53"], na.rm = TRUE)
      }else if(input$loc.type2 == "32"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "32"], na.rm = TRUE)
      }else if(input$loc.type2 == "62"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "62"], na.rm = TRUE)
      }else if(input$loc.type2 == "66"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "66"], na.rm = TRUE)
      }else if(input$loc.type2 == "51"){
        cs2 <- mean(cs1$cat.average.num[cs1$voter_precinct == "51"], na.rm = TRUE)
      }
    }
    
    if(input$loc.type == "Quadrant"){
      if(input$loc.type2 == "SE"){
        ls2 <- mean(ls1$cat.average.num[ls1$quadrant == "SE"], na.rm = TRUE)
      }else if(input$loc.type2 == "SW"){
        ls2 <- mean(ls1$cat.average.num[ls1$quadrant == "SW"], na.rm = TRUE)
      }else if(input$loc.type2 == "NE"){
        ls2 <- mean(ls1$cat.average.num[ls1$quadrant == "NE"], na.rm = TRUE)
      }else if(input$loc.type2 == "NW"){
        ls2 <- mean(ls1$cat.average.num[ls1$quadrant == "NW"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Ward"){
      if(input$loc.type2 == "1"){
        ls2 <- mean(ls1$cat.average.num[ls1$ward == "1"], na.rm = TRUE)
      }else if(input$loc.type2 == "2"){
        ls2 <- mean(ls1$cat.average.num[ls1$ward == "2"], na.rm = TRUE)
      }else if(input$loc.type2 == "3"){
        ls2 <- mean(ls1$cat.average.num[ls1$ward == "3"], na.rm = TRUE)
      }else if(input$loc.type2 == "4"){
        ls2 <- mean(ls1$cat.average.num[ls1$ward == "4"], na.rm = TRUE)
      }else if(input$loc.type2 == "5"){
        ls2 <- mean(ls1$cat.average.num[ls1$ward == "5"], na.rm = TRUE)
      }else if(input$loc.type2 == "6"){
        ls2 <- mean(ls1$cat.average.num[ls1$ward == "6"], na.rm = TRUE)
      }else if(input$loc.type2 == "7"){
        ls2 <- mean(ls1$cat.average.num[ls1$ward == "7"], na.rm = TRUE)
      }else if(input$loc.type2 == "8"){
        ls2 <- mean(ls1$cat.average.num[ls1$ward == "8"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Zip Code"){
      if(input$loc.type2 == "20227"){
        ls2 <- mean(ls1$cat.average.num[ls1$zip_code == "20227"], na.rm = TRUE)
      }else if(input$loc.type2 == "20032"){
        ls2 <- mean(ls1$cat.average.num[ls1$zip_code == "20032"], na.rm = TRUE)
      }else if(input$loc.type2 == "20037"){
        ls2 <- mean(ls1$cat.average.num[ls1$zip_code == "20037"], na.rm = TRUE)
      }else if(input$loc.type2 == "20019"){
        ls2 <- mean(ls1$cat.average.num[ls1$zip_code == "20019"], na.rm = TRUE)
      }else if(input$loc.type2 == "20020"){
        ls2 <- mean(ls1$cat.average.num[ls1$zip_code == "20020"], na.rm = TRUE)
      }else if(input$loc.type2 == "20018"){
        ls2 <- mean(ls1$cat.average.num[ls1$zip_code == "20018"], na.rm = TRUE)
      }else if(input$loc.type2 == "20024"){
        ls2 <- mean(ls1$cat.average.num[ls1$zip_code == "20024"], na.rm = TRUE)
      }else if(input$loc.type2 == "20002"){
        ls2 <- mean(ls1$cat.average.num[ls1$zip_code == "20002"], na.rm = TRUE)
      }else if(input$loc.type2 == "20003"){
        ls2 <- mean(ls1$cat.average.num[ls1$zip_code == "20003"], na.rm = TRUE)
      }else if(input$loc.type2 == "20001"){
        ls2 <- mean(ls1$cat.average.num[ls1$zip_code == "20001"], na.rm = TRUE)
      }else if(input$loc.type2 == "20005"){
        ls2 <- mean(ls1$cat.average.num[ls1$zip_code == "20005"], na.rm = TRUE)
      }else if(input$loc.type2 == "20009"){
        ls2 <- mean(ls1$cat.average.num[ls1$zip_code == "20009"], na.rm = TRUE)
      }else if(input$loc.type2 == "20017"){
        ls2 <- mean(ls1$cat.average.num[ls1$zip_code == "20017"], na.rm = TRUE)
      }else if(input$loc.type2 == "20010"){
        ls2 <- mean(ls1$cat.average.num[ls1$zip_code == "20010"], na.rm = TRUE)
      }else if(input$loc.type2 == "20016"){
        ls2 <- mean(ls1$cat.average.num[ls1$zip_code == "20016"], na.rm = TRUE)
      }else if(input$loc.type2 == "20008"){
        ls2 <- mean(ls1$cat.average.num[ls1$zip_code == "20008"], na.rm = TRUE)
      }else if(input$loc.type2 == "20011"){
        ls2 <- mean(ls1$cat.average.num[ls1$zip_code == "20011"], na.rm = TRUE)
      }else if(input$loc.type2 == "20007"){
        ls2 <- mean(ls1$cat.average.num[ls1$zip_code == "20007"], na.rm = TRUE)
      }else if(input$loc.type2 == "20374"){
        ls2 <- mean(ls1$cat.average.num[ls1$zip_code == "20374"], na.rm = TRUE)
      }else if(input$loc.type2 == "20015"){
        ls2 <- mean(ls1$cat.average.num[ls1$zip_code == "20015"], na.rm = TRUE)
      }else if(input$loc.type2 == "20012"){
        ls2 <- mean(ls1$cat.average.num[ls1$zip_code == "20012"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Advisory Neighborhood Commission"){
      if(input$loc.type2 == "2A"){
        ls2 <- mean(ls1$cat.average.num[ls1$anc == "2A"], na.rm = TRUE)
      }else if(input$loc.type2 == "8D"){
        ls2 <- mean(ls1$cat.average.num[ls1$anc == "8D"], na.rm = TRUE)
      }else if(input$loc.type2 == "8E"){
        ls2 <- mean(ls1$cat.average.num[ls1$anc == "8E"], na.rm = TRUE)
      }else if(input$loc.type2 == "8C"){
        ls2 <- mean(ls1$cat.average.num[ls1$anc == "8C"], na.rm = TRUE)
      }else if(input$loc.type2 == "7F"){
        ls2 <- mean(ls1$cat.average.num[ls1$anc == "7F"], na.rm = TRUE)
      }else if(input$loc.type2 == "7C"){
        ls2 <- mean(ls1$cat.average.num[ls1$anc == "7C"], na.rm = TRUE)
      }else if(input$loc.type2 == "7E"){
        ls2 <- mean(ls1$cat.average.num[ls1$anc == "7E"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C"){
        ls2 <- mean(ls1$cat.average.num[ls1$anc == "5C"], na.rm = TRUE)
      }else if(input$loc.type2 == "6D"){
        ls2 <- mean(ls1$cat.average.num[ls1$anc == "6D"], na.rm = TRUE)
      }else if(input$loc.type2 == "5D"){
        ls2 <- mean(ls1$cat.average.num[ls1$anc == "5D"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B"){
        ls2 <- mean(ls1$cat.average.num[ls1$anc == "6B"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E"){
        ls2 <- mean(ls1$cat.average.num[ls1$anc == "5E"], na.rm = TRUE)
      }else if(input$loc.type2 == "2F"){
        ls2 <- mean(ls1$cat.average.num[ls1$anc == "2F"], na.rm = TRUE)
      }else if(input$loc.type2 == "2B"){
        ls2 <- mean(ls1$cat.average.num[ls1$anc == "2B"], na.rm = TRUE)
      }else if(input$loc.type2 == "1B"){
        ls2 <- mean(ls1$cat.average.num[ls1$anc == "1B"], na.rm = TRUE)
      }else if(input$loc.type2 == "6E"){
        ls2 <- mean(ls1$cat.average.num[ls1$anc == "6E"], na.rm = TRUE)
      }else if(input$loc.type2 == "5B"){
        ls2 <- mean(ls1$cat.average.num[ls1$anc == "5B"], na.rm = TRUE)
      }else if(input$loc.type2 == "1A"){
        ls2 <- mean(ls1$cat.average.num[ls1$anc == "1A"], na.rm = TRUE)
      }else if(input$loc.type2 == "3C"){
        ls2 <- mean(ls1$cat.average.num[ls1$anc == "3C"], na.rm = TRUE)
      }else if(input$loc.type2 == "6A"){
        ls2 <- mean(ls1$cat.average.num[ls1$anc == "6A"], na.rm = TRUE)
      }else if(input$loc.type2 == "3D"){
        ls2 <- mean(ls1$cat.average.num[ls1$anc == "3D"], na.rm = TRUE)
      }else if(input$loc.type2 == "4C"){
        ls2 <- mean(ls1$cat.average.num[ls1$anc == "4C"], na.rm = TRUE)
      }else if(input$loc.type2 == "3E"){
        ls2 <- mean(ls1$cat.average.num[ls1$anc == "3E"], na.rm = TRUE)
      }else if(input$loc.type2 == "2E"){
        ls2 <- mean(ls1$cat.average.num[ls1$anc == "2E"], na.rm = TRUE)
      }else if(input$loc.type2 == "6C"){
        ls2 <- mean(ls1$cat.average.num[ls1$anc == "6C"], na.rm = TRUE)
      }else if(input$loc.type2 == "8A"){
        ls2 <- mean(ls1$cat.average.num[ls1$anc == "8A"], na.rm = TRUE)
      }else if(input$loc.type2 == "4D"){
        ls2 <- mean(ls1$cat.average.num[ls1$anc == "4D"], na.rm = TRUE)
      }else if(input$loc.type2 == "4B"){
        ls2 <- mean(ls1$cat.average.num[ls1$anc == "4B"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A"){
        ls2 <- mean(ls1$cat.average.num[ls1$anc == "4A"], na.rm = TRUE)
      }else if(input$loc.type2 == "3G"){
        ls2 <- mean(ls1$cat.average.num[ls1$anc == "3G"], na.rm = TRUE)
      }else if(input$loc.type2 == "3F"){
        ls2 <- mean(ls1$cat.average.num[ls1$anc == "3F"], na.rm = TRUE)
      }else if(input$loc.type2 == "5A"){
        ls2 <- mean(ls1$cat.average.num[ls1$anc == "5A"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Census Tract"){
      if(input$loc.type2 == "006202"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "006202"], na.rm = TRUE)
      }else if(input$loc.type2 == "009811"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "009811"], na.rm = TRUE)
      }else if(input$loc.type2 == "009807"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "009807"], na.rm = TRUE)
      }else if(input$loc.type2 == "009700"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "009700"], na.rm = TRUE)
      }else if(input$loc.type2 == "009804"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "009804"], na.rm = TRUE)
      }else if(input$loc.type2 == "007304"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "007304"], na.rm = TRUE)
      }else if(input$loc.type2 == "007301"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "007301"], na.rm = TRUE)
      }else if(input$loc.type2 == "010400"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "010400"], na.rm = TRUE)
      }else if(input$loc.type2 == "010800"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "010800"], na.rm = TRUE)
      }else if(input$loc.type2 == "005600"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "005600"], na.rm = TRUE)
      }else if(input$loc.type2 == "009603"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "009603"], na.rm = TRUE)
      }else if(input$loc.type2 == "007809"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "007809"], na.rm = TRUE)
      }else if(input$loc.type2 == "009902"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "009902"], na.rm = TRUE)
      }else if(input$loc.type2 == "009000"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "009000"], na.rm = TRUE)
      }else if(input$loc.type2 == "009905"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "009905"], na.rm = TRUE)
      }else if(input$loc.type2 == "007707"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "007707"], na.rm = TRUE)
      }else if(input$loc.type2 == "007806"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "007806"], na.rm = TRUE)
      }else if(input$loc.type2 == "007804"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "007804"], na.rm = TRUE)
      }else if(input$loc.type2 == "007807"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "007807"], na.rm = TRUE)
      }else if(input$loc.type2 == "007708"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "007708"], na.rm = TRUE)
      }else if(input$loc.type2 == "010500"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "010500"], na.rm = TRUE)
      }else if(input$loc.type2 == "006400"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "006400"], na.rm = TRUE)
      }else if(input$loc.type2 == "008803"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "008803"], na.rm = TRUE)
      }else if(input$loc.type2 == "010200"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "010200"], na.rm = TRUE)
      }else if(input$loc.type2 == "006500"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "006500"], na.rm = TRUE)
      }else if(input$loc.type2 == "008702"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "008702"], na.rm = TRUE)
      }else if(input$loc.type2 == "003301"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "003301"], na.rm = TRUE)
      }else if(input$loc.type2 == "004902"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "004902"], na.rm = TRUE)
      }else if(input$loc.type2 == "004201"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "004201"], na.rm = TRUE)
      }else if(input$loc.type2 == "003400"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "003400"], na.rm = TRUE)
      }else if(input$loc.type2 == "004400"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "004400"], na.rm = TRUE)
      }else if(input$loc.type2 == "011100"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "011100"], na.rm = TRUE)
      }else if(input$loc.type2 == "004702"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "004702"], na.rm = TRUE)
      }else if(input$loc.type2 == "009302"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "009302"], na.rm = TRUE)
      }else if(input$loc.type2 == "008701"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "008701"], na.rm = TRUE)
      }else if(input$loc.type2 == "010100"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "010100"], na.rm = TRUE)
      }else if(input$loc.type2 == "002900"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "002900"], na.rm = TRUE)
      }else if(input$loc.type2 == "004600"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "004600"], na.rm = TRUE)
      }else if(input$loc.type2 == "009400"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "009400"], na.rm = TRUE)
      }else if(input$loc.type2 == "001002"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "001002"], na.rm = TRUE)
      }else if(input$loc.type2 == "010600"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "010600"], na.rm = TRUE)
      }else if(input$loc.type2 == "000901"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "000901"], na.rm = TRUE)
      }else if(input$loc.type2 == "000502"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "000502"], na.rm = TRUE)
      }else if(input$loc.type2 == "009102"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "009102"], na.rm = TRUE)
      }else if(input$loc.type2 == "009201"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "009201"], na.rm = TRUE)
      }else if(input$loc.type2 == "002400"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "002400"], na.rm = TRUE)
      }else if(input$loc.type2 == "001001"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "001001"], na.rm = TRUE)
      }else if(input$loc.type2 == "000600"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "000600"], na.rm = TRUE)
      }else if(input$loc.type2 == "002501"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "002501"], na.rm = TRUE)
      }else if(input$loc.type2 == "009503"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "009503"], na.rm = TRUE)
      }else if(input$loc.type2 == "000300"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "000300"], na.rm = TRUE)
      }else if(input$loc.type2 == "000400"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "000400"], na.rm = TRUE)
      }else if(input$loc.type2 == "006700"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "006700"], na.rm = TRUE)
      }else if(input$loc.type2 == "007200"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "007200"], na.rm = TRUE)
      }else if(input$loc.type2 == "008302"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "008302"], na.rm = TRUE)
      }else if(input$loc.type2 == "006600"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "006600"], na.rm = TRUE)
      }else if(input$loc.type2 == "008904"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "008904"], na.rm = TRUE)
      }else if(input$loc.type2 == "008100"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "008100"], na.rm = TRUE)
      }else if(input$loc.type2 == "006802"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "006802"], na.rm = TRUE)
      }else if(input$loc.type2 == "007601"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "007601"], na.rm = TRUE)
      }else if(input$loc.type2 == "007100"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "007100"], na.rm = TRUE)
      }else if(input$loc.type2 == "008200"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "008200"], na.rm = TRUE)
      }else if(input$loc.type2 == "008001"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "008001"], na.rm = TRUE)
      }else if(input$loc.type2 == "008002"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "008002"], na.rm = TRUE)
      }else if(input$loc.type2 == "002101"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "002101"], na.rm = TRUE)
      }else if(input$loc.type2 == "001901"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "001901"], na.rm = TRUE)
      }else if(input$loc.type2 == "002600"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "002600"], na.rm = TRUE)
      }else if(input$loc.type2 == "001401"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "001401"], na.rm = TRUE)
      }else if(input$loc.type2 == "001803"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "001803"], na.rm = TRUE)
      }else if(input$loc.type2 == "001301"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "001301"], na.rm = TRUE)
      }else if(input$loc.type2 == "002102"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "002102"], na.rm = TRUE)
      }else if(input$loc.type2 == "001804"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "001804"], na.rm = TRUE)
      }else if(input$loc.type2 == "002001"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "002001"], na.rm = TRUE)
      }else if(input$loc.type2 == "001902"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "001902"], na.rm = TRUE)
      }else if(input$loc.type2 == "001100"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "001100"], na.rm = TRUE)
      }else if(input$loc.type2 == "001600"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "001600"], na.rm = TRUE)
      }else if(input$loc.type2 == "009509"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "009509"], na.rm = TRUE)
      }else if(input$loc.type2 == "001500"){
        ls2 <- mean(ls1$cat.average.num[ls1$census_tract == "001500"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Single Member District"){
      if(input$loc.type2 == "2A01"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "2A01"], na.rm = TRUE)
      }else if(input$loc.type2 == "8D03"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "8D03"], na.rm = TRUE)
      }else if(input$loc.type2 == "8D01"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "8D01"], na.rm = TRUE)
      }else if(input$loc.type2 == "8D06"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "8D06"], na.rm = TRUE)
      }else if(input$loc.type2 == "8E05"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "8E05"], na.rm = TRUE)
      }else if(input$loc.type2 == "8C07"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "8C07"], na.rm = TRUE)
      }else if(input$loc.type2 == "8E04"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "8E04"], na.rm = TRUE)
      }else if(input$loc.type2 == "8C05"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "8C05"], na.rm = TRUE)
      }else if(input$loc.type2 == "8C03"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "8C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "2A07"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "2A07"], na.rm = TRUE)
      }else if(input$loc.type2 == "2A03"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "2A03"], na.rm = TRUE)
      }else if(input$loc.type2 == "7F01"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "7F01"], na.rm = TRUE)
      }else if(input$loc.type2 == "7C04"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "7C04"], na.rm = TRUE)
      }else if(input$loc.type2 == "7E02"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "7E02"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C03"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "5C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "7E06"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "7E06"], na.rm = TRUE)
      }else if(input$loc.type2 == "7E01"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "7E01"], na.rm = TRUE)
      }else if(input$loc.type2 == "7C07"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "7C07"], na.rm = TRUE)
      }else if(input$loc.type2 == "7C03"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "7C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "7C06"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "7C06"], na.rm = TRUE)
      }else if(input$loc.type2 == "7F06"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "7F06"], na.rm = TRUE)
      }else if(input$loc.type2 == "6D03"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "6D03"], na.rm = TRUE)
      }else if(input$loc.type2 == "6D06"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "6D06"], na.rm = TRUE)
      }else if(input$loc.type2 == "5D01"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "5D01"], na.rm = TRUE)
      }else if(input$loc.type2 == "6D05"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "6D05"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B01"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "6B01"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E03"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "5E03"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E08"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "5E08"], na.rm = TRUE)
      }else if(input$loc.type2 == "2F06"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "2F06"], na.rm = TRUE)
      }else if(input$loc.type2 == "2B09"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "2B09"], na.rm = TRUE)
      }else if(input$loc.type2 == "1B01"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "1B01"], na.rm = TRUE)
      }else if(input$loc.type2 == "1B02"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "1B02"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C02"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "5C02"], na.rm = TRUE)
      }else if(input$loc.type2 == "6E07"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "6E07"], na.rm = TRUE)
      }else if(input$loc.type2 == "5B03"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "5B03"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E04"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "5E04"], na.rm = TRUE)
      }else if(input$loc.type2 == "2F08"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "2F08"], na.rm = TRUE)
      }else if(input$loc.type2 == "1A04"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "1A04"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E05"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "5E05"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C01"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "5C01"], na.rm = TRUE)
      }else if(input$loc.type2 == "3C06"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "3C06"], na.rm = TRUE)
      }else if(input$loc.type2 == "6A01"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "6A01"], na.rm = TRUE)
      }else if(input$loc.type2 == "2B05"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "2B05"], na.rm = TRUE)
      }else if(input$loc.type2 == "3D02"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "3D02"], na.rm = TRUE)
      }else if(input$loc.type2 == "3C03"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "3C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "5B01"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "5B01"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C06"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "5C06"], na.rm = TRUE)
      }else if(input$loc.type2 == "5E01"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "5E01"], na.rm = TRUE)
      }else if(input$loc.type2 == "4C08"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "4C08"], na.rm = TRUE)
      }else if(input$loc.type2 == "3E05"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "3E05"], na.rm = TRUE)
      }else if(input$loc.type2 == "3C09"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "3C09"], na.rm = TRUE)
      }else if(input$loc.type2 == "4C03"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "4C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "2E01"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "2E01"], na.rm = TRUE)
      }else if(input$loc.type2 == "3C08"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "3C08"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B05"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "6B05"], na.rm = TRUE)
      }else if(input$loc.type2 == "5C04"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "5C04"], na.rm = TRUE)
      }else if(input$loc.type2 == "6D07"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "6D07"], na.rm = TRUE)
      }else if(input$loc.type2 == "6C03"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "6C03"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B02"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "6B02"], na.rm = TRUE)
      }else if(input$loc.type2 == "5D05"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "5D05"], na.rm = TRUE)
      }else if(input$loc.type2 == "6A03"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "6A03"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B09"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "6B09"], na.rm = TRUE)
      }else if(input$loc.type2 == "8A03"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "8A03"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B07"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "6B07"], na.rm = TRUE)
      }else if(input$loc.type2 == "6B06"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "6B06"], na.rm = TRUE)
      }else if(input$loc.type2 == "6A02"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "6A02"], na.rm = TRUE)
      }else if(input$loc.type2 == "6A08"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "6A08"], na.rm = TRUE)
      }else if(input$loc.type2 == "4D03"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "4D03"], na.rm = TRUE)
      }else if(input$loc.type2 == "4B04"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "4B04"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A08"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "4A08"], na.rm = TRUE)
      }else if(input$loc.type2 == "3D03"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "3D03"], na.rm = TRUE)
      }else if(input$loc.type2 == "3G05"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "3G05"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A07"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "4A07"], na.rm = TRUE)
      }else if(input$loc.type2 == "3E02"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "3E02"], na.rm = TRUE)
      }else if(input$loc.type2 == "3F03"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "3F03"], na.rm = TRUE)
      }else if(input$loc.type2 == "4B06"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "4B06"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A04"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "4A04"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A06"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "4A06"], na.rm = TRUE)
      }else if(input$loc.type2 == "3E04"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "3E04"], na.rm = TRUE)
      }else if(input$loc.type2 == "4A02"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "4A02"], na.rm = TRUE)
      }else if(input$loc.type2 == "5A08"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "5A08"], na.rm = TRUE)
      }else if(input$loc.type2 == "3G04"){
        ls2 <- mean(ls1$cat.average.num[ls1$single_member_district == "3G04"], na.rm = TRUE)
      }
    }else if(input$loc.type == "Voter Precinct"){
      if(input$loc.type2 == "129"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "129"], na.rm = TRUE)
      }else if(input$loc.type2 == "125"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "125"], na.rm = TRUE)
      }else if(input$loc.type2 == "126"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "126"], na.rm = TRUE)
      }else if(input$loc.type2 == "121"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "121"], na.rm = TRUE)
      }else if(input$loc.type2 == "122"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "122"], na.rm = TRUE)
      }else if(input$loc.type2 == "120"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "120"], na.rm = TRUE)
      }else if(input$loc.type2 == "123"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "123"], na.rm = TRUE)
      }else if(input$loc.type2 == "2"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "2"], na.rm = TRUE)
      }else if(input$loc.type2 == "3"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "3"], na.rm = TRUE)
      }else if(input$loc.type2 == "102"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "102"], na.rm = TRUE)
      }else if(input$loc.type2 == "94"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "94"], na.rm = TRUE)
      }else if(input$loc.type2 == "110"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "110"], na.rm = TRUE)
      }else if(input$loc.type2 == "139"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "139"], na.rm = TRUE)
      }else if(input$loc.type2 == "105"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "105"], na.rm = TRUE)
      }else if(input$loc.type2 == "106"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "106"], na.rm = TRUE)
      }else if(input$loc.type2 == "93"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "93"], na.rm = TRUE)
      }else if(input$loc.type2 == "97"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "97"], na.rm = TRUE)
      }else if(input$loc.type2 == "95"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "95"], na.rm = TRUE)
      }else if(input$loc.type2 == "132"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "132"], na.rm = TRUE)
      }else if(input$loc.type2 == "128"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "128"], na.rm = TRUE)
      }else if(input$loc.type2 == "127"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "127"], na.rm = TRUE)
      }else if(input$loc.type2 == "76"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "76"], na.rm = TRUE)
      }else if(input$loc.type2 == "130"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "130"], na.rm = TRUE)
      }else if(input$loc.type2 == "75"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "75"], na.rm = TRUE)
      }else if(input$loc.type2 == "135"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "135"], na.rm = TRUE)
      }else if(input$loc.type2 == "141"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "141"], na.rm = TRUE)
      }else if(input$loc.type2 == "20"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "20"], na.rm = TRUE)
      }else if(input$loc.type2 == "22"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "22"], na.rm = TRUE)
      }else if(input$loc.type2 == "72"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "72"], na.rm = TRUE)
      }else if(input$loc.type2 == "1"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "1"], na.rm = TRUE)
      }else if(input$loc.type2 == "73"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "73"], na.rm = TRUE)
      }else if(input$loc.type2 == "42"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "42"], na.rm = TRUE)
      }else if(input$loc.type2 == "19"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "19"], na.rm = TRUE)
      }else if(input$loc.type2 == "69"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "69"], na.rm = TRUE)
      }else if(input$loc.type2 == "29"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "29"], na.rm = TRUE)
      }else if(input$loc.type2 == "82"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "82"], na.rm = TRUE)
      }else if(input$loc.type2 == "17"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "17"], na.rm = TRUE)
      }else if(input$loc.type2 == "9"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "9"], na.rm = TRUE)
      }else if(input$loc.type2 == "26"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "26"], na.rm = TRUE)
      }else if(input$loc.type2 == "74"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "74"], na.rm = TRUE)
      }else if(input$loc.type2 == "45"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "45"], na.rm = TRUE)
      }else if(input$loc.type2 == "30"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "30"], na.rm = TRUE)
      }else if(input$loc.type2 == "27"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "27"], na.rm = TRUE)
      }else if(input$loc.type2 == "48"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "48"], na.rm = TRUE)
      }else if(input$loc.type2 == "67"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "67"], na.rm = TRUE)
      }else if(input$loc.type2 == "6"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "6"], na.rm = TRUE)
      }else if(input$loc.type2 == "12"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "12"], na.rm = TRUE)
      }else if(input$loc.type2 == "88"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "88"], na.rm = TRUE)
      }else if(input$loc.type2 == "131"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "131"], na.rm = TRUE)
      }else if(input$loc.type2 == "85"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "85"], na.rm = TRUE)
      }else if(input$loc.type2 == "89"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "89"], na.rm = TRUE)
      }else if(input$loc.type2 == "79"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "79"], na.rm = TRUE)
      }else if(input$loc.type2 == "91"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "91"], na.rm = TRUE)
      }else if(input$loc.type2 == "133"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "133"], na.rm = TRUE)
      }else if(input$loc.type2 == "81"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "81"], na.rm = TRUE)
      }else if(input$loc.type2 == "71"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "71"], na.rm = TRUE)
      }else if(input$loc.type2 == "86"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "86"], na.rm = TRUE)
      }else if(input$loc.type2 == "56"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "56"], na.rm = TRUE)
      }else if(input$loc.type2 == "59"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "59"], na.rm = TRUE)
      }else if(input$loc.type2 == "50"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "50"], na.rm = TRUE)
      }else if(input$loc.type2 == "61"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "61"], na.rm = TRUE)
      }else if(input$loc.type2 == "31"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "31"], na.rm = TRUE)
      }else if(input$loc.type2 == "138"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "138"], na.rm = TRUE)
      }else if(input$loc.type2 == "57"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "57"], na.rm = TRUE)
      }else if(input$loc.type2 == "60"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "60"], na.rm = TRUE)
      }else if(input$loc.type2 == "53"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "53"], na.rm = TRUE)
      }else if(input$loc.type2 == "32"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "32"], na.rm = TRUE)
      }else if(input$loc.type2 == "62"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "62"], na.rm = TRUE)
      }else if(input$loc.type2 == "66"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "66"], na.rm = TRUE)
      }else if(input$loc.type2 == "51"){
        ls2 <- mean(ls1$cat.average.num[ls1$voter_precinct == "51"], na.rm = TRUE)
      }
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
        if(input$loc.type2 == "SE"){
          cs2 <- traffic.flow %>% filter(quadrant == "SE")
        }else if(input$loc.type2 == "SW"){
          cs2 <- traffic.flow %>% filter(quadrant == "SW")
        }else if(input$loc.type2 == "NE"){
          cs2 <- traffic.flow %>% filter(quadrant == "NE")
        }else if(input$loc.type2 == "NW"){
          cs2 <- traffic.flow %>% filter(quadrant == "NW")
        }
      }else if(input$loc.type == "Ward"){
        if(input$loc.type2 == "1"){
          cs2 <- traffic.flow %>% filter(ward == "1")
        }else if(input$loc.type2 == "2"){
          cs2 <- traffic.flow %>% filter(ward == "2")
        }else if(input$loc.type2 == "3"){
          cs2 <- traffic.flow %>% filter(ward == "3")
        }else if(input$loc.type2 == "4"){
          cs2 <- traffic.flow %>% filter(ward == "4")
        }else if(input$loc.type2 == "5"){
          cs2 <- traffic.flow %>% filter(ward == "5")
        }else if(input$loc.type2 == "6"){
          cs2 <- traffic.flow %>% filter(ward == "6")
        }else if(input$loc.type2 == "7"){
          cs2 <- traffic.flow %>% filter(ward == "7")
        }else if(input$loc.type2 == "8"){
          cs2 <- traffic.flow %>% filter(ward == "8")
        }
      }else if(input$loc.type == "Zip Code"){
        if(input$loc.type2 == "20227"){
          cs2 <- traffic.flow %>% filter(zip_code == "20227")
        }else if(input$loc.type2 == "20032"){
          cs2 <- traffic.flow %>% filter(zip_code == "20032")
        }else if(input$loc.type2 == "20037"){
          cs2 <- traffic.flow %>% filter(zip_code == "20037")
        }else if(input$loc.type2 == "20019"){
          cs2 <- traffic.flow %>% filter(zip_code == "20019")
        }else if(input$loc.type2 == "20020"){
          cs2 <- traffic.flow %>% filter(zip_code == "20020")
        }else if(input$loc.type2 == "20018"){
          cs2 <- traffic.flow %>% filter(zip_code == "20018")
        }else if(input$loc.type2 == "20024"){
          cs2 <- traffic.flow %>% filter(zip_code == "20024")
        }else if(input$loc.type2 == "20002"){
          cs2 <- traffic.flow %>% filter(zip_code == "20002")
        }else if(input$loc.type2 == "20003"){
          cs2 <- traffic.flow %>% filter(zip_code == "20003")
        }else if(input$loc.type2 == "20001"){
          cs2 <- traffic.flow %>% filter(zip_code == "20001")
        }else if(input$loc.type2 == "20005"){
          cs2 <- traffic.flow %>% filter(zip_code == "20005")
        }else if(input$loc.type2 == "20009"){
          cs2 <- traffic.flow %>% filter(zip_code == "20009")
        }else if(input$loc.type2 == "20017"){
          cs2 <- traffic.flow %>% filter(zip_code == "20017")
        }else if(input$loc.type2 == "20010"){
          cs2 <- traffic.flow %>% filter(zip_code == "20010")
        }else if(input$loc.type2 == "20016"){
          cs2 <- traffic.flow %>% filter(zip_code == "20016")
        }else if(input$loc.type2 == "20008"){
          cs2 <- traffic.flow %>% filter(zip_code == "20008")
        }else if(input$loc.type2 == "20011"){
          cs2 <- traffic.flow %>% filter(zip_code == "20011")
        }else if(input$loc.type2 == "20007"){
          cs2 <- traffic.flow %>% filter(zip_code == "20007")
        }else if(input$loc.type2 == "20374"){
          cs2 <- traffic.flow %>% filter(zip_code == "20374")
        }else if(input$loc.type2 == "20015"){
          cs2 <- traffic.flow %>% filter(zip_code == "20015")
        }else if(input$loc.type2 == "20012"){
          cs2 <- traffic.flow %>% filter(zip_code == "20012")
        }
      }else if(input$loc.type == "Advisory Neighborhood Commission"){
        if(input$loc.type2 == "2A"){
          cs2 <- traffic.flow %>% filter(anc == "2A")
        }else if(input$loc.type2 == "8D"){
          cs2 <- traffic.flow %>% filter(anc == "8D")
        }else if(input$loc.type2 == "8E"){
          cs2 <- traffic.flow %>% filter(anc == "8E")
        }else if(input$loc.type2 == "8C"){
          cs2 <- traffic.flow %>% filter(anc == "8C")
        }else if(input$loc.type2 == "7F"){
          cs2 <- traffic.flow %>% filter(anc == "7F")
        }else if(input$loc.type2 == "7C"){
          cs2 <- traffic.flow %>% filter(anc == "7C")
        }else if(input$loc.type2 == "7E"){
          cs2 <- traffic.flow %>% filter(anc == "7E")
        }else if(input$loc.type2 == "5C"){
          cs2 <- traffic.flow %>% filter(anc == "5C")
        }else if(input$loc.type2 == "6D"){
          cs2 <- traffic.flow %>% filter(anc == "6D")
        }else if(input$loc.type2 == "5D"){
          cs2 <- traffic.flow %>% filter(anc == "5D")
        }else if(input$loc.type2 == "6B"){
          cs2 <- traffic.flow %>% filter(anc == "6B")
        }else if(input$loc.type2 == "5E"){
          cs2 <- traffic.flow %>% filter(anc == "5E")
        }else if(input$loc.type2 == "2F"){
          cs2 <- traffic.flow %>% filter(anc == "2F")
        }else if(input$loc.type2 == "2B"){
          cs2 <- traffic.flow %>% filter(anc == "2B")
        }else if(input$loc.type2 == "1B"){
          cs2 <- traffic.flow %>% filter(anc == "1B")
        }else if(input$loc.type2 == "6E"){
          cs2 <- traffic.flow %>% filter(anc == "6E")
        }else if(input$loc.type2 == "5B"){
          cs2 <- traffic.flow %>% filter(anc == "5B")
        }else if(input$loc.type2 == "1A"){
          cs2 <- traffic.flow %>% filter(anc == "1A")
        }else if(input$loc.type2 == "3C"){
          cs2 <- traffic.flow %>% filter(anc == "3C")
        }else if(input$loc.type2 == "6A"){
          cs2 <- traffic.flow %>% filter(anc == "6A")
        }else if(input$loc.type2 == "3D"){
          cs2 <- traffic.flow %>% filter(anc == "3D")
        }else if(input$loc.type2 == "4C"){
          cs2 <- traffic.flow %>% filter(anc == "4C")
        }else if(input$loc.type2 == "3E"){
          cs2 <- traffic.flow %>% filter(anc == "3E")
        }else if(input$loc.type2 == "2E"){
          cs2 <- traffic.flow %>% filter(anc == "2E")
        }else if(input$loc.type2 == "6C"){
          cs2 <- traffic.flow %>% filter(anc == "6C")
        }else if(input$loc.type2 == "8A"){
          cs2 <- traffic.flow %>% filter(anc == "8A")
        }else if(input$loc.type2 == "4D"){
          cs2 <- traffic.flow %>% filter(anc == "4D")
        }else if(input$loc.type2 == "4B"){
          cs2 <- traffic.flow %>% filter(anc == "4B")
        }else if(input$loc.type2 == "4A"){
          cs2 <- traffic.flow %>% filter(anc == "4A")
        }else if(input$loc.type2 == "3G"){
          cs2 <- traffic.flow %>% filter(anc == "3G")
        }else if(input$loc.type2 == "3F"){
          cs2 <- traffic.flow %>% filter(anc == "3F")
        }else if(input$loc.type2 == "5A"){
          cs2 <- traffic.flow %>% filter(anc == "5A")
        }
      }else if(input$loc.type == "Census Tract"){
        if(input$loc.type2 == "006202"){
          cs2 <- traffic.flow %>% filter(census_tract == "006202")
        }else if(input$loc.type2 == "009811"){
          cs2 <- traffic.flow %>% filter(census_tract == "009811")
        }else if(input$loc.type2 == "009807"){
          cs2 <- traffic.flow %>% filter(census_tract == "009807")
        }else if(input$loc.type2 == "009700"){
          cs2 <- traffic.flow %>% filter(census_tract == "009700")
        }else if(input$loc.type2 == "009804"){
          cs2 <- traffic.flow %>% filter(census_tract == "009804")
        }else if(input$loc.type2 == "007304"){
          cs2 <- traffic.flow %>% filter(census_tract == "007304")
        }else if(input$loc.type2 == "007301"){
          cs2 <- traffic.flow %>% filter(census_tract == "007301")
        }else if(input$loc.type2 == "010400"){
          cs2 <- traffic.flow %>% filter(census_tract == "010400")
        }else if(input$loc.type2 == "010800"){
          cs2 <- traffic.flow %>% filter(census_tract == "010800")
        }else if(input$loc.type2 == "005600"){
          cs2 <- traffic.flow %>% filter(census_tract == "005600")
        }else if(input$loc.type2 == "009603"){
          cs2 <- traffic.flow %>% filter(census_tract == "009603")
        }else if(input$loc.type2 == "007809"){
          cs2 <- traffic.flow %>% filter(census_tract == "007809")
        }else if(input$loc.type2 == "009902"){
          cs2 <- traffic.flow %>% filter(census_tract == "009902")
        }else if(input$loc.type2 == "009000"){
          cs2 <- traffic.flow %>% filter(census_tract == "009000")
        }else if(input$loc.type2 == "009905"){
          cs2 <- traffic.flow %>% filter(census_tract == "009905")
        }else if(input$loc.type2 == "007707"){
          cs2 <- traffic.flow %>% filter(census_tract == "007707")
        }else if(input$loc.type2 == "007806"){
          cs2 <- traffic.flow %>% filter(census_tract == "007806")
        }else if(input$loc.type2 == "007804"){
          cs2 <- traffic.flow %>% filter(census_tract == "007804")
        }else if(input$loc.type2 == "007807"){
          cs2 <- traffic.flow %>% filter(census_tract == "007807")
        }else if(input$loc.type2 == "007708"){
          cs2 <- traffic.flow %>% filter(census_tract == "007708")
        }else if(input$loc.type2 == "010500"){
          cs2 <- traffic.flow %>% filter(census_tract == "010500")
        }else if(input$loc.type2 == "006400"){
          cs2 <- traffic.flow %>% filter(census_tract == "006400")
        }else if(input$loc.type2 == "008803"){
          cs2 <- traffic.flow %>% filter(census_tract == "008803")
        }else if(input$loc.type2 == "010200"){
          cs2 <- traffic.flow %>% filter(census_tract == "010200")
        }else if(input$loc.type2 == "006500"){
          cs2 <- traffic.flow %>% filter(census_tract == "006500")
        }else if(input$loc.type2 == "008702"){
          cs2 <- traffic.flow %>% filter(census_tract == "008702")
        }else if(input$loc.type2 == "003301"){
          cs2 <- traffic.flow %>% filter(census_tract == "003301")
        }else if(input$loc.type2 == "004902"){
          cs2 <- traffic.flow %>% filter(census_tract == "004902")
        }else if(input$loc.type2 == "004201"){
          cs2 <- traffic.flow %>% filter(census_tract == "004201")
        }else if(input$loc.type2 == "003400"){
          cs2 <- traffic.flow %>% filter(census_tract == "003400")
        }else if(input$loc.type2 == "004400"){
          cs2 <- traffic.flow %>% filter(census_tract == "004400")
        }else if(input$loc.type2 == "011100"){
          cs2 <- traffic.flow %>% filter(census_tract == "011100")
        }else if(input$loc.type2 == "004702"){
          cs2 <- traffic.flow %>% filter(census_tract == "004702")
        }else if(input$loc.type2 == "009302"){
          cs2 <- traffic.flow %>% filter(census_tract == "009302")
        }else if(input$loc.type2 == "008701"){
          cs2 <- traffic.flow %>% filter(census_tract == "008701")
        }else if(input$loc.type2 == "010100"){
          cs2 <- traffic.flow %>% filter(census_tract == "010100")
        }else if(input$loc.type2 == "002900"){
          cs2 <- traffic.flow %>% filter(census_tract == "002900")
        }else if(input$loc.type2 == "004600"){
          cs2 <- traffic.flow %>% filter(census_tract == "004600")
        }else if(input$loc.type2 == "009400"){
          cs2 <- traffic.flow %>% filter(census_tract == "009400")
        }else if(input$loc.type2 == "001002"){
          cs2 <- traffic.flow %>% filter(census_tract == "001002")
        }else if(input$loc.type2 == "010600"){
          cs2 <- traffic.flow %>% filter(census_tract == "010600")
        }else if(input$loc.type2 == "000901"){
          cs2 <- traffic.flow %>% filter(census_tract == "000901")
        }else if(input$loc.type2 == "000502"){
          cs2 <- traffic.flow %>% filter(census_tract == "000502")
        }else if(input$loc.type2 == "009102"){
          cs2 <- traffic.flow %>% filter(census_tract == "009102")
        }else if(input$loc.type2 == "009201"){
          cs2 <- traffic.flow %>% filter(census_tract == "009201")
        }else if(input$loc.type2 == "002400"){
          cs2 <- traffic.flow %>% filter(census_tract == "002400")
        }else if(input$loc.type2 == "001001"){
          cs2 <- traffic.flow %>% filter(census_tract == "001001")
        }else if(input$loc.type2 == "000600"){
          cs2 <- traffic.flow %>% filter(census_tract == "000600")
        }else if(input$loc.type2 == "002501"){
          cs2 <- traffic.flow %>% filter(census_tract == "002501")
        }else if(input$loc.type2 == "009503"){
          cs2 <- traffic.flow %>% filter(census_tract == "009503")
        }else if(input$loc.type2 == "000300"){
          cs2 <- traffic.flow %>% filter(census_tract == "000300")
        }else if(input$loc.type2 == "000400"){
          cs2 <- traffic.flow %>% filter(census_tract == "000400")
        }else if(input$loc.type2 == "006700"){
          cs2 <- traffic.flow %>% filter(census_tract == "006700")
        }else if(input$loc.type2 == "007200"){
          cs2 <- traffic.flow %>% filter(census_tract == "007200")
        }else if(input$loc.type2 == "008302"){
          cs2 <- traffic.flow %>% filter(census_tract == "008302")
        }else if(input$loc.type2 == "006600"){
          cs2 <- traffic.flow %>% filter(census_tract == "006600")
        }else if(input$loc.type2 == "008904"){
          cs2 <- traffic.flow %>% filter(census_tract == "008904")
        }else if(input$loc.type2 == "008100"){
          cs2 <- traffic.flow %>% filter(census_tract == "008100")
        }else if(input$loc.type2 == "006802"){
          cs2 <- traffic.flow %>% filter(census_tract == "006802")
        }else if(input$loc.type2 == "007601"){
          cs2 <- traffic.flow %>% filter(census_tract == "007601")
        }else if(input$loc.type2 == "007100"){
          cs2 <- traffic.flow %>% filter(census_tract == "007100")
        }else if(input$loc.type2 == "008200"){
          cs2 <- traffic.flow %>% filter(census_tract == "008200")
        }else if(input$loc.type2 == "008001"){
          cs2 <- traffic.flow %>% filter(census_tract == "008001")
        }else if(input$loc.type2 == "008002"){
          cs2 <- traffic.flow %>% filter(census_tract == "008002")
        }else if(input$loc.type2 == "002101"){
          cs2 <- traffic.flow %>% filter(census_tract == "002101")
        }else if(input$loc.type2 == "001901"){
          cs2 <- traffic.flow %>% filter(census_tract == "001901")
        }else if(input$loc.type2 == "002600"){
          cs2 <- traffic.flow %>% filter(census_tract == "002600")
        }else if(input$loc.type2 == "001401"){
          cs2 <- traffic.flow %>% filter(census_tract == "001401")
        }else if(input$loc.type2 == "001803"){
          cs2 <- traffic.flow %>% filter(census_tract == "001803")
        }else if(input$loc.type2 == "001301"){
          cs2 <- traffic.flow %>% filter(census_tract == "001301")
        }else if(input$loc.type2 == "002102"){
          cs2 <- traffic.flow %>% filter(census_tract == "002102")
        }else if(input$loc.type2 == "001804"){
          cs2 <- traffic.flow %>% filter(census_tract == "001804")
        }else if(input$loc.type2 == "002001"){
          cs2 <- traffic.flow %>% filter(census_tract == "002001")
        }else if(input$loc.type2 == "001902"){
          cs2 <- traffic.flow %>% filter(census_tract == "001902")
        }else if(input$loc.type2 == "001100"){
          cs2 <- traffic.flow %>% filter(census_tract == "001100")
        }else if(input$loc.type2 == "001600"){
          cs2 <- traffic.flow %>% filter(census_tract == "001600")
        }else if(input$loc.type2 == "009509"){
          cs2 <- traffic.flow %>% filter(census_tract == "009509")
        }else if(input$loc.type2 == "001500"){
          cs2 <- traffic.flow %>% filter(census_tract == "001500")
        }
      }else if(input$loc.type == "Single Member District"){
        if(input$loc.type2 == "2A01"){
          cs2 <- traffic.flow %>% filter(single_member_district == "2A01")
        }else if(input$loc.type2 == "8D03"){
          cs2 <- traffic.flow %>% filter(single_member_district == "8D03")
        }else if(input$loc.type2 == "8D01"){
          cs2 <- traffic.flow %>% filter(single_member_district == "8D01")
        }else if(input$loc.type2 == "8D06"){
          cs2 <- traffic.flow %>% filter(single_member_district == "8D06")
        }else if(input$loc.type2 == "8E05"){
          cs2 <- traffic.flow %>% filter(single_member_district == "8E05")
        }else if(input$loc.type2 == "8C07"){
          cs2 <- traffic.flow %>% filter(single_member_district == "8C07")
        }else if(input$loc.type2 == "8E04"){
          cs2 <- traffic.flow %>% filter(single_member_district == "8E04")
        }else if(input$loc.type2 == "8C05"){
          cs2 <- traffic.flow %>% filter(single_member_district == "8C05")
        }else if(input$loc.type2 == "8C03"){
          cs2 <- traffic.flow %>% filter(single_member_district == "8C03")
        }else if(input$loc.type2 == "2A07"){
          cs2 <- traffic.flow %>% filter(single_member_district == "2A07")
        }else if(input$loc.type2 == "2A03"){
          cs2 <- traffic.flow %>% filter(single_member_district == "2A03")
        }else if(input$loc.type2 == "7F01"){
          cs2 <- traffic.flow %>% filter(single_member_district == "7F01")
        }else if(input$loc.type2 == "7C04"){
          cs2 <- traffic.flow %>% filter(single_member_district == "7C04")
        }else if(input$loc.type2 == "7E02"){
          cs2 <- traffic.flow %>% filter(single_member_district == "7E02")
        }else if(input$loc.type2 == "5C03"){
          cs2 <- traffic.flow %>% filter(single_member_district == "5C03")
        }else if(input$loc.type2 == "7E06"){
          cs2 <- traffic.flow %>% filter(single_member_district == "7E06")
        }else if(input$loc.type2 == "7E01"){
          cs2 <- traffic.flow %>% filter(single_member_district == "7E01")
        }else if(input$loc.type2 == "7C07"){
          cs2 <- traffic.flow %>% filter(single_member_district == "7C07")
        }else if(input$loc.type2 == "7C03"){
          cs2 <- traffic.flow %>% filter(single_member_district == "7C03")
        }else if(input$loc.type2 == "7C06"){
          cs2 <- traffic.flow %>% filter(single_member_district == "7C06")
        }else if(input$loc.type2 == "7F06"){
          cs2 <- traffic.flow %>% filter(single_member_district == "7F06")
        }else if(input$loc.type2 == "6D03"){
          cs2 <- traffic.flow %>% filter(single_member_district == "6D03")
        }else if(input$loc.type2 == "6D06"){
          cs2 <- traffic.flow %>% filter(single_member_district == "6D06")
        }else if(input$loc.type2 == "5D01"){
          cs2 <- traffic.flow %>% filter(single_member_district == "5D01")
        }else if(input$loc.type2 == "6D05"){
          cs2 <- traffic.flow %>% filter(single_member_district == "6D05")
        }else if(input$loc.type2 == "6B01"){
          cs2 <- traffic.flow %>% filter(single_member_district == "6B01")
        }else if(input$loc.type2 == "5E03"){
          cs2 <- traffic.flow %>% filter(single_member_district == "5E03")
        }else if(input$loc.type2 == "5E08"){
          cs2 <- traffic.flow %>% filter(single_member_district == "5E08")
        }else if(input$loc.type2 == "2F06"){
          cs2 <- traffic.flow %>% filter(single_member_district == "2F06")
        }else if(input$loc.type2 == "2B09"){
          cs2 <- traffic.flow %>% filter(single_member_district == "2B09")
        }else if(input$loc.type2 == "1B01"){
          cs2 <- traffic.flow %>% filter(single_member_district == "1B01")
        }else if(input$loc.type2 == "1B02"){
          cs2 <- traffic.flow %>% filter(single_member_district == "1B02")
        }else if(input$loc.type2 == "5C02"){
          cs2 <- traffic.flow %>% filter(single_member_district == "5C02")
        }else if(input$loc.type2 == "6E07"){
          cs2 <- traffic.flow %>% filter(single_member_district == "6E07")
        }else if(input$loc.type2 == "5B03"){
          cs2 <- traffic.flow %>% filter(single_member_district == "5B03")
        }else if(input$loc.type2 == "5E04"){
          cs2 <- traffic.flow %>% filter(single_member_district == "5E04")
        }else if(input$loc.type2 == "2F08"){
          cs2 <- traffic.flow %>% filter(single_member_district == "2F08")
        }else if(input$loc.type2 == "1A04"){
          cs2 <- traffic.flow %>% filter(single_member_district == "1A04")
        }else if(input$loc.type2 == "5E05"){
          cs2 <- traffic.flow %>% filter(single_member_district == "5E05")
        }else if(input$loc.type2 == "5C01"){
          cs2 <- traffic.flow %>% filter(single_member_district == "5C01")
        }else if(input$loc.type2 == "3C06"){
          cs2 <- traffic.flow %>% filter(single_member_district == "3C06")
        }else if(input$loc.type2 == "6A01"){
          cs2 <- traffic.flow %>% filter(single_member_district == "6A01")
        }else if(input$loc.type2 == "2B05"){
          cs2 <- traffic.flow %>% filter(single_member_district == "2B05")
        }else if(input$loc.type2 == "3D02"){
          cs2 <- traffic.flow %>% filter(single_member_district == "3D02")
        }else if(input$loc.type2 == "3C03"){
          cs2 <- traffic.flow %>% filter(single_member_district == "3C03")
        }else if(input$loc.type2 == "5B01"){
          cs2 <- traffic.flow %>% filter(single_member_district == "5B01")
        }else if(input$loc.type2 == "5C06"){
          cs2 <- traffic.flow %>% filter(single_member_district == "5C06")
        }else if(input$loc.type2 == "5E01"){
          cs2 <- traffic.flow %>% filter(single_member_district == "5E01")
        }else if(input$loc.type2 == "4C08"){
          cs2 <- traffic.flow %>% filter(single_member_district == "4C08")
        }else if(input$loc.type2 == "3E05"){
          cs2 <- traffic.flow %>% filter(single_member_district == "3E05")
        }else if(input$loc.type2 == "3C09"){
          cs2 <- traffic.flow %>% filter(single_member_district == "3C09")
        }else if(input$loc.type2 == "4C03"){
          cs2 <- traffic.flow %>% filter(single_member_district == "4C03")
        }else if(input$loc.type2 == "2E01"){
          cs2 <- traffic.flow %>% filter(single_member_district == "2E01")
        }else if(input$loc.type2 == "3C08"){
          cs2 <- traffic.flow %>% filter(single_member_district == "3C08")
        }else if(input$loc.type2 == "6B05"){
          cs2 <- traffic.flow %>% filter(single_member_district == "6B05")
        }else if(input$loc.type2 == "5C04"){
          cs2 <- traffic.flow %>% filter(single_member_district == "5C04")
        }else if(input$loc.type2 == "6D07"){
          cs2 <- traffic.flow %>% filter(single_member_district == "6D07")
        }else if(input$loc.type2 == "6C03"){
          cs2 <- traffic.flow %>% filter(single_member_district == "6C03")
        }else if(input$loc.type2 == "6B02"){
          cs2 <- traffic.flow %>% filter(single_member_district == "6B02")
        }else if(input$loc.type2 == "5D05"){
          cs2 <- traffic.flow %>% filter(single_member_district == "5D05")
        }else if(input$loc.type2 == "6A03"){
          cs2 <- traffic.flow %>% filter(single_member_district == "6A03")
        }else if(input$loc.type2 == "6B09"){
          cs2 <- traffic.flow %>% filter(single_member_district == "6B09")
        }else if(input$loc.type2 == "8A03"){
          cs2 <- traffic.flow %>% filter(single_member_district == "8A03")
        }else if(input$loc.type2 == "6B07"){
          cs2 <- traffic.flow %>% filter(single_member_district == "6B07")
        }else if(input$loc.type2 == "6B06"){
          cs2 <- traffic.flow %>% filter(single_member_district == "6B06")
        }else if(input$loc.type2 == "6A02"){
          cs2 <- traffic.flow %>% filter(single_member_district == "6A02")
        }else if(input$loc.type2 == "6A08"){
          cs2 <- traffic.flow %>% filter(single_member_district == "6A08")
        }else if(input$loc.type2 == "4D03"){
          cs2 <- traffic.flow %>% filter(single_member_district == "4D03")
        }else if(input$loc.type2 == "4B04"){
          cs2 <- traffic.flow %>% filter(single_member_district == "4B04")
        }else if(input$loc.type2 == "4A08"){
          cs2 <- traffic.flow %>% filter(single_member_district == "4A08")
        }else if(input$loc.type2 == "3D03"){
          cs2 <- traffic.flow %>% filter(single_member_district == "3D03")
        }else if(input$loc.type2 == "3G05"){
          cs2 <- traffic.flow %>% filter(single_member_district == "3G05")
        }else if(input$loc.type2 == "4A07"){
          cs2 <- traffic.flow %>% filter(single_member_district == "4A07")
        }else if(input$loc.type2 == "3E02"){
          cs2 <- traffic.flow %>% filter(single_member_district == "3E02")
        }else if(input$loc.type2 == "3F03"){
          cs2 <- traffic.flow %>% filter(single_member_district == "3F03")
        }else if(input$loc.type2 == "4B06"){
          cs2 <- traffic.flow %>% filter(single_member_district == "4B06")
        }else if(input$loc.type2 == "4A04"){
          cs2 <- traffic.flow %>% filter(single_member_district == "4A04")
        }else if(input$loc.type2 == "4A06"){
          cs2 <- traffic.flow %>% filter(single_member_district == "4A06")
        }else if(input$loc.type2 == "3E04"){
          cs2 <- traffic.flow %>% filter(single_member_district == "3E04")
        }else if(input$loc.type2 == "4A02"){
          cs2 <- traffic.flow %>% filter(single_member_district == "4A02")
        }else if(input$loc.type2 == "5A08"){
          cs2 <- traffic.flow %>% filter(single_member_district == "5A08")
        }else if(input$loc.type2 == "3G04"){
          cs2 <- traffic.flow %>% filter(single_member_district == "3G04")
        }
      }else if(input$loc.type == "Voter Precinct"){
        if(input$loc.type2 == "129"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "129")
        }else if(input$loc.type2 == "125"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "125")
        }else if(input$loc.type2 == "126"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "126")
        }else if(input$loc.type2 == "121"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "121")
        }else if(input$loc.type2 == "122"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "122")
        }else if(input$loc.type2 == "120"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "120")
        }else if(input$loc.type2 == "123"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "123")
        }else if(input$loc.type2 == "2"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "2")
        }else if(input$loc.type2 == "3"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "3")
        }else if(input$loc.type2 == "102"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "102")
        }else if(input$loc.type2 == "94"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "94")
        }else if(input$loc.type2 == "110"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "110")
        }else if(input$loc.type2 == "139"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "139")
        }else if(input$loc.type2 == "105"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "105")
        }else if(input$loc.type2 == "106"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "106")
        }else if(input$loc.type2 == "93"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "93")
        }else if(input$loc.type2 == "97"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "97")
        }else if(input$loc.type2 == "95"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "95")
        }else if(input$loc.type2 == "132"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "132")
        }else if(input$loc.type2 == "128"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "128")
        }else if(input$loc.type2 == "127"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "127")
        }else if(input$loc.type2 == "76"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "76")
        }else if(input$loc.type2 == "130"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "130")
        }else if(input$loc.type2 == "75"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "75")
        }else if(input$loc.type2 == "135"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "135")
        }else if(input$loc.type2 == "141"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "141")
        }else if(input$loc.type2 == "20"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "20")
        }else if(input$loc.type2 == "22"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "22")
        }else if(input$loc.type2 == "72"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "72")
        }else if(input$loc.type2 == "1"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "1")
        }else if(input$loc.type2 == "73"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "73")
        }else if(input$loc.type2 == "42"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "42")
        }else if(input$loc.type2 == "19"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "19")
        }else if(input$loc.type2 == "69"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "69")
        }else if(input$loc.type2 == "29"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "29")
        }else if(input$loc.type2 == "82"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "82")
        }else if(input$loc.type2 == "17"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "17")
        }else if(input$loc.type2 == "9"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "9")
        }else if(input$loc.type2 == "26"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "26")
        }else if(input$loc.type2 == "74"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "74")
        }else if(input$loc.type2 == "45"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "45")
        }else if(input$loc.type2 == "30"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "30")
        }else if(input$loc.type2 == "27"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "27")
        }else if(input$loc.type2 == "48"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "48")
        }else if(input$loc.type2 == "67"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "67")
        }else if(input$loc.type2 == "6"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "6")
        }else if(input$loc.type2 == "12"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "12")
        }else if(input$loc.type2 == "88"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "88")
        }else if(input$loc.type2 == "131"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "131")
        }else if(input$loc.type2 == "85"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "85")
        }else if(input$loc.type2 == "89"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "89")
        }else if(input$loc.type2 == "79"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "79")
        }else if(input$loc.type2 == "91"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "91")
        }else if(input$loc.type2 == "133"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "133")
        }else if(input$loc.type2 == "81"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "81")
        }else if(input$loc.type2 == "71"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "71")
        }else if(input$loc.type2 == "86"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "86")
        }else if(input$loc.type2 == "56"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "56")
        }else if(input$loc.type2 == "59"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "59")
        }else if(input$loc.type2 == "50"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "50")
        }else if(input$loc.type2 == "61"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "61")
        }else if(input$loc.type2 == "31"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "31")
        }else if(input$loc.type2 == "138"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "138")
        }else if(input$loc.type2 == "57"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "57")
        }else if(input$loc.type2 == "60"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "60")
        }else if(input$loc.type2 == "53"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "53")
        }else if(input$loc.type2 == "32"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "32")
        }else if(input$loc.type2 == "62"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "62")
        }else if(input$loc.type2 == "66"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "66")
        }else if(input$loc.type2 == "51"){
          cs2 <- traffic.flow %>% filter(voter_precinct == "51")
        }
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
  })
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
                labs(x = "Date", y = "AQI",
                     title = str_c(input$loc.type))
            }
          }
        }
      }
    }
  }) 
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
                labs(x = "Date", y = "AQI",
                     title = str_c(input$loc.type))
            }
          }
        }
      }
    }
  }) 
  
  ## Raw Data Tables
  output$static.city <- renderDataTable({
    hourly
  })
  output$static.location <- renderDataTable({
    traffic.flow %>% 
      select(-all_lat_lon) %>% 
      select(-street.url)
  })
}

# Run ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)
