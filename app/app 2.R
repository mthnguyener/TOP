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
library(shinyBS)
library(remotes)
library(shinycssloaders)
library(shinycustomloader)
library(rgdal)

# Load Data ---------------------------------------------------------------
setwd('..')

hourly <- read_csv(str_c(getwd(), "/data/hourly.csv"),
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

traffic.flow <- read_csv(str_c(getwd(), "/data/traffic_flow.csv"),
                 col_types = cols(single_member_district = col_character()))

hourly$date <- lubridate::as_datetime(hourly$date)
traffic.flow$date <- lubridate::as_datetime(traffic.flow$date)

# Tidy Data ---------------------------------------------------------------
an.slim <- hourly %>% 
  dplyr::select(date, location, parameter_name, category, category_number, 
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
  dplyr::select(-parameter_name)
an.slim.s <- an.slim.s %>% 
  rename(so2_loc = location, 
         so2_loc_cat_num = category_number, 
         so2_loc_raw_conc = raw_concentration,
         so2_loc_aqi = AQI,
         so2_loc_cat = category) %>% 
  dplyr::select(-parameter_name)
an.slim.p <- an.slim.p %>% 
  rename(pm2.5_loc = location, 
         pm2.5_loc_cat_num = category_number, 
         pm2.5_loc_raw_conc = raw_concentration,
         pm2.5_loc_aqi = AQI,
         pm2.5_loc_cat = category) %>% 
  dplyr::select(-parameter_name)
an.slim.n <- an.slim.n %>% 
  rename(no2_loc = location, 
         no2_loc_cat_num = category_number, 
         no2_loc_raw_conc = raw_concentration,
         no2_loc_aqi = AQI,
         no2_loc_cat = category) %>% 
  dplyr::select(-parameter_name)

traffic.flow <- merge(traffic.flow, an.slim.o, by = c("date", "ozone_loc"))
traffic.flow <- merge(traffic.flow, an.slim.s, by = c("date", "so2_loc"))
traffic.flow <- merge(traffic.flow, an.slim.p, by = c("date", "pm2.5_loc"))
traffic.flow <- merge(traffic.flow, an.slim.n, by = c("date", "no2_loc"))

rm(an.slim, an.slim.n, an.slim.p, an.slim.s, an.slim.o)

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

traffic.flow$zip_code <- as.character(traffic.flow$zip_code)

hourly$jams_length <- hourly$jams_length * 0.621371

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

parameter_wider <- hourly %>%
  group_by(date, location, parameter_name) %>%
  pivot_wider(names_from = parameter_name, values_from = AQI)

# User Interface ----------------------------------------------------------
location.types <- c("Quadrant", "Ward", "Zip Code", 
                    "Advisory Neighborhood Commission",
                    "Census Tract",
                    "Single Member District",
                    "Voter Precinct")

sensor.location <- c(unique(parameter_wider$location))
sensor.types <- c("Average Across Sensors", "Single Sensor")

traffic.types <- c("Current Speed", "Free Flow Speed", 
                   "Current Travel Time", "Free Flow Travel Time")
air.types <- c("Ozone", "SO2", "PM2.5", "NO2")

current.date.time <- Sys.time()

c.date <- substr(current.date.time, start = 1, stop = 10)
c.time <- substr(current.date.time, start = 12, stop = 13)

ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("slate"),
  wellPanel(titlePanel("AirMotionDC")),
  
  tabsetPanel(
    tabPanel("Overview",
             h2("Welcome to AirMotionDC"),
             p("AirMotionDC is a real time data tool that compiles and analyzes traffic patterns, air pollution, and weather in the District of Columbia to explore the relationship between transportation and air quality. AirMotionDC utilizes a variety of data sources including AirNow, TomTom, Open Data DC, and OpenWeather. For more information on these sources and how the data was compiled, please visit the Background tab."),
             h3("Understanding the Tool"),
             p("Click the following to expand the section and explore what information is provided in each of the tabs above."),
             bsCollapse(id = "collapseOverview",
                        bsCollapsePanel("Citywide",
                                        p("The Citywide tab provides traffic patterns, air pollution, and weather information for the whole of D.C. Users are able to select which information they would like to view - data on current traffic, air quality, and weather for the date inputted; data on traffic, air quality, and weather from one week ago; and/or the full historical data on the three. Users are able to input which date and time they would like to view for the Current Date output, whether they would like the air quality data to be averaged across all six of the sensors or narrowed to only one sensor location, and which air quality/traffic parameters to include on the graphics. Output includes a variety of methods of exploration into air quality, traffic, and weather, including overall averages, tables, bar charts, trend lines, and maps.")),
                        bsCollapsePanel("Location Search",
                                        p("The Location Search tab provides traffic patterns and air pollution information for specific locations within the District of Columbia. Users are able to select which information they would like to view - data on current traffic and air quality for the date inputted; data on traffic and air quality from one week ago; and/or the full historical data on the two. Users are able to input which date and time they would like to view for the Current Date output, which location type (e.g., quadrant, ward) and specific location (e.g., SE quadrant, 8th ward) they would like the output to show, and which air quality/traffic parameters to include on the graphics. Output includes a variety of methods of exploration into air quality and traffic, including overall averages, tables, bar charts, trend lines, and maps.")),
                        bsCollapsePanel("Raw Citywide Data",
                                        p("The Raw Citywide Data tab provides the full data set utilized for the Citywide tab. Data is compiled at the city level for overall information on traffic, air quality, and weather in the District of Columbia. Users can explore the data present as well as filter and sort variables of interest.")),
                        bsCollapsePanel("Raw Location Search Data",
                                        p("The Raw Location Search Data tab provides the full data set utilized for the Location Search tab. Data is compiled at a more granular level (e.g., quadrants, wards, zip codes) for a more narrow look into traffic and air quality as well as how it differs across the District of Columbia. Users can explore the data present as well as filter and sort variables of interest.")),
                        bsCollapsePanel("Background",
                                        p("The Background tab provides additional information on how and where the data is sourced from, what the variables are and how they are measured, and who the team is behind AirMotionDC."))),
    ),
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
                 checkboxInput("include.weather", "Include Weather Data"),
                 checkboxInput("traffic", "Turn Map Traffic Index On", value = TRUE),
                 h5("Air Quality Parameters"),
                 checkboxInput("ozone", "OZONE", value = TRUE),
                 checkboxInput("so2", "SO2"),
                 checkboxInput("pm25", "PM2.5", value = TRUE),
                 checkboxInput("no2", "NO2"),
                 h5("Traffic Parameters"),
                 checkboxInput("tindex", "Traffic Index", value = TRUE), 
                 checkboxInput("jdelay", "Jams Delay"), 
                 checkboxInput("jlength", "Jams Length"),
                 checkboxInput("jcount", "Jams Count", value = TRUE),
                 h5("Weather Parameters"),
                 checkboxInput("temp", "Temperature", value = TRUE),
                 checkboxInput("weather.des", "Weather Description", value = TRUE),
                 checkboxInput("humidity", "Humidity"),
                 checkboxInput("wind.speed", "Wind Speed")
               ),
               
               mainPanel(
                 tags$style(type="text/css",
                            ".shiny-output-error { visibility: hidden; }",
                            ".shiny-output-error:before { visibility: hidden; }"),
                 h3("Overview"),
                 textOutput("current.congestion.o"),
                 textOutput("current.air.quality.o"),
                 textOutput("current.temperature.o"),
                 h4("Air Quality and Traffic"),
                 splitLayout(cellWidths = c("50%", "50%"),
                             textOutput("airtable.title.o"),
                             textOutput("airtable.title2.o")),
                 splitLayout(cellWidths = c("50%", "50%"),
                             withLoader(tableOutput("aqitable"), type = "html", loader = "loader6"),
                             withLoader(tableOutput("aqitable2"), type = "html", loader = "loader6")),
                 tableOutput("traffictable"),
                 tableOutput("weathertable"),
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
                 h5("Air Quality Parameters"),
                 checkboxInput("ozone1", "Ozone", value = TRUE), 
                 checkboxInput("so2.1", "SO2"), 
                 checkboxInput("pm2.5.1", "PM 2.5", value = TRUE),
                 checkboxInput("no2.1", "NO2"),
                 h5("Traffic Parameters"),
                 checkboxInput("cspeed", "Current Speed", value = TRUE), 
                 checkboxInput("ffspeed", "Free Flow Speed", value = TRUE), 
                 checkboxInput("ctravel", "Current Travel Time"),
                 checkboxInput("fftravel", "Free Flow Travel Time"),
                 selectInput("map.choice.air", "Which air quality map?",
                             choices = air.types,
                             selected = "Ozone"),
                 selectInput("map.choice.traffic", "Which traffic map?",
                             choices = traffic.types,
                             selected = "Current Speed")
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
                 splitLayout(cellWidths = c("50%", "50%"),
                             textOutput("airtable.title"),
                             textOutput("airtable.title2")),
                 splitLayout(cellWidths = c("50%", "50%"),
                             withLoader(tableOutput("airtable"), type = "html", loader = "loader6"),
                             withLoader(tableOutput("airtable2"), type = "html", loader = "loader6")),
                 plotOutput("airtable.graph"),
                 h3("Within Location"),
                 plotOutput("location.graph"),
                 h3("Location Type Comparison"),
                 h4("Air Quality"),
                 plotOutput("loc.type.graph1"),
                 h4("Traffic Volume"),
                 plotOutput("loc.type.graph2"),
                 h4("Maps"),
                 textOutput("airtable.title3"),
                 plotOutput("location.map"),
                 textOutput("airtable.title4"),
                 plotOutput("location.map2"))
             )),
    tabPanel("Raw Citywide Data",
             dataTableOutput("static.city")),
    tabPanel("Raw Location Specific Data",
             dataTableOutput("static.location")),
    tabPanel("Background",
             h3("Traffic Data"),
             p("Citywide and location specific traffic data is pulled hourly from TomTom, a Dutch location technology firm. TomTom sources their traffic information from government and third-party data (e.g., survey vehicles, GPS traces, community input, and vehicle sensor data). Data is obtained through TomTom's live traffic index for Washington, D.C. as well as their Traffic API. Below is the information available for the citywide and location search features. Data is then merged with information from the DC GIS Master Address Repository to allow for additional location attributed (e.g., quadrants, wards, zip codes)."),
             bsCollapse(id = "collapseTraffic",
                        bsCollapsePanel("Citywide",
                                        tableOutput("hourly.back")),
                        bsCollapsePanel("Location Search",
                                        tableOutput("traffic.flow.back"))),
             h3("Air Quality"),
             p("Air quality data is pulled from AirNow, the United States government's hub for air quality data. AirNow is a partnership between the U.S. Environmental Protection Agency, National Oceanic and Atmospheric Administration, National Park Service, NASA, Center for Disease Control, and tribal, state, and local air quality agencies. The data encompasses the U.S. Air Quality Index (AQI) which translates raw concentration levels of air pollutants into an index on whether the air quality is healthy or unhealthy."),
             bsCollapse(id = "collapseAir",
                        bsCollapsePanel("AQI Values",
                                        tableOutput("airnow.back"))),
             p("The AirNow sensors in Washington, D.C. include air quality data for four major pollutants - Ozone, Particulate Matter (PM 2.5), Sulfur Dioxide, and Nitrogen Dioxide. Information on the conversion of raw concentrations of the four pollutants into AQI scores as well as the recommended course of action for each of the pollutants at the various AQI levels is provided below."),
             bsCollapse(id = "collapseAir2",
                        bsCollapsePanel("Raw Concentrations for AQI",
                                        tableOutput("concentrations.back")),
                        bsCollapsePanel("Recommended Actions",
                                        tableOutput("pollutants.back"))),
             h3("Weather"),
             p("Weather data is pulled from Open Weather, an online service providing global weather data. Data is obtained hourly at the citywide level. Below is the information available for the citywide search feature."),
             bsCollapse(id = "collapseWeather",
                        bsCollapsePanel("Weather Metrics",
                                        tableOutput("weather.back"))),
             h3("Additional Information"),
             bsCollapse(id = "collapseAdditional",
                        bsCollapsePanel("Reducing Emissions",
                                        p("For information on how to decrease transportation emissions, please visit the Environmental Protection Agency's guide to reducing pollution from vehicles and engines: https://www.epa.gov/transportation-air-pollution-and-climate-change/what-you-can-do-reduce-pollution-vehicles-and-engines.")),
                        bsCollapsePanel("Contributors",
                                        p("Assessment of Air Quality and Traffic Volume tool was compiled and composed by Chace Paulson, Minh-Tuan Nguyen, and Shalini Ramachandra."),
                                        p("Advisory team includes Maria Barouti, Zois Boukouvalas, and Konstantinos Koukoulakis.")),
                        bsCollapsePanel("Sources",
                                        p("TomTom, Behind the Map: How We Keep Our Maps Up to Date: https://www.tomtom.com/blog/maps/continuous-map-processing/#:~:text=Our%20multi%2Dsource%20approach%20combines,will%20not%20deteriorate%20over%20time."),
                                        p("TomTom, Washington taffic: https://www.tomtom.com/en_gb/traffic-index/washington-traffic/."),
                                        p("TomTom, Traffic API: https://developer.tomtom.com/traffic-api/traffic-api-documentation-traffic-flow/flow-segment-data#request-data."),
                                        p("Open Data DC, Street Segments: https://opendata.dc.gov/datasets/street-segments-retired."),
                                        p("Open Data DC, DC Quadrants: https://opendata.dc.gov/datasets/dc-quadrants?geometry=-77.838%2C38.707%2C-76.190%2C39.081."),
                                        p("Open Data DC, Ward from 2012: https://opendata.dc.gov/datasets/ward-from-2012."),
                                        p("Open Data DC, Zip Codes: https://opendata.dc.gov/datasets/zip-codes."),
                                        p("Open Data DC, Advisory Neighborhood Commissions from 2013: https://opendata.dc.gov/datasets/advisory-neighborhood-commissions-from-2013?geometry=-77.838%2C38.707%2C-76.190%2C39.081."),
                                        p("Open Data DC, Census Tracts - 1990: https://opendata.dc.gov/datasets/census-tracts-1990."),
                                        p("Open Data DC, Single Member District from 2013: https://opendata.dc.gov/datasets/single-member-district-from-2013."),
                                        p("Open Data DC, Voting Precinct - 2012: https://opendata.dc.gov/datasets/voting-precinct-2012."),
                                        p("DC.gov Office of the Chief Technology Officer, DC GIS Master Address Repository: http://dcatlas.dcgis.dc.gov/mar/."),
                                        p("AirNow, About AirNow: https://www.airnow.gov/about-airnow/."),
                                        p("AirNow, AQI Basics: https://www.airnow.gov/aqi/aqi-basics/."),
                                        p("AirNow, AirNow API - Web Services: https://docs.airnowapi.org/webservices."),
                                        p("AirNow, Technical Assistance Document for the Reporting of Daily Air Quality - the Air Quality Index (AQI): https://www.airnow.gov/sites/default/files/2020-05/aqi-technical-assistance-document-sept2018.pdf."),
                                        p("OpenWeather, Current Weather Data: https://openweathermap.org/current#cityid."))))
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
  
  ## Overview numbers
  output$current.air.quality.o <- renderText({
    
    cs1 <- hourly %>% 
      filter(date == as_datetime(str_c(as.character(input$date2), 
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
      ls3 <- str_c("Average air quality one week ago: ", "Good")
    }else if(ls2 == 2){
      ls3 <- str_c("Average air quality one week ago: ", "Moderate")
    }else if(ls2 == 3){
      ls3 <- str_c("Average air quality one week ago: ", "Unhealthy for sensitive groups")
    }else if(ls2 == 4){
      ls3 <- str_c("Average air quality one week ago: ", "Unhealthy")
    }else if(ls2 == 5){
      ls3 <- str_c("Average air quality one week ago: ", "Very unhealthy")
    }else if(ls2 == 6){
      ls3 <- str_c("Average air quality one week ago: ", "Hazardous")
    }else if(ls2 == "NaN"){
      ls3 <- str_c("Average air quality one week ago: ", "NaN")
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
      filter(date == as_datetime(str_c(as.character(input$date2), 
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
    ls3 <- str_c("Traffic congestion level one week ago: ", ls2, "%")
    
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
  output$current.temperature.o <- renderText({
    
    cs1 <- hourly %>% 
      filter(date == as_datetime(str_c(as.character(input$date2), 
                                       " ", 
                                       as.character(input$hour2), 
                                       ":00:00")))
    
    ls1 <- hourly %>% 
      filter(date == as_datetime(str_c(as.character(input$date2), 
                                       " ", 
                                       as.character(input$hour2), 
                                       ":00:00")) - 604800)
    
    cs2 <- mean(cs1$temp, na.rm = TRUE)
    ls2 <- mean(ls1$temp, na.rm = TRUE)
    
    cs3 <- str_c("Current temperature: ", cs2)
    ls3 <- str_c("Temperature one week ago: ", ls2)
    
    if(input$include.weather == TRUE){
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
      str_c("One Week Ago")
    }
  })
  output$aqitable <- renderTable({
    hourly <- hourly %>% 
      filter(date == as_datetime(str_c(as.character(input$date2), 
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
      filter(date == as_datetime(str_c(as.character(input$date2), 
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
    
    df2 <- data.frame(time = c("one week ago"),
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
  output$weathertable <- renderTable({
    cs1 <- hourly %>% 
      filter(date == as_datetime(str_c(as.character(input$date2), 
                                       " ", 
                                       as.character(input$hour2), 
                                       ":00:00"))) %>% 
      dplyr::select(temp, weather_description, humidity, wind_speed) %>% 
      distinct()
    
    if(nrow(cs1) == 0){
      cs1 <- data.frame(temp = NA, 
                        weather_description = NA,
                        humidity = NA,
                        wind_speed = NA)
    }
    
    df1 <- data.frame(time = c("current"),
                      temperatre = round(cs1$temp, digits = 2),
                      weather_description = cs1$weather_description,
                      humidity = round(cs1$humidity, digits = 2),
                      wind_speed = round(cs1$wind_speed, digits = 2))
    
    ls1 <- hourly %>% 
      filter(date == as_datetime(str_c(as.character(input$date2), 
                                       " ", 
                                       as.character(input$hour2), 
                                       ":00:00")) - 604800) %>% 
      dplyr::select(temp, weather_description, humidity, wind_speed) %>% 
      distinct()
    
    df2 <- data.frame(time = c("one week ago"),
                      temperatre = round(ls1$temp, digits = 2),
                      weather_description = ls1$weather_description,
                      humidity = round(ls1$humidity, digits = 2),
                      wind_speed = round(ls1$wind_speed, digits = 2))
    
    if(input$temp == TRUE){
      if(input$weather.des == TRUE){
        if(input$humidity == TRUE){
          if(input$wind.speed == TRUE){
            df1 <- df1[,c(1, 2, 3, 4, 5)]
          }else if(input$wind.speed == FALSE){
            df1 <- df1[,c(1, 2, 3, 4)]
          }
        }else if(input$humidity == FALSE){
          if(input$wind.speed == TRUE){
            df1 <- df1[,c(1, 2, 3, 5)]
          }else if(input$wind.speed == FALSE){
            df1 <- df1[,c(1, 2, 3)]
          }
        }
      }else if(input$weather.des == FALSE){
        if(input$humidity == TRUE){
          if(input$wind.speed == TRUE){
            df1 <- df1[,c(1, 2, 4, 5)]
          }else if(input$wind.speed == FALSE){
            df1 <- df1[,c(1, 2, 4)]
          }
        }else if(input$humidity == FALSE){
          if(input$wind.speed == TRUE){
            df1 <- df1[,c(1, 2, 5)]
          }else if(input$wind.speed == FALSE){
            df1 <- df1[,c(1, 2)]
          }
        }
      }
    }else if(input$temp == FALSE){
      if(input$weather.des == TRUE){
        if(input$humidity == TRUE){
          if(input$wind.speed == TRUE){
            df1 <- df1[,c(1, 3, 4, 5)]
          }else if(input$wind.speed == FALSE){
            df1 <- df1[,c(1, 3, 4)]
          }
        }else if(input$humidity == FALSE){
          if(input$wind.speed == TRUE){
            df1 <- df1[,c(1, 3, 5)]
          }else if(input$wind.speed == FALSE){
            df1 <- df1[,c(1, 3)]
          }
        }
      }else if(input$weather.des == FALSE){
        if(input$humidity == TRUE){
          if(input$wind.speed == TRUE){
            df1 <- df1[,c(1, 4, 5)]
          }else if(input$wind.speed == FALSE){
            df1 <- df1[,c(1, 4)]
          }
        }else if(input$humidity == FALSE){
          if(input$wind.speed == TRUE){
            df1 <- df1[,c(1, 5)]
          }else if(input$wind.speed == FALSE){
            df1 <- df1[,c(1)]
          }
        }
      }
    }
    
    if(input$temp == TRUE){
      if(input$weather.des == TRUE){
        if(input$humidity == TRUE){
          if(input$wind.speed == TRUE){
            df2 <- df2[,c(1, 2, 3, 4, 5)]
          }else if(input$wind.speed == FALSE){
            df2 <- df2[,c(1, 2, 3, 4)]
          }
        }else if(input$humidity == FALSE){
          if(input$wind.speed == TRUE){
            df2 <- df2[,c(1, 2, 3, 5)]
          }else if(input$wind.speed == FALSE){
            df2 <- df2[,c(1, 2, 3)]
          }
        }
      }else if(input$weather.des == FALSE){
        if(input$humidity == TRUE){
          if(input$wind.speed == TRUE){
            df2 <- df2[,c(1, 2, 4, 5)]
          }else if(input$wind.speed == FALSE){
            df2 <- df2[,c(1, 2, 4)]
          }
        }else if(input$humidity == FALSE){
          if(input$wind.speed == TRUE){
            df2 <- df2[,c(1, 2, 5)]
          }else if(input$wind.speed == FALSE){
            df2 <- df2[,c(1, 2)]
          }
        }
      }
    }else if(input$temp == FALSE){
      if(input$weather.des == TRUE){
        if(input$humidity == TRUE){
          if(input$wind.speed == TRUE){
            df2 <- df2[,c(1, 3, 4, 5)]
          }else if(input$wind.speed == FALSE){
            df2 <- df2[,c(1, 3, 4)]
          }
        }else if(input$humidity == FALSE){
          if(input$wind.speed == TRUE){
            df2 <- df2[,c(1, 3, 5)]
          }else if(input$wind.speed == FALSE){
            df2 <- df2[,c(1, 3)]
          }
        }
      }else if(input$weather.des == FALSE){
        if(input$humidity == TRUE){
          if(input$wind.speed == TRUE){
            df2 <- df2[,c(1, 4, 5)]
          }else if(input$wind.speed == FALSE){
            df2 <- df2[,c(1, 4)]
          }
        }else if(input$humidity == FALSE){
          if(input$wind.speed == TRUE){
            df2 <- df2[,c(1, 5)]
          }else if(input$wind.speed == FALSE){
            df2 <- df2[,c(1)]
          }
        }
      }
    }
    
    if(input$include.weather == TRUE){
      if(input$currenttime.o == TRUE){
        if(input$lastweek.o == TRUE){
          rbind(df1[1,], df2[1,])
        }else{
          df1[1,]
        }
      }else{
        if(input$lastweek.o == TRUE){
          df2[1,]
        }else{
          
        }
      }
    }
    
  })
  
  ## Bar chart 
  output$airtable.graph2 <- renderPlot({
    if(input$sensor.type == "Average Across Sensors"){  
      x <- parameter_wider %>% 
        filter(agency == "District of Columbia - Department of Energy and Environment")
    }else if(input$sensor.type == "Single Sensor"){
      x <- parameter_wider %>% 
        filter(location == input$location.type)
    }
    
    cs1 <- x %>% 
      filter(date == as_datetime(str_c(as.character(input$date2), 
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
                NO2 = mean(NO2, na.rm = TRUE),
                .groups = 'drop')
    
    cs3a <- cs3
    
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
    
    if(max(cs3a$PM2.5, na.rm = TRUE) <=150 | sum(is.na(cs3a$PM2.5)) == nrow(cs3a)){
      p2 <- ggplot(data = cs3, aes(x = param, y = value, fill = as.character(date))) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(x = "Air Quality Parameters", y = "Air Quality Index", fill = "Date",
             title = "Weekly Comparison of AQI") +
        annotate("text", x = unique(cs3$param)[1], y = 45, label = "Healthy") +
        geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
        annotate("text", x = unique(cs3$param)[1], y = 95, label = "Moderate") +
        geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
        annotate("text", x = unique(cs3$param)[1], y = 145, label = "Unhealthy for Sensitive Groups") +
        geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1)
    }else if(max(cs3a$PM2.5, na.rm = TRUE) <=200 & max(cs3a$PM2.5, na.rm = TRUE) > 150){
      p2 <- ggplot(data = cs3, aes(x = param, y = value, fill = as.character(date))) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(x = "Air Quality Parameters", y = "Air Quality Index", fill = "Date",
             title = "Weekly Comparison of AQI and Traffic") +
        annotate("text", x = unique(cs3$param)[1], y = 45, label = "Healthy") +
        geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
        annotate("text", x = unique(cs3$param)[1], y = 95, label = "Moderate") +
        geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
        annotate("text", x = unique(cs3$param)[1], y = 145, label = "Unhealthy for Sensitive Groups") +
        geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) + 
        annotate("text", x = unique(cs3$param)[1], y = 195, label = "Unhealthy") +
        geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1)
    }else if(max(cs3a$PM2.5, na.rm = TRUE) <=300 & max(cs3a$PM2.5, na.rm = TRUE) > 200){
      p2 <- ggplot(data = cs3, aes(x = param, y = value, fill = as.character(date))) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(x = "Air Quality Parameters", y = "Air Quality Index", fill = "Date",
             title = "Weekly Comparison of AQI and Traffic") +
        annotate("text", x = unique(cs3$param)[1], y = 45, label = "Healthy") +
        geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
        annotate("text", x = unique(cs3$param)[1], y = 95, label = "Moderate") +
        geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
        annotate("text", x = unique(cs3$param)[1], y = 145, label = "Unhealthy for Sensitive Groups") +
        geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) + 
        annotate("text", x = unique(cs3$param)[1], y = 195, label = "Unhealthy") +
        geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
        annotate("text", x = unique(cs3$param)[1], y = 295, label = "Very Unhealthy") +
        geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1)
    }else if(max(cs3a$PM2.5, na.rm = TRUE) > 300){
      p2 <- ggplot(data = cs3, aes(x = param, y = value, fill = as.character(date))) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(x = "Air Quality Parameters", y = "Air Quality Index", fill = "Date",
             title = "Weekly Comparison of AQI and Traffic") +
        annotate("text", x = unique(cs3$param)[1], y = 45, label = "Healthy") +
        geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
        annotate("text", x = unique(cs3$param)[1], y = 95, label = "Moderate") +
        geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
        annotate("text", x = unique(cs3$param)[1], y = 145, label = "Unhealthy for Sensitive Groups") +
        geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) + 
        annotate("text", x = unique(cs3$param)[1], y = 195, label = "Unhealthy") +
        geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
        annotate("text", x = unique(cs3$param)[1], y = 295, label = "Very Unhealthy") +
        geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) +
        annotate("text", x = unique(cs3$param)[1], y = 395, label = "Hazardous") +
        geom_hline(yintercept = 401, linetype = "dashed", color = "brown", size = 1)
    }
    
    cs4 <- com.s1 %>% 
      group_by(date) %>% 
      summarize(Traffic_Index = mean(traffic_index_live, na.rm = TRUE),
                Jams_Delay = mean(jams_delay, na.rm = TRUE),
                Jams_Length = mean(jams_length, na.rm = TRUE),
                Jams_Count = mean(jams_count, na.rm = TRUE),
                .groups = 'drop')
    
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
      labs(x = "Traffic Parameters", y = "Traffic Value", fill = "Date",
           title = "Weekly Comparison of Traffic")
    
    cs5 <- com.s1 %>% 
      group_by(date) %>% 
      summarize(Temperature = mean(temp, na.rm = TRUE),
                Humidity = mean(humidity, na.rm = TRUE),
                Wind_Speed = mean(wind_speed, na.rm = TRUE),
                .groups = 'drop')
    
    if(input$temp == TRUE){
      if(input$humidity == TRUE){
        if(input$wind.speed == TRUE){
          cs5 <- cs5[,c(1, 2, 3, 4)]
        }else if(input$wind.speed == FALSE){
          cs5 <- cs5[,c(1, 2, 3)]
        }
      }else if(input$humidity == FALSE){
        if(input$wind.speed == TRUE){
          cs5 <- cs5[,c(1, 2, 4)]
        }else if(input$wind.speed == FALSE){
          cs5 <- cs5[,c(1, 2)]
        }
      }
    }else if(input$temp == FALSE){
      if(input$humidity == TRUE){
        if(input$wind.speed == TRUE){
          cs5 <- cs5[,c(1, 3, 4)]
        }else if(input$wind.speed == FALSE){
          cs5 <- cs5[,c(1, 3)]
        }
      }else if(input$humidity == FALSE){
        if(input$wind.speed == TRUE){
          cs5 <- cs5[,c(1, 4)]
        }else if(input$wind.speed == FALSE){
          cs5 <- cs5[,c(1)]
        }
      }
    }
    
    cs5 <- pivot_longer(cs5, -date, names_to = "param")
    
    cs5$date <- str_c(substr(cs5$date, start = 1, stop = 10), " T",
                      substr(cs5$date, start = 12, stop = 13))
    
    p3 <- ggplot(data = cs5, aes(x = param, y = value, fill = as.character(date))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Weather Parameters", y = "Weather Value", fill = "Date",
           title = "Weekly Comparison of Weather")
    
    if(input$include.weather == TRUE){
      grid.arrange(p2, p1, p3, ncol = 2) 
    }else if(input$include.weather == FALSE){
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
        dplyr::select(date, traffic_index_live) 
      
      param.slim <- param.slim[,2:3]
      
      param.slim <- param.slim %>% 
        distinct()
      
      x <- x %>% 
        group_by(date) %>% 
        summarize(Ozone = mean(OZONE, na.rm = TRUE),
                  SO2 = mean(SO2, na.rm = TRUE),
                  PM2.5 = mean(PM2.5, na.rm = TRUE),
                  NO2 = mean(NO2, na.rm = TRUE),
                  .groups = 'drop')
      
      x <- merge(x, param.slim, by = "date")
      
      colors <- c("Ozone" = "blue", "PM2.5" = "orange", 
                  "SO2" = "red", "NO2" = "green")
      
      #ptitle <- str_c("AQI and Traffic")
      if(max(parameter_wider$PM2.5, na.rm = TRUE) <=150 | sum(is.na(parameter_wider$PM2.5)) == nrow(parameter_wider)){
        p1 <- ggplot(data = x, aes(x = date)) +
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
      }else if(max(parameter_wider$PM2.5, na.rm = TRUE) <=200 & max(parameter_wider$PM2.5, na.rm = TRUE) > 150){
        p1 <- ggplot(data = x, aes(x = date)) +
          labs(x = "Date", y = "Air Quality Index",
               title = "AQI and Traffic",
               color = "Parameter") +
          scale_color_manual(values = colors) +
          annotate("text", x = as_datetime("2020-10-28 22:00:00"), y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1.5) +
          annotate("text", x = as_datetime("2020-10-28 22:00:00"), y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1.5) +
          annotate("text", x = as_datetime("2020-10-31 18:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1.5) +
          annotate("text", x = as_datetime("2020-10-28 22:00:00"), y = 195, label = "Unhealthy") +
          geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1.5) 
      }else if(max(parameter_wider$PM2.5, na.rm = TRUE) <=300 & max(parameter_wider$PM2.5, na.rm = TRUE) > 200){
        p1 <- ggplot(data = x, aes(x = date)) +
          labs(x = "Date", y = "Air Quality Index",
               title = "AQI and Traffic",
               color = "Parameter") +
          scale_color_manual(values = colors) +
          annotate("text", x = as_datetime("2020-10-28 22:00:00"), y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1.5) +
          annotate("text", x = as_datetime("2020-10-28 22:00:00"), y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1.5) +
          annotate("text", x = as_datetime("2020-10-31 18:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1.5) +
          annotate("text", x = as_datetime("2020-10-28 22:00:00"), y = 195, label = "Unhealthy") +
          geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1.5) +
          annotate("text", x = as_datetime("2020-10-28 22:00:00"), y = 295, label = "Very Unhealthy") +
          geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1.5) 
      }else if(max(parameter_wider$PM2.5, na.rm = TRUE) > 300){
        p1 <- ggplot(data = x, aes(x = date)) +
          labs(x = "Date", y = "Air Quality Index",
               title = "AQI and Traffic",
               color = "Parameter") +
          scale_color_manual(values = colors) +
          annotate("text", x = as_datetime("2020-10-28 22:00:00"), y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1.5) +
          annotate("text", x = as_datetime("2020-10-28 22:00:00"), y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1.5) +
          annotate("text", x = as_datetime("2020-10-31 18:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1.5) +
          annotate("text", x = as_datetime("2020-10-28 22:00:00"), y = 195, label = "Unhealthy") +
          geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1.5) +
          annotate("text", x = as_datetime("2020-10-28 22:00:00"), y = 295, label = "Very Unhealthy") +
          geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1.5) +
          annotate("text", x = as_datetime("2020-10-28 22:00:00"), y = 395, label = "Hazardous") +
          geom_hline(yintercept = 401, linetype = "dashed", color = "brown", size = 1.5) 
      }
      
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
        dplyr::select(date, traffic_index_live, jams_delay, jams_length, jams_count) 
      
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
      
      param.slim <- parameter_wider %>% 
        dplyr::select(date, temp, humidity, wind_speed) 
      
      param.slim <- param.slim %>% 
        distinct() %>% 
        rename(Temperature = temp,
               Humidity = humidity,
               Wind_Speed = wind_speed)
      
      ps1 <- param.slim %>% 
        filter(date == min(param.slim$date))
      
      param.slim <- param.slim[complete.cases(param.slim), ]
      param.slim <- rbind(ps1, param.slim)
      
      colors <- c("Temperature" = "blue", "Humidity" = "orange", 
                  "Wind_Speed" = "red")
      
      #ptitle <- str_c("AQI and Traffic")
      p1 <- 
        ggplot(data = param.slim, aes(x = date)) +
        labs(x = "Date", y = "Weather Value", title = "Weather",
             color = "Parameter") +
        scale_color_manual(values = colors) 
      
      pt <- geom_line(aes(y = Temperature, color = "Temperature"))
      ph <- geom_line(aes(y = Humidity, color = "Humidity"))
      pw <- geom_line(aes(y = Wind_Speed, color = "Wind_Speed"))
      #pt <- labs(title = ptitle, x = "Date", y = "")
      
      if(input$temp == TRUE){
        if(input$humidity == TRUE){
          if(input$wind.speed == TRUE){
            plot3 <- p1 + pt + ph + pw
          } else if(input$wind.speed == FALSE){
            plot3 <- p1 + pt + ph
          }
        }else if(input$humidity == FALSE){
          if(input$wind.speed == TRUE){
            plot3 <- p1 + pt + pw
          } else if(input$wind.speed == FALSE){
            plot3 <- p1 + pt
          }
        }
      } else if(input$temp == FALSE){
        if(input$humidity == TRUE){
          if(input$wind.speed == TRUE){
            plot3 <- p1 + ph + pw
          } else if(input$wind.speed == FALSE){
            plot3 <- p1 + ph
          }
        }else if(input$humidity == FALSE){
          if(input$wind.speed == TRUE){
            plot3 <- p1 + pw
          } else if(input$wind.speed == FALSE){
            plot3 <- p1 
          }
        }
      }
      
      if(input$include.weather == TRUE){
        grid.arrange(plot1, plot2, plot3, ncol = 2)
      }else if(input$include.weather == FALSE){
        grid.arrange(plot1, plot2, ncol = 2)
      }
      
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
      str_c("One Week Ago")
    }
  })
  output$airtable <- renderTable({
    if(input$currenttime == TRUE){
      cs1 <- traffic.flow %>% 
        filter(date == as_datetime(str_c(as.character(input$date1), 
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
      cs1 <- traffic.flow %>% 
        filter(date == as_datetime(str_c(as.character(input$date1), 
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
                  NO2 = mean(no2_loc_aqi, na.rm = TRUE),
                  .groups = 'drop')
      
      cs3a <- cs3
      
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
      
      if(max(cs3a$PM2.5, na.rm = TRUE) <=150 | sum(is.na(cs3a$PM2.5)) == nrow(cs3a)){
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
      }else if(max(cs3a$PM2.5, na.rm = TRUE) <=200 & max(cs3a$PM2.5, na.rm = TRUE) > 150){
        p2 <- ggplot(data = cs3, aes(x = param, y = value, fill = as.character(date))) +
          geom_bar(stat = "identity", position = "dodge") +
          labs(x = "Air Quality Parameters", y = "Air Quality Index", fill = "Date",
               title = "Weekly Comparison of AQI and Traffic") +
          annotate("text", x = unique(cs3$param)[1], y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
          annotate("text", x = unique(cs3$param)[1], y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
          annotate("text", x = unique(cs3$param)[1], y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) + 
          annotate("text", x = unique(cs3$param)[1], y = 195, label = "Unhealthy") +
          geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1)
      }else if(max(cs3a$PM2.5, na.rm = TRUE) <=300 & max(cs3a$PM2.5, na.rm = TRUE) > 200){
        p2 <- ggplot(data = cs3, aes(x = param, y = value, fill = as.character(date))) +
          geom_bar(stat = "identity", position = "dodge") +
          labs(x = "Air Quality Parameters", y = "Air Quality Index", fill = "Date",
               title = "Weekly Comparison of AQI and Traffic") +
          annotate("text", x = unique(cs3$param)[1], y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
          annotate("text", x = unique(cs3$param)[1], y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
          annotate("text", x = unique(cs3$param)[1], y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) + 
          annotate("text", x = unique(cs3$param)[1], y = 195, label = "Unhealthy") +
          geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
          annotate("text", x = unique(cs3$param)[1], y = 295, label = "Very Unhealthy") +
          geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1)
      }else if(max(cs3a$PM2.5, na.rm = TRUE) > 300){
        p2 <- ggplot(data = cs3, aes(x = param, y = value, fill = as.character(date))) +
          geom_bar(stat = "identity", position = "dodge") +
          labs(x = "Air Quality Parameters", y = "Air Quality Index", fill = "Date",
               title = "Weekly Comparison of AQI and Traffic") +
          annotate("text", x = unique(cs3$param)[1], y = 45, label = "Healthy") +
          geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
          annotate("text", x = unique(cs3$param)[1], y = 95, label = "Moderate") +
          geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
          annotate("text", x = unique(cs3$param)[1], y = 145, label = "Unhealthy for Sensitive Groups") +
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) + 
          annotate("text", x = unique(cs3$param)[1], y = 195, label = "Unhealthy") +
          geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
          annotate("text", x = unique(cs3$param)[1], y = 295, label = "Very Unhealthy") +
          geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) +
          annotate("text", x = unique(cs3$param)[1], y = 395, label = "Hazardous") +
          geom_hline(yintercept = 401, linetype = "dashed", color = "brown", size = 1)
      }
      
      cs4 <- cs2 %>% 
        group_by(date) %>% 
        summarize(Current_Speed = mean(current_speed, na.rm = TRUE),
                  Free_Flow_Speed = mean(free_flow_speed, na.rm = TRUE),
                  Current_Travel_Time = mean(current_travel_time, na.rm = TRUE),
                  Free_Flow_Travel_Time = mean(free_flow_travel_time, na.rm = TRUE),
                  .groups = 'drop')
      
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
  })
  
  ## Overview Numbers 
  output$current.speed <- renderText({
    cs1 <- traffic.flow %>% 
      filter(date == as_datetime(str_c(as.character(input$date1), 
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
              "Speed one week ago: ", round(ls2, digits = 0), " mph (Free flow speed: ", round(ls3, digits = 0), " mph)")
      }else{
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }
    }else{
      if(input$lastweek == TRUE){
        str_c("Speed one week ago: ", round(ls2, digits = 0), " mph (Free flow speed: ", round(ls3, digits = 0), " mph)")
      }else{
        
      }
    }
    
    
  })
  output$current.travel.time <- renderText({
    cs1 <- traffic.flow %>% 
      filter(date == as_datetime(str_c(as.character(input$date1), 
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
              "Travel time one week ago: ", round(ls2, digits = 0), " mph (Free flow travel time: ", round(ls3, digits = 0), " mph)")
      }else{
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }
    }else{
      if(input$lastweek == TRUE){
        str_c("Travel time one week ago: ", round(ls2, digits = 0), " mph (Free flow travel time: ", round(ls3, digits = 0), " mph)")
      }else{
        
      }
    }
    
    
  })
  output$current.air.quality <- renderText({
    cs1 <- traffic.flow %>% 
      filter(date == as_datetime(str_c(as.character(input$date1), 
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
      ls3 <- str_c("Average air quality one week ago: ", "Good")
    }else if(ls2 == 2){
      ls3 <- str_c("Average air quality one week ago: ", "Moderate")
    }else if(ls2 == 3){
      ls3 <- str_c("Average air quality one week ago: ", "Unhealthy for sensitive groups")
    }else if(ls2 == 4){
      ls3 <- str_c("Average air quality one week ago: ", "Unhealthy")
    }else if(ls2 == 5){
      ls3 <- str_c("Average air quality one week ago: ", "Very unhealthy")
    }else if(ls2 == 6){
      ls3 <- str_c("Average air quality one week ago: ", "Hazardous")
    }else if(ls2 == "NaN"){
      ls3 <- str_c("Average air quality one week ago: ", "NaN")
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
                  mean_no2_loc_aqi = mean(no2_loc_aqi, na.rm = TRUE),
                  .groups = 'drop')
      
      cs4 <- cs2 %>% 
        group_by(date) %>% 
        summarize(mean_current_speed = mean(current_speed, na.rm = TRUE),
                  mean_free_flow_speed = mean(free_flow_speed, na.rm = TRUE),
                  mean_current_travel_time = mean(current_travel_time, na.rm = TRUE),
                  mean_free_flow_travel_time = mean(free_flow_travel_time, na.rm = TRUE),
                  .groups = 'drop')
      
      colors <- c("Ozone" = "blue", "SO2" = "red", "PM 2.5" = "Orange",
                  "NO2" = "green")
      
      if(max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) <=150 | sum(is.na(cs3$mean_pm2.5_loc_aqi)) == nrow(cs3)){
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
      }else if(max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) <=200 & max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) > 150){
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
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
          geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1.5) 
      }else if(max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) <=300 & max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) > 200){
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
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
          geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1.5) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
          geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1.5) 
      }else if(max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) > 300){
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
          geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
          geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1.5) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
          geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1.5) +
          annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 395, label = "Hazardous") +
          geom_hline(yintercept = 401, linetype = "dashed", color = "brown", size = 1.5) 
      }
      
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
    traffic.flow <- traffic.flow %>% 
      mutate(ward = as.character(ward), 
             voter_precinct = as.character(voter_precinct))
    
    if(input$historical == TRUE){
      if(input$loc.type == "Quadrant"){
        cs3 <- traffic.flow %>% 
          group_by(date, quadrant) %>% 
          summarize(mean_ozone_loc_aqi = mean(ozone_loc_aqi, na.rm = TRUE),
                    mean_so2_loc_aqi = mean(so2_loc_aqi, na.rm = TRUE),
                    mean_pm2.5_loc_aqi = mean(pm2.5_loc_aqi, na.rm = TRUE),
                    mean_no2_loc_aqi = mean(no2_loc_aqi, na.rm = TRUE),
                    .groups = 'drop')
        
        if(max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) <= 150 | sum(is.na(cs3$mean_pm2.5_loc_aqi)) == nrow(cs3)){
          p1 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "Ozone AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_ozone_loc_aqi, color = quadrant)) +
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
        }else if(max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) <=200 & max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) > 150){
          p1 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "Ozone AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_ozone_loc_aqi, color = quadrant)) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) 
          
          p2 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "SO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_so2_loc_aqi, color = quadrant))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) 
          
          p3 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "PM 2.5 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_pm2.5_loc_aqi, color = quadrant))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) 
          
          p4 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "NO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_no2_loc_aqi, color = quadrant))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) + 
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) 
        }else if(max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) <=300 & max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) > 200){
          p1 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "Ozone AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_ozone_loc_aqi, color = quadrant)) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) 
          
          p2 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "SO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_so2_loc_aqi, color = quadrant))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) 
          
          p3 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "PM 2.5 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_pm2.5_loc_aqi, color = quadrant))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) 
          
          p4 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "NO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_no2_loc_aqi, color = quadrant))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) + 
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) 
        }else if(max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) > 300){
          p1 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "Ozone AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_ozone_loc_aqi, color = quadrant)) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 395, label = "Hazardous") +
            geom_hline(yintercept = 401, linetype = "dashed", color = "brown", size = 1) 
          
          p2 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "SO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_so2_loc_aqi, color = quadrant))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 395, label = "Hazardous") +
            geom_hline(yintercept = 401, linetype = "dashed", color = "brown", size = 1) 
          
          p3 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "PM 2.5 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_pm2.5_loc_aqi, color = quadrant))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 395, label = "Hazardous") +
            geom_hline(yintercept = 401, linetype = "dashed", color = "brown", size = 1) 
          
          p4 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "NO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_no2_loc_aqi, color = quadrant))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) + 
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 395, label = "Hazardous") +
            geom_hline(yintercept = 401, linetype = "dashed", color = "brown", size = 1) 
        }
      }else if(input$loc.type == "Ward"){
        cs3 <- traffic.flow %>% 
          group_by(date, ward) %>% 
          summarize(mean_ozone_loc_aqi = mean(ozone_loc_aqi, na.rm = TRUE),
                    mean_so2_loc_aqi = mean(so2_loc_aqi, na.rm = TRUE),
                    mean_pm2.5_loc_aqi = mean(pm2.5_loc_aqi, na.rm = TRUE),
                    mean_no2_loc_aqi = mean(no2_loc_aqi, na.rm = TRUE),
                    .groups = 'drop')
        
        if(max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) <=150 | sum(is.na(cs3$mean_pm2.5_loc_aqi)) == nrow(cs3)){
          p1 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "Ozone AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_ozone_loc_aqi, color = ward)) +
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
        }else if(max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) <=200 & max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) > 150){
          p1 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "Ozone AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_ozone_loc_aqi, color = ward)) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) 
          
          p2 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "SO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_so2_loc_aqi, color = ward))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) 
          
          p3 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "PM 2.5 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_pm2.5_loc_aqi, color = ward))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) 
          
          p4 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "NO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_no2_loc_aqi, color = ward))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) + 
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) 
        }else if(max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) <=300 & max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) > 200){
          p1 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "Ozone AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_ozone_loc_aqi, color = ward)) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) 
          
          p2 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "SO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_so2_loc_aqi, color = ward))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) 
          
          p3 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "PM 2.5 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_pm2.5_loc_aqi, color = ward))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) 
          
          p4 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "NO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_no2_loc_aqi, color = ward))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) + 
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) 
        }else if(max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) > 300){
          p1 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "Ozone AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_ozone_loc_aqi, color = ward)) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 395, label = "Hazardous") +
            geom_hline(yintercept = 401, linetype = "dashed", color = "brown", size = 1) 
          
          p2 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "SO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_so2_loc_aqi, color = ward))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 395, label = "Hazardous") +
            geom_hline(yintercept = 401, linetype = "dashed", color = "brown", size = 1) 
          
          p3 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "PM 2.5 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_pm2.5_loc_aqi, color = ward))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 395, label = "Hazardous") +
            geom_hline(yintercept = 401, linetype = "dashed", color = "brown", size = 1) 
          
          p4 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "NO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_no2_loc_aqi, color = ward))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) + 
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 395, label = "Hazardous") +
            geom_hline(yintercept = 401, linetype = "dashed", color = "brown", size = 1) 
        }
      }else if(input$loc.type == "Zip Code"){
        cs3 <- traffic.flow %>% 
          group_by(date, zip_code) %>% 
          summarize(mean_ozone_loc_aqi = mean(ozone_loc_aqi, na.rm = TRUE),
                    mean_so2_loc_aqi = mean(so2_loc_aqi, na.rm = TRUE),
                    mean_pm2.5_loc_aqi = mean(pm2.5_loc_aqi, na.rm = TRUE),
                    mean_no2_loc_aqi = mean(no2_loc_aqi, na.rm = TRUE),
                    .groups = 'drop')
        
        if(max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) <=150 | sum(is.na(cs3$mean_pm2.5_loc_aqi)) == nrow(cs3)){
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
        }else if(max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) <=200 & max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) > 150){
          p1 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "Ozone AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_ozone_loc_aqi, color = zip_code)) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) 
          
          p2 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "SO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_so2_loc_aqi, color = zip_code))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) 
          
          p3 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "PM 2.5 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_pm2.5_loc_aqi, color = zip_code))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) 
          
          p4 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "NO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_no2_loc_aqi, color = zip_code))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) + 
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) 
        }else if(max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) <=300 & max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) > 200){
          p1 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "Ozone AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_ozone_loc_aqi, color = zip_code)) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) 
          
          p2 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "SO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_so2_loc_aqi, color = zip_code))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) 
          
          p3 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "PM 2.5 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_pm2.5_loc_aqi, color = zip_code))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) 
          
          p4 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "NO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_no2_loc_aqi, color = zip_code))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) + 
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) 
        }else if(max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) > 300){
          p1 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "Ozone AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_ozone_loc_aqi, color = zip_code)) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 395, label = "Hazardous") +
            geom_hline(yintercept = 401, linetype = "dashed", color = "brown", size = 1) 
          
          p2 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "SO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_so2_loc_aqi, color = zip_code))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 395, label = "Hazardous") +
            geom_hline(yintercept = 401, linetype = "dashed", color = "brown", size = 1) 
          
          p3 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "PM 2.5 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_pm2.5_loc_aqi, color = zip_code))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 395, label = "Hazardous") +
            geom_hline(yintercept = 401, linetype = "dashed", color = "brown", size = 1) 
          
          p4 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "NO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_no2_loc_aqi, color = zip_code))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) + 
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 395, label = "Hazardous") +
            geom_hline(yintercept = 401, linetype = "dashed", color = "brown", size = 1) 
        }
      }else if(input$loc.type == "Advisory Neighborhood Commission"){
        cs3 <- traffic.flow %>% 
          group_by(date, anc) %>% 
          summarize(mean_ozone_loc_aqi = mean(ozone_loc_aqi, na.rm = TRUE),
                    mean_so2_loc_aqi = mean(so2_loc_aqi, na.rm = TRUE),
                    mean_pm2.5_loc_aqi = mean(pm2.5_loc_aqi, na.rm = TRUE),
                    mean_no2_loc_aqi = mean(no2_loc_aqi, na.rm = TRUE),
                    .groups = 'drop')
        
        if(max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) <=150 | sum(is.na(cs3$mean_pm2.5_loc_aqi)) == nrow(cs3)){
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
        }else if(max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) <=200 & max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) > 150){
          p1 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "Ozone AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_ozone_loc_aqi, color = anc)) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) 
          
          p2 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "SO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_so2_loc_aqi, color = anc))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) 
          
          p3 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "PM 2.5 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_pm2.5_loc_aqi, color = anc))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) 
          
          p4 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "NO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_no2_loc_aqi, color = anc))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) + 
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) 
        }else if(max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) <=300 & max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) > 200){
          p1 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "Ozone AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_ozone_loc_aqi, color = anc)) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) 
          
          p2 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "SO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_so2_loc_aqi, color = anc))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) 
          
          p3 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "PM 2.5 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_pm2.5_loc_aqi, color = anc))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) 
          
          p4 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "NO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_no2_loc_aqi, color = anc))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) + 
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) 
        }else if(max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) > 300){
          p1 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "Ozone AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_ozone_loc_aqi, color = anc)) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 395, label = "Hazardous") +
            geom_hline(yintercept = 401, linetype = "dashed", color = "brown", size = 1) 
          
          p2 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "SO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_so2_loc_aqi, color = anc))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 395, label = "Hazardous") +
            geom_hline(yintercept = 401, linetype = "dashed", color = "brown", size = 1) 
          
          p3 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "PM 2.5 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_pm2.5_loc_aqi, color = anc))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 395, label = "Hazardous") +
            geom_hline(yintercept = 401, linetype = "dashed", color = "brown", size = 1) 
          
          p4 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "NO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_no2_loc_aqi, color = anc))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) + 
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 395, label = "Hazardous") +
            geom_hline(yintercept = 401, linetype = "dashed", color = "brown", size = 1) 
        }
      }else if(input$loc.type == "Census Tract"){
        cs3 <- traffic.flow %>% 
          group_by(date, census_tract) %>% 
          summarize(mean_ozone_loc_aqi = mean(ozone_loc_aqi, na.rm = TRUE),
                    mean_so2_loc_aqi = mean(so2_loc_aqi, na.rm = TRUE),
                    mean_pm2.5_loc_aqi = mean(pm2.5_loc_aqi, na.rm = TRUE),
                    mean_no2_loc_aqi = mean(no2_loc_aqi, na.rm = TRUE),
                    .groups = 'drop')
        
        if(max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) <=150 | sum(is.na(cs3$mean_pm2.5_loc_aqi)) == nrow(cs3)){
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
        }else if(max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) <=200 & max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) > 150){
          p1 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "Ozone AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_ozone_loc_aqi, color = census_tract)) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) 
          
          p2 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "SO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_so2_loc_aqi, color = census_tract))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) 
          
          p3 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "PM 2.5 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_pm2.5_loc_aqi, color = census_tract))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) 
          
          p4 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "NO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_no2_loc_aqi, color = census_tract))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) + 
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) 
        }else if(max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) <=300 & max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) > 200){
          p1 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "Ozone AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_ozone_loc_aqi, color = census_tract)) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) 
          
          p2 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "SO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_so2_loc_aqi, color = census_tract))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) 
          
          p3 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "PM 2.5 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_pm2.5_loc_aqi, color = census_tract))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) 
          
          p4 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "NO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_no2_loc_aqi, color = census_tract))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) + 
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) 
        }else if(max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) > 300){
          p1 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "Ozone AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_ozone_loc_aqi, color = census_tract)) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 395, label = "Hazardous") +
            geom_hline(yintercept = 401, linetype = "dashed", color = "brown", size = 1) 
          
          p2 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "SO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_so2_loc_aqi, color = census_tract))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 395, label = "Hazardous") +
            geom_hline(yintercept = 401, linetype = "dashed", color = "brown", size = 1) 
          
          p3 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "PM 2.5 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_pm2.5_loc_aqi, color = census_tract))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 395, label = "Hazardous") +
            geom_hline(yintercept = 401, linetype = "dashed", color = "brown", size = 1) 
          
          p4 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "NO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_no2_loc_aqi, color = census_tract))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) + 
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 395, label = "Hazardous") +
            geom_hline(yintercept = 401, linetype = "dashed", color = "brown", size = 1) 
        }
      }else if(input$loc.type == "Single Member District"){
        cs3 <- traffic.flow %>% 
          group_by(date, single_member_district) %>% 
          summarize(mean_ozone_loc_aqi = mean(ozone_loc_aqi, na.rm = TRUE),
                    mean_so2_loc_aqi = mean(so2_loc_aqi, na.rm = TRUE),
                    mean_pm2.5_loc_aqi = mean(pm2.5_loc_aqi, na.rm = TRUE),
                    mean_no2_loc_aqi = mean(no2_loc_aqi, na.rm = TRUE),
                    .groups = 'drop')
        
        if(max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) <=150 | sum(is.na(cs3$mean_pm2.5_loc_aqi)) == nrow(cs3)){
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
            geom_line(aes(y = mean_pm2.5_loc_aqi, color = single_member_district))  +
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
        }else if(max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) <=200 & max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) > 150){
          p1 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "Ozone AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_ozone_loc_aqi, color = single_member_district)) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) 
          
          p2 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "SO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_so2_loc_aqi, color = single_member_district))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) 
          
          p3 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "PM 2.5 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_pm2.5_loc_aqi, color = single_member_district))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) 
          
          p4 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "NO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_no2_loc_aqi, color = single_member_district))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) + 
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) 
        }else if(max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) <=300 & max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) > 200){
          p1 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "Ozone AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_ozone_loc_aqi, color = single_member_district)) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) 
          
          p2 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "SO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_so2_loc_aqi, color = single_member_district))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) 
          
          p3 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "PM 2.5 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_pm2.5_loc_aqi, color = single_member_district))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) 
          
          p4 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "NO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_no2_loc_aqi, color = single_member_district))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) + 
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) 
        }else if(max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) > 300){
          p1 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "Ozone AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_ozone_loc_aqi, color = single_member_district)) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 395, label = "Hazardous") +
            geom_hline(yintercept = 401, linetype = "dashed", color = "brown", size = 1) 
          
          p2 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "SO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_so2_loc_aqi, color = single_member_district))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 395, label = "Hazardous") +
            geom_hline(yintercept = 401, linetype = "dashed", color = "brown", size = 1) 
          
          p3 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "PM 2.5 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_pm2.5_loc_aqi, color = single_member_district))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 395, label = "Hazardous") +
            geom_hline(yintercept = 401, linetype = "dashed", color = "brown", size = 1) 
          
          p4 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "NO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_no2_loc_aqi, color = single_member_district))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) + 
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 395, label = "Hazardous") +
            geom_hline(yintercept = 401, linetype = "dashed", color = "brown", size = 1) 
        }
      }else if(input$loc.type == "Voter Precinct"){
        cs3 <- traffic.flow %>% 
          group_by(date, voter_precinct) %>% 
          summarize(mean_ozone_loc_aqi = mean(ozone_loc_aqi, na.rm = TRUE),
                    mean_so2_loc_aqi = mean(so2_loc_aqi, na.rm = TRUE),
                    mean_pm2.5_loc_aqi = mean(pm2.5_loc_aqi, na.rm = TRUE),
                    mean_no2_loc_aqi = mean(no2_loc_aqi, na.rm = TRUE),
                    .groups = 'drop')
        
        if(max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) <=150 | sum(is.na(cs3$mean_pm2.5_loc_aqi)) == nrow(cs3)){
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
        }else if(max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) <=200 & max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) > 150){
          p1 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "Ozone AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_ozone_loc_aqi, color = voter_precinct)) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) 
          
          p2 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "SO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_so2_loc_aqi, color = voter_precinct))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) 
          
          p3 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "PM 2.5 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_pm2.5_loc_aqi, color = voter_precinct))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) 
          
          p4 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "NO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_no2_loc_aqi, color = voter_precinct))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) + 
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) 
        }else if(max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) <=300 & max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) > 200){
          p1 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "Ozone AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_ozone_loc_aqi, color = voter_precinct)) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) 
          
          p2 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "SO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_so2_loc_aqi, color = voter_precinct))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) 
          
          p3 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "PM 2.5 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_pm2.5_loc_aqi, color = voter_precinct))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) 
          
          p4 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "NO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_no2_loc_aqi, color = voter_precinct))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) + 
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) 
        }else if(max(cs3$mean_pm2.5_loc_aqi, na.rm = TRUE) > 300){
          p1 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "Ozone AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_ozone_loc_aqi, color = voter_precinct)) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 395, label = "Hazardous") +
            geom_hline(yintercept = 401, linetype = "dashed", color = "brown", size = 1) 
          
          p2 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "SO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_so2_loc_aqi, color = voter_precinct))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 395, label = "Hazardous") +
            geom_hline(yintercept = 401, linetype = "dashed", color = "brown", size = 1) 
          
          p3 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "PM 2.5 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_pm2.5_loc_aqi, color = voter_precinct))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 395, label = "Hazardous") +
            geom_hline(yintercept = 401, linetype = "dashed", color = "brown", size = 1) 
          
          p4 <- ggplot(data = cs3, aes(x = date)) +
            labs(x = "Date", y = "NO2 AQI",
                 title = str_c(input$loc.type)) +
            geom_line(aes(y = mean_no2_loc_aqi, color = voter_precinct))  +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 45, label = "Healthy") +
            geom_hline(yintercept = 51, linetype = "dashed", color = "green", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 95, label = "Moderate") +
            geom_hline(yintercept = 101, linetype = "dashed", color = "yellow", size = 1) +
            annotate("text", x = as_datetime("2020-11-08 1:00:00"), y = 145, label = "Unhealthy for Sensitive Groups") +
            geom_hline(yintercept = 151, linetype = "dashed", color = "orange", size = 1) + 
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 195, label = "Unhealthy") +
            geom_hline(yintercept = 201, linetype = "dashed", color = "red", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 295, label = "Very Unhealthy") +
            geom_hline(yintercept = 301, linetype = "dashed", color = "purple", size = 1) +
            annotate("text", x = as_datetime("2020-11-06 10:00:00"), y = 395, label = "Hazardous") +
            geom_hline(yintercept = 401, linetype = "dashed", color = "brown", size = 1) 
        }
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
    traffic.flow <- traffic.flow %>% 
      mutate(ward = factor(ward), 
             voter_precinct = as.character(voter_precinct))
    
    if(input$historical == TRUE){
      if(input$loc.type == "Quadrant"){
        cs4 <- traffic.flow %>% 
          group_by(date, quadrant) %>% 
          summarize(mean_current_speed = mean(current_speed, na.rm = TRUE),
                    mean_free_flow_speed = mean(free_flow_speed, na.rm = TRUE),
                    mean_current_travel_time = mean(current_travel_time, na.rm = TRUE),
                    mean_free_flow_travel_time = mean(free_flow_travel_time, na.rm = TRUE),
                    .groups = 'drop')
        
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
                    mean_free_flow_travel_time = mean(free_flow_travel_time, na.rm = TRUE),
                    .groups = 'drop')
        
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
                    mean_free_flow_travel_time = mean(free_flow_travel_time, na.rm = TRUE),
                    .groups = 'drop')
        
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
                    mean_free_flow_travel_time = mean(free_flow_travel_time, na.rm = TRUE),
                    .groups = 'drop')
        
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
                    mean_free_flow_travel_time = mean(free_flow_travel_time, na.rm = TRUE),
                    .groups = 'drop')
        
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
                    mean_free_flow_travel_time = mean(free_flow_travel_time, na.rm = TRUE),
                    .groups = 'drop')
        
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
                    mean_free_flow_travel_time = mean(free_flow_travel_time, na.rm = TRUE),
                    .groups = 'drop')
        
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
  
  ## Location Search Maps 
  output$airtable.title3 <- renderText({
    if(input$currenttime == TRUE){
      str_c("Current Date")
    }
  })
  output$airtable.title4 <- renderText({
    if(input$lastweek == TRUE){
      str_c("One Week Ago")
    }
  })
  output$location.map <- renderPlot({
    if(input$loc.type == "Quadrant"){
      ## Quadrants 
      map_df <- readOGR(dsn = paste0(getwd(), "/data/shape_files/DC_Quadrants-shp/"),
                        verbose = FALSE)
      
      map_fort <- fortify(map_df, region ="QUADRANT")
      
      map_id <- map_df@data$QUADRANT
      
      map_centroids <- as.data.frame(coordinates(map_df))
      names(map_centroids) <- c("Longitude", "Latitude")
      map_centroids <- cbind(map_id, map_centroids)
      map_centroids <- map_centroids %>% 
        rename(id = map_id)
      
      map_tf <- traffic.flow %>% 
        filter(date == as_datetime(str_c(as.character(input$date1), 
                                            " ", 
                                            as.character(input$hour), 
                                            ":00:00"))) %>% 
        group_by(quadrant) %>% 
        summarize(cs = mean(current_speed),
                  fs = mean(free_flow_speed),
                  tt = mean(current_travel_time),
                  ftt = mean(free_flow_travel_time),
                  ozone = mean(ozone_loc_aqi),
                  so2 = mean(so2_loc_aqi),
                  pm2.5 = mean(pm2.5_loc_aqi),
                  no2 = mean(no2_loc_aqi),
                  .groups = 'drop') %>% 
        rename(id = quadrant)
      
      map_tf <- merge(map_tf, map_centroids, by = "id", all.y = TRUE)
    }else if(input$loc.type == "Ward"){
      map_df <- readOGR(dsn = paste0(getwd(), "/data/shape_files/Ward_from_2012-shp/"),
                        verbose = FALSE)
      
      map_fort <- fortify(map_df, region ="WARD")
      
      map_id <- map_df@data$WARD
      
      map_centroids <- as.data.frame(coordinates(map_df))
      names(map_centroids) <- c("Longitude", "Latitude")
      map_centroids <- cbind(map_id, map_centroids)
      map_centroids <- map_centroids %>% 
        rename(id = map_id)
      
      map_tf <- traffic.flow %>% 
        filter(date == as_datetime(str_c(as.character(input$date1), 
                                            " ", 
                                            as.character(input$hour), 
                                            ":00:00"))) %>% 
        group_by(ward) %>% 
        summarize(cs = mean(current_speed),
                  fs = mean(free_flow_speed),
                  tt = mean(current_travel_time),
                  ftt = mean(free_flow_travel_time),
                  ozone = mean(ozone_loc_aqi),
                  so2 = mean(so2_loc_aqi),
                  pm2.5 = mean(pm2.5_loc_aqi),
                  no2 = mean(no2_loc_aqi),
                  .groups = 'drop') %>% 
        rename(id = ward) %>% 
        mutate(id = as.integer(id))
      
      map_tf <- merge(map_tf, map_centroids, by = "id", all.y = TRUE)
    }else if(input$loc.type == "Zip Code"){
      map_df <- readOGR(dsn = paste0(getwd(), "/data/shape_files/Zip_Codes-shp/"),
                        verbose = FALSE)
      
      map_fort <- fortify(map_df, region ="ZIPCODE")
      
      map_id <- map_df@data$ZIPCODE
      
      map_centroids <- as.data.frame(coordinates(map_df))
      names(map_centroids) <- c("Longitude", "Latitude")
      map_centroids <- cbind(map_id, map_centroids)
      map_centroids <- map_centroids %>% 
        rename(id = map_id)
      
      map_tf <- traffic.flow %>% 
        filter(date == as_datetime(str_c(as.character(input$date1), 
                                             " ", 
                                             as.character(input$hour), 
                                             ":00:00"))) %>% 
        group_by(zip_code) %>% 
        summarize(cs = mean(current_speed),
                  fs = mean(free_flow_speed),
                  tt = mean(current_travel_time),
                  ftt = mean(free_flow_travel_time),
                  ozone = mean(ozone_loc_aqi),
                  so2 = mean(so2_loc_aqi),
                  pm2.5 = mean(pm2.5_loc_aqi),
                  no2 = mean(no2_loc_aqi),
                  .groups = 'drop') %>% 
        rename(id = zip_code) %>% 
        mutate(id = as.integer(id))
      
      map_tf <- merge(map_tf, map_centroids, by = "id", all.y = TRUE)
    }else if(input$loc.type == "Advisory Neighborhood Commission"){
      map_df <- readOGR(dsn = paste0(getwd(), "/data/shape_files/Advisory_Neighborhood_Commissions_from_2013-shp/"),
                        verbose = FALSE)
      
      map_fort <- fortify(map_df, region ="ANC_ID")
      
      map_id <- map_df@data$ANC_ID
      
      map_centroids <- as.data.frame(coordinates(map_df))
      names(map_centroids) <- c("Longitude", "Latitude")
      map_centroids <- cbind(map_id, map_centroids)
      map_centroids <- map_centroids %>% 
        rename(id = map_id)
      
      map_tf <- traffic.flow %>% 
        filter(date == as_datetime(str_c(as.character(input$date1), 
                                            " ", 
                                            as.character(input$hour), 
                                            ":00:00"))) %>% 
        group_by(anc) %>% 
        summarize(cs = mean(current_speed),
                  fs = mean(free_flow_speed),
                  tt = mean(current_travel_time),
                  ftt = mean(free_flow_travel_time),
                  ozone = mean(ozone_loc_aqi),
                  so2 = mean(so2_loc_aqi),
                  pm2.5 = mean(pm2.5_loc_aqi),
                  no2 = mean(no2_loc_aqi),
                  .groups = 'drop') %>% 
        rename(id = anc) 
      
      map_tf <- merge(map_tf, map_centroids, by = "id", all.y = TRUE)
    }else if(input$loc.type == "Single Member District"){
      map_df <- readOGR(dsn = paste0(getwd(), "/data/shape_files/Single_Member_District_from_2013-shp/"),
                        verbose = FALSE)
      
      map_fort <- fortify(map_df, region ="SMD_ID")
      
      map_id <- map_df@data$SMD_ID
      
      map_centroids <- as.data.frame(coordinates(map_df))
      names(map_centroids) <- c("Longitude", "Latitude")
      map_centroids <- cbind(map_id, map_centroids)
      map_centroids <- map_centroids %>% 
        rename(id = map_id)
      
      map_tf <- traffic.flow %>% 
        filter(date == as_datetime(str_c(as.character(input$date1), 
                                            " ", 
                                            as.character(input$hour), 
                                            ":00:00"))) %>% 
        group_by(single_member_district) %>% 
        summarize(cs = mean(current_speed),
                  fs = mean(free_flow_speed),
                  tt = mean(current_travel_time),
                  ftt = mean(free_flow_travel_time),
                  ozone = mean(ozone_loc_aqi),
                  so2 = mean(so2_loc_aqi),
                  pm2.5 = mean(pm2.5_loc_aqi),
                  no2 = mean(no2_loc_aqi),
                  .groups = 'drop') %>% 
        rename(id = single_member_district) 
      
      map_tf <- merge(map_tf, map_centroids, by = "id", all.y = TRUE)
    }else if(input$loc.type == "Voter Precinct"){
      map_df <- readOGR(dsn = paste0(getwd(), "/data/shape_files/Voting_Precinct_-_2012-shp/"),
                        verbose = FALSE)
      
      map_df@data$GIS_ID <- sub("Vote19_", "", map_df@data$GIS_ID)
      
      map_fort <- fortify(map_df, region ="GIS_ID")
      
      map_id <- map_df@data$GIS_ID
      
      map_centroids <- as.data.frame(coordinates(map_df))
      names(map_centroids) <- c("Longitude", "Latitude")
      map_centroids <- cbind(map_id, map_centroids)
      map_centroids <- map_centroids %>% 
        rename(id = map_id)
      
      map_tf <- traffic.flow %>% 
        filter(date == as_datetime(str_c(as.character(input$date1), 
                                            " ", 
                                            as.character(input$hour), 
                                            ":00:00"))) %>% 
        group_by(voter_precinct) %>% 
        summarize(cs = mean(current_speed),
                  fs = mean(free_flow_speed),
                  tt = mean(current_travel_time),
                  ftt = mean(free_flow_travel_time),
                  ozone = mean(ozone_loc_aqi),
                  so2 = mean(so2_loc_aqi),
                  pm2.5 = mean(pm2.5_loc_aqi),
                  no2 = mean(no2_loc_aqi),
                  .groups = 'drop') %>% 
        rename(id = voter_precinct) 
      
      map_tf <- merge(map_tf, map_centroids, by = "id", all.y = TRUE)
    }
    
    if(input$loc.type != "Census Tract"){
      if(input$map.choice.air == "Ozone"){
        p1 <- ggplot(map_tf, aes(map_id = id)) +
          geom_map(aes(fill = ozone), stat = "identity", colour = "grey", map = map_fort) +
          expand_limits(x = map_fort$long, y = map_fort$lat) + 
          scale_fill_gradient(high = "red", low = "white", guide = "colorbar", labels = comma) +
          geom_text(aes(label = id, x = Longitude, y = Latitude), size = 3) +
          labs(x = "Longitude", y = "Latitude", title = str_c("AQI by ", input$loc.type),
               fill = "Ozone") +
          theme_bw()
      }else if(input$map.choice.air == "SO2"){
        p1 <- ggplot(map_tf, aes(map_id = id)) +
          geom_map(aes(fill = so2), stat = "identity", colour = "grey", map = map_fort) +
          expand_limits(x = map_fort$long, y = map_fort$lat) + 
          scale_fill_gradient(high = "red", low = "white", guide = "colorbar", labels = comma) +
          geom_text(aes(label = id, x = Longitude, y = Latitude), size = 3) +
          labs(x = "Longitude", y = "Latitude", title = str_c("AQI by ", input$loc.type),
               fill = "SO2") +
          theme_bw()
      }else if(input$map.choice.air == "PM2.5"){
        p1 <- ggplot(map_tf, aes(map_id = id)) +
          geom_map(aes(fill = pm2.5), stat = "identity", colour = "grey", map = map_fort) +
          expand_limits(x = map_fort$long, y = map_fort$lat) + 
          scale_fill_gradient(high = "red", low = "white", guide = "colorbar", labels = comma) +
          geom_text(aes(label = id, x = Longitude, y = Latitude), size = 3) +
          labs(x = "Longitude", y = "Latitude", title = str_c("AQI by ", input$loc.type),
               fill = "PM2.5") +
          theme_bw()
      }else if(input$map.choice.air == "NO2"){
        p1 <- ggplot(map_tf, aes(map_id = id)) +
          geom_map(aes(fill = no2), stat = "identity", colour = "grey", map = map_fort) +
          expand_limits(x = map_fort$long, y = map_fort$lat) + 
          scale_fill_gradient(high = "red", low = "white", guide = "colorbar", labels = comma) +
          geom_text(aes(label = id, x = Longitude, y = Latitude), size = 3) +
          labs(x = "Longitude", y = "Latitude", title = str_c("AQI by ", input$loc.type),
               fill = "NO2") +
          theme_bw()
      }
      
      if(input$map.choice.traffic == "Current Speed"){
        p2 <- ggplot(map_tf, aes(map_id = id)) +
          geom_map(aes(fill = cs), stat = "identity", colour = "grey", map = map_fort) +
          expand_limits(x = map_fort$long, y = map_fort$lat) + 
          scale_fill_gradient(high = "red", low = "white", guide = "colorbar", labels = comma) +
          geom_text(aes(label = id, x = Longitude, y = Latitude), size = 3) +
          labs(x = "Longitude", y = "Latitude", title = str_c("Traffic by ", input$loc.type),
               fill = "C Speed") +
          theme_bw()
      }else if(input$map.choice.traffic == "Free Flow Speed"){
        p2 <- ggplot(map_tf, aes(map_id = id)) +
          geom_map(aes(fill = fs), stat = "identity", colour = "grey", map = map_fort) +
          expand_limits(x = map_fort$long, y = map_fort$lat) + 
          scale_fill_gradient(high = "red", low = "white", guide = "colorbar", labels = comma) +
          geom_text(aes(label = id, x = Longitude, y = Latitude), size = 3) +
          labs(x = "Longitude", y = "Latitude", title = str_c("Traffic by ", input$loc.type),
               fill = "FF Speed") +
          theme_bw()
      }else if(input$map.choice.traffic == "Current Travel Time"){
        p2 <- ggplot(map_tf, aes(map_id = id)) +
          geom_map(aes(fill = tt), stat = "identity", colour = "grey", map = map_fort) +
          expand_limits(x = map_fort$long, y = map_fort$lat) + 
          scale_fill_gradient(high = "red", low = "white", guide = "colorbar", labels = comma) +
          geom_text(aes(label = id, x = Longitude, y = Latitude), size = 3) +
          labs(x = "Longitude", y = "Latitude", title = str_c("Traffic by ", input$loc.type),
               fill = "C Travel") +
          theme_bw()
      }else if(input$map.choice.traffic == "Free Flow Speed"){
        p2 <- ggplot(map_tf, aes(map_id = id)) +
          geom_map(aes(fill = ftt), stat = "identity", colour = "grey", map = map_fort) +
          expand_limits(x = map_fort$long, y = map_fort$lat) + 
          scale_fill_gradient(high = "red", low = "white", guide = "colorbar", labels = comma) +
          geom_text(aes(label = id, x = Longitude, y = Latitude), size = 3) +
          labs(x = "Longitude", y = "Latitude", title = str_c("Traffic by ", input$loc.type),
               fill = "FF Travel") +
          theme_bw()
      }
      
      grid.arrange(p1, p2, ncol = 2)
    }    
    
  })
  output$location.map2 <- renderPlot({
    if(input$loc.type == "Quadrant"){
      ## Quadrants 
      map_df <- readOGR(dsn = paste0(getwd(), "/data/shape_files/DC_Quadrants-shp/"),
                        verbose = FALSE)
      
      map_fort <- fortify(map_df, region ="QUADRANT")
      
      map_id <- map_df@data$QUADRANT
      
      map_centroids <- as.data.frame(coordinates(map_df))
      names(map_centroids) <- c("Longitude", "Latitude")
      map_centroids <- cbind(map_id, map_centroids)
      map_centroids <- map_centroids %>% 
        rename(id = map_id)
      
      map_tf <- traffic.flow %>% 
        filter(date == as_datetime(str_c(as.character(input$date1), 
                                         " ", 
                                         as.character(input$hour), 
                                         ":00:00")) - 604800) %>% 
        group_by(quadrant) %>% 
        summarize(cs = mean(current_speed),
                  fs = mean(free_flow_speed),
                  tt = mean(current_travel_time),
                  ftt = mean(free_flow_travel_time),
                  ozone = mean(ozone_loc_aqi),
                  so2 = mean(so2_loc_aqi),
                  pm2.5 = mean(pm2.5_loc_aqi),
                  no2 = mean(no2_loc_aqi),
                  .groups = 'drop') %>% 
        rename(id = quadrant)
      
      map_tf <- merge(map_tf, map_centroids, by = "id", all.y = TRUE)
    }else if(input$loc.type == "Ward"){
      map_df <- readOGR(dsn = paste0(getwd(), "/data/shape_files/Ward_from_2012-shp/"),
                        verbose = FALSE)
      
      map_fort <- fortify(map_df, region ="WARD")
      
      map_id <- map_df@data$WARD
      
      map_centroids <- as.data.frame(coordinates(map_df))
      names(map_centroids) <- c("Longitude", "Latitude")
      map_centroids <- cbind(map_id, map_centroids)
      map_centroids <- map_centroids %>% 
        rename(id = map_id)
      
      map_tf <- traffic.flow %>% 
        filter(date == as_datetime(str_c(as.character(input$date1), 
                                         " ", 
                                         as.character(input$hour), 
                                         ":00:00")) - 604800) %>% 
        group_by(ward) %>% 
        summarize(cs = mean(current_speed),
                  fs = mean(free_flow_speed),
                  tt = mean(current_travel_time),
                  ftt = mean(free_flow_travel_time),
                  ozone = mean(ozone_loc_aqi),
                  so2 = mean(so2_loc_aqi),
                  pm2.5 = mean(pm2.5_loc_aqi),
                  no2 = mean(no2_loc_aqi),
                  .groups = 'drop') %>% 
        rename(id = ward)
      
      map_tf <- merge(map_tf, map_centroids, by = "id", all.y = TRUE)
    }else if(input$loc.type == "Zip Code"){
      map_df <- readOGR(dsn = paste0(getwd(), "/data/shape_files/Zip_Codes-shp/"),
                        verbose = FALSE)
      
      map_fort <- fortify(map_df, region ="ZIPCODE")
      
      map_id <- map_df@data$ZIPCODE
      
      map_centroids <- as.data.frame(coordinates(map_df))
      names(map_centroids) <- c("Longitude", "Latitude")
      map_centroids <- cbind(map_id, map_centroids)
      map_centroids <- map_centroids %>% 
        rename(id = map_id)
      
      map_tf <- traffic.flow %>% 
        filter(date == as_datetime(str_c(as.character(input$date1), 
                                         " ", 
                                         as.character(input$hour), 
                                         ":00:00")) - 604800) %>% 
        group_by(zip_code) %>% 
        summarize(cs = mean(current_speed),
                  fs = mean(free_flow_speed),
                  tt = mean(current_travel_time),
                  ftt = mean(free_flow_travel_time),
                  ozone = mean(ozone_loc_aqi),
                  so2 = mean(so2_loc_aqi),
                  pm2.5 = mean(pm2.5_loc_aqi),
                  no2 = mean(no2_loc_aqi),
                  .groups = 'drop') %>% 
        rename(id = zip_code) %>% 
        mutate(id = as.integer(id))
      
      map_tf <- merge(map_tf, map_centroids, by = "id", all.y = TRUE)
    }else if(input$loc.type == "Advisory Neighborhood Commission"){
      map_df <- readOGR(dsn = paste0(getwd(), "/data/shape_files/Advisory_Neighborhood_Commissions_from_2013-shp/"),
                        verbose = FALSE)
      
      map_fort <- fortify(map_df, region ="ANC_ID")
      
      map_id <- map_df@data$ANC_ID
      
      map_centroids <- as.data.frame(coordinates(map_df))
      names(map_centroids) <- c("Longitude", "Latitude")
      map_centroids <- cbind(map_id, map_centroids)
      map_centroids <- map_centroids %>% 
        rename(id = map_id)
      
      map_tf <- traffic.flow %>% 
        filter(date == as_datetime(str_c(as.character(input$date1), 
                                         " ", 
                                         as.character(input$hour), 
                                         ":00:00")) - 604800) %>% 
        group_by(anc) %>% 
        summarize(cs = mean(current_speed),
                  fs = mean(free_flow_speed),
                  tt = mean(current_travel_time),
                  ftt = mean(free_flow_travel_time),
                  ozone = mean(ozone_loc_aqi),
                  so2 = mean(so2_loc_aqi),
                  pm2.5 = mean(pm2.5_loc_aqi),
                  no2 = mean(no2_loc_aqi),
                  .groups = 'drop') %>% 
        rename(id = anc)
      
      map_tf <- merge(map_tf, map_centroids, by = "id", all.y = TRUE)
    }else if(input$loc.type == "Single Member District"){
      map_df <- readOGR(dsn = paste0(getwd(), "/data/shape_files/Single_Member_District_from_2013-shp/"),
                        verbose = FALSE)
      
      map_fort <- fortify(map_df, region ="SMD_ID")
      
      map_id <- map_df@data$SMD_ID
      
      map_centroids <- as.data.frame(coordinates(map_df))
      names(map_centroids) <- c("Longitude", "Latitude")
      map_centroids <- cbind(map_id, map_centroids)
      map_centroids <- map_centroids %>% 
        rename(id = map_id)
      
      map_tf <- traffic.flow %>% 
        filter(date == as_datetime(str_c(as.character(input$date1), 
                                         " ", 
                                         as.character(input$hour), 
                                         ":00:00")) - 604800) %>% 
        group_by(single_member_district) %>% 
        summarize(cs = mean(current_speed),
                  fs = mean(free_flow_speed),
                  tt = mean(current_travel_time),
                  ftt = mean(free_flow_travel_time),
                  ozone = mean(ozone_loc_aqi),
                  so2 = mean(so2_loc_aqi),
                  pm2.5 = mean(pm2.5_loc_aqi),
                  no2 = mean(no2_loc_aqi),
                  .groups = 'drop') %>% 
        rename(id = single_member_district)
      
      map_tf <- merge(map_tf, map_centroids, by = "id", all.y = TRUE)
    }else if(input$loc.type == "Voter Precinct"){
      map_df <- readOGR(dsn = paste0(getwd(), "/data/shape_files/Voting_Precinct_-_2012-shp/"),
                        verbose = FALSE)
      
      map_df@data$GIS_ID <- sub("Vote19_", "", map_df@data$GIS_ID)
      
      map_fort <- fortify(map_df, region ="GIS_ID")
      
      map_id <- map_df@data$GIS_ID
      
      map_centroids <- as.data.frame(coordinates(map_df))
      names(map_centroids) <- c("Longitude", "Latitude")
      map_centroids <- cbind(map_id, map_centroids)
      map_centroids <- map_centroids %>% 
        rename(id = map_id)
      
      map_tf <- traffic.flow %>% 
        filter(date == as_datetime(str_c(as.character(input$date1), 
                                         " ", 
                                         as.character(input$hour), 
                                         ":00:00")) - 604800) %>% 
        group_by(voter_precinct) %>% 
        summarize(cs = mean(current_speed),
                  fs = mean(free_flow_speed),
                  tt = mean(current_travel_time),
                  ftt = mean(free_flow_travel_time),
                  ozone = mean(ozone_loc_aqi),
                  so2 = mean(so2_loc_aqi),
                  pm2.5 = mean(pm2.5_loc_aqi),
                  no2 = mean(no2_loc_aqi),
                  .groups = 'drop') %>% 
        rename(id = voter_precinct)
      
      map_tf <- merge(map_tf, map_centroids, by = "id", all.y = TRUE)
    }
    
    if(input$loc.type != "Census Tract"){
      if(input$map.choice.air == "Ozone"){
        p1 <- ggplot(map_tf, aes(map_id = id)) +
          geom_map(aes(fill = ozone), stat = "identity", colour = "grey", map = map_fort) +
          expand_limits(x = map_fort$long, y = map_fort$lat) + 
          scale_fill_gradient(high = "red", low = "white", guide = "colorbar", labels = comma) +
          geom_text(aes(label = id, x = Longitude, y = Latitude), size = 3) +
          labs(x = "Longitude", y = "Latitude", title = str_c("AQI by ", input$loc.type),
               fill = "Ozone") +
          theme_bw()
      }else if(input$map.choice.air == "SO2"){
        p1 <- ggplot(map_tf, aes(map_id = id)) +
          geom_map(aes(fill = so2), stat = "identity", colour = "grey", map = map_fort) +
          expand_limits(x = map_fort$long, y = map_fort$lat) + 
          scale_fill_gradient(high = "red", low = "white", guide = "colorbar", labels = comma) +
          geom_text(aes(label = id, x = Longitude, y = Latitude), size = 3) +
          labs(x = "Longitude", y = "Latitude", title = str_c("AQI by ", input$loc.type),
               fill = "SO2") +
          theme_bw()
      }else if(input$map.choice.air == "PM2.5"){
        p1 <- ggplot(map_tf, aes(map_id = id)) +
          geom_map(aes(fill = pm2.5), stat = "identity", colour = "grey", map = map_fort) +
          expand_limits(x = map_fort$long, y = map_fort$lat) + 
          scale_fill_gradient(high = "red", low = "white", guide = "colorbar", labels = comma) +
          geom_text(aes(label = id, x = Longitude, y = Latitude), size = 3) +
          labs(x = "Longitude", y = "Latitude", title = str_c("AQI by ", input$loc.type),
               fill = "PM2.5") +
          theme_bw()
      }else if(input$map.choice.air == "NO2"){
        p1 <- ggplot(map_tf, aes(map_id = id)) +
          geom_map(aes(fill = no2), stat = "identity", colour = "grey", map = map_fort) +
          expand_limits(x = map_fort$long, y = map_fort$lat) + 
          scale_fill_gradient(high = "red", low = "white", guide = "colorbar", labels = comma) +
          geom_text(aes(label = id, x = Longitude, y = Latitude), size = 3) +
          labs(x = "Longitude", y = "Latitude", title = str_c("AQI by ", input$loc.type),
               fill = "NO2") +
          theme_bw()
      }
      
      if(input$map.choice.traffic == "Current Speed"){
        p2 <- ggplot(map_tf, aes(map_id = id)) +
          geom_map(aes(fill = cs), stat = "identity", colour = "grey", map = map_fort) +
          expand_limits(x = map_fort$long, y = map_fort$lat) + 
          scale_fill_gradient(high = "red", low = "white", guide = "colorbar", labels = comma) +
          geom_text(aes(label = id, x = Longitude, y = Latitude), size = 3) +
          labs(x = "Longitude", y = "Latitude", title = str_c("Traffic by ", input$loc.type),
               fill = "C Speed") +
          theme_bw()
      }else if(input$map.choice.traffic == "Free Flow Speed"){
        p2 <- ggplot(map_tf, aes(map_id = id)) +
          geom_map(aes(fill = fs), stat = "identity", colour = "grey", map = map_fort) +
          expand_limits(x = map_fort$long, y = map_fort$lat) + 
          scale_fill_gradient(high = "red", low = "white", guide = "colorbar", labels = comma) +
          geom_text(aes(label = id, x = Longitude, y = Latitude), size = 3) +
          labs(x = "Longitude", y = "Latitude", title = str_c("Traffic by ", input$loc.type),
               fill = "FF Speed") +
          theme_bw()
      }else if(input$map.choice.traffic == "Current Travel Time"){
        p2 <- ggplot(map_tf, aes(map_id = id)) +
          geom_map(aes(fill = tt), stat = "identity", colour = "grey", map = map_fort) +
          expand_limits(x = map_fort$long, y = map_fort$lat) + 
          scale_fill_gradient(high = "red", low = "white", guide = "colorbar", labels = comma) +
          geom_text(aes(label = id, x = Longitude, y = Latitude), size = 3) +
          labs(x = "Longitude", y = "Latitude", title = str_c("Traffic by ", input$loc.type),
               fill = "C Travel") +
          theme_bw()
      }else if(input$map.choice.traffic == "Free Flow Speed"){
        p2 <- ggplot(map_tf, aes(map_id = id)) +
          geom_map(aes(fill = ftt), stat = "identity", colour = "grey", map = map_fort) +
          expand_limits(x = map_fort$long, y = map_fort$lat) + 
          scale_fill_gradient(high = "red", low = "white", guide = "colorbar", labels = comma) +
          geom_text(aes(label = id, x = Longitude, y = Latitude), size = 3) +
          labs(x = "Longitude", y = "Latitude", title = str_c("Traffic by ", input$loc.type),
               fill = "FF Travel") +
          theme_bw()
      }
      
      grid.arrange(p1, p2, ncol = 2)
    }    
    
  })
  
  ## Raw Data Tables
  output$static.city <- renderDataTable({
    hourly
  })
  output$static.location <- renderDataTable({
    traffic.flow %>% 
      dplyr::select(-all_lat_lon) %>% 
      dplyr::select(-street.url)
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
                               "Total length of traffic jams across Washington, D.C. in miles."))
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
  output$concentrations.back <- renderTable({
    data.frame("Abbrv." = c("O3", "PM2.5", "SO2", "NO2"),
               "Pollutant" = c("Ozone", "Particulate Matter", 
                               "Sulfur Dioxide", "Nitrogen Dioxide"),
               "Good" = c("8 Hour: 0-0.054 ppm", "24 Hour: 0-12.0 microgram/m3", "1 Hour: 0-35 ppb", "1 Hour: 0-53 ppb"),
               "Moderate" = c("8 Hour: 0.055-0.070 ppm", "24 Hour: 12.1-35.4 microgram/m3", "1 Hour: 36-75 ppb", "1 Hour: 54-100 ppb"),
               "Sensitive" = c("8 Hour: 0.071-0.085 ppm; 1 Hour: 0.125-0.164 ppm", "24 Hour: 35.5-55.4 microgram/m3", "1 Hour: 76-185 ppb", "1 Hour: 101-360 ppb"),
               "Unhealthy" = c("8 Hour: 0.086-0.105 ppm; 1 Hour: 0.165-0.204 ppm", "24 Hour: 55.5-150.4 microgram/m3", "1 Hour: 186-304 ppb", "1 Hour: 361-649 ppb"),
               "Very Unhealthy" = c("8 Hour: 0.106-0.200 ppm; 1 Hour: 0.205-0.404 ppm", "24 Hour: 150.5-250.4 microgram/m3", "1 Hour: 305-604 ppb", "1 Hour: 650-1,249 ppb"),
               "Hazardous" = c("1 Hour: 0.405-0.604 ppm", "24 Hour: 250.5-500.4 microgram/m3", "1 Hour: 605-1,004 ppb", "1 Hour: 1,250-2,049 ppb"))
  })
  output$weather.back <- renderTable({
    data.frame(Metric = c("Temperature",
                          "Weather Description",
                          "Humidity",
                          "Wind Speed"),
               Description = c("Temperature across Washington, D.C. in Farenheit.",
                               "Description of the current weather across Washington, D.C..",
                               "Humidity across Washington, D.C. in percent.",
                               "Wind speed across Washington, D.C. in miles per hour."))
  })
  
  observeEvent(input$historical.o,{
    if(input$historical.o == TRUE){
      shinyjs::show("myaqi")
    }else if(input$historical.o == FALSE){
      shinyjs::hide("myaqi")
    }
  })
  observeEvent(input$currenttime,{
    if(input$currenttime == TRUE){
      shinyjs::show("location.map")
    }else if(input$currenttime == FALSE){
      shinyjs::hide("location.map")
    }
  })
  observeEvent(input$lastweek,{
    if(input$lastweek == TRUE){
      shinyjs::show("location.map2")
    }else if(input$lastweek == FALSE){
      shinyjs::hide("location.map2")
    }
  })
  observeEvent(input$historical,{
    if(input$historical == TRUE){
      shinyjs::show("location.graph");shinyjs::show("loc.type.graph1");shinyjs::show("loc.type.graph2")
    }else if(input$historical == FALSE){
      shinyjs::hide("location.graph");shinyjs::hide("loc.type.graph1");shinyjs::hide("loc.type.graph2")
    }
  })
  observeEvent(input$currenttime.o,{
    if(input$currenttime.o == TRUE){
      shinyjs::show("airtable.graph2")
    }else if(input$currenttime.o == FALSE){
      shinyjs::hide("airtable.graph2")
    }
  })
  observeEvent(input$currenttime,{
    if(input$currenttime == TRUE){
      shinyjs::show("airtable.graph")
    }else if(input$currenttime == FALSE){
      shinyjs::hide("airtable.graph")
    }
  })
  observeEvent(input$include.weather,{
    if(input$include.weather == TRUE){
      shinyjs::show("temp");shinyjs::show("weather.des");shinyjs::show("humidity");shinyjs::show("wind.speed")
    }else if(input$include.weather == FALSE){
      shinyjs::hide("temp");shinyjs::hide("weather.des");shinyjs::hide("humidity");shinyjs::hide("wind.speed")
    }
  })
  observeEvent(input$sensor.type,{
    if (input$sensor.type == "Average Across Sensors"){
      shinyjs::hide("location.type")
    }else{
      shinyjs::show("location.type")
    }
  })

}

# Run ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)

