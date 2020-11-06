# Load Packages -----------------------------------------------------------
library(shiny)
library(tidyverse)
library(leaflet)
library(ggplot2)
library(shinyTime)

# Load Data ---------------------------------------------------------------
hourly <- read_csv("hourly.csv")
traffic.flow <- read_csv("traffic_flow.csv")

agencies <- as.data.frame(unique(hourly$agency)) %>%
  rename(c("agency"="unique(hourly$agency)"))

parameters <- as.data.frame(unique(hourly$parameter_name)) %>%
  rename(c("agency"="unique(hourly$parameter_name)"))

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

head(traffic.flow)


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
    tabPanel("Citywide",
             sidebarLayout(
               sidebarPanel(
                 dateInput("date", "Date:", value = Sys.Date()),
                 timeInput("time", "Time:", value = Sys.time()),
                 uiOutput(outputId = "text1"),
                 checkboxInput("traffic", "On"),
                 uiOutput(outputId = "text2"),
                 checkboxInput("ozone", "OZONE"),
                 checkboxInput("so2", "SO2"),
                 checkboxInput("pm25", "PM2.5"),
                 checkboxInput("no2", "NO2")
               ),
               
               mainPanel(#h3("Traffic and AQI"),
                 #plotOutput("trafficaqi"),
                 h3("Daily AQI"),
                 plotOutput("myaqi"),
                 h3("Map"),
                 leafletOutput("mymap"))
             )
    ),
    tabPanel("Location Search",
             sidebarLayout(
               sidebarPanel(
                 dateInput("date", "What day?",
                           value = "2020-11-05"),
                 sliderInput("hour", "Which hour?",
                             value = 17, min = 0, max = 24),
                 selectInput("loc.type", "Which location type?",
                             choices = location.types, 
                             selected = "Quadrant"),
                 textInput("loc.type2", "Which location?",
                           value = "SE"),
                 checkboxInput("ozone", "Ozone"), 
                 checkboxInput("so2", "SO2"), 
                 checkboxInput("pm2.5", "PM 2.5"),
                 checkboxInput("no2", "NO2")
               ),
               mainPanel(h3("Overview"),
                         textOutput("current.speed"),
                         textOutput("free.flow.speed"),
                         textOutput("current.travel.time"),
                         textOutput("free.flow.travel.time"),
                         plotOutput("bar"),
                         h3("Number of Confirmed Cases"))
             ))
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output) {
  ## Citywide Tab
  output$text1 <- renderText({
    HTML(paste0("<b>","Turn on Traffic Index?","</b>"))
  })
  
  output$text2 <- renderText({
    HTML(paste0("<b>","Parameters","</b>"))
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
    if(input$traffic==TRUE){
      hourly %>%
        ggplot(aes(x=date, y = log(AQI), color=parameter_name)) +
        geom_line() +
        geom_line(aes(x=date, y = log(traffic_index_live)), size = 1, color = "black") +
        ggtitle(paste0("Traffic and AQI")) +
        xlab("Date") + ylab("AQI")
    }else{
      hourly %>%
        ggplot(aes(x=date, y = log(AQI), color=parameter_name)) +
        geom_line() +
        ggtitle(paste0("AQI Count")) +
        xlab("Date") + ylab("AQI")
    }
  })
  
  ## Third graph for the Overview tab
  output$mymap <- renderLeaflet({
    if(input$traffic==TRUE){
      leaflet(hourly) %>% addTiles() %>%
        addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                   radius = ~(AQI+12)*10, popup = ~location, color = "#FF0000") %>%
        addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                   radius = ~(traffic_index_live+5)*10, popup = ~location, color = "#52E74B")
    }else{
      leaflet(hourly) %>% addTiles() %>%
        addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                   radius = ~(AQI+12)*10, popup = ~location, color = "#FF0000")
      
    }
  })
  
  
  ## Location Search
  output$current.speed <- renderText({
    cs1 <- traffic.flow %>% 
      filter(date == parse_datetime(str_c(as.character(input$date), 
                                          " ", 
                                          as.character(input$hour), 
                                          ":00:00")))
    if(input$loc.type == "Quadrant"){
      if(input$loc.type2 == "SE"){
        cs2 <- mean(cs1$current_speed[cs1$quadrant == "SE"], na.rm = TRUE)
      }else if(input$loc.type2 == "SW"){
        cs2 <- mean(cs1$current_speed[cs1$quadrant == "SW"], na.rm = TRUE)
      }else if(input$loc.type2 == "NE"){
        cs2 <- mean(cs1$current_speed[cs1$quadrant == "NE"], na.rm = TRUE)
      }else if(input$loc.type2 == "NW"){
        cs2 <- mean(cs1$current_speed[cs1$quadrant == "NW"], na.rm = TRUE)
      }
    }
    
    str_c("Current speed: ", round(cs2, digits = 0), " mph")
    
  })
}

# Run ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)
