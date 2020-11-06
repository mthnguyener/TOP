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

# User Interface ----------------------------------------------------------
location.types <- c("Quadrant", "Ward", "Zip Code", 
                    "Advisory Neighborhood Commission",
                    "Census Tract",
                    "Single Member District",
                    "Voter Precinct")

ui <- fluidPage(#theme = shinytheme("darkly"),
  shinythemes::themeSelector(),
  titlePanel("Assessment of Air Quality and Traffic Volume"),
  h4("Washington, D.C."),
  h4("American University Team - Chace Paulson, Minh Nguyen & Shalini Ramachandra"),
  
  tabsetPanel(
    tabPanel("Citywide",
             sidebarLayout(
               sidebarPanel(
                 dateInput("date2", "What day?",
                           value = Sys.Date()),
                 sliderInput("hour2", "Which hour?",
                             value = 17, min = 0, max = 24),
                 uiOutput(outputId = "text1"),
                 checkboxInput("traffic", "On", value = TRUE),
                 uiOutput(outputId = "text2"),
                 checkboxInput("ozone", "OZONE"),
                 checkboxInput("so2", "SO2"),
                 checkboxInput("pm25", "PM2.5", value = TRUE),
                 checkboxInput("no2", "NO2")
               ),
               
               mainPanel(#h3("Traffic and AQI"),
                 #plotOutput("trafficaqi"),
                 fluidRow(title = "Current Quality",
                          column(3,
                                 h4("OZONE:",
                                    tableOutput("ozonetext"))
                                 ),
                          column(3,
                                 h4("SO2:",
                                    tableOutput("so2text"))
                                 ),
                          column(3,
                                 h4("PM2.5:",
                                    tableOutput("pm25text"))
                                 ),
                          column(3,
                                 h4("NO2:",
                                    tableOutput("no2text"))
                          )
                          ),
                 
                 h3("Daily AQI"),
                 plotOutput("myaqi"),
                 h3("Map"),
                 leafletOutput("mymap"))
             )),
    tabPanel("Location Search",
             sidebarLayout(
               sidebarPanel(
                 dateInput("date1", "What day?",
                           value = "2020-11-05"),
                 sliderInput("hour", "Which hour?",
                             value = 17, min = 0, max = 24),
                 selectInput("loc.type", "Which location type?",
                             choices = location.types, 
                             selected = "Quadrant"),
                 textInput("loc.type2", "Which location?",
                           value = "SE"),
                 h5("Air Quality Parameters"),
                 checkboxInput("ozone1", "Ozone", value = TRUE), 
                 checkboxInput("so2.1", "SO2"), 
                 checkboxInput("pm2.5.1", "PM 2.5"),
                 checkboxInput("no2.1", "NO2"),
                 h5("Traffic Parameters"),
                 checkboxInput("cspeed", "Current Speed", value = TRUE), 
                 checkboxInput("ffspeed", "Free Flow Speed", value = TRUE), 
                 checkboxInput("ctravel", "Current Travel Time"),
                 checkboxInput("fftravel", "Free Flow Travel Time")
               ),
               mainPanel(h3("Overview"),
                         textOutput("current.speed"),
                         textOutput("current.travel.time"),
                         textOutput("current.air.quality"),
                         h3("Within Location"),
                         plotOutput("location.graph"),
                         h3("Location Type Comparison"),
                         h4("Air Quality"),
                         plotOutput("loc.type.graph1"),
                         h4("Traffic Volume"),
                         plotOutput("loc.type.graph2"),)
             ))
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
  
  output$ozonetext <- renderTable({
    y <- hourly %>%
      rename(c("datetime"="date")) %>%
      separate(datetime, into = c('date', 'hour'), sep=' ', remove = FALSE) %>%
      mutate(date = as.Date(date),
             hour = as.numeric(str_remove(hour,"\\:\\d+\\:\\d+"))) %>% 
      filter(parameter_name == "OZONE" & date == as.Date(input$date2) & hour == input$hour2) %>% select(AQI)
    
    table <- as.data.frame(summary(y)) %>%
      separate(Freq, into = c('stat', 'result'), sep=':', remove = FALSE)
    
    table[,4:5]
  })
  
  output$so2text <- renderTable({
    y <- hourly %>%
      rename(c("datetime"="date")) %>%
      separate(datetime, into = c('date', 'hour'), sep=' ', remove = FALSE) %>%
      mutate(date = as.Date(date),
             hour = as.numeric(str_remove(hour,"\\:\\d+\\:\\d+"))) %>% 
      filter(parameter_name == "SO2" & date == as.Date(input$date2) & hour == input$hour2) %>% select(AQI)
    
    table <- as.data.frame(summary(y)) %>%
      separate(Freq, into = c('stat', 'result'), sep=':', remove = FALSE)
    
    table[,4:5]
  })

  output$pm25text <- renderTable({
    y <- hourly %>%
      rename(c("datetime"="date")) %>%
      separate(datetime, into = c('date', 'hour'), sep=' ', remove = FALSE) %>%
      mutate(date = as.Date(date),
             hour = as.numeric(str_remove(hour,"\\:\\d+\\:\\d+"))) %>% 
      filter(parameter_name == "PM2.5" & date == as.Date(input$date2) & hour == input$hour2) %>% select(AQI)
    
    table <- as.data.frame(summary(y)) %>%
      separate(Freq, into = c('stat', 'result'), sep=':', remove = FALSE)
    
    table[,4:5]
  })
  
  output$no2text <- renderTable({
    y <- hourly %>%
      rename(c("datetime"="date")) %>%
      separate(datetime, into = c('date', 'hour'), sep=' ', remove = FALSE) %>%
      mutate(date = as.Date(date),
             hour = as.numeric(str_remove(hour,"\\:\\d+\\:\\d+"))) %>% 
      filter(parameter_name == "NO2" & date == as.Date(input$date2) & hour == input$hour2) %>% select(AQI)
    
    table <- as.data.frame(summary(y)) %>%
      separate(Freq, into = c('stat', 'result'), sep=':', remove = FALSE)
    
    table[,4:5]
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
    x <- parameter_wider %>% 
      filter(agency == "District of Columbia - Department of Energy and Environment")
    
    x[is.na(x)] <- 0
    
    ptitle <- str_c("AQI and Traffic")
    
    p1 <- ggplot(data = x, aes(x = date))
    ptra <- geom_line(aes(y = traffic_index_live), color = "black")
    po <- geom_line(aes(y = OZONE), color = "orange")
    pc <- geom_line(aes(y = SO2), color = "blue")
    ph <- geom_line(aes(y = PM2.5), color = "green") 
    pl <- geom_line(aes(y = NO2), color = "red")
    pt <- labs(title = ptitle, x = "Date", y = "")
      
    if(input$traffic == TRUE) {
      if(input$ozone == TRUE){
        if(input$so2 == TRUE){
          if(input$pm25 == TRUE){
            if(input$no2 == TRUE){
                p1 + ptra + po + pc + ph + pl + pt
            } else if(input$no2 == FALSE){
                p1 + ptra + po + pc + ph + pt
            }
          }else if(input$pm25 == FALSE){
            if(input$no2 == TRUE){
              p1 + ptra + po + pc + pl + pt
            } else if(input$no2 == FALSE){
              p1 + ptra + po + pc + pt
            }
          }
        } else if(input$so2 == FALSE){
          if(input$pm25 == TRUE){
            if(input$no2 == TRUE){
              p1 + ptra + po + ph + pl + pt
            } else if(input$no2 == FALSE){
              p1 + ptra + po + ph + pt
            }
          }else if(input$pm25 == FALSE){
            if(input$no2 == TRUE){
              p1 + ptra + po + pl + pt
            } else if(input$no2 == FALSE){
              p1 + ptra + po + pt
            }
          }
        }
      } else if(input$ozone == FALSE){
        if(input$so2 == TRUE){
          if(input$pm25 == TRUE){
            if(input$no2 == TRUE){
              p1 + ptra + pc + ph + pl + pt
            } else if(input$no2 == FALSE){
              p1 + ptra + pc + ph + pt
            }
          }else if(input$pm25 == FALSE){
            if(input$no2 == TRUE){
              p1 + ptra + pc + pl + pt
            } else if(input$no2 == FALSE){
              p1 + ptra + pc + pt
            }
          }
        } else if(input$so2 == FALSE){
          if(input$pm25 == TRUE){
            if(input$no2 == TRUE){
              p1 + ptra + ph + pl + pt
            } else if(input$no2 == FALSE){
              p1 + ptra + ph + pt
            }
          }else if(input$pm25 == FALSE){
            if(input$no2 == TRUE){
              p1 + ptra + pl + pt
            } else if(input$no2 == FALSE){
              p1 + ptra + pt
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
                p1 + po + pc + ph + pl + pt
              } else if(input$no2 == FALSE){
                p1 + po + pc + ph + pt
              }
            }else if(input$pm25 == FALSE){
              if(input$no2 == TRUE){
                p1 + po + pc + pl + pt
              } else if(input$no2 == FALSE){
                p1 + po + pc + pt
              }
            }
          } else if(input$so2 == FALSE){
            if(input$pm25 == TRUE){
              if(input$no2 == TRUE){
                p1 + po + ph + pl + pt
              } else if(input$no2 == FALSE){
                p1 + po + ph + pt
              }
            }else if(input$pm25 == FALSE){
              if(input$no2 == TRUE){
                p1 + po + pl + pt
              } else if(input$no2 == FALSE){
                p1 + po + pt
              }
            }
          }
        } else if(input$ozone == FALSE){
          if(input$so2 == TRUE){
            if(input$pm25 == TRUE){
              if(input$no2 == TRUE){
                p1 + pc + ph + pl + pt
              } else if(input$no2 == FALSE){
                p1 + pc + ph + pt
              }
            }else if(input$pm25 == FALSE){
              if(input$no2 == TRUE){
                p1 + pc + pl + pt
              } else if(input$no2 == FALSE){
                p1 + pc + pt
              }
            }
          } else if(input$so2 == FALSE){
            if(input$pm25 == TRUE){
              if(input$no2 == TRUE){
                p1 + ph + pl + pt
              } else if(input$no2 == FALSE){
                p1 + ph + pt
              }
            }else if(input$pm25 == FALSE){
              if(input$no2 == TRUE){
                p1 + pl + pt
              } else if(input$no2 == FALSE){
                p1 + pt
              }
            }
          }
        }
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
      filter(date == parse_datetime(str_c(as.character(input$date1), 
                                          " ", 
                                          as.character(input$hour), 
                                          ":00:00")))
    if(input$loc.type == "Quadrant"){
      if(input$loc.type2 == "SE"){
        cs2 <- mean(cs1$current_speed[cs1$quadrant == "SE"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$quadrant == "SE"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "SW"){
        cs2 <- mean(cs1$current_speed[cs1$quadrant == "SW"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$quadrant == "SW"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "NE"){
        cs2 <- mean(cs1$current_speed[cs1$quadrant == "NE"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$quadrant == "NE"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "NW"){
        cs2 <- mean(cs1$current_speed[cs1$quadrant == "NW"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$quadrant == "NW"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }
    }else if(input$loc.type == "Ward"){
      if(input$loc.type2 == "1"){
        cs2 <- mean(cs1$current_speed[cs1$ward == "1"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$ward == "1"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "2"){
        cs2 <- mean(cs1$current_speed[cs1$ward == "2"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$ward == "2"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3"){
        cs2 <- mean(cs1$current_speed[cs1$ward == "3"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$ward == "3"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "4"){
        cs2 <- mean(cs1$current_speed[cs1$ward == "4"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$ward == "4"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5"){
        cs2 <- mean(cs1$current_speed[cs1$ward == "5"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$ward == "5"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6"){
        cs2 <- mean(cs1$current_speed[cs1$ward == "6"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$ward == "6"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "7"){
        cs2 <- mean(cs1$current_speed[cs1$ward == "7"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$ward == "7"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "8"){
        cs2 <- mean(cs1$current_speed[cs1$ward == "8"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$ward == "8"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }
    }else if(input$loc.type == "Zip Code"){
      if(input$loc.type2 == "20227"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20227"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20227"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20032"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20032"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20032"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20037"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20037"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20037"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20019"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20019"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20019"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20020"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20020"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20020"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20018"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20018"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20018"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20024"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20024"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20024"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20002"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20002"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20002"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20003"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20003"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20003"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20001"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20001"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20001"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20005"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20005"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20005"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20009"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20009"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20009"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20017"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20017"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20017"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20010"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20010"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20010"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20016"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20016"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20016"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20008"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20008"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20008"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20011"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20011"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20011"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20007"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20007"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20007"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20374"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20374"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20374"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20015"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20015"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20015"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20012"){
        cs2 <- mean(cs1$current_speed[cs1$zip_code == "20012"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$zip_code == "20012"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }
    }else if(input$loc.type == "Advisory Neighborhood Commission"){
      if(input$loc.type2 == "2A"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "2A"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "2A"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "8D"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "8D"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "8D"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "8E"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "8E"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "8E"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "8C"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "8C"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "8C"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "7F"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "7F"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "7F"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "7C"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "7C"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "7C"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "7E"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "7E"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "7E"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5C"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "5C"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "5C"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6D"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "6D"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "6D"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5D"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "5D"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "5D"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6B"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "6B"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "6B"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5E"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "5E"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "5E"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "2F"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "2F"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "2F"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "2B"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "2B"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "2B"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "1B"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "1B"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "1B"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6E"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "6E"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "6E"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5B"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "5B"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "5B"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "1A"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "1A"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "1A"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3C"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "3C"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "3C"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6A"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "6A"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "6A"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3D"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "3D"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "3D"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "4C"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "4C"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "4C"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3E"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "3E"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "3E"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "2E"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "2E"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "2E"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6C"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "6C"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "6C"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "8A"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "8A"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "8A"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "4D"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "4D"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "4D"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "4B"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "4B"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "4B"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "4A"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "4A"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "4A"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3G"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "3G"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "3G"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3F"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "3F"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "3F"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5A"){
        cs2 <- mean(cs1$current_speed[cs1$anc == "5A"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$anc == "5A"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }
    }else if(input$loc.type == "Census Tract"){
      if(input$loc.type2 == "006202"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "006202"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "006202"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "009811"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "009811"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "009811"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "009807"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "009807"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "009807"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "009700"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "009700"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "009700"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "009804"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "009804"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "009804"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "007304"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "007304"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "007304"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "007301"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "007301"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "007301"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "010400"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "010400"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "010400"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "010800"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "010800"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "010800"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "005600"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "005600"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "005600"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "009603"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "009603"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "009603"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "007809"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "007809"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "007809"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "009902"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "009902"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "009902"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "009000"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "009000"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "009000"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "009905"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "009905"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "009905"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "007707"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "007707"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "007707"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "007806"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "007806"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "007806"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "007804"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "007804"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "007804"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "007807"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "007807"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "007807"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "007708"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "007708"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "007708"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "010500"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "010500"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "010500"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "006400"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "006400"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "006400"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "008803"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "008803"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "008803"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "010200"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "010200"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "010200"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "006500"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "006500"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "006500"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "008702"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "008702"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "008702"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "003301"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "003301"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "003301"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "004902"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "004902"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "004902"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "004201"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "004201"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "004201"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "003400"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "003400"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "003400"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "004400"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "004400"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "004400"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "011100"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "011100"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "011100"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "004702"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "004702"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "004702"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "009302"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "009302"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "009302"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "008701"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "008701"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "008701"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "010100"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "010100"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "010100"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "002900"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "002900"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "002900"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "004600"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "004600"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "004600"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "009400"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "009400"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "009400"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "001002"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "001002"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "001002"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "010600"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "010600"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "010600"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "000901"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "000901"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "000901"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "000502"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "000502"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "000502"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "009102"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "009102"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "009102"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "009201"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "009201"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "009201"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "002400"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "002400"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "002400"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "001001"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "001001"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "001001"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "000600"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "000600"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "000600"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "002501"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "002501"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "002501"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "009503"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "009503"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "009503"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "000300"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "000300"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "000300"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "000400"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "000400"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "000400"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "006700"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "006700"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "006700"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "007200"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "007200"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "007200"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "008302"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "008302"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "008302"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "006600"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "006600"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "006600"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "008904"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "008904"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "008904"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "008100"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "008100"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "008100"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "006802"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "006802"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "006802"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "007601"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "007601"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "007601"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "007100"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "007100"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "007100"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "008200"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "008200"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "008200"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "008001"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "008001"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "008001"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "008002"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "008002"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "008002"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "002101"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "002101"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "002101"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "001901"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "001901"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "001901"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "002600"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "002600"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "002600"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "001401"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "001401"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "001401"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "001803"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "001803"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "001803"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "001301"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "001301"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "001301"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "002102"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "002102"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "002102"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "001804"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "001804"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "001804"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "002001"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "002001"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "002001"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "001902"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "001902"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "001902"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "001100"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "001100"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "001100"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "001600"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "001600"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "001600"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "009509"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "009509"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "009509"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "001500"){
        cs2 <- mean(cs1$current_speed[cs1$census_tract == "001500"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$census_tract == "001500"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }
    }else if(input$loc.type == "Single Member District"){
      if(input$loc.type2 == "2A01"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "2A01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "2A01"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "8D03"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "8D03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "8D03"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "8D01"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "8D01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "8D01"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "8D06"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "8D06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "8D06"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "8E05"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "8E05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "8E05"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "8C07"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "8C07"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "8C07"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "8E04"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "8E04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "8E04"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "8C05"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "8C05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "8C05"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "8C03"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "8C03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "8C03"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "2A07"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "2A07"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "2A07"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "2A03"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "2A03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "2A03"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "7F01"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "7F01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "7F01"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "7C04"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "7C04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "7C04"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "7E02"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "7E02"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "7E02"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5C03"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "5C03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "5C03"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "7E06"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "7E06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "7E06"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "7E01"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "7E01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "7E01"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "7C07"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "7C07"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "7C07"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "7C03"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "7C03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "7C03"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "7C06"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "7C06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "7C06"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "7F06"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "7F06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "7F06"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6D03"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "6D03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "6D03"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6D06"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "6D06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "6D06"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5D01"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "5D01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "5D01"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6D05"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "6D05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "6D05"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6B01"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "6B01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "6B01"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5E03"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "5E03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "5E03"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5E08"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "5E08"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "5E08"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "2F06"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "2F06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "2F06"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "2B09"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "2B09"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "2B09"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "1B01"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "1B01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "1B01"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "1B02"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "1B02"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "1B02"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5C02"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "5C02"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "5C02"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6E07"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "6E07"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "6E07"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5B03"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "5B03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "5B03"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5E04"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "5E04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "5E04"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "2F08"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "2F08"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "2F08"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "1A04"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "1A04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "1A04"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5E05"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "5E05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "5E05"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5C01"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "5C01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "5C01"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3C06"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "3C06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "3C06"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6A01"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "6A01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "6A01"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "2B05"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "2B05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "2B05"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3D02"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "3D02"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "3D02"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3C03"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "3C03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "3C03"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5B01"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "5B01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "5B01"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5C06"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "5C06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "5C06"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5E01"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "5E01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "5E01"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "4C08"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "4C08"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "4C08"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3E05"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "3E05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "3E05"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3C09"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "3C09"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "3C09"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "4C03"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "4C03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "4C03"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "2E01"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "2E01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "2E01"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3C08"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "3C08"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "3C08"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6B05"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "6B05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "6B05"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5C04"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "5C04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "5C04"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6D07"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "6D07"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "6D07"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6C03"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "6C03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "6C03"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6B02"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "6B02"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "6B02"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5D05"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "5D05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "5D05"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6A03"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "6A03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "6A03"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6B09"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "6B09"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "6B09"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "8A03"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "8A03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "8A03"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6B07"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "6B07"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "6B07"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6B06"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "6B06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "6B06"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6A02"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "6A02"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "6A02"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6A08"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "6A08"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "6A08"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "4D03"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "4D03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "4D03"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "4B04"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "4B04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "4B04"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "4A08"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "4A08"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "4A08"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3D03"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "3D03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "3D03"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3G05"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "3G05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "3G05"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "4A07"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "4A07"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "4A07"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3E02"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "3E02"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "3E02"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3F03"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "3F03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "3F03"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "4B06"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "4B06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "4B06"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "4A04"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "4A04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "4A04"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "4A06"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "4A06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "4A06"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3E04"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "3E04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "3E04"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "4A02"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "4A02"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "4A02"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5A08"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "5A08"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "5A08"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3G04"){
        cs2 <- mean(cs1$current_speed[cs1$single_member_district == "3G04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$single_member_district == "3G04"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }
    }else if(input$loc.type == "Voter Precinct"){
      if(input$loc.type2 == "129"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "129"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "129"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "125"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "125"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "125"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "126"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "126"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "126"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "121"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "121"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "121"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "122"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "122"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "122"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "120"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "120"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "120"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "123"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "123"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "123"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "2"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "2"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "2"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "3"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "3"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "102"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "102"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "102"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "94"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "94"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "94"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "110"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "110"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "110"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "139"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "139"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "139"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "105"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "105"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "105"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "106"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "106"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "106"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "93"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "93"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "93"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "97"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "97"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "97"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "95"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "95"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "95"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "132"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "132"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "132"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "128"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "128"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "138"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "127"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "127"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "127"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "76"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "76"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "76"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "130"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "130"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "130"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "75"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "75"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "75"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "135"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "135"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "135"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "141"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "141"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "141"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "20"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "20"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "22"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "22"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "22"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "72"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "72"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "72"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "1"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "1"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "1"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "73"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "73"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "73"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "42"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "42"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "42"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "19"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "19"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "19"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "69"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "69"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "69"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "29"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "29"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "29"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "82"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "82"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "82"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "17"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "17"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "17"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "9"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "9"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "9"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "26"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "26"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "26"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "74"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "74"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "74"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "45"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "45"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "45"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "30"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "30"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "30"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "27"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "27"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "27"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "48"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "48"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "48"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "67"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "67"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "67"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "6"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "6"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "12"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "12"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "12"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "88"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "88"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "88"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "131"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "131"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "131"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "85"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "85"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "85"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "89"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "89"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "89"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "79"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "79"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "79"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "91"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "91"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "91"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "133"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "133"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "133"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "81"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "81"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "81"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "71"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "71"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "71"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "86"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "86"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "86"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "56"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "56"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "56"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "59"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "59"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "59"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "50"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "50"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "50"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "61"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "61"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "61"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "31"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "31"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "31"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "138"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "138"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "138"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "57"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "57"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "57"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "60"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "60"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "60"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "53"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "53"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "53"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "32"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "32"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "32"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "62"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "62"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "62"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "66"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "66"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "66"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "51"){
        cs2 <- mean(cs1$current_speed[cs1$voter_precinct == "51"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_speed[cs1$voter_precinct == "51"], na.rm = TRUE)
        str_c("Current speed: ", round(cs2, digits = 0), " mph (Free flow speed: ", 
              round(cs3, digits = 0), " mph)")
      }
    }
    
    
    
  })
  output$current.travel.time <- renderText({
    cs1 <- traffic.flow %>% 
      filter(date == parse_datetime(str_c(as.character(input$date1), 
                                          " ", 
                                          as.character(input$hour), 
                                          ":00:00")))
    if(input$loc.type == "Quadrant"){
      if(input$loc.type2 == "SE"){
        cs2 <- mean(cs1$current_travel_time[cs1$quadrant == "SE"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$quadrant == "SE"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "SW"){
        cs2 <- mean(cs1$current_travel_time[cs1$quadrant == "SW"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$quadrant == "SW"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "NE"){
        cs2 <- mean(cs1$current_travel_time[cs1$quadrant == "NE"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$quadrant == "NE"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "NW"){
        cs2 <- mean(cs1$current_travel_time[cs1$quadrant == "NW"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$quadrant == "NW"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }
    }else if(input$loc.type == "Ward"){
      if(input$loc.type2 == "1"){
        cs2 <- mean(cs1$current_travel_time[cs1$ward == "1"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$ward == "1"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "2"){
        cs2 <- mean(cs1$current_travel_time[cs1$ward == "2"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$ward == "2"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3"){
        cs2 <- mean(cs1$current_travel_time[cs1$ward == "3"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$ward == "3"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "4"){
        cs2 <- mean(cs1$current_travel_time[cs1$ward == "4"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$ward == "4"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5"){
        cs2 <- mean(cs1$current_travel_time[cs1$ward == "5"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$ward == "5"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6"){
        cs2 <- mean(cs1$current_travel_time[cs1$ward == "6"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$ward == "6"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "7"){
        cs2 <- mean(cs1$current_travel_time[cs1$ward == "7"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$ward == "7"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "8"){
        cs2 <- mean(cs1$current_travel_time[cs1$ward == "8"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$ward == "8"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }
    }else if(input$loc.type == "Zip Code"){
      if(input$loc.type2 == "20227"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20227"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20227"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20032"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20032"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20032"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20037"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20037"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20037"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20019"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20019"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20019"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20020"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20020"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20020"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20018"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20018"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20018"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20024"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20024"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20024"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20002"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20002"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20002"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20003"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20003"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20003"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20001"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20001"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20001"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20005"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20005"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20005"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20009"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20009"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20009"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20017"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20017"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20017"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20010"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20010"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20010"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20016"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20016"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20016"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20008"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20008"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20008"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20011"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20011"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20011"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20007"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20007"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20007"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20374"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20374"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20374"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20015"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20015"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20015"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20012"){
        cs2 <- mean(cs1$current_travel_time[cs1$zip_code == "20012"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$zip_code == "20012"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }
    }else if(input$loc.type == "Advisory Neighborhood Commission"){
      if(input$loc.type2 == "2A"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "2A"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "2A"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "8D"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "8D"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "8D"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "8E"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "8E"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "8E"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "8C"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "8C"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "8C"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "7F"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "7F"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "7F"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "7C"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "7C"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "7C"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "7E"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "7E"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "7E"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5C"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "5C"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "5C"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6D"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "6D"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "6D"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5D"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "5D"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "5D"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6B"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "6B"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "6B"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5E"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "5E"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "5E"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "2F"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "2F"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "2F"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "2B"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "2B"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "2B"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "1B"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "1B"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "1B"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6E"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "6E"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "6E"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5B"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "5B"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "5B"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "1A"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "1A"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "1A"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3C"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "3C"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "3C"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6A"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "6A"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "6A"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3D"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "3D"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "3D"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "4C"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "4C"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "4C"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3E"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "3E"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "3E"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "2E"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "2E"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "2E"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6C"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "6C"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "6C"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "8A"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "8A"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "8A"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "4D"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "4D"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "4D"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "4B"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "4B"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "4B"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "4A"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "4A"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "4A"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3G"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "3G"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "3G"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3F"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "3F"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "3F"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5A"){
        cs2 <- mean(cs1$current_travel_time[cs1$anc == "5A"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$anc == "5A"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }
    }else if(input$loc.type == "Census Tract"){
      if(input$loc.type2 == "006202"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "006202"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "006202"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "009811"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "009811"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "009811"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "009807"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "009807"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "009807"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "009700"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "009700"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "009700"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "009804"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "009804"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "009804"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "007304"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "007304"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "007304"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "007301"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "007301"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "007301"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "010400"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "010400"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "010400"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "010800"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "010800"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "010800"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "005600"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "005600"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "005600"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "009603"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "009603"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "009603"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "007809"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "007809"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "007809"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "009902"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "009902"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "009902"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "009000"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "009000"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "009000"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "009905"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "009905"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "009905"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "007707"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "007707"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "007707"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "007806"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "007806"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "007806"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "007804"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "007804"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "007804"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "007807"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "007807"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "007807"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "007708"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "007708"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "007708"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "010500"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "010500"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "010500"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "006400"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "006400"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "006400"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "008803"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "008803"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "008803"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "010200"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "010200"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "010200"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "006500"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "006500"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "006500"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "008702"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "008702"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "008702"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "003301"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "003301"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "003301"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "004902"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "004902"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "004902"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "004201"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "004201"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "004201"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "003400"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "003400"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "003400"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "004400"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "004400"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "004400"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "011100"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "011100"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "011100"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "004702"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "004702"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "004702"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "009302"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "009302"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "009302"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "008701"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "008701"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "008701"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "010100"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "010100"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "010100"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "002900"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "002900"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "002900"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "004600"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "004600"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "004600"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "009400"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "009400"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "009400"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "001002"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "001002"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "001002"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "010600"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "010600"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "010600"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "000901"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "000901"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "000901"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "000502"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "000502"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "000502"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "009102"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "009102"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "009102"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "009201"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "009201"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "009201"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "002400"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "002400"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "002400"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "001001"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "001001"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "001001"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "000600"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "000600"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "000600"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "002501"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "002501"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "002501"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "009503"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "009503"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "009503"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "000300"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "000300"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "000300"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "000400"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "000400"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "000400"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "006700"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "006700"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "006700"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "007200"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "007200"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "007200"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "008302"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "008302"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "008302"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "006600"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "006600"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "006600"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "008904"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "008904"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "008904"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "008100"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "008100"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "008100"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "006802"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "006802"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "006802"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "007601"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "007601"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "007601"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "007100"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "007100"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "007100"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "008200"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "008200"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "008200"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "008001"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "008001"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "008001"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "008002"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "008002"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "008002"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "002101"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "002101"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "002101"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "001901"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "001901"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "001901"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "002600"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "002600"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "002600"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "001401"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "001401"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "001401"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "001803"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "001803"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "001803"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "001301"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "001301"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "001301"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "002102"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "002102"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "002102"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "001804"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "001804"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "001804"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "002001"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "002001"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "002001"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "001902"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "001902"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "001902"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "001100"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "001100"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "001100"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "001600"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "001600"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "001600"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "009509"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "009509"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "009509"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "001500"){
        cs2 <- mean(cs1$current_travel_time[cs1$census_tract == "001500"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$census_tract == "001500"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }
    }else if(input$loc.type == "Single Member District"){
      if(input$loc.type2 == "2A01"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "2A01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "2A01"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "8D03"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "8D03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "8D03"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "8D01"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "8D01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "8D01"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "8D06"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "8D06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "8D06"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "8E05"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "8E05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "8E05"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "8C07"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "8C07"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "8C07"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "8E04"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "8E04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "8E04"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "8C05"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "8C05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "8C05"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "8C03"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "8C03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "8C03"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "2A07"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "2A07"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "2A07"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "2A03"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "2A03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "2A03"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "7F01"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "7F01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "7F01"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "7C04"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "7C04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "7C04"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "7E02"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "7E02"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "7E02"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5C03"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "5C03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "5C03"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "7E06"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "7E06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "7E06"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "7E01"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "7E01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "7E01"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "7C07"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "7C07"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "7C07"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "7C03"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "7C03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "7C03"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "7C06"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "7C06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "7C06"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "7F06"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "7F06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "7F06"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6D03"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "6D03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "6D03"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6D06"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "6D06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "6D06"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5D01"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "5D01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "5D01"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6D05"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "6D05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "6D05"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6B01"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "6B01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "6B01"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5E03"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "5E03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "5E03"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5E08"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "5E08"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "5E08"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "2F06"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "2F06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "2F06"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "2B09"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "2B09"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "2B09"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "1B01"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "1B01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "1B01"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "1B02"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "1B02"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "1B02"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5C02"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "5C02"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "5C02"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6E07"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "6E07"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "6E07"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5B03"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "5B03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "5B03"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5E04"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "5E04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "5E04"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "2F08"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "2F08"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "2F08"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "1A04"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "1A04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "1A04"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5E05"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "5E05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "5E05"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5C01"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "5C01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "5C01"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3C06"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "3C06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "3C06"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6A01"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "6A01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "6A01"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "2B05"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "2B05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "2B05"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3D02"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "3D02"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "3D02"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3C03"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "3C03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "3C03"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5B01"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "5B01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "5B01"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5C06"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "5C06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "5C06"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5E01"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "5E01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "5E01"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "4C08"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "4C08"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "4C08"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3E05"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "3E05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "3E05"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3C09"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "3C09"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "3C09"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "4C03"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "4C03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "4C03"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "2E01"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "2E01"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "2E01"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3C08"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "3C08"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "3C08"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6B05"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "6B05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "6B05"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5C04"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "5C04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "5C04"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6D07"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "6D07"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "6D07"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6C03"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "6C03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "6C03"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6B02"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "6B02"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "6B02"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5D05"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "5D05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "5D05"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6A03"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "6A03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "6A03"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6B09"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "6B09"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "6B09"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "8A03"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "8A03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "8A03"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6B07"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "6B07"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "6B07"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6B06"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "6B06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "6B06"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6A02"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "6A02"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "6A02"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6A08"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "6A08"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "6A08"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "4D03"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "4D03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "4D03"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "4B04"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "4B04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "4B04"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "4A08"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "4A08"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "4A08"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3D03"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "3D03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "3D03"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3G05"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "3G05"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "3G05"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "4A07"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "4A07"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "4A07"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3E02"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "3E02"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "3E02"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3F03"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "3F03"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "3F03"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "4B06"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "4B06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "4B06"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "4A04"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "4A04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "4A04"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "4A06"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "4A06"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "4A06"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3E04"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "3E04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "3E04"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "4A02"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "4A02"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "4A02"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "5A08"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "5A08"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "5A08"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3G04"){
        cs2 <- mean(cs1$current_travel_time[cs1$single_member_district == "3G04"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$single_member_district == "3G04"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }
    }else if(input$loc.type == "Voter Precinct"){
      if(input$loc.type2 == "129"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "129"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "129"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "125"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "125"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "125"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "126"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "126"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "126"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "121"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "121"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "121"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "122"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "122"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "122"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "120"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "120"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "120"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "123"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "123"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "123"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "2"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "2"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "2"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "3"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "3"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "3"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "102"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "102"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "102"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "94"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "94"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "94"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "110"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "110"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "110"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "139"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "139"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "139"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "105"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "105"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "105"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "106"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "106"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "106"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "93"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "93"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "93"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "97"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "97"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "97"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "95"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "95"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "95"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "132"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "132"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "132"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "128"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "128"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "138"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "127"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "127"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "127"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "76"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "76"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "76"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "130"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "130"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "130"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "75"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "75"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "75"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "135"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "135"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "135"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "141"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "141"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "141"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "20"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "20"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "20"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "22"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "22"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "22"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "72"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "72"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "72"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "1"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "1"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "1"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "73"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "73"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "73"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "42"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "42"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "42"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "19"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "19"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "19"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "69"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "69"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "69"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "29"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "29"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "29"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "82"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "82"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "82"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "17"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "17"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "17"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "9"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "9"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "9"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "26"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "26"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "26"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "74"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "74"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "74"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "45"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "45"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "45"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "30"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "30"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "30"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "27"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "27"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "27"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "48"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "48"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "48"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "67"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "67"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "67"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "6"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "6"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "6"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "12"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "12"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "12"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "88"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "88"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "88"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "131"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "131"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "131"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "85"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "85"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "85"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "89"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "89"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "89"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "79"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "79"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "79"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "91"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "91"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "91"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "133"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "133"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "133"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "81"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "81"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "81"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "71"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "71"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "71"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "86"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "86"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "86"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "56"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "56"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "56"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "59"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "59"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "59"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "50"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "50"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "50"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "61"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "61"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "61"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "31"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "31"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "31"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "138"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "138"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "138"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "57"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "57"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "57"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "60"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "60"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "60"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "53"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "53"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "53"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "32"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "32"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "32"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "62"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "62"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "62"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "66"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "66"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "66"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", round(cs3, digits = 0), " mph)")
      }else if(input$loc.type2 == "51"){
        cs2 <- mean(cs1$current_travel_time[cs1$voter_precinct == "51"], na.rm = TRUE)
        cs3 <- mean(cs1$free_flow_travel_time[cs1$voter_precinct == "51"], na.rm = TRUE)
        str_c("Current travel time: ", round(cs2, digits = 0), " mph (Free flow travel time: ", 
              round(cs3, digits = 0), " mph)")
      }
    }
    
    
    
  })
  output$current.air.quality <- renderText({
    cs1 <- traffic.flow %>% 
      filter(date == parse_datetime(str_c(as.character(input$date1), 
                                          " ", 
                                          as.character(input$hour), 
                                          ":00:00")))
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
    
    cs2 <- round(cs2, digits = 0)
    
    if(cs2 == 1){
      str_c("Current average air quality: ", "Good")
    }else if(cs2 == 2){
      str_c("Current average air quality: ", "Moderate")
    }else if(cs2 == 3){
      str_c("Current average air quality: ", "Unhealthy for sensitive groups")
    }else if(cs2 == 4){
      str_c("Current average air quality: ", "Unhealthy")
    }else if(cs2 == 5){
      str_c("Current average air quality: ", "Very unhealthy")
    }else if(cs2 == 6){
      str_c("Current average air quality: ", "Hazardous")
    }
    
    
    
  })
  
  output$location.graph <- renderPlot({
    if(input$loc.type == "Quadrant"){
      if(input$loc.type2 == "SE"){
        cs2 <- traffic.flow %>% filter(quadrant == "SE")
        traffic.flow %>% filter(quadrant == "SE")
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
    
    g <- cs3 %>% 
      ggplot(data = ., aes(x = date)) +
      labs(x = "Date", y = "Air Quality Index", 
           title = str_c(input$loc.type, ": ", input$loc.type2))
    g1 <- geom_line(aes(y = mean_ozone_loc_aqi), color = "blue")
    g2 <- geom_line(aes(y = mean_so2_loc_aqi), color = "red")
    g3 <- geom_line(aes(y = mean_pm2.5_loc_aqi), color = "orange")
    g4 <- geom_line(aes(y = mean_no2_loc_aqi), color = "green")
    
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
    
    g <- cs4 %>% 
      ggplot(data = ., aes(x = date)) +
      labs(x = "Date", y = "Traffic Value",
           title = str_c(input$loc.type, ": ", input$loc.type2))
    g1 <- geom_line(aes(y = mean_current_speed), color = "blue")
    g2 <- geom_line(aes(y = mean_free_flow_speed), color = "red")
    g3 <- geom_line(aes(y = mean_current_travel_time), color = "orange")
    g4 <- geom_line(aes(y = mean_free_flow_travel_time), color = "green")
    
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
  })
  output$loc.type.graph1 <- renderPlot({
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
        geom_line(aes(y = mean_ozone_loc_aqi, color = quadrant))
      
      p2 <- ggplot(data = cs3, aes(x = date)) +
        labs(x = "Date", y = "SO2 AQI",
             title = str_c(input$loc.type)) +
        geom_line(aes(y = mean_so2_loc_aqi, color = quadrant)) 
      
      p3 <- ggplot(data = cs3, aes(x = date)) +
        labs(x = "Date", y = "PM 2.5 AQI",
             title = str_c(input$loc.type)) +
        geom_line(aes(y = mean_pm2.5_loc_aqi, color = quadrant)) 
      
      p4 <- ggplot(data = cs3, aes(x = date)) +
        labs(x = "Date", y = "NO2 AQI",
             title = str_c(input$loc.type)) +
        geom_line(aes(y = mean_no2_loc_aqi, color = quadrant)) 
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
        geom_line(aes(y = mean_ozone_loc_aqi, color = ward))
      
      p2 <- ggplot(data = cs3, aes(x = date)) +
        labs(x = "Date", y = "SO2 AQI",
             title = str_c(input$loc.type)) +
        geom_line(aes(y = mean_so2_loc_aqi, color = ward)) 
      
      p3 <- ggplot(data = cs3, aes(x = date)) +
        labs(x = "Date", y = "PM 2.5 AQI",
             title = str_c(input$loc.type)) +
        geom_line(aes(y = mean_pm2.5_loc_aqi, color = ward)) 
      
      p4 <- ggplot(data = cs3, aes(x = date)) +
        labs(x = "Date", y = "NO2 AQI",
             title = str_c(input$loc.type)) +
        geom_line(aes(y = mean_no2_loc_aqi, color = ward)) 
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
        geom_line(aes(y = mean_ozone_loc_aqi, color = zip_code))
      
      p2 <- ggplot(data = cs3, aes(x = date)) +
        labs(x = "Date", y = "SO2 AQI",
             title = str_c(input$loc.type)) +
        geom_line(aes(y = mean_so2_loc_aqi, color = zip_code)) 
      
      p3 <- ggplot(data = cs3, aes(x = date)) +
        labs(x = "Date", y = "PM 2.5 AQI",
             title = str_c(input$loc.type)) +
        geom_line(aes(y = mean_pm2.5_loc_aqi, color = zip_code)) 
      
      p4 <- ggplot(data = cs3, aes(x = date)) +
        labs(x = "Date", y = "NO2 AQI",
             title = str_c(input$loc.type)) +
        geom_line(aes(y = mean_no2_loc_aqi, color = zip_code)) 
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
        geom_line(aes(y = mean_ozone_loc_aqi, color = anc))
      
      p2 <- ggplot(data = cs3, aes(x = date)) +
        labs(x = "Date", y = "SO2 AQI",
             title = str_c(input$loc.type)) +
        geom_line(aes(y = mean_so2_loc_aqi, color = anc)) 
      
      p3 <- ggplot(data = cs3, aes(x = date)) +
        labs(x = "Date", y = "PM 2.5 AQI",
             title = str_c(input$loc.type)) +
        geom_line(aes(y = mean_pm2.5_loc_aqi, color = anc)) 
      
      p4 <- ggplot(data = cs3, aes(x = date)) +
        labs(x = "Date", y = "NO2 AQI",
             title = str_c(input$loc.type)) +
        geom_line(aes(y = mean_no2_loc_aqi, color = anc)) 
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
        geom_line(aes(y = mean_ozone_loc_aqi, color = census_tract))
      
      p2 <- ggplot(data = cs3, aes(x = date)) +
        labs(x = "Date", y = "SO2 AQI",
             title = str_c(input$loc.type)) +
        geom_line(aes(y = mean_so2_loc_aqi, color = census_tract)) 
      
      p3 <- ggplot(data = cs3, aes(x = date)) +
        labs(x = "Date", y = "PM 2.5 AQI",
             title = str_c(input$loc.type)) +
        geom_line(aes(y = mean_pm2.5_loc_aqi, color = census_tract)) 
      
      p4 <- ggplot(data = cs3, aes(x = date)) +
        labs(x = "Date", y = "NO2 AQI",
             title = str_c(input$loc.type)) +
        geom_line(aes(y = mean_no2_loc_aqi, color = census_tract)) 
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
        geom_line(aes(y = mean_ozone_loc_aqi, color = single_member_district))
      
      p2 <- ggplot(data = cs3, aes(x = date)) +
        labs(x = "Date", y = "SO2 AQI",
             title = str_c(input$loc.type)) +
        geom_line(aes(y = mean_so2_loc_aqi, color = single_member_district)) 
      
      p3 <- ggplot(data = cs3, aes(x = date)) +
        labs(x = "Date", y = "PM 2.5 AQI",
             title = str_c(input$loc.type)) +
        geom_line(aes(y = mean_pm2.5_loc_aqi, color = single_member_district)) 
      
      p4 <- ggplot(data = cs3, aes(x = date)) +
        labs(x = "Date", y = "NO2 AQI",
             title = str_c(input$loc.type)) +
        geom_line(aes(y = mean_no2_loc_aqi, color = single_member_district)) 
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
        geom_line(aes(y = mean_ozone_loc_aqi, color = voter_precinct))
      
      p2 <- ggplot(data = cs3, aes(x = date)) +
        labs(x = "Date", y = "SO2 AQI",
             title = str_c(input$loc.type)) +
        geom_line(aes(y = mean_so2_loc_aqi, color = voter_precinct)) 
      
      p3 <- ggplot(data = cs3, aes(x = date)) +
        labs(x = "Date", y = "PM 2.5 AQI",
             title = str_c(input$loc.type)) +
        geom_line(aes(y = mean_pm2.5_loc_aqi, color = voter_precinct)) 
      
      p4 <- ggplot(data = cs3, aes(x = date)) +
        labs(x = "Date", y = "NO2 AQI",
             title = str_c(input$loc.type)) +
        geom_line(aes(y = mean_no2_loc_aqi, color = voter_precinct)) 
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
  }) 
  output$loc.type.graph2 <- renderPlot({
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
  }) 
}


# Run ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)
