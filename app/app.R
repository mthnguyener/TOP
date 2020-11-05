# Load Packages -----------------------------------------------------------
library(shiny)
library(tidyverse)

# Load Data ---------------------------------------------------------------
hourly <- read_csv("hourly.csv")
traffic.flow <- read_csv("traffic_flow.csv")

# User Interface ----------------------------------------------------------

ui <- fluidPage(
  titlePanel("Assessment of Air Quality and Traffic Volume"),
  h4("Washington, D.C."),
  
  tabsetPanel(
    tabPanel("Citywide"),
    tabPanel("Location Search")
  )
)
?titlePanel

# Server ------------------------------------------------------------------


server <- function(input, output) {
  
  
}


# Run ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)