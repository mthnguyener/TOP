# Packages ----------------------------------------------------------------
library(tidyverse)
library(cronR)

# AirNow Automation -------------------------------------------------------
working.directory <- getwd()
airnow.auto.file <- str_c(working.directory, "/airnow.R")

cmd.auto.airnow <- cron_rscript(airnow.auto.file)
cron_add(command = cmd.auto.airnow, frequency = "hourly")

# Hourly Traffic Automation -----------------------------------------------
traffic.auto.file <- str_c(working.directory, "/hourly_traffic.R")

cmd.auto.traffic <- cron_rscript(traffic.auto.file)
cron_add(command = cmd.auto.traffic, frequency = "hourly")

## to stop running, cron_clear()

