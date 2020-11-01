# Packages ----------------------------------------------------------------
library(tidyverse)
library(cronR)

# AirNow Automation -------------------------------------------------------
working.directory <- getwd()
airnow.auto.file <- str_c(working.directory, "/airnow.R")

cmd.auto <- cron_rscript(airnow.auto.file)
cron_add(command = cmd.auto, frequency = "hourly")

## to stop running, cron_clear()

