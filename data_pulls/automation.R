# Packages ----------------------------------------------------------------
library(tidyverse)
library(cronR)

# Hourly Traffic and Sensor Data ------------------------------------------
working.directory <- getwd()
hourly.auto.file <- str_c(working.directory, "/hourly.R")

cmd.auto.hourly <- cron_rscript(hourly.auto.file)
cron_add(command = cmd.auto.hourly, frequency = "hourly")

## to stop running, cron_clear()



