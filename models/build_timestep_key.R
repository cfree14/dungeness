
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
datadir <- "models"


# Build key
################################################################################

# Dates
start_date <- ymd("2014-10-01")
end_date <- ymd("2019-10-01")
dates <- seq(start_date, end_date, by="week")[1:(52*5)]
seasons <- c("2014-15", "2015-16", "2016-17", "2017-18", "2018-19")

# Build date key
# season, date, week, fishing_n, fishing_c
# Central: Nov 15 - Jun 30 (Weeks 8-40); Northern: Dec 1 - Jul 15 (Weeks 10-42)
date_key <- tibble(date=dates, 
                   week=rep(1:52, 5),
                   season=sort(rep(seasons, 52))) %>% 
  select(season, week, date) %>% 
  mutate(fishing_c=week %in% 8:40,
         fishing_n=week %in% 10:42,
         year=substr(season, 1, 4) %>% as.numeric) %>% 
  select(year, everything())

# Export
write.csv(date_key, file=file.path(datadir, "model_time_step_key.csv"), row.names = F)


