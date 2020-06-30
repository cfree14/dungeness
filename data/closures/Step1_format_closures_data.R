
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
datadir <- "data/closures/data"
  
# Read data
data_orig <- read.csv(file.path(datadir, "CA fishery closures spreadsheet - Data.csv"), as.is=T)


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  janitor::clean_names() %>% 
  mutate(date=mdy(date))
