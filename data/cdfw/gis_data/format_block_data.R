
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(tidyverse)

# Directories
inputdir <- "data/cdfw/gis_data/raw/MAN_CA_CRFS_microblocks2013"
outputdir <- "data/cdfw/gisdata/processed"

# Read
data_orig <- sf::st_read(dsn=inputdir, layer="MAN_CA_CRFS_microblocks2013")

# Setup
################################################################################

# Format
data <- data_orig %>% 
  group_by(BLOCK10_ID) %>% 
  summarize()

plot(data)
