

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(rio)
library(zoo)
library(tidyverse)
library(lubridate)

# Directories
datadir <- "data/da_sampling/processed"
charmdir <- "data/charm/processed"
outputdir <- "models/contamination/data"

# Read data
data_orig <- read.csv(file.path(datadir, "CDPH_crab_viscera_da_data.csv"), as.is=T)
pn_brick <- brick(file.path(charmdir, "PN_nowcast_2014present_brick.grd"))
dap_brick <- brick(file.path(charmdir, "DAP_nowcast_2014present_brick.grd"))
dac_brick <- brick(file.path(charmdir, "DAC_nowcast_2014present_brick.grd"))

# Build data
################################################################################

# Parameters
da_ppm_trigger <- 30 # DA concentration that triggers mgmt action
nday_prev <- 30 # number of days before sampling that could contribute to contamination

# Format crab sample data
data <- data_orig %>%
  # Dungeness crab with DA measurements after 2014
  filter(species=="Dungeness crab" & !is.na(da_ppm)) %>% 
  rename(sample_date=date) %>% 
  # Samples above action threshold
  mutate(over=ifelse(da_ppm >= da_ppm_trigger, 1, 0)) %>% 
  # Add lat/long to use (use specific coords, then block coords)
  mutate(lat_dd_use=ifelse(!is.na(lat_dd) & !is.na(long_dd), lat_dd, block_lat_dd),
         long_dd_use=ifelse(!is.na(lat_dd) & !is.na(long_dd), long_dd, block_long_dd)) %>% 
  filter(!is.na(lat_dd_use) & !is.na(long_dd_use))

# Loop through samples and build time series of DAP exposure
x <- "083110215B2"
df <- purrr::map_df(data$sampleid, function(x) {
  
  # Sample date
  sample_date <- data$sample_date[data$sampleid==x]
  first_date <- ymd(sample_date) - nday_prev
  
  # Sample coordinates
  xy <- data %>% 
    filter(sampleid==x) %>% 
    select(long_dd_use, lat_dd_use) %>% 
    as.matrix()
  
  # Extract DAP for sample up to X days before sample date
  vals <- raster::extract(dap_brick, xy, df = TRUE) %>% 
    gather(key="date", value="dap_prob", 2:ncol(.)) %>% 
    mutate(sampleid=x,
           date=ymd(gsub("X", "", date))) %>% 
    select(sampleid, date, dap_prob) %>% 
    # Reduce to days before sample date
    filter(date<=sample_date & date>=first_date)
    
})

# Inspect sample size to make sure everything < 31
df_n <- df %>% 
  group_by(sampleid) %>% 
  summarize(n=n())

# Spread long-to-wide
df_wide <- df %>% 
  group_by(sampleid) %>% 
  mutate(day=paste0("day", n():1-1)) %>% 
  select(-date) %>% 
  spread(key="day", value="dap_prob") %>% 
  filter(!is.na(day0))

# Build results
results <- data %>% 
  left_join(df_wide, by="sampleid") %>% 
  filter(!is.na(day0))

# Export results
saveRDS(results, file.path(outputdir, "CHARM_crab_da_data.Rds"))






