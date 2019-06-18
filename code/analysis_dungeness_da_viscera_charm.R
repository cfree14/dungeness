

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
datadir <- "data/da_sampling/raw"
charmdir <- "data/charm/processed"

# Read data
data <- read.csv(file.path(datadir, "crab_da_data_temp.csv"), as.is=T)
pn_brick <- readRDS(file.path(charmdir, "PN_nowcast_2014present_brick.Rds"))
dap_brick <- readRDS(file.path(charmdir, "DAP_nowcast_2014present_brick.Rds"))
dac_brick <- readRDS(file.path(charmdir, "DAC_nowcast_2014present_brick.Rds"))


# Build data
################################################################################

# Build data
# Just D crabs with DA measurements
data1 <- data %>%
  filter(species=="Dungeness crab" & !is.na(da_ppm)) %>% 
  rename(sample_date=date) %>% 
  mutate(over=da_ppm>=30)


# Loop through samples and build time series of DAC exposure
x <- "083110215B2"
df <- purrr::map_df(data1$sampleid, function(x) {
  
  # Sample coordinates
  xy <- data1 %>% 
    filter(sampleid==x) %>% 
    select(block_long_dd, block_lat_dd) %>% 
    as.matrix()
  
  vals <- raster::extract(dac_brick, xy, df = TRUE) %>% 
    gather(key="date", value="dac_prob", 2:ncol(.)) %>% 
    mutate(sampleid=x) %>% 
    select(sampleid, date, dac_prob)
  
})

# Build results
results <- df %>% 
  # Format date
  mutate(date=ymd(gsub("X", "", date))) %>% 
  # Add sample date
  left_join(select(data1, sampleid, sample_date, da_ppm), by="sampleid") %>% 
  # Filter to only days 30 days before sample date
  group_by(sampleid) %>% 
  filter(date <= sample_date & date > (ymd(sample_date) - days(30))) %>% 
  arrange(sampleid, desc(date)) %>% 
  mutate(day=as.numeric(floor(difftime(sample_date, date, units="days")))+1,
         dac_prob_avg=cumsum(dac_ppm)/day) %>% 
  # Spread long-to-wide
  spread(key="day", value="dac_prob_avg")
  
  
sdata <- results %>% 
  filter(day==30)

plot(da_ppm ~ dac_prob_avg, sdata)











