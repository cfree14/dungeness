
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(raster)
library(tidyverse)
library(lubridate)
library(fasterize)
library(cowplot)
library(lubridate)
library(grid)
library(gridExtra)

# Directories
outputdir <- "data/whale_sdms/data"
plotdir <- "data/whale_sdms/figures"
depthdir <- "data/bathymetry/processed"

# Read whale data
data <- brick(file.path(outputdir, "forney_humpback_sdm_3km_2014forward_brick_100fathoms.grd"))


# Build data
################################################################################

# Convert raster to data frame
build <- F
if(build){
  
  # Build data
  data_df <- data %>% 
    as.data.frame(xy=T) %>% 
    rename(long_dd=x, lat_dd=y) %>% 
    gather(key="date", value="nwhales", 3:ncol(.)) %>% 
    filter(!is.na(nwhales)) %>% 
    mutate(date=gsub("X", "", date) %>% ymd())
  
  # Export data
  saveRDS(data_df, file=file.path(outputdir, "forney_humpback_sdm_3km_2014forward_100fathoms_df.Rds"))
  
}else{
  
  # Read data
  data_df <-   readRDS(file.path(outputdir, "forney_humpback_sdm_3km_2014forward_100fathoms_df.Rds"))
}

# Build Hovmoller data
hov_data <- data_df %>% 
  mutate(whales_sqkm=nwhales / 9) %>% 
  group_by(date, lat_dd) %>% 
  summarize(whales_sqkm=median(nwhales)) %>% 
  ungroup()

# Whale-weighted latitude
lat_gravity <- hov_data %>% 
  group_by(date) %>% 
  summarize(lat_wt=weighted.mean(lat_dd, whales_sqkm))



# Calculate migration corridor
################################################################################

# RAMP zone lats
# Zone 1: 32.5 - 36
# Zone 2: 36 - 37.18333
# Zone 3: 37.18333 - 38.76875
# Zone 4: 38.76875 - 40.08333
# Zone 5: 40.08333 - 42
ramp_lats <- c(42, 
               40.08333, 
               38.76875, 
               37.18333,
               36,
               32.5) 

# Model day 1
model_day1 <- yday(ymd("2015-10-01"))

# Build data
sdata <- lat_gravity %>% 
  mutate(year=year(date),
         julian_day=yday(date), 
         model_day=ifelse(julian_day>=model_day1, julian_day-model_day1, julian_day+(365-model_day1 )))

# Closures:
# Zone 1: 85-225
# Zone 2: 85-250
# Zone 3: 45-85, 210-273
closures <- tibble(zone = paste("Zone", c(1:3, 3)),
                   lat_lo = c(34.8, 36, 37.18333, 37.18333),
                   lat_hi = c(36, 37.18333, 38.76875, 38.76875),
                   day1 = c(85, 85, 45, 210),
                   day2 = c(225, 250, 85, 273))
  
# Latitude: 
# Central: Nov 15 - Jun 30; Northern: Dec 1 - Jul 15
c_open <- yday(ymd("2015-11-15")) - model_day1
c_close <- yday(ymd("2016-06-30")) + 365 - model_day1
n_open <- yday(ymd("2015-12-01")) - model_day1
n_close <- yday(ymd("2016-07-15"))  + 365 - model_day1

# Plot
g <- ggplot(sdata, aes(x=model_day, y=lat_wt, group=year, color=as.factor(year))) +
  geom_line() +
  # Add RAMP zone lines
  geom_hline(yintercept=ramp_lats, color="black", lwd=0.3, linetype="dashed") +
  # Plot Mendocino country line
  # geom_hline(yintercept=38.77) +
  # Plot central season open/close
  geom_segment(mapping=aes(x = c_open, y = 32.5, xend = c_open, yend = 38.77), inherit.aes=F, lwd=1.5) +
  geom_segment(mapping=aes(x = c_close, y = 32.5, xend = c_close, yend = 38.77), inherit.aes=F, lwd=1.5) +
  # Plot central season open/close
  geom_segment(mapping=aes(x = n_open, y = 38.77, xend = n_open, yend = 42.5), inherit.aes=F, lwd=1.5) +
  geom_segment(mapping=aes(x = n_close, y = 38.77, xend = n_close, yend = 42.5), inherit.aes=F, lwd=1.5) +
  # Plot migration corridor closures
  geom_rect(data=closures, mapping=aes(ymin=lat_lo, ymax=lat_hi, 
                                       xmin=day1, xmax=day2, group=zone),
            inherit.aes = F, alpha=0.3) +
  # Theme
  theme_bw()
g




