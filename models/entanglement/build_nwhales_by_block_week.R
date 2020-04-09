
# To do list
# I think the average is wider than 2-weeks right now b/c bi-daily

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(raster)
library(zoo)
library(tidyverse)
library(lubridate)
library(fasterize)
library(cowplot)
library(lubridate)

# Directories
datadir <- "data/whale_sdms/data"
plotdir <- "models/figures"
gisdir <- "data/cdfw/gis_data/processed"
outputdir <- "output"

# Read time step key
date_key <- read.csv(file.path("models", "model_time_step_key.csv"), as.is=T)

# Read whale SDM data
data <- readRDS(file=file.path(datadir, "forney_humpback_sdm_3km_2014forward.Rds"))

# Read CDFW blocks shapefile
blocks_sf <- readRDS(file.path(gisdir, "CA_commercial_fishing_blocks.Rds"))


# Build data
################################################################################

# Steps
# 1. Calculate two-week rolling averages
# 2. Select appropriate dates
# 3. Sum number of whales per cell


# 1. Calculate two-week rolling average for all dates
data_avg <- data %>% 
  # Remove old column
  select(-week) %>% 
  # Calculate rolling average for each cell
  arrange(cell, date) %>% 
  group_by(cell) %>% 
  mutate(whales_n_avg=rollmean(whales_n, k=15, align="center", fill=NA)) # 2-week (15 day) rolling average centered on day of interest

# 2. Reduce to dates of interest
data_avg_use <- data_avg %>% 
  ungroup() %>% 
  filter(date%in%as.Date(date_key$date))

# 3. Convert to raster so that you can sum by cell
data_avg_use_ras <- data_avg_use %>% 
  select(long_dd, lat_dd, date, whales_n_avg) %>% 
  spread(key="date", value="whales_n_avg") %>% 
  rasterFromXYZ(crs=crs(blocks_sf))

# 4. Sum by CA fishing block
data_avg_use_block <- raster::extract(x=data_avg_use_ras, y=blocks_sf, method="simple", fun="sum", na.rm=T)
data2 <- data_avg_use_block %>% 
  magrittr::set_rownames(blocks_sf$block_id) %>% 
  magrittr::set_colnames(unique(data_avg_use$date)) %>% 
  as.data.frame() %>% 
  mutate(block_id=rownames(.) %>% as.numeric()) %>% 
  select(block_id, everything()) %>% 
  gather(key="date", value="whales_n", 2:ncol(.)) %>% 
  mutate(date=as.Date.numeric(as.numeric(date)))

# 5. Add to SF layer
blocks_nwhales <- blocks_sf %>% 
  left_join(data2, by="block_id") %>% 
  filter(block_type=="Inshore") %>% 
  mutate(whales_sqkm=whales_n/block_area_sqkm, 
         whales_100sqkm_cap=pmin(whales_sqkm*100, 10))

# Export weekly block-level whale abundance predictions
saveRDS(data2, file=file.path(outputdir, "nwhales_block_week.Rds"))
  

# Build animation
################################################################################

hist(blocks_nwhales$whales_sqkm*100)
hist(blocks_nwhales$whales_100sqkm_cap)


# Animation packages
# devtools::install_github('thomasp85/gganimate', force=T)
library(gganimate)
library(gifski)

# Get US states and Mexico
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# Plot 
g <- ggplot() +
  # Plot whale density
  # geom_sf(blocks_nwhales %>% filter(date=="2014-10-01"), mapping=aes(fill=whales_100sqkm_cap)) + # single date
  geom_sf(blocks_nwhales, mapping=aes(fill=whales_100sqkm_cap, group=date)) +
  scale_fill_gradientn(name="Humpback density\n (whales per 100 sqkm)",
                       limits=c(0,10),
                       breaks=seq(0,10,2),
                       colors=rev(RColorBrewer::brewer.pal(9, "RdBu")),
                       na.value = NA) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Plot CA/Mexico
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Crop extent
  coord_sf(xlim = c(-125, -116), ylim = c(32, 42)) +
  theme_bw() + 
  theme(axis.title=element_blank(),
        legend.position = "bottom") +
  # Build animation
  transition_manual(date) +
  labs(title="{current_frame}")
#g

# Export single state to get size right
# ggsave(g, filename=file.path(plotdir, "whale_n_by_block_date_single.png"),
#        width=4.5, height=6.5, units="in", dpi=600)


# Save animation
anim_save(g, filename=file.path(plotdir, "whale_n_by_block_date_animation.gif"), duration=10,
          width=4.5, height=6.5, units="in", res=600)





