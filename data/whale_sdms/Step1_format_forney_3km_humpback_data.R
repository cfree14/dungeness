
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

# Directories
inputdir <- "data/whale_sdms/data"
outputdir <- "data/whale_sdms/data"
plotdir <- "data/whale_sdms/figures"

# Read data
# This takes 33 minutes
start_time <- Sys.time()
data_orig <- read.csv(file.path(inputdir, "Humpback_3km_Model1_2005-01-01to2019-08-14_bi-daily_densities.csv"))
# data_orig <- read.csv(file.path(inputdir, "Humpback_3km_Model1_2005-01-01to2019-08-14_bi-daily_densities.csv"), nrows=10)
end_time <- Sys.time()
read_time <- end_time - start_time

# Format data
################################################################################

# Dates
# 1/1/2005 - 8/14/2019 every other day

# Inspect columns
# First 13 are cell specific
# Remainder are date specific: mean/standard error density
colnames(data_orig)[1:13]
# pixel -------- pixel number
# mlat --------- pixel latitude
# mlon --------- pixel longitude (-180, 180)
# mlon360 ------ pixel longitude (0, 360)
# areakm ------- pixel area (sq. km)
# n.Dens ------- mean number of whales
# Avg.Dens ----- mean density of whales
# STD.Dens ----- standard deviation density of whales
# L90STD.dens -- lower 90th percentile density of whales (SD)
# U90STD.dens -- upper 90th percentile density of whales (SD)
# SE.Dens ------ standard error density of whales
# L90SE.dens --- lower 90th percentile density of whales (SE)
# U90SE.dens --- upper 90th percentile density of whales (SE)

# Column names
all_cols <- colnames(data_orig)
dens_cols <- all_cols[grepl("X76.dens", all_cols)]
dens_se_cols <- all_cols[grepl("X76.dens", all_cols)]
key_cols <- c("pixel", "mlat", "mlon", "areakm")

# Format data
# This takes 7 minutes
data <- data_orig %>% 
  # Reduce to key cell columns and mean densities
  select(c(key_cols, dens_cols)) %>% 
  # Convert wide to long
  gather(key="date", value="whales_sqkm", (length(key_cols)+1):ncol(.)) %>% 
  # Format date
  mutate(date=date %>% gsub("X76.dens.", "", .) %>% ymd()) %>% 
  # Rename columns
  rename(cell=pixel, lat_dd=mlat, long_dd=mlon, area_sqkm=areakm) %>% 
  # Add number of whales
  mutate(whales_n=area_sqkm*whales_sqkm)

# Format time
format_time <- Sys.time() - end_time

# Plot one date
g <- ggplot(data %>% filter(date=="2005-05-01"), aes(x=long_dd, y=lat_dd, fill=whales_n)) +
  geom_raster() +
  theme_bw() +
  theme(axis.title = element_blank()) +
  scale_fill_gradientn(name="Number of whales", colors=rev(RColorBrewer::brewer.pal(9, "RdBu")))
g

# Export formatted data
# This takes 4.5 minutes
start_time <- Sys.time()
saveRDS(data, file=file.path(outputdir, "Humpback_3km_Model1_2005-01-01to2019-08-14_bi-daily_densities.Rds"))
export_time <- Sys.time() - start_time

# Export subsetted data
data_use <- data %>% 
  filter(date>="2014-01-01")

# Export formatted data
saveRDS(data_use, file=file.path(outputdir, "forney_humpback_sdm_3km_2014forward.Rds"))


# Calculate weekly average
################################################################################

# Calculate weekly averages
stats <- data %>% 
  # Reduce to post 2015
  filter(date >= "2015-01-01") %>% 
  # Add Julian week
  mutate(week=week(date)) %>% 
  # Average cell by week
  group_by(cell, lat_dd, long_dd, area_sqkm, week) %>% 
  summarize(whales_sqkm_avg=mean(whales_sqkm))


#  Whale migration animation
################################################################################

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
  geom_raster(data=stats, mapping=aes(x=long_dd, y=lat_dd, fill=whales_sqkm_avg*100, group=week)) +
  scale_fill_gradientn(name="Density\n(whales per 100 km2)",
                       limits=c(0,6),
                       breaks=seq(0,6,1),
                       colors=rev(RColorBrewer::brewer.pal(11, "RdBu")),
                       na.value = NA) +
  # guides(fill=guide_colorbar(title.position="top")) +
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
  transition_manual(week) +
  labs(title="Week {current_frame}") 
  # ease_aes('linear')
  # transition_states(week, state_length = 0.2, transition_length = 0) + 
  # ggtitle("Week {closest_state}")



# print(g)

# Export single state to get size right
# ggsave(g, filename=file.path(plotdir, "whale_density_animation_single.png"), 
#        width=4.5, height=6.5, units="in", dpi=600)


# Save animation
anim_save(g, filename=file.path(plotdir, "whale_density_animation.gif"), duration=10,
          width=4.5, height=6.5, units="in", res=600)




