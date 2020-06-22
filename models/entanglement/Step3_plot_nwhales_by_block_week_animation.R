
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(tidyverse)

# Animation packages
# devtools::install_github('thomasp85/gganimate', force=T)
library(gganimate)
library(gifski)

# Directories
gisdir <- "data/cdfw/gis_data/processed"
datadir <- "output"
plotdir <- "models/figures"

# Export weekly block-level whale abundance predictions
data <- readRDS(file.path(datadir, "nwhales_block_week.Rds"))

# Read CDFW blocks shapefile
blocks_sf <- readRDS(file.path(gisdir, "CA_commercial_fishing_blocks.Rds"))


# Build data
################################################################################

# Add weekly whale count to spatial layer
blocks_nwhales <- blocks_sf %>% 
  left_join(data, by="block_id") %>% 
  filter(block_type=="Inshore") %>% 
  mutate(whales_sqkm=whales_n/block_area_sqkm, 
         whales_100sqkm_cap=pmin(whales_sqkm*100, 10))

# Where to cap density in visualization?
hist(blocks_nwhales$whales_sqkm*100)
hist(blocks_nwhales$whales_100sqkm_cap)


# Plot data
################################################################################

# Get US states and Mexico
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# Plot 
g <- ggplot() +
  # Plot whale density
  geom_sf(blocks_nwhales %>% filter(date=="2014-10-01"), mapping=aes(fill=whales_100sqkm_cap)) + # single date
  # geom_sf(blocks_nwhales, mapping=aes(fill=whales_100sqkm_cap, group=date)) +
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
ggsave(g, filename=file.path(plotdir, "whale_n_by_block_date_single.png"),
       width=4.5, height=6.5, units="in", dpi=600)


# Save animation
anim_save(g, filename=file.path(plotdir, "whale_n_by_block_date_animation.gif"), duration=10,
          width=4.5, height=6.5, units="in", res=600)



