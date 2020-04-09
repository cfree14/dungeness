
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(rnaturalearth)

# Directories
outputdir <- "output"
plotdir <- "figures"
gisdir <- "data/cdfw/gis_data/processed"

# Read data
data <- readRDS(file.path(outputdir, "one_year_simulation.Rds"))

# Read CDFW blocks shapefile
blocks_sf <- readRDS(file.path(gisdir, "CA_commercial_fishing_blocks.Rds"))


# Build animation
################################################################################

# Calculate weekly block stats
wbstats <- data %>% 
  rename(week=step) %>% 
  group_by(week, block_id) %>% 
  summarize(b_tot_mt=sum(biomass_mt, na.rm=T),
            c_tot_mt=sum(catch_mt, na.rm=T),
            ntraps=unique(traps_n)) %>% 
  mutate(ntraps=ifelse(is.na(ntraps), 0, ntraps))

# Calculate weekly stats
wstats <- wbstats %>% 
  group_by(week) %>% 
  summarize(ntraps=sum(ntraps))

# Add p(contaminated) statistics to blocks SF object
blocks_sf_ntraps <- blocks_sf %>% 
  filter(block_type=="Inshore") %>% 
  left_join(wbstats, by="block_id")

# Animation packages
# devtools::install_github('thomasp85/gganimate', force=T)
library(gganimate)
library(gifski)

# Get US states and Mexico
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# Plot 
g <- ggplot() +
  # Plot p(contaminated)
  # geom_sf(blocks_sf_ntraps %>% filter(week==10), mapping=aes(fill=ntraps)) + # single date
  geom_sf(blocks_sf_ntraps, mapping=aes(fill=ntraps, group=week)) +
  scale_fill_gradientn(name="Number of traps",
                       colors=rev(RColorBrewer::brewer.pal(11, "RdBu")),
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
  transition_manual(week) +
  labs(title="Week {current_frame}")
#g

# Export single state to get size right
# ggsave(g, filename=file.path(plotdir, "whale_n_by_block_date_single.png"),
#        width=4.5, height=6.5, units="in", dpi=600)


# Save animation
anim_save(g, filename=file.path(plotdir, "animation_ntraps_block_week.gif"), duration=10,
          width=4.5, height=6.5, units="in", res=600)

