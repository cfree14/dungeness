
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
codedir <- "models/popdy/v2/functions"
indir <- "output/raw"
outdir <- "output/processed"
plotdir <- "models/figures"

# Read data
data <- readRDS(file.path(outdir, "Scenario1_no_management.Rds"))

# Read CDFW blocks shapefile
gisdir <- "data/cdfw/gis_data/processed"
blocks_sf <- readRDS(file.path(gisdir, "CA_commercial_fishing_blocks.Rds"))


# Setup
################################################################################

# Subset data
sdata <- data %>% 
  filter(iteration==1 & week==8) %>% 
  select(block_id, biomass_mt, traps_n, whales_n, pcontam)

# Add to blocks
blocks <- blocks_sf %>% 
  left_join(sdata) %>% 
  filter(!is.na(biomass_mt))

# Get US states and Mexico
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# My theme
my_theme <- my_theme <- theme(axis.text=element_text(size=4),
                              axis.title=element_blank(),
                              plot.title=element_blank(),
                              legend.position="bottom", 
                              legend.text=element_text(size=4),
                              legend.title=element_text(size=6),
                              panel.grid.major = element_blank(), 
                              panel.grid.minor = element_blank(),
                              panel.background = element_blank(), 
                              axis.line = element_line(colour = "black"),
                              axis.text.y = element_text(angle = 90, hjust = 0.5))

# Plot crab biomass
g1 <- ggplot() +
  geom_sf(blocks, mapping=aes(fill=biomass_mt), lwd=0.1) +
  # Add California and Mexico
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Crop extent
  coord_sf(xlim = c(-125, -116), ylim = c(32, 43)) +
  # Legend
  scale_fill_gradientn(name="Crab biomass (mt)", 
                      colors=RColorBrewer::brewer.pal(9, "Oranges"), na.value=NA) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", title.position="top")) +
  # Theme
  theme_bw() + my_theme
g1

# Plot number of traps
g2 <- ggplot() +
  geom_sf(blocks, mapping=aes(fill=traps_n), lwd=0.1) +
  # Add California and Mexico
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Crop extent
  coord_sf(xlim = c(-125, -116), ylim = c(32, 43)) +
  # Legend
  scale_fill_gradientn(name="Number of traps", 
                       colors=RColorBrewer::brewer.pal(9, "Greens"), na.value=NA) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", title.position="top")) +
  # Theme
  theme_bw() + my_theme
#g2

# Plot proportion of contaminated catch
g3 <- ggplot() +
  geom_sf(blocks, mapping=aes(fill=pcontam), lwd=0.1) +
  # Add California and Mexico
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Crop extent
  coord_sf(xlim = c(-125, -116), ylim = c(32, 43)) +
  # Legend
  scale_fill_gradientn(name="Proportion contaminated", 
                        colors=RColorBrewer::brewer.pal(9, "Reds"), na.value=NA) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", title.position="top")) +
  # Theme
  theme_bw() + my_theme
#g1

# Number of whales
g4 <- ggplot() +
  geom_sf(blocks, mapping=aes(fill=whales_n), lwd=0.1) +
  # Add California and Mexico
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Crop extent
  coord_sf(xlim = c(-125, -116), ylim = c(32, 43)) +
  # Legend
  scale_fill_gradientn(name="Number of whales", 
                        colors=RColorBrewer::brewer.pal(9, "Blues"), na.value=NA) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", title.position="top")) +
  # Theme
  theme_bw() + my_theme
#g4

# Merge plots
g <- grid.arrange(g1, g2, g3, g4, ncol=4)

# Export plot
ggsave(g, filename=file.path(plotdir, "model_example_week.png"), 
       width=6.5, height=3.25, units="in", dpi=600)






  