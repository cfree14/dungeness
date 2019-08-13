
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(raster)
library(tidyverse)
library(rnaturalearth)
library(cowplot)

# Directories
plotdir <- "figures"
romsdir <- "data/roms/processed"
bathydir <- "data/bathymetry/processed"

# Get US states and Mexico
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# Setup theme
my_theme <- theme(axis.text=element_text(size=5),
                  axis.text.y = element_text(angle = 90, hjust = 0.5),
                  axis.title=element_text(size=5),
                  plot.title=element_text(size=7),
                  legend.text=element_text(size=6),
                  legend.title=element_text(size=7),
                  legend.position = "bottom",
                  panel.grid.major = element_line(colour = 'transparent'),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Read data
load(file.path(bathydir, "ca_bathymetry_data.Rdata"))
sal <- readRDS(file.path(romsdir, "CA_ROMS_brick_salt.Rds"))
sst <- readRDS(file.path(romsdir, "CA_ROMS_brick_temp.Rds"))
ssh <- readRDS(file.path(romsdir, "CA_ROMS_brick_zeta.Rds"))


# Format data for plotting
################################################################################

# Format SST
sst_df <- sst[[1]] %>% 
  as("SpatialPixelsDataFrame") %>% 
  as.data.frame() %>% 
  setNames(c("value", "x", "y"))

# Format salinity
sal_df <- sal[[1]] %>% 
  as("SpatialPixelsDataFrame") %>% 
  as.data.frame() %>% 
  setNames(c("value", "x", "y"))

# Format sea surface height
ssh_df <- ssh[[1]] %>% 
  as("SpatialPixelsDataFrame") %>% 
  as.data.frame() %>% 
  setNames(c("value", "x", "y"))

# Format bathy
ca_bathy_df <- ca_bathy %>% 
  as("SpatialPixelsDataFrame") %>% 
  as.data.frame() %>% 
  setNames(c("value", "x", "y"))

# Format slope
ca_slope_df <- ca_slope %>% 
  as("SpatialPixelsDataFrame") %>% 
  as.data.frame() %>% 
  setNames(c("value", "x", "y"))

# Format aspect
ca_aspect_df <- ca_aspect %>% 
  as("SpatialPixelsDataFrame") %>% 
  as.data.frame() %>% 
  setNames(c("value", "x", "y"))

# Plot data
################################################################################

# DYNAMIC VAIRABLES
######################################

# Plot SST
g1 <- ggplot() +
  # Plot
  geom_raster(data=sst_df, mapping=aes(x=x, y=y, fill=value)) +
  scale_fill_gradientn(name="SST (°C)", colors=rev(RColorBrewer::brewer.pal( 10, "RdBu"))) +
  # Plot CA/Mexico
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Crop extent
  coord_sf(xlim = c(-125, -116), ylim = c(32, 42)) +
  # Small things
  labs(x="", y="") +
  ggtitle("A. Sea surface temperature (SST)*") +
  theme_bw() + my_theme
# g1

# Plot salinity
g2 <- ggplot() +
  # Plot 
  geom_raster(data=sal_df, mapping=aes(x=x, y=y, fill=value)) +
  scale_fill_gradientn(name="Salinity (psu)", colors=rev(RColorBrewer::brewer.pal( 10, "PiYG"))) +
  # Plot CA/Mexico
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Crop extent
  coord_sf(xlim = c(-125, -116), ylim = c(32, 42)) +
  # Small things
  labs(x="", y="") +
  ggtitle("B. Salinity") +
  theme_bw() + my_theme
# g2

# Plot SSH
g3 <- ggplot() +
  # Plot 
  geom_raster(data=ssh_df, mapping=aes(x=x, y=y, fill=value)) +
  scale_fill_gradientn(name="SSH (m)", colors=RColorBrewer::brewer.pal(9, "Oranges")) +
  # Plot CA/Mexico
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Crop extent
  coord_sf(xlim = c(-125, -116), ylim = c(32, 42)) +
  # Small things
  labs(x="", y="") +
  ggtitle("C. Sea surface height (SSH)*") +
  theme_bw() + my_theme
# g3


# DYNAMIC VAIRABLES
######################################

# Plot bathy
b1 <- ggplot() +
  # Plot bathymetry
  geom_raster(data=ca_bathy_df, mapping=aes(x=x, y=y, fill=value)) +
  scale_fill_continuous(name="Depth (m)") +
  # Plot CA/Mexico
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Crop extent
  coord_sf(xlim = c(-125, -116), ylim = c(32, 42)) +
  # Small things
  labs(x="", y="") +
  ggtitle("D. Bathymetric depth*") +
  theme_bw() + my_theme
# g1  

# Plot slope
b2 <- ggplot() +
  # Plot bathymetry
  geom_raster(data=ca_slope_df, mapping=aes(x=x, y=y, fill=value)) +
  scale_fill_continuous(name="Slope (°)") +
  # Plot CA/Mexico
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Crop extent
  coord_sf(xlim = c(-125, -116), ylim = c(32, 42)) +
  # Small things
  labs(x="", y="") +
  ggtitle("E. Bathymetric slope") +
  theme_bw() + my_theme
# g2  

# Plot slope
b3 <- ggplot() +
  # Plot bathymetry
  geom_raster(data=ca_aspect_df, mapping=aes(x=x, y=y, fill=value)) +
  scale_fill_continuous(name="Aspect (°)") +
  # Plot CA/Mexico
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Crop extent
  coord_sf(xlim = c(-125, -116), ylim = c(32, 42)) +
  # Small things
  labs(x="", y="") +
  ggtitle("F. Bathymetric aspect") +
  theme_bw() + my_theme
# g3




# Merge plots and export
################################################################################

# Merge
g <- plot_grid(g1, g2, g3, 
               b1, b2, b3, ncol=3)

# Export
ggsave(g, filename=file.path(plotdir, "FigS1_whale_sdm_inputs.png"), width=6.5, height=7.5, units="in", dpi=600)



