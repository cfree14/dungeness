
# Data from CetSound
# https://cetsound.noaa.gov/cda

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

# Directories
inputdir <- "data/whale_sdms/data/raw"
outputdir <- "data/whale_sdms/data"
plotdir <- "data/whale_sdms/figures"

# Read data
summer <- sf::st_read(dsn=file.path(inputdir, "swfsc_CCE_SummerFall_Becker_et_al_2016"), layer="Humpback_whale")
winter <- sf::st_read(dsn=file.path(inputdir, "swfsc_CalCOFI_WinterSpring_Becker_et_al_2017"), layer="Humpback_whale")

# Projections
moll <- CRS("+proj=moll")
wgs84 <- CRS("+init=epsg:4326")

# Reproject
summer_moll <- sf::st_transform(summer, moll)
winter_moll <-  sf::st_transform(winter, moll)

# Build data
################################################################################

# Build template raster
temp_ras <- fasterize::raster(summer_moll, res=10*1000)

# Format summer raster
summer_ras <- summer_moll %>% 
  # Convert polygon to 10km x 10km raster
  fasterize::fasterize(temp_ras, field="DENSITY") %>% 
  # Reporject to lat/long
  raster::projectRaster(crs=wgs84) %>% 
  # Convert to dataframe
  raster::as.data.frame(xy=TRUE) %>% 
  rename(long_dd=x, lat_dd=y, dens=layer)

# Format winter raster
winter_ras <- winter_moll %>% 
  # Convert polygon to 10km x 10km raster
  fasterize::fasterize(temp_ras, field="DENSITY") %>% 
  # Reporject to lat/long
  raster::projectRaster(crs=wgs84) %>% 
  # Convert to dataframe
  raster::as.data.frame(xy=TRUE) %>% 
  rename(long_dd=x, lat_dd=y, dens=layer)

# Maximum values
max(summer_ras$dens, na.rm=T)
max(winter_ras$dens, na.rm=T)

# Export data
save(summer_ras, winter_ras, file=file.path(outputdir, "becker_mean_humpback_density_rasters.Rdata"))


# Plot data
################################################################################


# Get US states and Mexico
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# Setup theme
my_theme <- theme(axis.text=element_text(size=5),
                  axis.text.y = element_text(angle = 90, hjust = 0.5),
                  axis.title=element_text(size=5),
                  plot.title=element_text(size=7),
                  plot.subtitle = element_text(size=6),
                  legend.text=element_text(size=6),
                  legend.title=element_text(size=7),
                  legend.position = "bottom",
                  panel.grid.major = element_line(colour = 'transparent'),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Plot summer
g1 <- ggplot() +
  # Plot
  geom_raster(data=summer_ras, mapping=aes(x=long_dd, y=lat_dd, fill=dens*100)) +
  scale_fill_gradientn(name="Density (whales per 100 km2)", 
                       limits=c(0,4),
                       breaks=seq(0,4,1),
                       colors=rev(RColorBrewer::brewer.pal( 10, "RdBu")), 
                       na.value = NA) +
  # Plot CA/Mexico
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Crop extent
  coord_sf(xlim = c(-125, -116), ylim = c(32, 42)) +
  # Small things
  labs(x="", y="",
       title="Mean summer/fall (Jul-Dec) distribution",
       subtitle="Becker et al. 2016") +
  theme_bw() + my_theme
g1

# Plot winter
g2 <- ggplot() +
  # Plot
  geom_raster(data=winter_ras, mapping=aes(x=long_dd, y=lat_dd, fill=dens*100)) +
  scale_fill_gradientn(name="Density (whales per 100 km2)", 
                       limits=c(0,4),
                       breaks=seq(0,4,1),
                       colors=rev(RColorBrewer::brewer.pal( 10, "RdBu")), 
                       na.value = NA) +
  # Plot CA/Mexico
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Crop extent
  coord_sf(xlim = c(-125, -116), ylim = c(32, 42)) +
  # Small things
  labs(x="", y="",
       title="Mean winter/spring (Jan-Apr) distribution",
       subtitle="Becker et al. 2017") +
  theme_bw() + my_theme
g2

# Merge plots
g <- plot_grid(g1, g2, ncol=2)

# Export plots
ggsave(g, filename=file.path(plotdir, "FigSX_whale_dists_mean.png"), width=6.5, height=5, units="in", dpi=600)



