
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
outputdir <- "data/whale_sdms/data"
plotdir <- "data/whale_sdms/figures"
depthdir <- "data/bathymetry/processed"

# Read whale data
data <- brick(file.path(outputdir, "forney_humpback_sdm_3km_2014forward_brick.grd"))

# Read 100 fathoms
bathy100 <- readRDS(file=file.path(depthdir, "CA_100_fathoms_polgyon_crude.Rds")) %>% 
  sf::st_transform(crs(data)) 


# Build mask
###########################

# Mask whale data by 100 fathoms
data_mask <- raster::mask(x=data, mask=bathy100)

# Export mask
writeRaster(data_mask, file.path(outputdir, "forney_humpback_sdm_3km_2014forward_brick_100fathoms.grd"), overwrite=T)

