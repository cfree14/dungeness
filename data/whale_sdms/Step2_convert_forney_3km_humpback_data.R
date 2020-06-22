
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
data <- readRDS(file=file.path(outputdir, "forney_humpback_sdm_3km_2014forward.Rds"))

# Projections
wgs84 <- "+init=epsg:4326"


# Convert data frame to raster brick
################################################################################

# Convert to brick
data_brick <- freeR::df2brick(df=data, x_col="long_dd", y_col="lat_dd", z_col="whales_n", layer_col="date")

# Add coordinate system
crs(data_brick) <- CRS("+init=epsg:4326")

# Export brick
writeRaster(data_brick, file.path(outputdir, "forney_humpback_sdm_3km_2014forward_brick.grd"), overwrite=T)




