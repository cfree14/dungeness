

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(tidyverse)

# Directories
datadir <- "data/da_sampling"
gisdir <- "data/cdfw/gis_data/processed"
charmdir <- "models/contamination/paper/output"
outputdir <- "models/contamination/data"

# Read data
data_orig <- read.csv(file.path(datadir, "2014_crab_da_sampling_sites.csv"), as.is=T)

# Read p(contam) predictions
pcontam_brick <- raster::brick(file.path(charmdir, "dungeness_crab_model_rf_cda_predictions.gri"))

# Read blocks shapefile
blocks_sf <- readRDS(file.path(gisdir, "CA_commercial_fishing_blocks.Rds"))

# Format data
################################################################################

# Format data
# Need to adjust Fort Bragg longitudes west to get prediction
data1 <- data_orig %>% 
  filter(state=="California") %>% 
  dplyr::select(-state) %>% 
  mutate(long_dd_use=ifelse(area=="Fort Bragg", long_dd-0.03, long_dd)) %>% 
  arrange(desc(lat_dd))

# Extract predictions
raster::extract(x=pcontam_brick[[1]],
                y=data1 %>% dplyr::select(long_dd_use, lat_dd))

# Extract block ids
pts <- st_multipoint(data1 %>% dplyr::select(long_dd_use, lat_dd) %>% as.matrix(), dim="XY")
pol <- blocks_sf
block_id_indices <- sf::st_intersects(x=pts, y=pol) %>% unlist()
block_ids <- blocks_sf$block_id[block_id_indices]

# Finalize data
data2 <- data1 %>% 
  select(-long_dd) %>% 
  rename(long_dd=long_dd_use) %>% 
  mutate(block_id=block_ids) %>% 
  select(area:lat_dd, long_dd, everything())


# Plot to make sure block IDs are right
g <- ggplot() +
  geom_sf(data=blocks_sf %>% filter(block_id %in% data2$block_id)) +
  geom_sf_text(data=blocks_sf %>% filter(block_id %in% data2$block_id), mapping=aes(label=block_id)) +
  geom_point(data=data2, mapping=aes(x=long_dd, y=lat_dd, fill=area)) +
  ggrepel::geom_text_repel(data=data2, mapping=aes(x=long_dd, y=lat_dd, label=location, color=area)) +
  theme_bw()
g

# Export
write.csv(data2, file=file.path(outputdir, "da_sampling_sites.csv"), row.names=F)
