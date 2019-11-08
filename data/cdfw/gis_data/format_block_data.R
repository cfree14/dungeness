
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(tidyverse)

# Directories
datadir <- "data/cdfw/gis_data/processed"
countydir <- "data/cdfw/gis_data/raw/county_boundaries"
landingsdir <- "data/cdfw/landings_confidential/processed"
plotdir <- "data/cdfw/gis_data/figures"

# Read data
blocks_orig <- sf::st_read(dsn=datadir, layer="CA_commercial_fishing_blocks")

# Build block key 
block_key <- read.csv(file.path(landingsdir, "block_key.csv"), as.is=T)

# Read CA county boundaries
counties <- sf::st_read(dsn=countydir, layer="cnty24k09_1_poly") %>% 
  filter(ISLAND=="N")

# Projections
wgs84 <- sf::st_crs("+init=epsg:4326")

# Plot data
plot(blocks_orig)


# Setup
################################################################################

# North/Central latitude
# Sonoma-Mendocino County line
nc_lat <- 38.77

# Block centrouds
centroids <- blocks_orig %>% 
  rename(block_id=BLOCK10_ID, area_sqkm=AREA_SQKM) %>% 
  sf::st_centroid() %>% 
  sf::st_transform(wgs84) %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(block_lat_dd=Y, block_long_dd=X) %>% 
  mutate(block_id=blocks_orig$BLOCK10_ID)

# Format data
blocks <- blocks_orig %>% 
  sf::st_transform(wgs84) %>% 
  rename(block_id=BLOCK10_ID, block_area_sqkm=AREA_SQKM) %>% 
  # Add inshore/offshore type
  mutate(block_type=ifelse(block_id>1000, "Offshore", "Inshore")) %>% 
  # Add centroid lat/long
  left_join(centroids, by="block_id") %>% 
  # Add north/central
  mutate(block_region=ifelse(block_lat_dd>nc_lat, "Northern", "Central/Southern")) %>% 
  # Add area
  left_join(select(block_key, block_id, block_area_orig), by="block_id") %>% 
  rename(block_area=block_area_orig) %>% 
  # Correct Northern area that should actually be Central areas
  mutate(block_area=ifelse(block_id >= 407 & block_id <= 421, "Central California", block_area)) %>% 
  # Fill missing areas
  mutate(block_area=ifelse(is.na(block_area) & block_id<=281, "Northern California", block_area),
         block_area=ifelse(is.na(block_area) & block_id>=422 & block_id <=630, "Central California", block_area),
         block_area=ifelse(is.na(block_area) & block_id>=631 & block_id <=683, "Southern California", block_area),
         block_area=ifelse(is.na(block_area) & block_id>=684 & block_id <=690, "No. Channel Islands", block_area),
         block_area=ifelse(is.na(block_area) & block_id>=707 & block_id <=713, "So. Channel Islands", block_area),
         block_area=ifelse(is.na(block_area) & ((block_id>=760 & block_id <=762) | (block_id>=806 & block_id <=808)), "Catalina Island", block_area),
         block_area=ifelse(is.na(block_area) & block_id>=631 & block_id <=999, "Southern California", block_area)) %>%  
  # Add district
  mutate(block_district=ifelse(block_id<=207, 6, 
                         ifelse(block_id==208, 8,
                                ifelse(block_id==209, 9,
                                       ifelse(block_id==301, 12,
                                              ifelse(block_id %in% 488:489, 13,
                                                     ifelse(block_id<=406, 7,
                                                            ifelse(block_id<=487, 10,
                                                                   ifelse(block_id<=531, 17,
                                                                          ifelse(block_id<=663, 18,
                                                                                 ifelse(block_id<=1000, 19, NA))))))))))) %>% 
  mutate(block_district=ifelse(block_id==545, 17, block_district)) %>% 
  # Assign offshore zones to districts
  mutate(block_district=ifelse(block_id%in%1032:1035, 19,
                         ifelse(block_id%in%1036:1037, 18,
                               ifelse(block_id==1038, 10, 
                                      ifelse(block_id%in%1040:1041, 7, 
                                             ifelse(block_id==1042, 6, block_district)))))) %>% 
  # Make district character
  mutate(block_district=paste("District", block_district)) %>% 
  # Rearrange columns
  select(block_id, block_type, block_region, block_area, block_district, block_lat_dd, block_long_dd, everything()) %>% 
  arrange(block_id)

# Inspect
freeR::complete(blocks)


# Export formatted blocks
blocks_df <- st_drop_geometry(blocks)
write.csv(blocks_df, file=file.path(datadir, "CA_commercial_fishing_blocks.csv"), row.names = F)
saveRDS(blocks, file=file.path(datadir, "CA_commercial_fishing_blocks.Rds"))
sf::st_write(blocks, dsn=file.path(datadir, "CA_commercial_fishings_blocks_formatted.shp"), delete_layer=TRUE)


# Plot data
################################################################################

# Setup theme
my_theme <- theme(axis.text=element_text(size=12),
                  axis.text.y = element_text(angle = 90, hjust = 0.5),
                  legend.text = element_text(size=14),
                  legend.title = element_text(size=16),
                  legend.position = c(0.8, 0.9),
                  panel.grid.major = element_line(colour = 'transparent'),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Get US states and Mexico
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# Add new group column for plotting
blocks <- blocks %>% 
  mutate(group=ifelse(grepl("-Offshore", block_area), "Offshore", block_area))

# Plot blocks
g <- ggplot() +
  # Plot land
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.1) +
  # Plot and label counties
  geom_sf(data=counties, fill=NA, col="white", size=0.2) +
  geom_sf_text(counties, mapping=aes(label=NAME_PCASE), size=2.8, col="white") +
  # Plot blocks
  geom_sf(blocks, mapping=aes(fill=block_region)) +
  geom_sf_text(blocks, mapping=aes(label=block_id), size=2) +
  # Crop plot
  coord_sf(xlim = c(-125.5, -116.6), ylim = c(32, 42)) +
  # Add Mendoncino-Sonoma county line
  geom_hline(yintercept=38.77, size=0.5) + 
  annotate("text", x=-123.5, y=38.9, label="Mendocino-Sonoma county line (38.77°N)", hjust=0, size=5) +
  # Legend
  scale_fill_discrete(name="Block region") +
  labs(x="", y="") +
  theme_bw() + my_theme
g

# Export plot
# A3 paper: 11.7" x 16.5"
ggsave(g, filename=file.path(plotdir, "CA_comm_fishing_blocks_by_region.pdf"), 
       width=11.7, height=16.5, units="in", dpi=600)



