
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(tidyverse)

# Directories
gisdir <- "data/cdfw/gis_data/processed"
plotdir <- "figures"

# Read code to assign blocks to zones
source("/Users/cfree/Dropbox/Chris/UCSB/projects/dungeness/models/popdy/v2/functions/map_domoic_zones.R")

# Read CDFW blocks shapefile
blocks_sf <- readRDS(file.path(gisdir, "CA_commercial_fishing_blocks.Rds"))

# Read and format ports data
landingsdir <- "data/cdfw/landings_public/processed"
ports <- read.csv(file.path(landingsdir, "dungeness_ports.csv"), as.is=T)
da_ports <- filter(ports, da_sampling!="") %>% 
  mutate(da_sampling=recode_factor(da_sampling, 
                                   "routine"="Routine",
                                   "ad-hoc"="Ad-hoc"))

# Read and format crab sampling data
dasurvdir <- "data/da_sampling/2019_request/processed"
dasurvdir1 <- "data/da_sampling/2019_request/raw"
da_crabs <- read.csv(file.path(dasurvdir, "CDPH_crab_viscera_da_data.csv"), as.is=T)
da_sites <- rio::import(file.path(dasurvdir1, "2014_crab_da_sampling_sites.xlsx")) %>% 
  mutate(long_dd=long_dd*-1)

# New sampling sites
da_sites_add <- blocks_sf %>% 
  filter(block_id %in% c(402, 409, 517, 527, 607, 623)) %>% 
  select(block_lat_dd, block_long_dd) %>% 
  rename(lat_dd=block_lat_dd, long_dd=block_long_dd) %>% 
  mutate(area=c("Point Arena", "Point Arena",
                "Monterey Bay", "Monterey Bay", # Blocks 517, 527
                "Morro Bay", "Morro Bay")) # Blocks 607, 623
da_sites_new <- da_sites %>% 
  select(area, long_dd, lat_dd) %>% 
  bind_rows(da_sites_add %>% select(area, long_dd, lat_dd) %>% sf::st_drop_geometry())


# Build data
blocks_sf_dzones_curr <- map_domoic_zones(lats=da_sites$lat_dd, zones=da_sites$area)
blocks_sf_dzones_exp <- map_domoic_zones(lats=da_sites_new$lat_dd, zones=da_sites_new$area)


# Plot data
################################################################################

# Get US states and Mexico
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# Setup theme
my_theme <- theme(axis.title=element_blank(),
                  axis.text = element_text(size=6),
                  axis.text.y = element_text(angle = 90, hjust = 0.5),
                  plot.title=element_text(size=9),
                  legend.text=element_text(size=5),
                  legend.title=element_text(size=7),
                  legend.spacing.y = unit(0.05, "cm"),
                  legend.box = "vertical",
                  panel.grid.major = element_line(colour = 'transparent'),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))


# Plot current
g1 <- ggplot() +
  # Plot blocks
  geom_sf(data=blocks_sf_dzones_curr, mapping=aes(fill=block_dzone), lwd=0.2) +
  # Plot land
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Add sampling areas and legend
  geom_point(data=da_sites, aes(x=long_dd, y=lat_dd), size=0.6, shape=4) +
  geom_point(data=da_ports, aes(x=long_dd, y=lat_dd, col=da_sampling), size=0.9) +
  scale_color_manual(name="Monitoring type", values=c("black", "grey40"), guide=F) +
  # Crop map
  coord_sf(xlim = c(-125.5, -116.5), ylim = c(32, 42)) +
  # Legend
  scale_fill_discrete(name="DA mgmt zone", na.value=NA) +
  # Little things
  labs(x="", y="", title="A. Current sampling program") +
  theme_bw() + my_theme +
  theme(legend.position=c(0.8, 0.8),
        legend.justification=1)
#g1

# Plot alternate
g2 <- ggplot() +
  # Plot blocks
  geom_sf(data=blocks_sf_dzones_exp, mapping=aes(fill=block_dzone), lwd=0.2) +
  # Plot land
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Add sampling areas and legend
  geom_point(data=da_sites_new, aes(x=long_dd, y=lat_dd), size=0.6, shape=4) +
  geom_point(data=da_ports, aes(x=long_dd, y=lat_dd, col=da_sampling), size=0.9) +
  scale_color_manual(name="Monitoring type", values=c("black", "grey40"), guide=F) +
  # Crop map
  coord_sf(xlim = c(-125.5, -116.5), ylim = c(32, 42)) +
  # Legend
  scale_fill_discrete(name="DA mgmt zone", na.value=NA) +
  # Little things
  labs(x="", y="", title="B. Expanded sampling program") +
  theme_bw() + my_theme + my_theme +
  theme(legend.position=c(0.8, 0.8),
        legend.justification=1)
#g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, ncol=2)

# Export
ggsave(g, filename=file.path(plotdir, "FigS9_da_sampling_curr_alt_maps.png"), 
       width=6.5, height=4.5, units="in", dpi=600)
