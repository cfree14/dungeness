
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(lubridate)
library(tidyverse)
library(RColorBrewer)

# Directories
gisdir <- "data/cdfw/gis_data/processed"
datadir <- "data/cdfw/landings_confidential/processed"
plotdir <- "figures"

# Read data
load(file.path(datadir, "CDFW_dungeness_landings_data.Rdata"))
data_orig <- data
rm(data)

# Read CDFW blocks shapefile
blocks <- readRDS(file.path(gisdir, "CA_commercial_fishing_blocks.Rds"))


# SBuild data
################################################################################

# Reduce to CA only
data <- data_orig %>% 
  # California inshore small blocks
  filter(block_state=="California" & block_type=="Inshore" & season_week_first>=0) %>% 
  # Group by season, season week, latitudinal band
  group_by(season, season_week_first, block_band_lat_dd) %>% 
  summarize(catch_mt=sum(landings_mt, na.rm=T)) %>% 
  ungroup() %>% 
  # Calucluate proportion of seasonal catch
  group_by(season) %>% 
  mutate(season_mt=sum(catch_mt),
         catch_prop=catch_mt/season_mt) %>% 
  ungroup() %>% 
  # Format columns
  rename(week=season_week_first, lat_dd=block_band_lat_dd)

# Setup theme
my_theme <- theme(axis.text=element_text(size=6),
                  axis.title=element_text(size=8),
                  strip.text=element_text(size=8),
                  plot.title=element_blank(),
                  legend.text=element_text(size=6),
                  legend.title=element_text(size=8),
                  legend.position = "bottom",
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"),
                  axis.text.y = element_text(angle = 90, hjust = 0.5))

# Plot data
g <- ggplot(data, aes(x=week, y=lat_dd, fill=catch_prop)) + 
  facet_wrap(~season, ncol=5) +
  geom_tile() + 
  # Mendocino county line
  geom_hline(yintercept=38.77, size=0.5) + 
  # Labels
  labs(x="Week of the season\n(weeks since the Nov 1 central region opener)", y="Block latitude") +
  scale_y_continuous(breaks=seq(34, 42, 2)) +
  # Legend
  scale_fill_gradientn(name="Proportion of\ntotal seasonal catch", colors=rev(RColorBrewer::brewer.pal(9, "RdBu")), na.value="grey90") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigS4_catch_spatial_temporal.png"), 
       width=6.5, height=6.5, units="in", dpi=600)

