
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(scales)
library(tidyverse)

# Directories
gisdir <- "data/cdfw/gis_data/processed"
catchdir <- "data/cdfw/landings_confidential/processed"
whaledir <- "output"
modeldir <- "models"
plotdir <- "figures"

# Read weekly block-level whale abundance predictions
whales <- readRDS(file.path(whaledir, "nwhales_block_week.Rds"))

# Read block data
blocks <- readRDS(file.path(gisdir, "CA_commercial_fishing_blocks.Rds")) %>% 
  sf::st_drop_geometry()

# Read time step key
time_step_key <- read.csv(file.path(modeldir, "model_time_step_key.csv"))


# Build data
################################################################################

# Build data
data <- whales %>% 
  # Add RAMP zones and depth classifications
  left_join(dplyr::select(blocks, block_id, block_ramp, depth), by="block_id") %>% 
  # Reduce to <100 fathoms
  filter(depth=="< 100 fathoms") %>% 
  # Total number of whales
  group_by(date, block_ramp) %>% 
  summarize(nwhales=sum(whales_n)) %>% 
  ungroup() %>% 
  # Add season
  left_join(time_step_key %>% dplyr::select(season, date, week), by="date") %>% 
  # Arrange
  dplyr::select(season, date, week, block_ramp, nwhales) %>% 
  rename(ramp_zone=block_ramp) %>% 
  # Cap number of whales
  mutate(nwhales_cap=pmin(200, nwhales)) %>% 
  # Arrange zones for plotting
  mutate(ramp_zone=factor(ramp_zone, levels=paste("Zone", c(5,4,3,2,1)))) %>% 
  # Examine where closures would happen
  # mutate(thresh=ifelse(week>=8 & week <=14, 20, 
  #                      ifelse(week>=23 & week<=42, 10, NA)),
  #        closed=nwhales>=thresh) %>% 
  # Determine type of closure that should happen with perfect knowledge of whale population
  mutate(closure=ifelse(week>=8 & week <=14 & nwhales>=20, "Zone",
                        ifelse(week>=23 & week <= 42 & nwhales >=20, "State-wide",
                               ifelse(week>=23 & week <= 42 & nwhales >=10, "Zone", NA))))

# Export data
write.csv(data, file=file.path(whaledir, "nwhales_by_ramp_zone_and_week.csv"), row.names = F)
  
# Plot data
# g <- ggplot(data, aes(x=ymd(date), y=ramp_zone, fill=nwhales_cap)) +
g <- ggplot(data, aes(x=week, y=ramp_zone, fill=nwhales_cap)) +
  geom_raster() +
  facet_wrap(~season, ncol=1) +
  # Add closure points
  geom_point(data %>% filter(!is.na(closure)), mapping=aes(shape=closure), size=1.5) +
  # Labels
  labs(x="Model week", y="RAMP fishing zone\n(north top, south bottom)") +
  geom_vline(xintercept=c(8,42), lwd=1.5) +
  geom_vline(xintercept=c(14,23), lwd=1, col="grey30") +
  scale_shape(name="Closure type") +
  scale_fill_gradientn(name="Number of whales", 
                       breaks=seq(0,200,50), labels=c(seq(0,150,50), ">200"),
                       colors=rev(RColorBrewer::brewer.pal(9, "RdBu"))) +
  # scale_x_date(labels=date_format("%m/%d"), date_breaks = "1 week") +
  theme_bw() +
  theme(legend.position="bottom",
        axis.text=element_text(size=7),
        axis.title=element_text(size=9),
        legend.text=element_text(size=7),
        legend.title=element_text(size=9),
        plot.title=element_text(size=11),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0))
g  

# Export
ggsave(g, filename=file.path(plotdir, "figure_nwhales_by_zone_time.png"), 
       width=6.5, height=7, units="in", dpi=600)
  
  
