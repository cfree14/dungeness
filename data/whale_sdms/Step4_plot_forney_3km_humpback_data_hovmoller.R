
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
library(grid)
library(gridExtra)

# Directories
outputdir <- "data/whale_sdms/data"
plotdir <- "data/whale_sdms/figures"
depthdir <- "data/bathymetry/processed"

# Read whale data
data <- brick(file.path(outputdir, "forney_humpback_sdm_3km_2014forward_brick_100fathoms.grd"))


# Build data
################################################################################

# Convert raster to data frame
build <- F
if(build){
  
  # Build data
  data_df <- data %>% 
    as.data.frame(xy=T) %>% 
    rename(long_dd=x, lat_dd=y) %>% 
    gather(key="date", value="nwhales", 3:ncol(.)) %>% 
    filter(!is.na(nwhales)) %>% 
    mutate(date=gsub("X", "", date) %>% ymd())
  
  # Export data
  saveRDS(data_df, file=file.path(outputdir, "forney_humpback_sdm_3km_2014forward_100fathoms_df.Rds"))
  
}else{
  
  # Read data
  data_df <-   readRDS(file.path(outputdir, "forney_humpback_sdm_3km_2014forward_100fathoms_df.Rds"))
}

# Build Hovmoller data
hov_data <- data_df %>% 
  group_by(date, lat_dd) %>% 
  summarize(nwhales=sum(nwhales)) %>% 
  ungroup()

# Whale-weighted latitude
lat_gravity <- hov_data %>% 
  group_by(date) %>% 
  summarize(lat_wt=weighted.mean(lat_dd, nwhales))


# Plot data
################################################################################

# RAMP zone lats
ramp_lats <- c(42, 
               40+5/60, 
               38+46.125/60, 
               37+11/60,
               36,
               32.5)

# Base theme
base_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=8),
                    plot.title=element_text(size=8),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=8),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    legend.position="bottom",
                    axis.text.y = element_text(angle = 90, hjust = 0.5))

# Get US states and Mexico
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# Plot map
g1 <- ggplot() +
  # Plot p(contamination)
  geom_tile(data=data_df %>% filter(date=="2014-01-02"), 
            mapping=aes(x=long_dd, y=lat_dd, fill=nwhales)) +
  # Add California and Mexico
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Add RAMP zone lines
  geom_hline(yintercept=ramp_lats, lwd=0.3, linetype="dotted") +
  # Crop extent
  coord_sf(xlim = c(-125, -116), ylim = c(32, 43)) +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_gradientn(name="Number of whales", 
                       # colors=rev(RColorBrewer::brewer.pal(9, "RdBu")),
                       colors=RColorBrewer::brewer.pal(9, "Blues")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme
g1

# Plot data
g2 <- ggplot(hov_data, aes(x=date, y=lat_dd, fill=nwhales)) +
  # Plot raster
  geom_tile() +
  # Plot line
  geom_line(lat_gravity, mapping=aes(x=date, y=lat_wt), inherit.aes = F, 
            color="black", lwd=0.2) +
  # Add RAMP zone lines
  geom_hline(yintercept=ramp_lats, color="black", lwd=0.3, linetype="dotted") +
  # Labels 
  labs(x="", y="") +
  scale_y_continuous(breaks=seq(32,42,2), lim=c(32, 43)) +
  scale_x_date(breaks=seq(ymd("2014-01-01"), ymd("2020-01-01"), by="year"), labels=2014:2020) +
  # Legend
  scale_fill_gradientn(name="Number of whales", 
                       # colors=rev(RColorBrewer::brewer.pal(9, "RdBu")),
                       colors=RColorBrewer::brewer.pal(9, "Blues")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme
g2

# Merge plots
g <- grid.arrange(g1, g2, ncol=2, widths=c(0.3,0.8))

# Export plot
ggsave(g, filename=file.path(plotdir, "figure_whales_fishing_grounds_time.png"), 
       width=10.5, height=4.5, units="in", dpi=600)



# Plot by zone
################################################################################

zdata <- data_df %>% 
  # Add zone
  mutate(zone=cut(lat_dd, ramp_lats, labels=paste("Zone", 5:1))) %>% 
  # Summarize by zone and date
  group_by(zone, date) %>% 
  summarise(nwhales=sum(nwhales))

str(zdata)






