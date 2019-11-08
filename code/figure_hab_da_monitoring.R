
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(rio)
library(raster)
library(tidyverse)
library(rnaturalearth)
library(cowplot)

# Directories
plotdir <- "figures"
landingsdir <- "data/landings/processed"
charmdir <- "data/charm/processed"
pierdir <- "data/pier_sampling/processed"
gisdir <- "data/cdfw/gis_data/processed"
dasurvdir <- "data/da_sampling/processed"
dasurvdir1 <- "data/da_sampling/raw"

# Get US states and Mexico
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# Setup theme
my_theme <- theme(axis.text=element_text(size=5),
                  axis.text.y = element_text(angle = 90, hjust = 0.5),
                  axis.title=element_text(size=5),
                  plot.title=element_text(size=7),
                  legend.text=element_text(size=7),
                  legend.title=element_text(size=9),
                  legend.position = "bottom",
                  panel.grid.major = element_line(colour = 'transparent'),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))


# Plot Pseudo-nitzschia
################################################################################

# Date to plot
date <- "2015-07-01"
date1 <- gsub("-", ".", paste0("X", date))

# Read data
# pn_brick <- readRDS(file.path(charmdir, "PN_nowcast_2014present_brick.Rds"))
pn_brick <- brick(file.path(charmdir, "PN_nowcast_2014present_brick.grd"))
piers <- read.csv(file.path(pierdir, "pier_sampling_locations.csv"), as.is=T)

# Quant piers
q_piers <- piers %>% 
  filter(type=="continuous") %>% 
  mutate(label_name=plyr::revalue(long_name, c("La Jolla, Scripp's Pier"="Scripps Pier",
                                         "Fisherman's Wharf, Monterey"="Monterey Wharf")))

# Subset one date and format for plotting
pn_df <- pn_brick[[date1]] %>% 
  raster::as.data.frame(xy=T) %>% 
  setNames(c("long_dd", "lat_dd", "prob"))

# Plot Pseudo-nitzschia
g2 <- ggplot() +
  # Plot p(Pseudo-nitzschia) raster and legend
  geom_raster(pn_df, mapping=aes(x=long_dd, y=lat_dd, fill=prob)) +
  scale_fill_gradientn(name=expression("p(" * italic("Pseudo-nitzschia") * ")"),
                       colors=rev(RColorBrewer::brewer.pal(9, "RdYlBu")),
                       limits=c(0,1),
                       na.value=NA) +
  # Add California and Mexico
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Add piers and legend
  geom_point(data=arrange(piers, desc(type)), aes(x=long_dd, y=lat_dd, col=type), size=0.8) +
  scale_color_manual(name="Monitoring type", values=c("black", "grey40")) +
  ggrepel::geom_text_repel(data=q_piers, aes(x=long_dd, y=lat_dd, label=label_name),
                           size=1.2, direction = "y", hjust = 0, nudge_x=0.10, segment.size=0.2) +
  # Crop extent
  coord_sf(xlim = c(-125, -116), ylim = c(32, 42)) +
  # Little things
  labs(x="", y="") +
  ggtitle(expression("B. " * italic("Pseudo-nitzschia") * " monitoring program")) + 
  theme_bw() + my_theme + theme(axis.text.y=element_blank())
g2


# Plot domoic acid
################################################################################

# Read data
# dac_brick <- readRDS(file.path(charmdir, "DAC_nowcast_2014present_brick.Rds"))
dac_brick <- brick(file.path(charmdir, "DAC_nowcast_2014present_brick.grd"))
da_crabs <- read.csv(file.path(dasurvdir, "CDPH_crab_viscera_da_data.csv"), as.is=T)
da_sites <- import(file.path(dasurvdir1, "2014_crab_da_sampling_sites.xlsx")) %>% 
  mutate(long_dd=long_dd*-1)
da_ports <- filter(ports, da_sampling!="")

# Subset one date and format for plotting
dac_df <- dac_brick[[date1]] %>% 
  as.data.frame(xy=T) %>% 
  setNames(c("long_dd", "lat_dd", "prob"))

# Plot domoic acid
g3 <- ggplot() +
  # Plot p(Pseudo-nitzschia) raster and legend
  geom_raster(dac_df, mapping=aes(x=long_dd, y=lat_dd, fill=prob)) +
  scale_fill_gradientn(name="p(Cellular domoic acid)",
                       colors=rev(RColorBrewer::brewer.pal(9, "RdYlBu")),
                       limits=c(0,1),
                       na.value=NA) +
  # Add California and Mexico
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Add sampling areas and legend
  geom_point(data=da_sites, aes(x=long_dd, y=lat_dd), size=0.5, shape=4) +
  geom_point(data=da_ports, aes(x=long_dd, y=lat_dd, col=da_sampling), size=0.8) +
  scale_color_manual(name="Monitoring type", values=c("grey40", "black")) +
  ggrepel::geom_text_repel(data=filter(da_ports, da_sampling=="routine"), aes(x=long_dd, y=lat_dd, label=port),
                           size=1.2, direction = "y", hjust = 0, nudge_x=0.10, segment.size=0.2) +
  # geom_point(data=da_crabs, aes(x=long_dd, y=lat_dd, col=species), size=0.8) +
  # Crop extent
  coord_sf(xlim = c(-125, -116), ylim = c(32, 42)) +
  # Little things
  labs(x="", y="") +
  ggtitle("C. Domoic acid monitoring program") + 
  theme_bw() + my_theme + theme(axis.text.y=element_blank())
g3


# Merge plots and export
################################################################################

g <- plot_grid(g1, g2, g3, ncol=3)

ggsave(g, filename=file.path(plotdir, "Fig1_maps.png"), width=6.5, height=3, units="in", dpi=600)



