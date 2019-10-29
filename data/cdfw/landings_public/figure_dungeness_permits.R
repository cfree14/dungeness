
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(rio)
library(tidyverse)
library(rnaturalearth)

# Directories
datadir <- "data/landings/processed"
plotdir <- "data/landings/figures"

# Read data
ports <- read.csv(file.path(datadir, "dungeness_ports.csv"), as.is=T)
load(file.path(datadir, "CA_dungeness_permit_data.Rdata"))

# Add coordinates to permits
permits1 <- permits %>% 
  left_join(ports, by="port") %>% 
  filter(!is.na(lat_dd))

# Get US states
usa <- rnaturalearth::ne_states(country = "United States of America")
usa <- sf::st_as_sf(usa)

# Setup
################################################################################

# Setup theme
my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=9),
                  plot.title=element_text(size=11),
                  legend.text=element_text(size=7),
                  legend.title=element_text(size=9),
                  panel.grid.major = element_line(colour = 'transparent'),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))


g <- ggplot(data=usa) +
  geom_sf(fill="grey80", col="white", size=0.5) +
  geom_point(permits1, mapping=aes(x=long_dd, y=lat_dd, size=n_permits)) +
  annotate("text", x=permits1$long_dd+0.4, y=permits1$lat_dd, label=permits1$port, hjust=0, size=2) +
  coord_sf(xlim = c(-125, -113), ylim = c(32, 42)) +
  scale_size_continuous(name="Number of permits") +
  labs(x="", y="") +
  theme_bw() + my_theme
g

ggsave(g, filename=file.path(plotdir, "FigX_dungeness_permits_map.png"), width=4.5, height=3.5, units="in", dpi=600)


g <- ggplot(data=usa) +
  geom_sf(fill="grey80", col="white", size=0.5) +
  geom_point(permits1, mapping=aes(x=long_dd, y=lat_dd, size=n_traps)) +
  annotate("text", x=permits1$long_dd+0.4, y=permits1$lat_dd, label=permits1$port, hjust=0, size=2) +
  coord_sf(xlim = c(-125, -113), ylim = c(32, 42)) +
  scale_size_continuous(name="Number of traps") +
  labs(x="", y="") +
  theme_bw() + my_theme
g

ggsave(g, filename=file.path(plotdir, "FigX_dungeness_traps_map.png"), width=4.5, height=3.5, units="in", dpi=600)





