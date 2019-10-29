
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
load(file.path(datadir, "CA_dungeness_landings_data.Rdata"))

# Mean landings by port
port_tl <- cdfw_port %>% 
  group_by(port) %>% 
  slice(1:10) %>% 
  summarize(tl_lb_avg=mean(tl_lb, na.rm=T))

# Add mean landings to ports df
ports1 <- ports %>% 
  left_join(port_tl, by="port") %>% 
  mutate(tl_lb_avg=ifelse(is.na(tl_lb_avg), 1, tl_lb_avg))

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
  geom_point(ports1, mapping=aes(x=long_dd, y=lat_dd, size=tl_lb_avg/1e6)) +
  annotate("text", x=ports$long_dd+0.4, y=ports$lat_dd, label=ports$port, hjust=0, size=2) +
  coord_sf(xlim = c(-125, -113), ylim = c(32, 42)) +
  scale_size_continuous(name="Mean landings\n(millions of lbs)") +
  labs(x="", y="") +
  theme_bw() + my_theme
g

ggsave(g, filename=file.path(plotdir, "Fig0_dungeness_ports_map.png"), width=4.5, height=3.5, units="in", dpi=600)







