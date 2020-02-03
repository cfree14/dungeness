
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(readxl)
library(tidyverse)
library(ggrepel)

# Directories
datadir <- "data/mgmt_plans/ccca"

# Read data
data_orig <- read_excel(file.path(datadir, "ccca_whale_closure_districts.xlsx"))



# Format data
################################################################################

# Format
data <- data_orig %>% 
  mutate(lat_dd=sapply(latlong, function(x) unlist(strsplit(x, ","))[1]) %>% as.numeric(),
         long_dd=sapply(latlong, function(x) unlist(strsplit(x, ","))[2]) %>% str_trim() %>% as.numeric())


# Get US states
usa <- rnaturalearth::ne_states(country = "United States of America")
usa <- sf::st_as_sf(usa)

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

# Plot data
g <- ggplot(data=usa) +
  # Plot USA
  geom_sf(fill="grey80", col="white", size=0.5) +
  # Plot points
  geom_point(data, mapping=aes(x=long_dd, y=lat_dd)) +
  # Label points
  ggrepel::geom_text_repel(data, mapping=aes(x=long_dd, y=lat_dd, label=landmark), nudge_x=0) +
  # Crop and small things
  coord_sf(xlim = c(-125, -113), ylim = c(32, 42)) +
  labs(x="", y="") +
  theme_bw() + my_theme
g
