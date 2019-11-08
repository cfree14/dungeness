
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/rockfish_survey/data"

# Read data
data_orig <- read.delim(file.path(datadir, "0006779-190813142620410.csv"), as.is=T)

# Inspect data
head(data_orig)

# Get US states and Mexico
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")


# Format data
################################################################################

# Build tracks
data <- data_orig %>% 
  group_by(year, month, day, decimalLongitude, decimalLatitude) %>% 
  summarise(n=n()) %>% 
  rename(long_dd=decimalLongitude, lat_dd=decimalLatitude)

# Plot tracks
g <- ggplot() +
  # Plot
  geom_line(data=data, mapping=aes(x=long_dd, y=lat_dd, color=year)) +
  # Plot CA/Mexico
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Crop extent
  coord_sf(xlim = c(-125, -116), ylim = c(32, 42)) +
  # Small things
  labs(x="", y="") +
  theme_bw()
g
