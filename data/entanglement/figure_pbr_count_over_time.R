
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
datadir <- "data/entanglement/data"
plotdir <- "data/entanglement/figures"

# Read data
data <- readRDS(file=file.path(datadir, "2015-2018_WC_marine_mammal_mortality_injury_data.Rds"))


# Plot data
################################################################################

# PBR by year
stats <- data %>% 
  filter(species=="Humpback whale") %>% 
  group_by(year) %>% 
  summarize(pbr_n=sum(pbr=="Y"))

# Setup theme
my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=9),
                  axis.text.x = element_text(angle = 90, hjust = 1),
                  plot.title=element_text(size=11),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Plot PBR by year
pbr <- 16.7
pbr_label <- paste("Potential Biological Removal (PBR) =", pbr, "whales")
g <- ggplot(stats, aes(x=year, y=pbr_n)) +
  geom_line() +
  geom_point() +
  # Labels
  labs(x="", y="Number of whales\ncounting towards the PBR") +
  scale_x_continuous(breaks=2007:2017) +
  # PBR line
  geom_hline(yintercept=pbr, color="grey60", linetype="dotted") +
  annotate("text", label=pbr_label, x=2007, y=pbr+1.2, hjust=0, color="grey60", size=2) +
  # Theme
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigSX_humpback_pbr_count.png"), width=6.5, height=3.5, units="in", dpi=600)

