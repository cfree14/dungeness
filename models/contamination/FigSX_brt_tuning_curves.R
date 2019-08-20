
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(rio)
library(zoo)
library(tidyverse)
library(lubridate)
library(gbm)
library(caret)
library(pROC)
library(tidymodels)


# Directories
datadir <- "models/contamination/data"
plotdir <- "models/contamination/figures"

# Load model
brtfit <- readRDS(file.path(datadir, "brtmodel2.Rds"))

# Get tuning results
brt_tune <- brtfit$results

# Plot figure
################################################################################

# Setup theme
my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=9),
                  plot.title=element_text(size=11),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Plot figure
g <- ggplot(brt_tune, aes(x=n.trees, y=Kappa, color=as.factor(interaction.depth))) +
  geom_line() +
  facet_wrap(~ shrinkage) +
  scale_color_discrete(name="Tree depth") +
  labs(x="Number of trees", y="Cohen's kappa") +
  scale_x_continuous(labels=seq(0,15000,5000), limits=c(0,15000)) +
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_brt_tuning_curves.png"), width=6.5, height=2.5, units="in", dpi=600)
