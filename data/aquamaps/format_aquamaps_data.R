
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(rio)
library(tidyverse)

# Directories
inputdir <- "data/aquamaps/raw"
outputdir <- "data/aquamaps/processed"
plotdir <- "data/aquamaps/figures"

# Read data
list.files(inputdir)
data_orig <- read.csv(file.path(inputdir, "1559064778.csv"), as.is=T)

# Read code for formatting AquaMaps data
source("/Users/cfree/Dropbox/Chris/UCSB/projects/edf_climate/cc_trade/data/aquamaps/functions/format_aquamaps.R")
source("/Users/cfree/Dropbox/Chris/UCSB/projects/edf_climate/cc_trade/data/aquamaps/functions/plot_aquamaps.R")


# Format data
################################################################################

# Format AquaMaps data
data <- format_aquamaps(file="1559064778.csv", filedir = inputdir)

# Extract data
bbox <- data$bbox
pdata <- data$pdata
pdata_mat <- data$pdata_mat
edata <- data$edata
qdata <- data$qdata


# Plot data
################################################################################

g <- plot_aquamaps(data)
ggsave(g, filename=file.path(plotdir, "Fig3_dungeness_aquamaps_distribution.png"), width=6.5, height=3.5, units="in", dpi=600)





  
