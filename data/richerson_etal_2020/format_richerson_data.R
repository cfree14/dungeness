
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/richerson_etal_2020"

# Read data
n_north <- read.csv(file.path(datadir, "NCA_abundance.csv"), as.is=T)
n_central <- read.csv(file.path(datadir, "CCA_abundance.csv"), as.is=T)
p_north <- read.csv(file.path(datadir, "NCA_percent_caught.csv"), as.is=T)
p_central <- read.csv(file.path(datadir, "CCA_percent_caught.csv"), as.is=T)

# Format data
################################################################################

n <- n_north %>% 
  setNames(c("year", "n"))
