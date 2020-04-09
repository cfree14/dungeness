
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(lubridate)
library(tidyverse)

# Directories
codedir <- "models/popdy/v2"
outputdir <- "output"

# Read helper functions
source(file.path(codedir, "read_data.R"))
source(file.path(codedir, "calc_abundance.R"))
source(file.path(codedir, "calc_catch.R"))
source(file.path(codedir, "calc_effort.R"))
source(file.path(codedir, "run_model.R"))
source(file.path(codedir, "plot_results.R"))

# Run one year with no fishing
data1 <- run_model(yrs2sim=2015, effort_dynamics="none")
plot_results(data1)

# Run one year with constant fishing
data2 <- run_model(yrs2sim=2015, effort_dynamics="constant")
plot_results(data2)

# Run one year with biomass-coupled fishing
data3 <- run_model(yrs2sim=2015, effort_dynamics="biomass-coupled", a=0.15, b=0.1)
plot_results(data3)









