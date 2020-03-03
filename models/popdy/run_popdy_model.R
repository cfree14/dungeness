
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(lubridate)
library(tidyverse)

# Directories
codedir <- "models/popdy"

# Read helper functions
source(file.path(codedir, "0read_data.R"))
source(file.path(codedir, "calc_recruits.R"))
source(file.path(codedir, "run_model.R"))
source(file.path(codedir, "plot_results.R"))


# Run model on annual time steps to identify stable age distribution
data <- run_model(nyears=20, step_length_yr=1, effort_dynamics="none", r0=42*10^6)

# Visualize results
plot_results(data)

# Extract stable age distribution
age_dist_stable <- data %>% 
  filter(step==max(step)) %>% 
  select(block_id, sex, age_yr, biomass_n)

# Run model on annual times beginning at stable age distribution to verify distribution
data2 <- run_model(nyears=20, step_length_yr=1, effort_dynamics="none", r0=42*10^6, age_dist = age_dist_stable)

# Visualize results
plot_results(data2)

# Run model one year but on weekly time steps with fishing
data3 <- run_model(nyears=1, step_length_yr=1/52, effort_dynamics="constant", r0=42*10^6, age_dist = age_dist_stable)

# Visualize results
plot_results(data3)







