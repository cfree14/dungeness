
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
outputdir <- "output"

# Read helper functions
source(file.path(codedir, "read_data.R"))
source(file.path(codedir, "calc_abundance.R"))
source(file.path(codedir, "calc_eggprod.R"))
source(file.path(codedir, "calc_recruits.R"))
source(file.path(codedir, "calc_catch.R"))
source(file.path(codedir, "calc_effort.R"))
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
data2 <- run_model(nyears=3, step_length_yr=1/52, effort_dynamics="none", r0=42*10^6, age_dist = age_dist_stable)

# Visualize results
plot_results(data2)

# Run model one year but on weekly time steps with fishing
# Catch 2 - a=0.15 and b=0.4 seems to work okay
# Raising a from 0.15 to 0.30 resulted in a faster decay in effort and a slightly smaller amount of catch caught
# Raising b from 0.20 to 0.80 resulted in more quickly catching
data3 <- run_model(nyears=1, step_length_yr=1/52, effort_dynamics="biomass-coupled", 
                   r0=42*10^6, age_dist = age_dist_stable, a=0.15, b=0.1)

# Visualize results
plot_results(data3)

# Export one-year simulations
saveRDS(data3, file.path(outputdir, "one_year_simulation.Rds"))







