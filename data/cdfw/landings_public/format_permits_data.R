
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(rio)
library(gridExtra)
library(tidyverse)

# Directories
inputdir <- "data/landings/raw"
outputdir <- "data/landings/processed"
plotdir <- "data/landings/figures"

# Read data
traps_orig <- import(file.path(inputdir, "permit_data_manually_modified.xlsx"), which=1)
permits_orig <- import(file.path(inputdir, "permit_data_manually_modified.xlsx"), which=2)

# Format data
################################################################################

# Format traps data
traps <- traps_orig %>% 
  mutate(port=plyr::revalue(port, c("SF Bay"="San Francisco",
                                    "Princeton/HMB"="Princeton",
                                    "Santa Cruz south"="Santa Cruz")))

# Summarize traps data
# n_permits is correct checked against original file
stats <- traps %>% 
  group_by(region, port) %>% 
  summarize(n_permits=sum(n_permits),
            n_traps=sum(n_traps)) %>% 
  ungroup()

# Total number of traps
sum(stats$n_traps)
  
# Format permits data
# Sleuthing in the original data file made me realize that
# San Francisco should have 81 and not 82 active permits
permits <- permits_orig %>% 
  mutate(port=plyr::revalue(port, c("SF Bay"="San Francisco",
                                    "Princeton/HMB"="Princeton",
                                    "Santa Cruz south"="Santa Cruz"))) %>% 
  mutate(n_active=ifelse(port=="San Francisco", 81, n_active)) %>% 
  mutate(check=(n_active+n_inactive)==n_total) %>% 
  select(-check) %>% 
  rename(n_permits=n_total) %>% 
  select(region, port, n_permits, everything()) %>% 
  left_join(select(stats, port, n_traps), by="port")

# Export
save(permits, traps, file=file.path(outputdir, "CA_dungeness_permit_data.Rdata"))

