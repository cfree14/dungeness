
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
pubdir <- "data/cdfw/landings_public/processed"
privdir <- "data/cdfw/landings_confidential/processed"

# Read landings (public)
load(file.path(pubdir, "CA_dungeness_landings_data.Rdata"))

# Read landings (private)
load(file.path(privdir, "CDFW_dungeness_landings_data.Rdata"))


# Calculate stats
################################################################################

# Calc s
stats <- data %>% 
  group_by(season) %>% 
  summarize(tl_lb=sum(tl_lb, na.rm=T)/1e6,
            tl_usd=sum(tl_usd, na.rm=T)/1e6)

# Percent difference in 2015-16 outcomes vs 2014-15 outcomes
tl15 <- stats$tl_lb[stats$season=="2015-2016"]
usd15 <- stats$tl_usd[stats$season=="2015-2016"]
tl14 <- stats$tl_lb[stats$season=="2014-2015"]
usd14 <- stats$tl_usd[stats$season=="2014-2015"]

(tl15 - tl14) / tl14 * 100
(usd15 - usd14) / usd14 * 100
tl15-tl14
usd15 - usd14
