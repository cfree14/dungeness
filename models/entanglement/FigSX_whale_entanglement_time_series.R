
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(raster)
library(tidyverse)
library(lubridate)
library(rio)

# Directories
datadir <- "models/entanglement/data"
plotdir <- "models/entanglement/figures"

# Read data
data1_orig <- import(file.path(datadir, "2000-18_WC_confirmed_entanglements.xlsx"))
data2_orig <- import(file.path(datadir, "2015-18_WC_confirmed_entanglements_by_fishery.xlsx"))

# Setup theme
my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=9),
                  axis.text.x = element_text(angle = 90, hjust = 1),
                  plot.title=element_text(size=11),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))


# Build and plot full time series
################################################################################

# Format data
data1 <- data1_orig %>% 
  rename(year=Year) %>% 
  gather(key="species", value="n", 2:ncol(.)) %>%
  mutate(species=factor(species, levels=c("Blue", "Unidentified", "Humpback", "Gray")))

# Check sample size
# 192 humpbacks, 110 grays, 36 others, 7 blues
# My recreation of the dataset is missing 7 unidentified whales 
# but I've looked super closely and think it's their fault
nspp <- data1 %>% 
  group_by(species) %>% 
  summarize(n=sum(n))

# Plot data
g <- ggplot(data1, aes(x=year, y=n, fill=species)) +
  geom_bar( stat="identity") +
  labs(x="", y="Number of entanglements") +
  scale_x_continuous(breaks=2000:2018) +
  scale_fill_manual(name="Species", values=c("navy", "orange", "lightblue", "darkgrey")) +
  theme_bw() + my_theme
g


# Build and plot shorter time series
################################################################################

# Formtat data
data2 <- data2_orig %>% 
  mutate(fishery=plyr::revalue(fishery, c("CA commercial spot prawn trap fishery" = "CA Spot prawn commercial trap fishery",
                                          "CA recreational spot prawn trap fishery" = "CA Spot prawn recreational trap fishery",
                                          "Commercial Dungeness crab commercial trap fishery, state unknown" = "Dungeness crab commercial trap fishery",
                                          "Sablefish/coonstripe shrimp commercial trap fishery*" = "Sablefish/coonstripe shrimp commercial trap fishery",
                                          "WA Dungeness crab commercial trap fishery including tribal fisheries" = "WA Dungeness crab commercial+tribal trap fishery",
                                          "WA Dungeness crab commercial trap fishery, including tribal fisheries" = "WA Dungeness crab commercial+tribal trap fishery")))

# Inspect fisheries
sort(unique(data2_orig$fishery))
sort(unique(data2$fishery))

# Plot data
g <- data2 %>% 
  filter(humpback >0) %>% 
  ggplot(., aes(x=year, y=humpback, fill=fishery)) +
  geom_bar(stat="identity") +
  labs(x="", y="Number of entangled humpback whales") +
  theme_bw()
g


# Build humpback data
################################################################################

# Categories
# CA Dungeness gear
# OR/WA Dungeness gear
# Other Dungeness gear
# Non-Dungeness gear
# Unknown gear

# Build 2000-2014 portion of humpback time series
hb2000 <- data1 %>% 
  filter(species=="Humpback") %>% 
  mutate(fishery="Unknown fishery") %>% 
  select(year, fishery, n)

# Gear types
ca_gears <- c("CA Dungeness crab commercial trap fishery", "CA Dungeness crab recreational trap fishery")
orwa_gears <- c("OR Dungeness crab commercial trap fishery", "WA Dungeness crab commercial+tribal trap fishery")
other_gears <- c("Dungeness crab commercial trap fishery", "Dungeness crab recreational trap fishery")

# Build 2015-2018 portion of humpback time series
hb2015 <- data2 %>% 
  select(year, fishery, humpback) %>% 
  rename(n=humpback, fishery_orig=fishery) %>% 
  filter(n>0) %>% 
  mutate(fishery=ifelse(fishery_orig %in% ca_gears, "CA Dungeness fishery",
                        ifelse(fishery_orig %in% orwa_gears, "OR/WA Dungeness fishery", 
                               ifelse(fishery_orig %in% other_gears, "Unknown Dungeness fishery", "Non-Dungeness fishery")))) %>% 
  select(year, fishery, n)

# Figure out how many "Unknown fisheries" to add to the recent data
hb2015unk <- hb2015 %>% 
  group_by(year) %>% 
  summarize(n=sum(n)) %>% 
  # Add correct totals from longer time series
  left_join(hb2000, by="year") %>% 
  # Calculate number of missing entanglements, which belong to unknown
  mutate(n_missing=n.y - n.x) %>% 
  select(year, fishery, n_missing) %>% 
  rename(n=n_missing)

# Merge humpback datasets
# 2000-14 data (doesn't have fishery type), 2015-18 data (missing unknown fisheries), 2015-18 data (unknown fisheries)
data <- rbind(filter(hb2000, year <= 2014), hb2015, hb2015unk) %>% 
  arrange(year, fishery) %>% 
  # Add factor order to fisheries
  mutate(fishery=factor(fishery, levels=c("CA Dungeness fishery", "OR/WA Dungeness fishery", 
                                          "Unknown Dungeness fishery", "Non-Dungeness fishery", "Unknown fishery")))

# Check totals against original
check <- data %>% 
  group_by(year) %>% 
  summarize(n=sum(n)) %>% 
  left_join(hb2000, by="year")
check$n.x==check$n.y


# Plot humpback data
################################################################################

# Plot final figure
g <- ggplot(data, aes(x=year, y=n, fill=fishery)) +
  geom_bar(stat="identity") +
  labs(x="", y="Number of entanglements", 
       title="U.S. West Coast Humpback whale entanglements") +
  geom_hline(yintercept=11, color="grey60", linetype="dotted") +
  annotate("text", label="Potential Biological Removal (PBR)", x=2000, y=12.5, hjust=0, color="grey60", size=2) +
  scale_x_continuous(breaks=2000:2018) +
  scale_fill_discrete(name="Fishery") +
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigSX_whale_entanglement_ts.png"), width=6.5, height=3.5, units="in", dpi=600)







