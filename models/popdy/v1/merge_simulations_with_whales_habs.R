
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)
library(rnaturalearth)

# Directories
outputdir <- "output"
plotdir <- "figures"
gisdir <- "data/cdfw/gis_data/processed"

# Read data
simdata <- readRDS(file.path(outputdir, "one_year_simulation.Rds"))
nwhales <- readRDS(file.path(outputdir, "nwhales_block_week.Rds"))
pcontam <- readRDS(file.path(outputdir, "pcontaminated_block_week.Rds"))

# Read CDFW blocks shapefile
blocks_sf <- readRDS(file.path(gisdir, "CA_commercial_fishing_blocks.Rds"))
blocks_df <- blocks_sf %>% 
  sf::st_drop_geometry()

# Read time step key
date_key <- read.csv("models/model_time_step_key.csv", as.is=T) %>% 
  mutate(date=ymd(date)) %>% 
  select(-c(fishing_c, fishing_n))


# Whales
################################################################################

calc_p_encounter <- function(D=0.0135, v=3.37, t=168, p){
  p_encounter=1-exp(-2*D*v*t*p)
}

# Build data
data <- simdata %>% 
  # Rename columns
  rename(week=step) %>% 
  # Weekly stats
  group_by(week, block_id) %>% 
  summarize(ntraps=unique(traps_n),
            catch_mt=sum(catch_mt)) %>% 
  mutate(ntraps=ifelse(is.na(ntraps), 0, ntraps)) %>% 
  ungroup()

# Merge whales with effort
wdata <- nwhales %>% 
  # Add week
  left_join(date_key, by="date") %>% 
  # Add area and calculate density
  left_join(blocks_df %>% select(block_id, block_area_sqkm), by="block_id") %>% 
  mutate(whales_sqkm=whales_n/block_area_sqkm) %>% 
  # Add number of traps
  left_join(data, by=c("week", "block_id")) %>% 
  # Filter to only blocks with traps
  filter(!is.na(ntraps)) %>% 
  # Calculate numnber of encounters
  mutate(p_encounter=calc_p_encounter(p=whales_sqkm),
         n_encounters=p_encounter*ntraps)

# Calculate weekly number of whale encounters by season
wdata1 <- wdata %>% 
  group_by(season, week) %>% 
  summarize(ntraps=sum(ntraps),
            nwhales=sum(whales_n),
            nencounters=sum(n_encounters))

# Setup theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  plot.title=element_text(size=12),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Plot
g <- ggplot(wdata1, aes(x=week, y=nencounters/1000, color=season)) +
  geom_line() +
  labs(x="Model week", y="Number of whale-trap encounters (1000s)") +
  theme_bw()
g

# Export
ggsave(g, filename=file.path(plotdir, "figure_nencounters_week_season_no_mgmt.png"), 
       width=6.5, height=4.5, units="in", dpi=600)


# Domoic acid
################################################################################

# Merge p(contam) with catch
cdata <- pcontam %>% 
  # Add week
  left_join(date_key, by="date") %>% 
  # Add number of traps
  left_join(data, by=c("week", "block_id")) %>% 
  # Filter to only blocks with traps
  filter(!is.na(ntraps)) %>% 
  # Calculate amount of contaminated catch
  mutate(contam_catch_mt=p_over_avg * catch_mt)


# Calculate weekly amount of contaminated catch
cdata1 <- cdata %>% 
  group_by(season, week) %>% 
  summarize(ntraps=sum(ntraps),
            catch_mt=sum(catch_mt),
            contam_catch_mt=sum(contam_catch_mt),
            pcontam=contam_catch_mt/catch_mt)

# Setup theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  plot.title=element_text(size=12),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Plot
g <- ggplot(cdata1, aes(x=week, y=contam_catch_mt/1000, color=season)) +
  geom_line() +
  labs(x="Model week", y="Contaminated catch (1000s mt)") +
  theme_bw()
g

# Export
ggsave(g, filename=file.path(plotdir, "figure_contam_catch_week_season_no_mgmt.png"), 
       width=6.5, height=4.5, units="in", dpi=600)

