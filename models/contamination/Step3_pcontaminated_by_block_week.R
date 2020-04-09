

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(raster)
library(zoo)
library(tidyverse)
library(lubridate)
library(fasterize)
library(cowplot)
library(lubridate)

# Directories
charmdir <- "data/charm/processed"
datadir <- "models/contamination/data"
codedir <- "models/contamination"
gisdir <- "data/cdfw/gis_data/processed"
plotdir <- "models/figures"
outputdir <- "output"

# Read C-HARM data
data_orig <- readRDS(file.path(datadir, "CHARM_crab_da_data.Rds"))

# Load helper functions
source(file.path(codedir, "helper_functions.R"))

# Read particulate DA data
dap_brick <- brick(file.path(charmdir, "DAP_nowcast_2014present_brick.grd"))

# Read RF model
rf_model <- readRDS(file.path(datadir, "rf_model.Rds"))

# Read time step key
date_key <- read.csv(file.path("models", "model_time_step_key.csv"), as.is=T)

# Read CDFW blocks shapefile
blocks_sf <- readRDS(file.path(gisdir, "CA_commercial_fishing_blocks.Rds"))



# Downscale raster
################################################################################

# pDA key
pda_dates <- names(dap_brick)

# Clip temporally and spatially
dap_brick1 <- dap_brick[[194:nlayers(dap_brick)]] %>% 
  # Crop by extent
  # crop(blocks_sf)
  mask(mask=blocks_sf)


# Low resolution version
################################################################################

# Mean pDA by CA fishing block
# block_pda_avg <- raster::extract(x=dap_brick, y=blocks_sf, method="simple", fun="mean", na.rm=T)

# Convert blocks SF to raster
ras_temp <- dap_brick[[1]]
block_ras <- fasterize::fasterize(sf=blocks_sf, raster=ras_temp, field = "block_id")

# Zonal statistics using blocks raster
# Rows = blocks and columns equal dates
zstats <- raster::zonal(x=dap_brick, z=block_ras, fun="mean", na.rm=T)

# Format zonal stats
zstats1 <- zstats %>% 
  as.data.frame() %>% 
  gather(key="date", value="pda", 2:ncol(.)) %>% 
  mutate(date=gsub("X", "", date) %>% ymd()) %>% 
  rename(block_id=zone)

# Build data frame for predictions
zstats2 <- zstats1 %>% 
  arrange(block_id, date) %>% 
  filter(!is.na(pda)) %>% 
  group_by(block_id) %>% 
  mutate(day=1:n(),
         day0=pda,
         day1=dplyr::lag(x=pda, n=1, default=NA),
         day2=dplyr::lag(x=pda, n=2, default=NA),
         day3=dplyr::lag(x=pda, n=3, default=NA),
         day4=dplyr::lag(x=pda, n=4, default=NA),
         day5=dplyr::lag(x=pda, n=5, default=NA),
         day6=dplyr::lag(x=pda, n=6, default=NA),
         day7=dplyr::lag(x=pda, n=7, default=NA),
         day8=dplyr::lag(x=pda, n=8, default=NA),
         day9=dplyr::lag(x=pda, n=9, default=NA),
         day10=dplyr::lag(x=pda, n=10, default=NA),
         day11=dplyr::lag(x=pda, n=11, default=NA),
         day12=dplyr::lag(x=pda, n=12, default=NA),
         day13=dplyr::lag(x=pda, n=13, default=NA),
         day14=dplyr::lag(x=pda, n=14, default=NA),
         day15=dplyr::lag(x=pda, n=15, default=NA),
         day16=dplyr::lag(x=pda, n=16, default=NA),
         day17=dplyr::lag(x=pda, n=17, default=NA),
         day18=dplyr::lag(x=pda, n=18, default=NA),
         day19=dplyr::lag(x=pda, n=19, default=NA),
         day20=dplyr::lag(x=pda, n=20, default=NA),
         day21=dplyr::lag(x=pda, n=21, default=NA),
         day22=dplyr::lag(x=pda, n=22, default=NA),
         day23=dplyr::lag(x=pda, n=23, default=NA),
         day24=dplyr::lag(x=pda, n=24, default=NA),
         day25=dplyr::lag(x=pda, n=25, default=NA),
         day26=dplyr::lag(x=pda, n=26, default=NA),
         day27=dplyr::lag(x=pda, n=27, default=NA),
         day28=dplyr::lag(x=pda, n=28, default=NA),
         day29=dplyr::lag(x=pda, n=29, default=NA),
         day30=dplyr::lag(x=pda, n=30, default=NA)) %>% 
  ungroup() %>% 
  na.omit()


# Make predictions
preds <- predict(rf_model, newdata=zstats2, type="prob") # use "newdata" not "new_data"
preds_df <- preds %>% 
  rename(p_under="0", p_over="1")

# Add predictions to dataframe
zstats3 <- cbind(zstats2, preds_df)

# Calculate mean prediction
zstats4 <- zstats3 %>% 
  arrange(block_id, date) %>% 
  group_by(block_id) %>% 
  mutate(p_over_avg=zoo::rollmean(p_over, k=7, fill=NA, align="left")) %>% 
  ungroup()

# Reduce to dates of interest
zstats5 <- zstats4 %>% 
  select(block_id, date, p_over_avg) %>% 
  filter(date %in% ymd(date_key$date))

# Export weekly block-level p(contaminated) predictions
saveRDS(zstats5, file=file.path(outputdir, "pcontaminated_block_week.Rds"))


# Build animation
################################################################################

# Add p(contaminated) statistics to blocks SF object
blocks_sf_pcontam <- blocks_sf %>% 
  filter(block_type=="Inshore") %>% 
  left_join(zstats5, by="block_id")

# Animation packages
# devtools::install_github('thomasp85/gganimate', force=T)
library(gganimate)
library(gifski)

# Get US states and Mexico
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# Plot 
g <- ggplot() +
  # Plot p(contaminated)
  # geom_sf(blocks_sf_pcontam %>% filter(date=="2014-10-01"), mapping=aes(fill=p_over_avg)) + # single date
  geom_sf(blocks_sf_pcontam, mapping=aes(fill=p_over_avg, group=date)) +
  scale_fill_gradientn(name="Proportion of crabs\nabove DA threshold",
                       limits=c(0,1),
                       breaks=seq(0,1,0.2),
                       colors=RColorBrewer::brewer.pal(9, "Reds"),
                       na.value = NA) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Plot CA/Mexico
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Crop extent
  coord_sf(xlim = c(-125, -116), ylim = c(32, 42)) +
  theme_bw() + 
  theme(axis.title=element_blank(),
        legend.position = "bottom") +
  # Build animation
  transition_manual(date) +
  labs(title="{current_frame}")
#g

# Export single state to get size right
# ggsave(g, filename=file.path(plotdir, "whale_n_by_block_date_single.png"),
#        width=4.5, height=6.5, units="in", dpi=600)


# Save animation
anim_save(g, filename=file.path(plotdir, "animation_pcontaminated_block_week.gif"), duration=10,
          width=4.5, height=6.5, units="in", res=600)






# High resolution version
################################################################################

# Convert pDA brick to dataframe
pda_df <- as.data.frame(dap_brick1, xy=T)

# Number cells
cell_key <- pda_df %>% 
  select(x, y) %>% 
  unique() %>% 
  mutate(cell=1:n())

# Format pDA dataframe
pda_df1 <- pda_df %>% 
  # Wide to long
  gather(key="date", value="pda", 3:ncol(.)) %>% 
  # Add cell key
  left_join(cell_key) %>% 
  select(cell, x, y, date, pda) %>% 
  # Format data
  mutate(date=gsub("X", "", date) %>% ymd())

# Remove NA values
pda_df2 <- pda_df1 %>% 
  filter(!is.na(pda))

# Format for making predictions (2.3 minutes)
start_time <- Sys.time()
pda_df3 <- pda_df2 %>% 
  arrange(cell, date) %>% 
  group_by(cell) %>% 
  mutate(day=1:n(),
         day0=pda,
         day1=dplyr::lag(x=pda, n=1, default=NA),
         day2=dplyr::lag(x=pda, n=2, default=NA),
         day3=dplyr::lag(x=pda, n=3, default=NA),
         day4=dplyr::lag(x=pda, n=4, default=NA),
         day5=dplyr::lag(x=pda, n=5, default=NA),
         day6=dplyr::lag(x=pda, n=6, default=NA),
         day7=dplyr::lag(x=pda, n=7, default=NA),
         day8=dplyr::lag(x=pda, n=8, default=NA),
         day9=dplyr::lag(x=pda, n=9, default=NA),
         day10=dplyr::lag(x=pda, n=10, default=NA),
         day11=dplyr::lag(x=pda, n=11, default=NA),
         day12=dplyr::lag(x=pda, n=12, default=NA),
         day13=dplyr::lag(x=pda, n=13, default=NA),
         day14=dplyr::lag(x=pda, n=14, default=NA),
         day15=dplyr::lag(x=pda, n=15, default=NA),
         day16=dplyr::lag(x=pda, n=16, default=NA),
         day17=dplyr::lag(x=pda, n=17, default=NA),
         day18=dplyr::lag(x=pda, n=18, default=NA),
         day19=dplyr::lag(x=pda, n=19, default=NA),
         day20=dplyr::lag(x=pda, n=20, default=NA),
         day21=dplyr::lag(x=pda, n=21, default=NA),
         day22=dplyr::lag(x=pda, n=22, default=NA),
         day23=dplyr::lag(x=pda, n=23, default=NA),
         day24=dplyr::lag(x=pda, n=24, default=NA),
         day25=dplyr::lag(x=pda, n=25, default=NA),
         day26=dplyr::lag(x=pda, n=26, default=NA),
         day27=dplyr::lag(x=pda, n=27, default=NA),
         day28=dplyr::lag(x=pda, n=28, default=NA),
         day29=dplyr::lag(x=pda, n=29, default=NA),
         day30=dplyr::lag(x=pda, n=30, default=NA))
end_time <- Sys.time()
difftime(end_time, start_time)

# Missing values?
# freeR::complete(pda_df3)

# Impute missing values
# start_time <- Sys.time()
# pda_df4 <- pda_df3 %>%
#   VIM::kNN(k=3) %>%
#   ungroup()
# end_time <- Sys.time()
# difftime(end_time, start_time)

# Erase missing values (4.7 minutes)
start_time <- Sys.time()
pda_df4 <- pda_df3 %>%
  ungroup() %>% 
  na.omit()
end_time <- Sys.time()
difftime(end_time, start_time)

# Make predictions
preds <- predict(rf_model, newdata=pda_df4, type="prob") # use "newdata" not "new_data"
preds_df <- preds %>% 
  rename(p_under="0", p_over="1")

# Add predictions to dataframe
pda_df3 <- cbind(pda_df2, preds_df)






