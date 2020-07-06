
# Setup
################################################################################

# Directories
landingsdir <- "data/cdfw/landings_confidential/processed"
gisdir <- "data/cdfw/gis_data/processed"
stressordir <- "output"
injurydir <- "data/entanglement/data"

# Seasons to use
seasons_use <- c(2013, 2014)
seasons_use_long <- c("2013-2014", "2014-2015")

# Build time step key
################################################################################

# Read time step key
time_step_key <- read.csv("models/model_time_step_key.csv", as.is=T) %>% 
  mutate(date=ymd(date))

# Read number of whales by block and date
nwhales <- readRDS(file.path(stressordir, "nwhales_block_week.Rds")) %>% 
  mutate(date=ymd(date))

# Read proportion contaminated with DA by block and date
pcontam <- readRDS(file.path(stressordir, "pcontaminated_block_week.Rds")) %>% 
  rename(pcontam=p_over_avg) %>% 
  mutate(date=ymd(date))

# Read number of whales by RAMP fishing zone and date
nwhales_ramp_wk <- read.csv(file.path(stressordir, "nwhales_by_ramp_zone_and_week.csv"), as.is=T) %>% 
  mutate(season_yr=substr(season, 1, 4) %>% as.numeric())

# Inputs
################################################################################

# Read scheduled closures key
scheduled_closures_key <- read.csv(file="models/popdy/v2/inputs/scheduled_closures.csv", as.is=T)

# DA sampling
################################################################################

# Read key
da_site_key <- read.csv("models/contamination/data/da_sampling_sites.csv")

# Read p(contam predictions)
pcontam_brick <- brick("models/contamination/paper/output/dungeness_crab_model_rf_cda_predictions.gri") # REPLACE THIS WITH FINAL MODEL


# Initial biomass
################################################################################

# Read Richerson et al. 2020 data
B_yr <- read.csv("data/richerson_etal_2020/richerson_etal_2020_ca_preseason_dcrab_abundance.csv", as.is=T) %>% 
  group_by(year) %>% 
  summarize_at(.vars=c("crabs_mt", "crabs_mt_lo", "crabs_mt_hi"), .funs = sum, na.rm=T)

# Initial biomass stats
B0_stats <- B_yr %>% 
  filter(year>=2014) %>% 
  summarize(B0_mt_avg=mean(crabs_mt),
            B0_mt_sd=sd(crabs_mt))

# Plot
B0_mt_draws <- rnorm(n=1000, mean=B0_stats$B0_mt_avg, sd=B0_stats$B0_mt_sd)
hist(B0_mt_draws, breaks=seq(0,18000,1000), col="grey80")
hist(B_yr$crabs_mt, breaks=seq(0,18000,1000), col="grey80")


# Biomass distribution
################################################################################

# Read data
########################

# Read data
load(file.path(landingsdir, "CDFW_dungeness_landings_data.Rdata"))

# Read CDFW blocks shapefile
blocks_sf <- readRDS(file.path(gisdir, "CA_commercial_fishing_blocks.Rds"))

# Extract block key
blocks_key <- blocks_sf %>% 
  sf::st_drop_geometry()


# Build data
########################

# Reduce to CA only
data_ca <- data %>% 
  filter(block_state=="California")

# Calculate mean seasonal catch by block
data_bk <- data_ca %>% 
  # Filter to CA landings
  filter(block_region %in% c("Northern", "Central/Southern") & season!="out-of-season") %>% 
  # Calculate sum seasonal landings by block
  group_by(season, block_region, block_id, block_sqkm) %>% 
  summarize(landings_mt=sum(landings_mt, na.rm=T)) %>% 
  # Reduce to seasons to evaluate
  mutate(year1=substr(season, 1, 4) %>% as.numeric()) %>% 
  filter(year1 %in% seasons_use) %>% 
  # Calculate mean landings by block
  group_by(block_region, block_id, block_sqkm) %>% 
  summarize(landings_mt=mean(landings_mt, na.rm=T)) %>% 
  ungroup() %>% 
  # Remove large blocks
  filter(block_id<1000) %>% 
  # Remove unmapped blocks
  filter(!is.na(block_sqkm))

# Plot data
########################

# Setup theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.text.y = element_text(angle = 90, hjust = 0.5),
                  legend.text=element_text(size=8),
                  legend.title=element_text(size=10),
                  legend.position = "bottom",
                  panel.grid.major = element_line(colour = 'transparent'),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Get US states and Mexico
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# Plot
blocks_sf1 <- blocks_sf %>% 
  left_join(data_bk, by="block_id") %>% 
  sf::st_transform(crs=sf::st_crs(usa)) %>% 
  filter(block_id < 1000)

# Plot landings
g1 <- ggplot() +
  # Plot blocks
  geom_sf(data=blocks_sf1, mapping=aes(fill=landings_mt/100)) +
  # scale_fill_gradientn(name="Landings\n(100s mt per year)", colors=RColorBrewer::brewer.pal(9, "Reds"), na.value=NA) +
  scale_fill_gradientn(name="Landings\n(100s mt per year)", colors=rev(RColorBrewer::brewer.pal(9, "RdBu")), na.value=NA) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Plot land
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Crop map
  coord_sf(xlim = c(-125.5, -116.5), ylim = c(32, 42)) +
  # Little things
  labs(x="", y="") +
  theme_bw() + my_theme
g1

# Blocks
blocks <- data_bk %>% 
  mutate(pbiomass=landings_mt/sum(landings_mt)) %>% 
  left_join(blocks_key %>% select(block_id, block_ramp))
sum(blocks$pbiomass) # must be one


# Number of traps per week
################################################################################

# Build data
########################

# Number of traps per week (all seasons)
data_wk <- data_ca %>% 
  # Filter to 2013-2019 in-season landings with tier info
  filter(season!="out-of-season" & season_year>=2013) %>% 
  # Fill in missing tier data (arbitrary decision)
  mutate(permit_tier=ifelse(is.na(permit_tier), 0, permit_tier)) %>% 
  mutate(permit_ntraps=ifelse(permit_tier==0, 350, permit_ntraps)) %>% 
  # Calculate season week with reference to model start date
  mutate(model_start_date=ymd(paste(season_year, 10, 1, sep="-")), 
         model_week=difftime(receipt_date, model_start_date, units = "weeks") %>% as.numeric() %>% floor()) %>% 
  # Number of operators by tier per week
  group_by(season, permit_tier, permit_ntraps, model_week) %>% 
  summarise(n_vessels=n_distinct(vessel_id),
            landings_mt=sum(landings_mt, na.rm=T)) %>% 
  mutate(n_traps=n_vessels * permit_ntraps) %>% 
  ungroup() %>% 
  # Calculate number of vessels/traps by week
  group_by(season, model_week) %>% 
  summarize(n_vessels=sum(n_vessels),
         n_traps=sum(n_traps),
         landings_mt=sum(landings_mt))

# Number of traps per weeks (seasons of interest)
data_wk_use <- data_wk %>% 
  filter(season %in% seasons_use_long)

# Calculate average to fit to
data_wk_avg <- data_wk_use %>%
  # Group by week
  group_by(model_week) %>% 
  summarize(n_vessels=mean(n_vessels, na.rm=T),
            n_traps=mean(n_traps, na.rm=T),
            landings_mt=mean(landings_mt, na.rm=T)) %>% 
  mutate(season="Average") %>% 
  ungroup()

# Merge season data and season average
data_wk_plot <- bind_rows(data_wk_use, data_wk_avg)

# Plot number of traps by week
ntraps_max_pos <- 173900
ntraps_max_obs <- max(data_wk_plot$n_traps)
g <- ggplot(data_wk_plot %>% filter(model_week>4), aes(x=model_week, y=n_traps/1000, color=season)) +
  geom_line() +
  labs(x="Model week", y="Number of traps (1000s)") +
  scale_color_discrete(name="Season") +
  # Add maximum number of traps possible
  geom_hline(yintercept=ntraps_max_pos/1000, linetype="dotted") +
  annotate(geom="text", x=42, y=ntraps_max_pos/1000, hjust=1, vjust=-0.5, 
           label=paste0("Maximum possible: ", format(ntraps_max_pos, big.mark=","), " traps"), size=2) +
  # Add maximum number of traps observed
  geom_hline(yintercept=ntraps_max_obs/1000, linetype="dotted") +
  annotate(geom="text", x=42, y=ntraps_max_obs/1000, hjust=1, vjust=-0.5, 
           label=paste0("Maximum observed: ", format(ntraps_max_obs, big.mark=","), " traps"), size=2) +
  # Theme
  theme_bw() + my_theme
g

# Plot amount of catch by week
g <- ggplot(data_wk_plot %>% filter(model_week>4), aes(x=model_week, y=landings_mt/1000, color=season)) +
  geom_line() +
  labs(x="Season week", y="Landings (1000s of mt)") +
  scale_color_discrete(name="Season") +
  # Theme
  theme_bw() + my_theme
g


# Calculate probability of entanglement
################################################################################

# Build dataframe with the following columns:
# season, model_week, model_date, block_id, block_sqkm, landings_mt, vessels_n, traps_n, whales_n, p_encounter, encounters_n

# Build data
data1 <- nwhales %>% 
  # Add, organize, and rename time step meta-data
  left_join(time_step_key, by="date") %>% 
  select(season, week, date, block_id, whales_n) %>% 
  rename(model_week=week, model_date=date) %>% 
  # Add, organize, and rename block meta-data
  left_join(blocks_key %>% select(block_region, block_id, block_sqkm), by="block_id") %>% 
  select(season, model_week, model_date, block_region, block_id, block_sqkm, whales_n) %>% 
  # Add, organize, and rename catch/effort data
  mutate(season=gsub("-", "-20", season)) %>% 
  left_join(data_wk, by=c("season", "model_week")) %>% 
  rename(vessels_n=n_vessels, traps_n=n_traps) %>% 
  select(season, model_week, model_date, block_region, block_id, block_sqkm, landings_mt, vessels_n, traps_n, whales_n) %>% 
  # Remove block/weeks without catch/effort data
  filter(!is.na(vessels_n)) %>%
  # Calculate probability of encounter
  mutate(whales_sqkm=whales_n / block_sqkm, 
         p_encounter=calc_p_encounter(p=whales_sqkm),
         encounters_n=traps_n * p_encounter)
  
# Inspect data
freeR::complete(data1)

# Number of encounters by season
encounters_yr <- data1 %>% 
  group_by(season) %>% 
  summarize(encounters_n=sum(encounters_n))


# Read injury data
# injury_data <- readRDS(file.path(injurydir, "2015-2018_WC_marine_mammal_mortality_injury_data.Rds"))
# 
# idata <- injury_data %>% 
#   filter(species=="Humpback whale" & interaction_type%in%"Dungeness crab pot fishery (california)") %>% 
#   group_by(year) %>% 
#   summarize(n=n())












