
# Setup
################################################################################

# Directories
datadir <- "data/cdfw/landings_confidential/processed"
gisdir <- "data/cdfw/gis_data/processed"
lhdir <- "data/life_history/data"

# Read life history data
vonb <- read.csv(file.path(lhdir, "dcrab_length_weight_at_age.csv"), as.is=T) %>% 
  mutate(sex=recode(sex, "Female"="females", "Male"="males")) %>% 
  rename(age_yr=age_yr_int)

# Read mean cumulative catch
avg_cum_catch <- read.csv(file.path(datadir, "mean_cum_catch_by_wk_in_typical_season.csv"), as.is=T)


# Biomass distribution
################################################################################

# Read data
load(file.path(datadir, "CDFW_dungeness_landings_data.Rdata"))

# Read CDFW blocks shapefile
blocks_sf <- readRDS(file.path(gisdir, "CA_commercial_fishing_blocks.Rds"))

# Reduce to CA only
data <- data %>% 
  filter(block_state=="California")

# Calculate mean seasonal catch by block
data_bk <- data %>% 
  # Filter to CA landings
  filter(block_region %in% c("Northern", "Central/Southern") & season!="out-of-season") %>% 
  # Calculate sum seasonal landings by block
  group_by(season, block_id) %>% 
  summarize(landings_mt=sum(landings_mt, na.rm=T)) %>% 
  # Reduce to recent years
  mutate(year1=substr(season, 1, 4) %>% as.numeric()) %>% 
  filter(year1>=2014) %>% 
  # Calculate mean landings by block
  group_by(block_id) %>% 
  summarize(landings_mt=mean(landings_mt, na.rm=T)) %>% 
  ungroup() %>% 
  # Remove large blocks
  filter(block_id<1000)

# Annual summaries
# Eliminate seasons with major closures: 2016-16, 2018-19
catch_tot <- data %>% 
  # Filter to CA landings
  filter(block_region %in% c("Northern", "Central/Southern") & !season%in%c("out-of-season", "2015-2016", "2018-2019")) %>% 
  # Calculate sum seasonal landings by block
  group_by(season) %>% 
  summarize(landings_mt=sum(landings_mt, na.rm=T))

# Total annual catch
barplot(catch_tot$landings_mt)
hist(catch_tot$landings_mt, breaks=seq(0,20000,2000), 
     main="Total catch", xlab="Landings (mt)", col="grey70")
abline(v=median(catch_tot$landings_mt), lwd=3)

# Total biomass of legal-sized males
hist(catch_tot$landings_mt/0.85, breaks=seq(0,25000,2000), 
     main="Biomass of legal-sized males", xlab="Biomass (mt)", col="grey70")
abline(v=median(catch_tot$landings_mt/0.85), lwd=3)
legal_male_pop_est_mt <- 9200

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
  scale_fill_gradientn(name="Landings\n(100s mt per year)", colors=rev(RColorBrewer::brewer.pal(9, "RdYlBu")), na.value=NA) +
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
  mutate(pbiomass=landings_mt/sum(landings_mt))
sum(blocks$pbiomass) # must be one


