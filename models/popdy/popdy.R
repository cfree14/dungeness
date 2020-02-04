
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(tidyverse)

# Directories
datadir <- "data/cdfw/landings_confidential/processed"
gisdir <- "data/cdfw/gis_data/processed"
lhdir <- "data/life_history/data"

# Read life history data
vonb <- read.csv(file.path(lhdir, "dcrab_length_weight_at_age.csv"), as.is=T) %>% 
  mutate(sex=recode(sex, "Female"="females", "Male"="males")) %>% 
  rename(age=age_yr_int)


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


# Parameters
################################################################################

# Model parameters
############################################

# Parameters
nyears <- 20
step_length_yr <- 1 # 1/52 # 1 week = 1/52, 1 year = 1
nsteps <- nyears / step_length_yr
nsteps_per_year = 1/step_length_yr 
ncells <- nrow(blocks)

# On converting mortality time-scales
# Hordyk et al. (2015) Some explorations of the life history ratios to describe length composition, spawning-per-recruit, and the spawning potential ratio. ICES JMS.
# "Converting between time-scales is straightforward: M = 4.6 year−1 can be converted to a monthly rate M = 4.6/12 = 0.38 month−1`,
# where tmax = 12 months, or a weekly rate M = 4.6/52 = 0.088 week−1`, where tmax = 52 week"

# Life history parameters
############################################

# Constant life history 
tmax_yr <- 10
h <- 0.65 # steepness
epr <- 630222 # eggs per recruit
r0 <- 42*10^6 # virgin recruitment (R0)
hsat <- 0.1 # half-saturation constant

# Age-specific life history
nmort_yr_vec <- c(0.8, 0.8, rep(0.3, tmax_yr-2))
fecundity_vec <- c(0, 1, 2, 2, 2, rep(1, tmax_yr-5)) * 1e6
pmature_vec <- c(0, 0.2, rep(1, tmax_yr-2))
pretain_vec_m <- c(0,0,0,rep(1, tmax_yr-3))
pretain_vec_f <- rep(0, tmax_yr)
q_m <- c(0, 0, rep(0.00035, tmax_yr-2))
q_f <- c(0, 0, 0, rep(0.00035, tmax_yr-3))

# Merge age-specific life history
lh_at_age <- tibble(age=1:tmax_yr,
                    nmort_yr=nmort_yr_vec,
                    nmort_step=nmort_yr_vec/nsteps_per_year,
                    fecundity=fecundity_vec,
                    pmature=pmature_vec,
                    pretained_m=pretain_vec_m,
                    pretained_f=pretain_vec_f,
                    catchability_m=q_m,
                    catchability_f=q_f)

# Dervied values
nages <- length(1:tmax_yr)
nsexes <- 2

# Functions
############################################

# Calculate number of recruits
calc_recruits <- function(h, r0, eggs_n, epr){
  r = (4 * h * r0 * eggs_n) / (epr * r0 * (1-h) + (5*h+1) * eggs_n)
}



# Run model
################################################################################

# Initialize population
################################

# 1. Setup population data template
# 1st time step only, others get appended as calculated
pop_df_temp <- expand_grid(step=1, select(blocks, block_id, pbiomass), sex=c("males", "females"), age=1:tmax_yr, n=NA)

# 2. Setup initial population
pop_df <- pop_df_temp %>% 
  mutate(n=ifelse(step==1, 1e4, NA))


# Run population model
################################

# Loop through time steps
t <- 2
for(t in 2:nsteps){
  
  # Loop through cells: x <- block_ids[1]
  print(t)
  block_ids <- sort(unique(pop_df$block_id))
  pop_t_df <- map_df(block_ids, function(x){
    
    # Last timestep's biomass
    N_male_a_t <- pop_df %>% filter(block_id==x & step==(t-1) & sex=="males") %>% pull(n)
    N_female_a_t <- pop_df %>% filter(block_id==x & step==(t-1) & sex=="females") %>% pull(n)
    
    # Mortality: this timestep's biomass
    N_male_a_t1 <- N_male_a_t * exp(-lh_at_age$nmort_step)
    N_female_a_t1 <- N_female_a_t * exp(-lh_at_age$nmort_step)
    
    # Record this years biomasses using initial as template
    out_df <- pop_df_temp %>% 
      filter(block_id==x) %>% 
      mutate(step=t,
             n=ifelse(sex=="males", N_male_a_t1, N_female_a_t1))
    return(out_df)
  
  })
  
  # If its the last time step of the year, conduct recruitment and senescence
  yr_steps <- nsteps_per_year * 1:nyears
  if(t %in% yr_steps){
    
    # Calculate population-wide reproduction and recruitment and
    # distribute recruits proportional to catch
    
    # Reproduction (sperm limited)
    # rho <- cumsum(n_m_t1[2:tmax_yr]) / n_f_t1[1:(tmax_yr-1)]
    # err_a_t <- c(rho / (hsat + rho), 0)
    # eggs_t <- sum(n_f_t1 * lh_at_age$fecundity * lh_at_age$pmature * err_a_t)
    
    # Reproduction (not sperm limited)
    N_female_a_t_global <- pop_t_df %>% 
      filter(sex=="females") %>% 
      group_by(age) %>% 
      summarise(n=sum(n, na.rm=T)) %>% 
      pull(n)
    eggs_t_global <- sum(N_female_a_t_global * lh_at_age$pmature * lh_at_age$fecundity)
    
    # Recruitment (age-1s)
    recruits_t_global <- calc_recruits(h=h, r0=r0, eggs_n=eggs_t_global, epr=epr)
    
    # Add recruits (age-1s) to year t dataframe
    # Assume 50:50 sex ratio (that's why divided by zero)
    pop_t_df <- pop_t_df %>% 
      # Age each age class by one year
      group_by(block_id, sex) %>% 
      mutate(n1=c(NA, n[1:(tmax_yr-1)])) %>% 
      # Add recruits
      mutate(n1=ifelse(age==1, pbiomass*recruits_t_global/2, n1)) %>% 
      # Remove n column and rename (I kept this in just to check that things were working)
      select(-n) %>% 
      rename(n=n1) %>% 
      # Ungroup
      ungroup()
    
    # Confirm that recruits were distributed correctly
    if(!all.equal(sum(pop_t_df$n[pop_t_df$age==1]), recruits_t_global)){
      stop("Number of recruits distributed does not match number produced.")
    }
    
  }
  
  # Merge data from each time point
  pop_df <- rbind(pop_df, pop_t_df)
  
}

# Check dimensions
if(!nrow(pop_df) == ncells * nages * nsexes * nsteps){stop("Number of rows is wrong")}


# Visualize results
################################################################################

# Add cohort weights
pop_df1 <- pop_df %>% 
  left_join(select(vonb, -cw_mm), by=c("sex", "age")) %>% 
  mutate(biomass_mt=n*weight_g/1000/1000)

# Total population stats
#############################################

# Total population stats
tot_stats <- pop_df1 %>% 
  group_by(step) %>% 
  summarize(q_total_mt=sum(biomass_mt),
            q_legal_mt=sum(biomass_mt[sex=="males" & age>=4])) %>% 
  gather(key="type", value="biomass_mt", 2:ncol(.)) %>% 
  mutate(type=recode(type, "q_total_mt"="Total", "q_legal_mt"="Legal"))

# Plot total population stats
g <- ggplot(tot_stats, aes(x=step, y=biomass_mt, color=type)) +
  geom_line() +
  geom_hline(yintercept = legal_male_pop_est_mt) +
  labs(x="Week", y="Biomass (mt)") +
  expand_limits(y = 0) +
  theme_bw()
g









