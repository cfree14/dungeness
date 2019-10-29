
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
list.files(inputdir)
cdfw_port_orig <- import(file.path(inputdir, "Commercial_Dungeness_Landings_Data_CDFW_by_Port_and_Year_2000-2017.xlsx"))
cdfw_region_orig <- import(file.path(inputdir, "Commercial_Dungeness_Landings_Data_CDFW_by_Season_and_Region_1915-2018.xlsx"))
nmfs_orig <- import(file.path(inputdir, "Commercial_Dungeness_Landings_Data_NMFS_by_Year_1950-2016.xlsx"))
psmfc_orig <- import(file.path(inputdir, "Commercial_Dungeness_Landings_Data_PSMFC_by_Season_and_Region_1915-2005.xlsx"))
permits_orig <- import(file.path(inputdir, "Commercial_Dungeness_Permit_Summary_Data-2013-2014-season-only.xlsx"))
cpfv_rec_orig <- import(file.path(inputdir, "Recreational_CPFV_Dungeness_Landings_1935-2016.xlsx"))
crfs_rec_orig <- import(file.path(inputdir, "Recreational_CRFS-RECFIN_Dungeness_Landings_2005-2014.xlsx"), which=2) # multiple sheets to this file


# Format data
################################################################################

# CDFW 2000-2017 landings by port
cdfw_port <- cdfw_port_orig %>% 
  select(1:4) %>% 
  setNames(tolower(colnames(.))) %>% 
  rename(tl_lb="landings (lbs)",
         value_usd="ex-vessel value ($)") %>% 
  # Make factor in South to North order
  mutate(port=factor(port, levels=c("San Diego", "Los Angeles", "Santa Barbara", 
                                    "Morro Bay", "Monterey", "San Francisco", "Bodega Bay", "Fort Bragg", "Eureka")))
  arrange(year, port)

# CDFW 1915-2018 landings by region
cdfw_region <- cdfw_region_orig %>% 
  select(1:10) %>% 
  setNames(tolower(colnames(.))) %>% 
  rename(tl_lb_north="northern (lbs)",
         tl_lb_central="central (lbs)",
         tl_lb_total="statewide (lbs)",
         tl_mt_north="northern (mt)",
         tl_mt_central="central (mt)",
         tl_mt_total="statewide (mt)",
         value_usd_total="ex-vessel value",
         price_usd_lb="average price per lb",
         price_usd_kg="average price per kg") %>% 
  mutate(year=as.numeric(substr(season, 1, 4))) %>% 
  select(year, season, everything()) %>% 
  arrange(year)

# NMFS 1950-2016 landings
nmfs <- nmfs_orig %>% 
  select(1:5) %>% 
  setNames(tolower(colnames(.))) %>% 
  rename(tl_lb=pounds,
         tl_mt="metric tons",
         value_usd="$") %>% 
  select(-species)

# PSMFC 1915-2005 landings by region
psmfc <- psmfc_orig %>% 
  select(1:8) %>% 
  rename(season="Season",
         tl_lb_crescent_city="CrescentCity/Eureka/Fort Bragg (lbs)",
         tl_lb_bodega_bay_sf="BodegaBay/SanFrancisco (lbs)",
         tl_lb_monterey="Monterey/MorroBay (lbs)",
         tl_lb_total="California (lbs)",
         value_usd="Value",
         price_usd_lb="Avg. price per pound",
         n_vessels="Number of vessels") %>% 
  # Fix "..." in Monterey column
  mutate(tl_lb_monterey=as.numeric(ifelse(tl_lb_monterey=="...", "", tl_lb_monterey))) %>% 
  # Remove "preliminary data" flag from season column and make new column
  mutate(notes=ifelse(grepl("\\*", season), "preliminary data", NA),
         season=gsub("\\*", "", season)) %>% 
  # Add year column
  mutate(year=as.numeric(substr(season, 1, 4))) %>% 
  select(year, season, everything()) %>% 
  arrange(year)

# CPFV 1935-2016 recreational landings # CORRECT LANDINGS
cpfv_rec <- cpfv_rec_orig %>% 
  select(1:2) %>% 
  rename(year=Year,
         tl_n="CPFV recreational landings (number of crabs)") %>% 
  # Fix "no data" in landings
  mutate(tl_n=as.numeric(gsub("no data", "", tl_n)))

# CRFS 2005-2014 recreational landings
crfs_rec <- crfs_rec_orig %>%
  rename(year="RecFIN Year",
         month="RecFIN Month",
         week="RecFIN Week",
         agency="Agency",
         district="District Name",
         mode="RecFIN Mode Name",
         water_area="RecFIN Water_Area Name",
         subregion="RecFIN Subregion Name",
         trip_type="RecFIN Trip Type Name",
         species="Species Name",
         retained_n="Retained (# fish)",
         retained_mt="Retained (mt)",
         released_alive_n="Released Alive (# crabs)",
         released_alive_mt="Released Alive (mt)",
         released_dead_n="Released Dead (# fish)",
         released_dead_mt="Released Dead (mt)",
         total_n="Total Mortality (# crabs)",
         total_mt="Total Mortality (mt)")

  

# Export data
################################################################################

# Export data
save(cdfw_port, cdfw_region, cpfv_rec, crfs_rec, nmfs, psmfc,
     file=file.path(outputdir, "CA_dungeness_landings_data.Rdata"))



# Format for plotting
################################################################################

# Commercial landings
############################################

# CDFW 2000-2017 landings by port
cdfw1 <- cdfw_port %>% 
  group_by(year) %>% 
  summarize(tl_lb=sum(tl_lb, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  mutate(source="CDFW (2000-2017 by port)") %>% 
  select(source, everything())

# CDFW 1915-2018 landings by region
cdfw2 <- cdfw_region %>% 
  select(year, tl_lb_total, value_usd_total) %>% 
  rename(tl_lb=tl_lb_total, value_usd=value_usd_total) %>% 
  mutate(source="CDFW (1915-2018 by region)") %>% 
  select(source, everything())

# NMFS 1950-206 landings
nmfs1 <- nmfs %>%
  select(-tl_mt) %>% 
  mutate(source="NMFS (1950-2016 overall)") %>% 
  select(source, everything())

# PSMFC 1915-2005 landings by region
psmfc1 <- psmfc %>% 
  select(year, tl_lb_total, value_usd) %>% 
  rename(tl_lb=tl_lb_total) %>% 
  mutate(source="PSMFC (1915-2005 by region)") %>% 
  select(source, everything())
  
# Merge commercial landings
comm <- plyr::rbind.fill(cdfw1, cdfw2, nmfs1, psmfc1)

# Recreational landings
############################################

# CRFV rec landings
cpfv_rec1 <- cpfv_rec %>% 
  mutate(source="CPFV (1957-2016 overall)") %>% 
  select(source, everything())

# CRFS rec landings
crfs_rec1 <- crfs_rec %>%
  group_by(year) %>% 
  summarize(tl_n=sum(total_n)) %>% 
  mutate(source="CRFS (2005-2014, highly resolved)") %>% 
  select(source, everything())

# Merge recreational landings
rec <- plyr::rbind.fill(cpfv_rec1, crfs_rec1)


# Price data
############################################

# CDFW region
# PSMFC

cdfw_prices <- cdfw_region %>% 
  select(year, price_usd_lb) %>% 
  filter(!is.na(price_usd_lb)) %>% 
  mutate(source="CDFW region")

psmfc_prices <- psmfc %>% 
  select(year, price_usd_lb) %>% 
  filter(!is.na(price_usd_lb)) %>% 
  mutate(source="PSMFC")

prices <- rbind(cdfw_prices, psmfc_prices)

# Plot data
################################################################################

# Setup theme
my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=9),
                  plot.title=element_text(size=11),
                  legend.text=element_text(size=7),
                  legend.title=element_text(size=9),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Commercial landings
g <- ggplot(comm, aes(x=year, y=tl_lb/1e6, col=source)) +
  geom_line(size=0.3) +
  geom_point(size=0.8) +
  labs(x="", y="Total landings (millions of lbs)") +
  scale_color_discrete(name="Source") +
  theme_bw() + my_theme
g
ggsave(g, filename=file.path(plotdir, "Fig1_dungeness_landings_commercial.png"), width=6.5, height=3, units="in", dpi=600)


# Recreational landings
g <- ggplot(rec, aes(x=year, y=tl_n/1e3, col=source)) +
  geom_line(size=0.3) +
  geom_point(size=0.8) +
  labs(x="", y="Total landings (1000s of crabs)") +
  scale_color_discrete(name="Source") +
  theme_bw() + my_theme
g
ggsave(g, filename=file.path(plotdir, "Fig2_dungeness_landings_recreational.png"), width=6.5, height=3, units="in", dpi=600)


# CDFW by port
g <- ggplot(cdfw_port, aes(x=year, y=tl_lb/1e6, fill=port)) +
  geom_area() + 
  labs(x="", y="Total landings (millions of pounds)") +
  scale_fill_discrete(name="Port (south to north)") +
  theme_bw() + my_theme
g
ggsave(g, filename=file.path(plotdir, "Fig3_dungeness_landings_comm_port.png"), width=6.5, height=3, units="in", dpi=600)


# Prices
##################################################################

g <- ggplot(prices, aes(x=year, y=price_usd_lb, col=source)) +
  geom_point() +
  geom_line() +
  labs(x="", y="Price (USD / lb)") +
  scale_color_discrete(name="Source") +
  theme_bw() + my_theme
g
ggsave(g, filename=file.path(plotdir, "FigX_dungeness_prices.png"), width=6.5, height=3, units="in", dpi=600)

# Catch vs. profits
##################################################################

# CDFW catch and profts comparison
p1 <- ggplot(cdfw_port, aes(x=tl_lb/1e6, y=value_usd/1e6, col=port)) +
  geom_point() +
  ggtitle("CDFW 2000-2017 landings by port") +
  labs(x="Landings (millions of lbs)", y="Value (millions of USD)") +
  scale_colour_discrete(name="Port") +
  theme_bw() + my_theme
p1

# NMFS catch and profits comparison
p2 <- ggplot(nmfs, aes(x=tl_lb/1e6, y=value_usd/1e6, col=year)) +
  geom_point() +
  ggtitle("NMFS 1950-2016 landings") +
  labs(x="Landings (millions of lbs)", y="Value (millions of USD)") +
  scale_color_gradientn(colors=RColorBrewer::brewer.pal(9, "YlOrRd"), name="Year") +
  theme_bw() + my_theme
p2

# PSMFC 1915-2005 landings
p3 <- ggplot(psmfc, aes(x=tl_lb_total/1e6, y=value_usd/1e6)) +
  geom_point() +
  ggtitle("PSMFC landings") +
  labs(x="Landings (millions of lbs)", y="Value (millions of USD)") +
  theme_bw() + my_theme
p3

# Arrange
g <- grid.arrange(p1, p2, p3, ncol=3)

ggsave(g, filename=file.path(plotdir, "FigX_value_landings_correlation.png"), width=10.5, height=3, units="in", dpi=600)




