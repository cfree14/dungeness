

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(raster)
library(tidyverse)
library(lubridate)

# Directories
datadir <- "data/charm/processed"
plotdir <- "data/charm/figures"
gisdir <- "data/cdfw/gis_data/raw/StateWaterJurisdiction/"
  
# Read data
pn_brick <- brick(file.path(datadir, "PN_nowcast_2014present_brick.grd"))
dap_brick <- brick(file.path(datadir, "DAP_nowcast_2014present_brick.grd"))
dac_brick <- brick(file.path(datadir, "DAC_nowcast_2014present_brick.grd"))

# Read CA state waters
ca_water_orig <- sf::st_read(dsn=gisdir, layer="MAN_CA_StateWater")

# Plot CA state waters
g <- ggplot(ca_water_orig, aes(fill=as.factor(ID))) +
  scale_fill_discrete(name="ID") +
  geom_sf()
g

# Format CA state waters
ca_water <- ca_water_orig %>% 
  sf::st_transform(crs=crs(pn_brick)) %>% 
  filter(ID==1)

# Plot formatted state waters
g <- ggplot(ca_water) +
  geom_sf()
g

# Build data
################################################################################

# Extract dates
dates <- names(dap_brick) %>% gsub("X", "", .) %>% ymd()

# Number of non-NA values in each raster
# values <- sum(!is.na(getValues(dap_brick[[1]])))
values <- sapply(1:length(dates), function(x) sum(!is.na(getValues(dap_brick[[x]]))))

# Merge data
data <- tibble(date=dates, nvals=values)

# Any days missing from data?
dates_should <- seq.Date(min(dates), max(dates), 1)
dates_missing <- dates_should[!dates_should %in% dates]

# Plot data
g <- ggplot(data, aes(x=date, y=nvals)) +
  geom_point() +
  labs(x="", y="Number of values") +
  theme_bw()
g



# Calculate average PN, pDA, and cDA in CA state waters (excluding islands)
ca_pn_avg <- extract(pn_brick, ca_water, method="simple", fun=mean, na.rm=T)
ca_dap_avg <- extract(dap_brick, ca_water, method="simple", fun=mean, na.rm=T)
ca_dac_avg <- extract(dac_brick, ca_water, method="simple", fun=mean, na.rm=T)

# Merge mean values into single data frame
data <- tibble(type=c(rep("PN", length(ca_pn_avg)), rep("pDA", length(ca_dap_avg)), rep("cDA", length(ca_dac_avg))),
               date=c(colnames(ca_pn_avg), colnames(ca_dap_avg), colnames(ca_dac_avg)),
               value=c(as.numeric(ca_pn_avg), as.numeric(ca_dap_avg), as.numeric(ca_dac_avg))) %>% 
  mutate(date=ymd(gsub("X", "", date)),
         type=plyr::revalue(type, c("PN"="Pseudo-nitzschia (>10^4 cells/ml)", 
                              "pDA"="Particulate domoic acid (>500 ng/l)",
                              "cDA"="Cellular domoic acid (>10 pg/cell)")))


# Setup theme
my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=9),
                  plot.subtitle = element_text(size=9),
                  plot.title=element_text(size=11),
                  legend.position="none",
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Things to add to this plot
# Add seasons (light grey)
# Add closures (darker grey)
# Add missing C-HARM measurements (something?)

# Plot data
g <- ggplot(data, aes(date, y=value, color=type)) +
  # Add season rectangles
  geom_rect(aes(xmin=ymd("2013-11-15"), xmax=ymd("2014-07-15"), ymin=0, ymax=1), 
            fill="grey95", color=NA, alpha = 0.8) +
  geom_rect(aes(xmin=ymd("2014-11-15"), xmax=ymd("2015-07-15"), ymin=0, ymax=1), 
            fill="grey95", color=NA, alpha = 0.8) +
  geom_rect(aes(xmin=ymd("2015-11-15"), xmax=ymd("2016-07-15"), ymin=0, ymax=1), 
            fill="grey95", color=NA, alpha = 0.8) +
  geom_rect(aes(xmin=ymd("2016-11-15"), xmax=ymd("2017-07-15"), ymin=0, ymax=1), 
            fill="grey95", color=NA, alpha = 0.8) +
  geom_rect(aes(xmin=ymd("2017-11-15"), xmax=ymd("2018-07-15"), ymin=0, ymax=1), 
            fill="grey95", color=NA, alpha = 0.8) +
  geom_rect(aes(xmin=ymd("2018-11-15"), xmax=ymd("2019-07-15"), ymin=0, ymax=1), 
            fill="grey95", color=NA, alpha = 0.8) +
  # Plot data
  geom_line(lwd=0.2) +
  facet_wrap(~ type, nrow=3) +
  labs(x="", y="Probability") +
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_charm_coverage.png"), width=6.5, height=4, units="in", dpi=600)







