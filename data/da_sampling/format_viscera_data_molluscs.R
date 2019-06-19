
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(rio)
library(tidyverse)
library(lubridate)

# Directories
inputdir <- "data/da_sampling/raw"
outputdir <- "data/da_sampling/processed"
plotdir <- "data/da_sampling/figures"

# Read data
data_orig <- import(file.path(inputdir, "DA_CFree_0619.xlsx"))


# Data notes
################################################################################

# Vanessa Zubkousky, 6/12/19 email
# 1. The “Mod” data field before the biotoxin value field is a modifier field that 
#    should be added in front of the biotoxin value (IE. <2.5 ppm domoic acid).
# 2. Sample station coordinates are approximate. These have not been QCd or ground-truthed 
#    and may be incorrect.  If there are no coordinates, there are none in our database.
# 3. Only a portion of the commercial growing areas are tested for domoic acid routinely. 
#    For other areas it is usually event driven based on presence of Pseudo-nitzschia.
# 4. The shellfish growing area samples are generally collected weekly. There may be 
#    some breaks in data due to an area being temporarily closed for a variety of reasons.
# 5. The non-commercial sample sites are not sampled on a routine basis. Volunteer sampling 
#    is sporadic, based on the schedules of volunteers and plankton events. This data is not good for time series analysis. 
# 6. Sentinel mussels are mussels that have been placed in bags at a specific location 
#    (usually on a pier or buoy) for the purpose of collecting samples.
# 7. The razor clam samples are generally a single samples of meat from one 
#    individual clam. There may be some instances where there was a pooled sample of a few individuals. 
#    If you need more information for these samples, please let me know and I can see what we can find out.
# 8. Citations: Please cite the California Department of Public Health, Environmental Management Branch.

# Format data
################################################################################

# Format data
data <- data_orig %>% 
  rename(sampleid="SRL #",
         date="Date -Sampled-",
         location="Sample Site", 
         da_ppm_prefix="Mod-ASP", 
         da_ppm="ASP -ug/g-",
         type_orig="Sample Type",
         county="County",
         lat_dd="Latitude",
         long_dd="Longitude") %>% 
  # Add time columns
  mutate(date=ymd(date),
         year=year(date)) %>% 
  # Add species and type columns
  # 1. Add commas where missing so you can split based on commmas
  # 2. Split species and type based on comma
  # 3. Species to sentence case
  # 4. Replace cultured species
  mutate(type_orig=plyr::revalue(type_orig, c("Clam, razor"="Razor Clam",
                                              "Gaper Clam meat"="Gaper Clam, meat",
                                              "Gaper Clam viscera"="Gaper Clam, viscera",
                                              "Razor Clam meat"="Razor Clam, meat",
                                              "Razor Clam viscera"="Razor Clam, viscera",
                                              "Rock Scallop adductor"="Rock Scallop, adductor",
                                              "Rock Scallop viscera"="Rock Scallop, viscera")),
         species=sub('\\s*,.*','', type_orig),
         type_new=sub('.*,\\s*','\\1', type_orig),
         type_new=tolower(ifelse(species==type_new, NA, type_new)),
         tissue=ifelse(type_new%in%c("wild", "cultured", "sentinel") | is.na(type_new), "whole", type_new),
         type=ifelse(type_new%in%c("wild", "cultured", "sentinel"), type_new, "wild"),
         type=ifelse(species=="Cultured Rock Scallop", "cultured", type),
         species=freeR::sentcase(species),
         species=plyr::revalue(species, c("Cultured rock scallop"="Rock scallop")))  %>% 
  # Arrange columns
  select(sampleid,
         county, location, long_dd, lat_dd,
         year, date,
         type_orig, species, type, tissue, da_ppm, da_ppm_prefix) %>% 
  arrange(species, date)

# Inspect data
table(data$species)
table(data$tissue)
table(data$type)
freeR::complete(data)

# Export data
saveRDS(data, file=file.path(outputdir, "CDPH_mollusc_viscera_da_data.Rds"))


# Plot data
################################################################################

# Theme
my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=9),
                  plot.title=element_text(size=11),
                  legend.text=element_text(size=7),
                  legend.title=element_text(size=9),
                  panel.grid.major = element_line(colour = 'transparent'),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Plot data
g <- ggplot(data, aes(x=long_dd, y=lat_dd, size=da_ppm, col=species)) +
  geom_point() +
  facet_wrap(~year) +
  labs(x="", y="") +
  scale_color_discrete(name="Species") +
  scale_size_continuous(name="Domoic acid\nconcentration (ppm)") +
  theme_bw() + my_theme
g

ggsave(g, filename=file.path(plotdir, "CDPH_mollusc_viscera_da_data.png"), width=6.5, height=5.5, units="in", dpi=600)

