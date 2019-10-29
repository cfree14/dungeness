
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
datadir <- "data/entanglement/data"

# Read data
data_orig <- read.csv(file.path(datadir, "Human Caused Mortality and Serious Injury Determinations SW and NW Regions Beginning 2007.csv"), as.is=T)


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename columns
  setNames(tolower(colnames(.))) %>% 
  rename(stock=stock.or.area, interaction_type=interaction.type, location=county.locale, 
         injury_initial=initial.injury.assessment, injury_final=final.injury.assessment,
         injury_code=si.criteria.supporting.assessment, proration=si.proration.value, lof=count.against.lof,
         pbr=count.against.pbr) %>% 
  # Format columns
  mutate(species=freeR::sentcase(species), 
         # Format date info
         # Some have no month/day info
         date=ymd(paste(year, month, day, sep="-")),
         # Format location info
         stock=stringr::str_to_title(stock),
         stock=recode(stock, "Ca-Or-Wa"="CA-OR-WA", 
                      "Ca-Or-Wa Offshore"="CA-OR-WA Offshore",
                      "Eastern N Pacific - Pcfg"="Eastern N Pacific - PCFG",
                      "Eastern N Pacific-Pcfg"="Eastern N Pacific - PCFG",
                      "N Oregon-Wa Coast"="Northern Oregon-Washington Coast",
                      "Northern Oregon - Washington Coast"="Northern Oregon-Washington Coast",
                      "Oregon - Washington Coast"="OR-WA Coast",
                      "Oregon-Washington Coast"="OR-WA Coast",
                      "U.s."="United States"),
         location=stringr::str_to_title(location),
         state=recode(state, "BRITISH COLUMBIA"="Canada (BC)", "MEXICO"="Mexico"),
         # Format interaction info
         interaction_type=freeR::sentcase(interaction_type),
         comments=tolower(str_trim(comments)),
         # Format injury type
         injury_initial=freeR::sentcase(injury_initial), 
         injury_initial=recode(injury_initial, "Nsi"="Non-serious injury", "Si"="Serious injury",
                               "Si (prorate)"="Serious injury (prorate)"),
         injury_final=freeR::sentcase(injury_final), 
         injury_final=recode(injury_initial, "Nsi"="Non-serious injury", "Si"="Serious injury",
                               "Si (prorate)"="Serious injury (prorate)")) %>% 
  # Rearrange columns
  select(species, stock, year, month, day, date, state, location, 
         interaction_type, comments, injury_initial, injury_final, injury_code, proration, lof, pbr, everything())

# Inspect data
table(data$species)
table(data$stock)
table(data$interaction_type)
table(data$location)
table(data$state)
table(data$injury_initial)
table(data$injury_final)
table(data$injury_code)
table(data$lof)
table(data$pbr)

# Humpback whale data
hb_ts <- data %>% 
  filter(species=="Humpback whale") 

# Humback whale stats
hb_stats <- hb_ts %>% 
  group_by(year) %>% 
  summarize(pbr_count=sum(pbr=="Y"),
            lof_count=sum(lof=="Y"),
            n_mort=sum(injury_final=="Dead"),
            n_si=sum(injury_final %in% c("Serious injury", "Serious injury (prorate)")))

# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(datadir, "2015-2018_WC_marine_mammal_mortality_injury_data.Rds"))

