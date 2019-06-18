
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(rio)
library(tidyverse)
library(lubridate)

# Directories
inputdir <- "data/da_sampling/raw"
outputdir <- "data/da_sampling/processed"
plotdir <- "data/da_sampling/figures"
gisdir <- "data/cdfw/gis_data/processed"

# Read data
crab1517_orig <- import(file.path(inputdir, "Free Request 061019 2015-2019 DA.xlsx"), which=1)
crab1718_orig <- import(file.path(inputdir, "Free Request 061019 2015-2019 DA.xlsx"), which=2)
crab1819_orig <- import(file.path(inputdir, "Free Request 061019 2015-2019 DA.xlsx"), which=3)
lobster16_orig <- import(file.path(inputdir, "Free Request 061019 2015-2019 DA.xlsx"), which=4)
lobster1718_orig <- import(file.path(inputdir, "Free Request 061019 2015-2019 DA.xlsx"), which=5)
lobster1819_orig <- import(file.path(inputdir, "Free Request 061019 2015-2019 DA.xlsx"), which=6)

# Read block centroids
blocks <- sf::st_read(dsn=gisdir, layer="CA_fishing_blocks_clip_centroids") %>% 
  st_transform("+init=epsg:4326") 
block_ids <- blocks$BLOCK10_ID
block_coords <- sf::st_coordinates(blocks)
blocks_df <- data.frame(block=block_ids, block_coords) %>% 
  rename(block_lat_dd=Y, block_long_dd=X)

# Read sampling site coords
sites <- import(file.path(inputdir, "crab_da_sampling_sites.xlsx"))


# Format individual files
################################################################################

crab1517 <- crab1517_orig %>% 
  setNames(tolower(colnames(.))) %>% 
  rename(sampleid="is#",
         block="block #",
         depth_f="depth (fathoms)",
         da_ppm="result (ppm)")
colnames(crab1517)

crab1718 <- crab1718_orig %>% 
  setNames(tolower(colnames(.))) %>% 
  rename(sampleid="is#",
         block="block #",
         species="species - viscera",
         depth_f="depth (fathoms)",
         da_ppm="result (ppm)",
         lat_long="lat/long coordinates")
colnames(crab1718)

crab1819 <- crab1819_orig %>% 
  setNames(tolower(colnames(.))) %>% 
  rename(sampleid="is#",
         date="date of catch",
         species="species - viscera",
         block="block #",
         lat_long="lat/long coordinates",
         depth_f="depth (fathoms)",
         da_ppm="result (ppm)") %>% 
  # select(-c(...10, ...11, ...12))
  select(-c(x__1, x__2, x__3))
colnames(crab1819)

lobster16 <- lobster16_orig %>% 
  setNames(tolower(colnames(.))) %>% 
  rename(sampleid="is#",
         block="block #",
         depth_f="depth (fathoms)",
         da_ppm="result (ppm)") %>% 
  # select(-...9)
  select(-x__1)
colnames(lobster16)  

lobster1718 <- lobster1718_orig %>% 
  setNames(tolower(colnames(.))) %>% 
  rename(sampleid="is#",
         block="block #",
         lat_long="lat/long coordinates",
         depth_f="depth (fathoms)",
         da_ppm="result (ppm)") 
colnames(lobster1718)  

lobster1819 <- lobster1819_orig %>% 
  setNames(tolower(colnames(.))) %>% 
  rename(sampleid="is#",
         date="date of catch",
         species="species - viscera",
         block="block #",
         lat_long="lat/long coordinates",
         depth_f="depth (fathoms)",
         da_ppm="result (ppm)") 
colnames(lobster1819) 

# Merge and format files
################################################################################

# Merge
data_merged <- plyr::rbind.fill(crab1517, crab1718, crab1819, 
                         lobster16, lobster1718, lobster1819)

# Format
data <- data_merged %>% 
  # Format date
  mutate(date=ymd(date),
         year=year(date)) %>% 
  # Format ports
  mutate(port=plyr::revalue(port, c("Ft. Bragg"="Fort Bragg"))) %>% # Monterey == Monterey Bay?
  # Format depth
  # 1 fathom = 6 feet = 1.8 meters
  # Convert everything to fathoms then meters
  mutate(depth_f=plyr::revalue(depth_f, c("Unk"="",
                                          "unknown"="",
                                          "< 25"=25 / 6,
                                          "< 30"=30 / 6,
                                          "> 25"=25 / 6,
                                          "> 30"=30 / 6,
                                          "10 ft"=10 / 6, 
                                          "10-20 feet"=15 / 6, 
                                          "110 feet"=110 / 6,
                                          "140 feet"=140 / 6,
                                          "15 feet"=15 / 6,
                                          "22-35 feet"=28.5 / 6,
                                          "30 feet"=30 / 6,
                                          "30 to 40 feet"=35 / 6,
                                          "31 to 40 feet"= 35.5 / 6,
                                          "32 to 40 feet"=36 / 6,
                                          "33 to 40 feet"=36.5 / 6,
                                          "34 to 40 feet"=38 / 6,
                                          "35 feet"=35 / 6,
                                          "40 feet"=40 / 6,
                                          "45 feet"=45 / 6,
                                          "60 feet"=60 / 6)),
         depth_f=as.numeric(depth_f),
         depth_ft=depth_f*6,
         depth_m=depth_ft * 0.3048) %>%
  # Format species
  mutate(species=freeR::sentcase(species),
         species=plyr::revalue(species, c("Lobster"="Spiny lobster",
                                          "Lobster (f)"="Spiny lobster (F)", 
                                          "Lobster (m)"="Spiny lobster (M)"))) %>% 
  # Format DA ppm
  mutate(da_ppm_prefix=ifelse(grepl("<", da_ppm), "<", NA), 
         da_ppm=as.numeric(gsub("<| |ND|nd", "", da_ppm))) %>%
  # Format lat/long
  rename(latlong_orig=lat_long) %>% 
  mutate(latlong_orig=str_trim(latlong_orig)) %>% 
  # Rearrange columns
  left_join(blocks_df, by="block") %>% 
  select(sampleid, year, date, 
         port, area, block, block_long_dd, block_lat_dd,
         latlong_orig, depth_m, 
         species, da_ppm_prefix, da_ppm)
  

# Write CSV
write.csv(data, file=file.path(inputdir, "crab_da_data_temp.csv"), 
          fileEncoding="UTF-8", row.names=F)


# Read data for lat/long formatting
################################################################################

# Read file
data1 <- read.csv(file.path(inputdir, "crab_da_data_temp.csv"), as.is=T, fileEncoding = "UTF-8", strip.white = T)

# Lat/long key
latlong <- data1 %>% 
  select(latlong_orig) %>% 
  unique() %>% 
  mutate(latlong=trimws(latlong_orig),
         # Erase NA info
         latlong=ifelse(latlong %in% c("not avail / SB Operation", "Doran Beach", "not available"), NA, latlong),
         # Remove degree/minute symbols
         latlong=gsub("’|°|'", "", latlong), 
         # Erase N/S designators
         latlong=gsub("N|N |W|W ", "", latlong),
         # Convert to comma seperated values
         latlong=gsub(" -", ", ", latlong),
         latlong=gsub(" / ", ", ", latlong),
         latlong=gsub(",,", ",", latlong), 
         latlong=gsub(" , ", ",", latlong),
         latlong=gsub(" -", ", ", latlong),
         # Revalue a few troublesome values
         latlong=sapply(latlong, function(x) trimws(x)),
         latlong=plyr::revalue(latlong, c("34.052324,-119.568205" = "34.052324, 119.568205",
                                          "34 00 945, 119 21 340" = "34 00.945, 119 21.340",
                                          "38 41 123 27" = "38 41, 123 27",
                                          "34 00.533120 02.569" = "34 00.533, 120 02.569",
                                          "40 39.03 124 24.01 or 40 38.95 124 24.05" = "40 39.03, 124 24.01",
                                          "40 39.06 124 25.82 or 40 39.00 124 25.84" = "40 39.06, 124 25.82",
                                          "39 00.42, 123 44.772 and \n 39 01.25, 123 46.779" = "39 00.42, 123 44.772",
                                          "1) 34 02.56, 119 31.93. 2) 34 02.69, 119 32.54. 3) 34 01.77, 119 36.52" = "34 02.56, 119 31.93",
                                          "33 59.598 119 33.772 or 33 59.624 119 34.298" = "33 59.598, 119 33.772",
                                          "34 02.26, 119 31.50 or 34 02.45, 119 31.73" = "34 02.26, 119 31.50",
                                          "41 16.00 -124 09.00" = "41 16.00, 124 09.00",
                                          "41 33.00 -124 11.00" = "41 33.00, 124 11.00",
                                          "41 46.00 124 15.00" = "41 46.00, 124 15.00",
                                          "40 39.8 124 23.5"  = "40 39.8, 124 23.5",
                                          "40 39.3 124 23.7"  = "40 39.3, 124 23.7",
                                          "41.46.62 124.19.13"  = "41.46.62, 124.19.13",
                                          "41 38 124  10" = "41 38, 124 10",
                                          "41 45 124  19" = "41 45, 124 19",
                                          "41 45.700  124 19.400" = "41 45.700, 124 19.400",
                                          "41 45.000  124 18.705" = "41 45.000, 124 18.705",
                                          "41 10.47  124 11.32" = "41 10.47, 124 11.32",
                                          "41 10.11  124 13.06" = "41 10.11, 124 13.06",
                                          "33 59.383 119 32.134" = "33 59.383, 119 32.134",
                                          "34 03.2499 119 32.80" = "34 03.2499, 119 32.80",
                                          "34 02.60 119 32.24" = "34 02.60, 119 32.24",
                                          "33 59.310 119 32.427" = "33 59.310, 119 32.427",
                                          "34 02.69 119 32.45" = "34 02.69, 119 32.45"))) %>% 
  # Add length of names
  mutate(nchar=nchar(latlong)) %>% 
  mutate(lat=sapply(latlong, function(x) unlist(strsplit(x, ","))[1]),
         lat=trimws(lat),
         long=sapply(latlong, function(x) unlist(strsplit(x, ","))[2]),
         long=trimws(long),
         lat_dd=as.numeric(substr(lat, 1, 2)) + as.numeric(substr(lat, 4, nchar(lat)))/60,
         long_dd=as.numeric(substr(long, 1, 3)) + as.numeric(substr(long, 5, nchar(lat)))/60) %>% 
  # Fix ones already in decimal degrees
  mutate(lat_dd=ifelse(lat_dd>180, lat, lat_dd),
         long_dd=ifelse(long_dd>180, long, long_dd))


# Add coordinates to original data
data2 <- data1 %>% 
  left_join(select(latlong, latlong_orig, lat_dd, long_dd), by="latlong_orig")

sdata <- data2 %>%
  filter(!is.na(latlong_orig) & is.na(lat_dd))



plot(lat_dd ~ long_dd*-1, data2)




