library(sf)
library(tidyverse)

datfile <- here::here("data/cdfw/gis_data/processed/CDFW_Districts.shp")

CDFWdistricts <- st_read(datfile) %>%
				st_transform("+init=epsg:4326")


st_geometry_type(CDFWdistricts)
st_crs(CDFWdistricts)
st_bbox(CDFWdistricts)

ggplot() +
	geom_sf(data=CDFWdistricts)


marinedistnames<-6:21 # NEED TO CHANGE THIS

mardists <- CDFWdistricts %>% 
				filter(DISTRICT %in% marinedistnames)

findN<-function(x) st_bbox()

marcoords <- mardists %>%
				group_by(DISTRICT) %>% nest() %>%
				mutate(Northern=map_dbl(data,function(x)st_bbox(x)$ymax),Southern=map_dbl(data,function(x)st_bbox(x)$ymin))
				