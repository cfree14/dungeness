
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# THREDDS package
# devtools::install_github("bocinsky/thredds")
library(thredds)

# Packages
library(ncdf4)
library(raster)
library(tidyverse)

# Directories
inputdir <- "data/charm/raw"
outputdir <- "data/charm/processed"

# Look up datasets in CeNCOOS THREDDS server
cencoos <- "http://thredds.cencoos.org/thredds/catalog.html"
datasets <- tds_list_datasets(thredds_url = cencoos)

# Functions
################################################################################

# Download CHARM nowcast NetCDFs
# Datasets: HAB Pseudo Nitzschia Nowcast, HAB Cellular Domoic Acid Nowcast, HAB Particulate Domoic Acid Nowcast
dataset <- "HAB Cellular Domoic Acid Nowcast"
download_charm_nowcasts <- function(dataset){
  
  # Dataset URL
  url <- datasets$path[datasets$dataset==dataset]
  
  # Available services
  services <- tds_list_services(url)
  
  # NetCDF URL
  ncdf_url <- services$path[services$service=="NetcdfSubset"]
  ncdf_vars <- tds_ncss_list_vars(ncss_url = ncdf_url)
  
  # Download NetCDF
  if(dataset=="HAB Pseudo Nitzschia Nowcast"){outfile_name <- "PN_nowcast_2014present.nc"}
  if(dataset=="HAB Cellular Domoic Acid Nowcast"){outfile_name <- "DAC_nowcast_2014present.nc"}
  if(dataset=="HAB Particulate Domoic Acid Nowcast"){outfile_name <- "DAP_nowcast_2014present.nc"}
  ncdf <- tds_ncss_download(ncss_url = ncdf_url,
                            out_file = file.path(inputdir, outfile_name),
                            var=ncdf_vars$name,
                            bbox = NULL, # download all grid cells
                            ncss_args=list(temporal = "all", accept="netcdf4", addLatLon="true"), # download all dates
                            overwrite=T)
  
  # Read NetCDF as raster brick
  # charm_ncdf <- nc_open(file.path(inputdir, outfile_name)) # deprecated - don't use this
  charm_brick <- raster::brick(ncdf)
  layer_names <- names(charm_brick)

  # Transpose x to y and double flip the map
  # https://stackoverflow.com/questions/44425548/plotting-netcdf-file-with-raster-package-leads-to-distorted-representation-r
  charm_brick_flipped <- flip(flip(t(charm_brick), direction = "y"), direction = "x")
  
  # Rotate from 0:360 to -180:180
  charm_brick_rotated <- rotate(charm_brick_flipped)
  
  # Restore lost layer names
  names(charm_brick_rotated) <- layer_names
  
  # Assign coordinate system
  crs(charm_brick_rotated) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  
  # Plot raster
  plot_1raster(date="2016-05-20", data=charm_brick_rotated, title=dataset)
  
  # Export brick
  # if(dataset=="HAB Pseudo Nitzschia Nowcast"){outfile_name <- "PN_nowcast_2014present_brick.Rds"}
  # if(dataset=="HAB Cellular Domoic Acid Nowcast"){outfile_name <- "DAC_nowcast_2014present_brick.Rds"}
  # if(dataset=="HAB Particulate Domoic Acid Nowcast"){outfile_name <- "DAP_nowcast_2014present_brick.Rds"}
  # saveRDS(charm_brick_rotated, file=file.path(outputdir, outfile_name))
  
  # Export raster
  if(dataset=="HAB Pseudo Nitzschia Nowcast"){outfile_name <- "PN_nowcast_2014present_brick.grd"}
  if(dataset=="HAB Cellular Domoic Acid Nowcast"){outfile_name <- "DAC_nowcast_2014present_brick.grd"}
  if(dataset=="HAB Particulate Domoic Acid Nowcast"){outfile_name <- "DAP_nowcast_2014present_brick.grd"}
  writeRaster(charm_brick_rotated, file=file.path(outputdir, outfile_name))
    
}

# Plot one layer
plot_1raster <- function(date, data, title){
  
  # Subset one date
  date1 <- gsub("-", ".", paste0("X", date))
  df <- data[[date1]] %>% 
    as.data.frame(xy=T) %>% 
    setNames(c("long_dd", "lat_dd", "prob"))
  
  # Plot one date
  g <- ggplot(df, aes(x=long_dd, y=lat_dd, fill=prob)) +
    geom_raster() +
    labs(x="", y="") +
    ggtitle(paste0(title, ": ", date)) +
    scale_fill_continuous(name="Probability") +
    theme_bw()
  g
  
}


# Download data
download_charm_nowcasts(dataset="HAB Cellular Domoic Acid Nowcast")
download_charm_nowcasts(dataset="HAB Particulate Domoic Acid Nowcast")
download_charm_nowcasts(dataset="HAB Pseudo Nitzschia Nowcast")



# # Build data
# ################################################################################
# 
# # Get list of available files
# catalog_url <- "https://coastwatch.pfeg.noaa.gov/erddap/files/charmForecast0day/"
# catalog_orig <- catalog_url %>% read_html() %>% html_table()
# 
# # Format catalog
# catalog <- catalog_orig[[3]] %>% 
#   select(2:4) %>% 
#   setNames(gsub(" ", "_", tolower(colnames(.)))) %>% 
#   filter(grepl("charm", name))
#   
# # Loop through catalog
# for(i in 1:nrow(catalog)){
#   
#   # C-HARM url
#   print(i)
#   charm_file <- catalog$name[i]
#   charm_url <- paste0("https://coastwatch.pfeg.noaa.gov/erddap/files/charmForecast0day/", charm_file)
#   
#   # Download C-HARM nowcast
#   download.file(url=charm_url, 
#                 destfile=paste(inputdir, charm_file, sep="/"), 
#                 method="auto",
#                 quiet=T, 
#                 mode="wb", 
#                 cacheOK = TRUE)
#   
# }

# # Convert to dataframe for plotting
# pn_df <- as.data.frame(pn_brick, xy=T) %>% 
#   gather(key="date", value="prob", 3:ncol(.)) %>% 
#   rename(long_dd=x, lat_dd=y) %>% 
#   select(date, everything()) %>% 
#   mutate(date=as.Date(gsub("\\.", "-", gsub("X", "", date))))
# 
# # Export data frame
# saveRDS(pn_df, file=file.path(outputdir, "charm_pn_nowcast_dataframe.Rds"))





