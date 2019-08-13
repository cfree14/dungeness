
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
inputdir <- "data/roms/raw"
outputdir <- "data/roms/processed"

# Look up datasets on THREDDS server
catalog <- "http://oceanmodeling.pmc.ucsc.edu:8080/thredds/catalog/ccsra_2016a_phys_agg_zlevs/fmrc/catalog.html"
datasets <- tds_list_datasets(thredds_url = catalog)


# Functions
################################################################################

# Dataset name
dataset <- "Best Time Series"

# Dataset URL
url <- datasets$path[datasets$dataset==dataset]

# Available services
services <- tds_list_services(url)
  
# Get ROMS output through THREDDS server via OpenDap
url <- "http://oceanmodeling.pmc.ucsc.edu:8080/thredds/dodsC/ccsra_2016a_phys_agg_zlevs/fmrc/CCSRA_2016a_Phys_ROMS_z-level_(depth)_Aggregation_best.ncd"
data <- nc_open(url)

# Inspect data
print(data) # summary
data$nvars # number of variables
names(data) # properties
names(data$dim) # dimensions
names(data$var) # variables


# Function to download data
################################################################################

# Download ROMS data
# Variables: temp, salt, zeta
# variable <- "zeta"
download_roms_data <- function(variable){

  # Variable info
  var_name <- variable
  var_data <- data$var[[var_name]]
  # SSH is surface only: [X, Y, time] = [xi_rho, eta_rho, time] = [NA, NA, hours since 2011-01-02T00:00:00Z]
  # SST/salinity are depth-specific: [X, Y, Z, time] = [xi_rho, eta_rho, z, time] = [NA, NA, meters, hours since 2011-01-02T00:00:00Z]
  # Depths: -250, -200, -150, -100, -80, -60, -40, -20, -10, -5, -2 meters
  var_ndims <- var_data$ndims
  var_size <- var_data$varsize # 186 lons, 181 lats, 11 depths, 3100 times
  var_nsteps <- var_size[var_ndims]
  
  # Is the variable depth-specific?
  z_yn <- ifelse(var_ndims==4, T, F)
  
  # Get and format time steps
  times <- ncvar_get(data, "time")
  time_units <- ncatt_get(data, "time", "units") # hours since 2011-01-02T00:00:00Z
  dates <- as.Date(times/24, origin="2011-01-02")
  
  # Get lat/long in DD for converting arbitrary xi_rho and eta_rho coords to true DD coords
  lons <- ncvar_get(data, "lon_rho") # matrix
  lats <- ncvar_get(data, "lat_rho") # matrix
  lon_key <- lons %>% 
    raster() %>% 
    as.data.frame(xy=T) %>% 
    rename(long_dd=layer)
  lat_key <- lats %>% 
    raster() %>% 
    as.data.frame(xy=T) %>% 
    rename(lat_dd=layer)
  coord_key <- left_join(lon_key, lat_key, by=c("x", "y"))
  
  # Build list of rasters for each time step
  raster_list <- list()
  for(i in 1:5){
  # for(i in 1:var_nsteps) {
    
    # Figure out what to read
    start <- rep(1, var_ndims)
    start[var_ndims] <- i # start at the current time step
    count <- var_size # go all the way to finish of the current time step
    count[var_ndims] <- 1
    
    # Get data at time step t (includes all depths)
    v_t <- ncvar_get(data, var_name, start=start, count=count)
    
    # Isolate surface data (depth = -5 meters?, second to last depth in depth vector), if necessary
    if(z_yn){
      v_t_surface <- v_t[,,var_size[3]-1]
    }else{
      v_t_surface <- v_t
    }
    
    # Convert to raster (but coords are messed up)
    v_t_surface_ras <- raster(v_t_surface)
    
    # Convert to data frame
    v_t_surface_ras1 <- as.data.frame(v_t_surface_ras, xy=T) %>% 
      left_join(coord_key, by=c("x", "y")) %>% 
      rename(var=layer) %>% 
      select(long_dd, lat_dd, var) %>% 
      rasterFromXYZ()
    
    # Plot raster
    # plot(v_t_surface_ras1)
    
    # Save raster in list
    # https://stackoverflow.com/questions/42982599/netcdf-to-raster-brick-unable-to-find-inherited-method-for-function-brick-for
    raster_list[i] <- v_t_surface_ras1
      
  }
  
  # Build brick
  sst_brick <- brick(raster_list)
  names(sst_brick) <- dates[1:5]
  
  # Export brick
  saveRDS(sst_brick, file.path(outputdir, paste0("CA_ROMS_brick_", variable, ".Rds")))

}


# Download data
################################################################################

download_roms_data(variable="temp")
download_roms_data(variable="salt")
download_roms_data(variable="zeta")


