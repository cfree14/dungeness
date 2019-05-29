
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
inputdir <- "data/raw/charm"

# Theoretically, I could use the rerdapp package to do this but it doesn't work
# https://cran.r-project.org/web/packages/rerddap/vignettes/Using_rerddap.html
# library(rerdapp)
# charmInfo <- info("charmForecast0day")
# charmData <- griddap(charmInfo, ...)
# Funnily, the following code does find C-HARM on the ERDAPP servers
# charmId <- ed_search(query = "charm", url="https://coastwatch.pfeg.noaa.gov/erddap")


# Build data
################################################################################

# Get list of available files
catalog_url <- "https://coastwatch.pfeg.noaa.gov/erddap/files/charmForecast0day/"
catalog_orig <- catalog_url %>% read_html() %>% html_table()

# Format catalog
catalog <- catalog_orig[[3]] %>% 
  select(2:4) %>% 
  setNames(gsub(" ", "_", tolower(colnames(.)))) %>% 
  filter(grepl("charm", name))
  
# Loop through catalog
for(i in 1:nrow(catalog)){
  
  # C-HARM url
  print(i)
  charm_file <- catalog$name[i]
  charm_url <- paste0("https://coastwatch.pfeg.noaa.gov/erddap/files/charmForecast0day/", charm_file)
  
  # Download C-HARM nowcast
  download.file(url=charm_url, 
                destfile=paste(inputdir, charm_file, sep="/"), 
                method="auto",
                quiet=T, 
                mode="wb", 
                cacheOK = TRUE)
  
}


