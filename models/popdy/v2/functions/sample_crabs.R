
# Sample crabs from coordinates
# Site key must have: date, long_dd, lat_dd
# site_key <- da_sample_schedule; ncrabs <- 6
sample_crabs <- function(site_key, ncrabs){

  # Loop through the unique dates:  x <- sample_dates[1]
  sample_dates <- sort(unique(site_key$sample_date))
  sample_results <- purrr::map_df(sample_dates, function(x) {
    
    # Get DA contamination predictions for date
    date_name <- gsub("-", ".", x) %>% paste0("X", .)
    pcontam_date <- pcontam_brick[[date_name]]
    
    # Sites sampled on date X
    site_key_date <- site_key %>% 
      filter(sample_date==x)
      
    # Extract values at points
    site_pts <- site_key_date %>%
      select(long_dd, lat_dd)
    site_probs <- raster::extract(x=pcontam_date, y=site_pts)
    
    # Sample N crabs and determine how many are contaminated
    n_over <- sapply(site_probs, function(x) sum(runif(n=ncrabs, min=0, max=1) <= x))
    
    # Build dataframe to return
    out_df <- site_key_date %>%
      mutate(pcontam=site_probs,
             nover=n_over)
    
  })
  
  # Format data
  sample_results <- sample_results %>% 
    arrange(area, location, sample_num)
  

  # Return sampling results
  return(sample_results)

}
