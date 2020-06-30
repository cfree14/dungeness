
# Determine sampling schedule
# da_sites <- da_site_key; first_date <- min(pop_df$date); last_date <- max(pop_df$date)
set_da_sampling_schedule <- function(da_sites, first_date, last_date){
  
  # Number of sampling sites
  nsites <- nrow(da_sites)
  nzones <- n_distinct(da_sites$area)
  zones <- unique(da_sites$area) %>% as.character()
  
  # Build sampling schedule
  # Sample sites in the same zone on the same dates
  da_sample_schedule <- purrr::map_df(zones, function(x) {
    
    # Site details
    site_info <- da_sites %>% 
      filter(area==x) %>% 
      select(area, location, block_id, lat_dd, long_dd) %>% 
      left_join(blocks_key %>% select(block_id, block_ramp), by="block_id") %>% 
      select(area:block_id, block_ramp, everything())
    
    # Draw initial sampling date
    oct1 <- paste(year(first_date), "10-01", sep="-") %>% ymd()
    oct31 <- paste(year(first_date), "10-31", sep="-") %>% ymd()
    init_date <- sample(x=seq(oct1, oct31, by="1 day"), size=1)
  
    # Schedule all sampling dates
    all_dates <- seq(init_date, last_date, by="1 week")
    all_dates_df <- expand_grid(location=site_info$location, sample_date=all_dates)
    
    # Build data frame
    df <- site_info %>% 
      full_join(all_dates_df, by="location") %>% 
      group_by(location) %>% 
      mutate(year=year(first_date), 
             season=paste(year, year-2000+1, sep="-"),
             sample_num=1:n(),
             sample_week=ceiling((sample_date-first_date)/7) %>% as.numeric()) %>% 
      ungroup() %>% 
      select(season, year, area:long_dd, sample_num, sample_week, sample_date, everything())
    
  })
  
}

