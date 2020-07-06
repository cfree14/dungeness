
# Schedule hotspot closures
schedule_hotspot_closures <- function(pop_df, closures_key, yr){
  
  # Format scheduled closures
  # (i.e., update dates of zone opens/closures to match current season)
  closures_key_yr <- closures_key %>%
    # Convert dates to date objects
    rename(date1_key=date1, date2_key=date2) %>%
    mutate(date1_key=mdy(date1_key),
           date2_key=mdy(date2_key)) %>%
    # Convert to match season
    mutate(season=paste(yr, yr-2000+1, sep="-")) %>%
    mutate(date1=ifelse(month(date1_key)<=7, paste(yr+1, month(date1_key), mday(date1_key), sep="-"), paste(yr, month(date1_key), mday(date1_key), sep="-")),
           date2=ifelse(month(date2_key)<=7, paste(yr+1, month(date2_key), mday(date2_key), sep="-"), paste(yr, month(date2_key), mday(date2_key), sep="-")),
           date1=ymd(date1),
           date2=ymd(date2)) %>%
    # Add model date, calculate days/weeks since model start date
    mutate(model_date_init=min(pop_df$date),
           model_day1=date1-model_date_init,
           model_day2=date2-model_date_init, 
           week1=floor(model_day1/7) %>% as.numeric(),
           week2=ceiling(model_day2/7)  %>% as.numeric())
  
  # Make a long key
  closures_key_yr_long <- purrr::map_df(1:nrow(closures_key_yr), function(x) {
    closure <- closures_key_yr %>% 
      slice(x) %>% 
      select(ramp_zone, week1, week2)
    closure_df <- tibble(zone=closure$ramp_zone, week=closure$week1:closure$week2, scheduled_closure=T)
  })
  
  # Add scheduled closures to population data frame
  pop_df <- pop_df %>% 
    # Add closures
    left_join(closures_key_yr_long, by=c("block_ramp"="zone", "week")) %>% 
    # Record closures
    mutate(closure=ifelse(!is.na(scheduled_closure) & scheduled_closure==T & closure=="Season open", "Marine life concentration closure", closure),
           fishing_yn=ifelse(!is.na(scheduled_closure) & scheduled_closure==T, F, fishing_yn)) %>% 
    # Remove excess column
    select(-scheduled_closure)
  
  # Return
  return(pop_df)
  
}