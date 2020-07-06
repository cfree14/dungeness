
# Conduct whale surveys
# zones <- sort(unique(pop_df$block_ramp)); fall_window <- "10/15-11/1"; spring_window <- "3/1-3/15"
conduct_whale_surveys <- function(zones, fall_window, spring_window, yr){
  
  # Number of zones
  nzones <- length(zones)
  
  # Schedule fall surveys
  fall_date1 <- strsplit(fall_window, split="-")[[1]][1]
  fall_date2 <- strsplit(fall_window, split="-")[[1]][2]
  fall_date1_full <- paste(fall_date1, yr, sep="/") %>% mdy()
  fall_date2_full <- paste(fall_date2, yr, sep="/") %>% mdy()
  fall_date_range <- seq(fall_date1_full, fall_date2_full, by="1 day")
  fall_survey_dates <- sample(x=fall_date_range, size= nzones) 
  
  # Schedule spring surveys
  spring_date1 <- strsplit(spring_window, split="-")[[1]][1]
  spring_date2 <- strsplit(spring_window, split="-")[[1]][2]
  spring_date1_full <- paste(spring_date1, yr, sep="/") %>% mdy()
  spring_date2_full <- paste(spring_date2, yr, sep="/") %>% mdy()
  spring_date_range <- seq(spring_date1_full, spring_date2_full, by="1 day")
  spring_survey_dates <- sample(x=spring_date_range, size= nzones)
  
  # Build survey schedule
  whale_surveys <- tibble(zone=rep(zones, 2),
                          survey=c(rep("fall",  nzones), rep("spring",  nzones)),
                          survey_date=c(fall_survey_dates, spring_survey_dates))
  
  
  
}