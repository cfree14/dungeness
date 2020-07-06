  
# Schedule April 1st closure
schedule_early_closure <- function(pop_df, date_md){
  
  # Closure date
  yr <- max(year(pop_df$date))
  date_dummy <- paste(date_md, "2000", sep="/") %>% mdy()
  date_close <- ymd(paste(yr, month(date_dummy), mday(date_dummy), sep="-"))
  
  # Schedule closure
  pop_df <- pop_df %>% 
    mutate(closure=ifelse(date >= date_close & closure=="Season open", "Early closure", closure),
           fishing_yn=ifelse(date >= date_close, F, fishing_yn))
  
  # Return
  return(pop_df)
  
}
