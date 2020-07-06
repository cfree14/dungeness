
# Schedule dynamic closures
schedule_dynamic_closures <- function(pop_df, whales_sqkm_thresh){

  # Schedule closures
  pop_df <- pop_df %>% 
    mutate(closure=ifelse(whales_sqkm >= whales_sqkm_thresh & closure=="Season open", "Marine life concentration closure", closure),
           fishing_yn=ifelse(whales_sqkm >= whales_sqkm_thresh, F, fishing_yn))
  
  # Return
  return(pop_df)
  
}