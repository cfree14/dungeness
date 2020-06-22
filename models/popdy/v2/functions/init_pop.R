
# Setup population container and initial population
init_pop <- function(b0_mt, step_key, blocks, nwhales){
  
  # Population container
  pop_df <- expand_grid(step_key,
                        select(blocks, block_region, block_ramp, block_id, block_sqkm, pbiomass), 
                        biomass_mt=0, traps_n=0, catch_mt=0, catch_mt_contam=0,
                        encounters_n=0, entanglements_n=0) %>% 
    # Mark whether season is open in block on date
    mutate(fishing_yn=ifelse(block_region=="Central/Southern", fishing_c, fishing_n),
           closure=ifelse(fishing_yn, "Season open", "Out-of-season")) %>% 
    # Add number of whales
    mutate(date=ymd(date)) %>% 
    left_join(nwhales, by=c("block_id", "date")) %>% 
    mutate(whales_sqkm=whales_n/block_sqkm) %>% 
    # Add p(contaminated)
    left_join(pcontam, by=c("block_id", "date")) %>% 
    # FILL IN MISSING CONTAMINATION VALUES (TEMPORARY HACK)
    mutate(pcontam=ifelse(is.na(pcontam), 0, pcontam)) %>%
    # Sample number of contaminated crabs ahead of time
    mutate(ncontam=sum(runif(n=6, min=0, max=1) < pcontam)) %>% 
    # Arrange columns
    select(-c(fishing_c, fishing_n, date_whale)) %>% 
    select(year, season, date, week, 
           block_region, block_ramp, block_id, block_sqkm, fishing_yn, closure,
           pbiomass, pcontam, biomass_mt, traps_n, catch_mt, catch_mt_contam,
           whales_n, whales_sqkm, encounters_n, entanglements_n, everything()) %>% 
    # Setup initial population
    mutate(biomass_mt=ifelse(week==1, pbiomass * b0_mt, 0)) %>%
    # Remove pbiomass column (only for initialization)
    select(-pbiomass)
  
  # Inspect
  # freeR::complete(pop_df)
  
  # Return
  return(pop_df)
  
}





