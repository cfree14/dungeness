
# For testing
if(F){
  yrs2sim <- 2015
  effort_dynamics <- "biomass-coupled" # none or constant
  a <- 0.15
  b <- 0.1
}

# Function to run model
run_model <- function(yrs2sim, effort_dynamics, a, b){
  
  # 1. Parameters
  ################################################################################
  
  # 1a. Model parameters
  ############################################
  
  # Parameters
  ncells <- nrow(blocks)
  nyears <- length(yrs2sim)
  step_key <- time_step_key %>% 
    filter(year %in% yrs2sim)
  
  # 1b. Life history parameters
  ############################################
  
  # On converting mortality time-scales
  # Hordyk et al. (2015) Some explorations of the life history ratios to describe length composition, spawning-per-recruit, and the spawning potential ratio. ICES JMS.
  # "Converting between time-scales is straightforward: M = 4.6 year−1 can be converted to a monthly rate M = 4.6/12 = 0.38 month−1`,
  # where tmax = 12 months, or a weekly rate M = 4.6/52 = 0.088 week−1`, where tmax = 52 week"
  
  # Constant life history
  tmax_yr <- 10
  q <- 0.00035
  Nmort <- 0.6925
  Nmort_step <- Nmort / 52
  
  # 1c. Fishing parameters
  ############################################
  
  # If fishing is turned on (i.e., effort_dynamics!="none")
  if(effort_dynamics!="none"){
  
    # Create vector marking the weeks in which fishing happens (fishing season)
    fishing_yn_c <- step_key$fishing_c
    fishing_yn_n <- step_key$fishing_n
    
    # Fleet parameters
    ntraps_max <- 173900
    ntraps_max_obs <- 143550
    ntraps_max_fix <- 100000

  }

  # 1d. Initialize population
  ################################
  
  # Build data container
  b0_mt <- 6500
  pop_df <- expand_grid(step_key,
                        select(blocks, block_region, block_id, pbiomass), 
                        biomass_mt=NA, traps_n=NA, catch_mt=NA, 
                        encounters_n=NA, entanglements_n=NA) %>% 
    # Mark whether season is open in block on date
    mutate(fishing_yn=ifelse(block_region=="Central/Southern", fishing_c, fishing_n)) %>% 
    # Arrange columns
    select(-c(fishing_c, fishing_n)) %>% 
    select(year, season, date, week, block_region, block_id, pbiomass, fishing_yn,
           biomass_mt, traps_n, catch_mt, encounters_n, entanglements_n, everything()) %>% 
    # Setup initial population
    mutate(biomass_mt=ifelse(week==1, pbiomass * b0_mt, NA))

  
  # 2. Run population model
  ################################
  
  # Loop through time steps
  t <- 2; x <- 102
  for(t in 2:52){
    
    # Calculate CA wide values
    #########################################
    
    # Number of legal-sized males in previous time step
    B_tot_prev <- pop_df %>% 
      filter(week==(t-1)) %>% 
      pull(biomass_mt) %>% sum()
    
    # Effort in previous time step
    E_tot_prev <- pop_df %>% 
      filter(week==(t-1)) %>%
      select(block_id, traps_n) %>% 
      unique() %>% pull(traps_n) %>% sum()
    
    # Loop through cells
    #########################################
    
    # Loop through cells: x <- block_ids[1]
    print(t)
    block_ids <- sort(unique(pop_df$block_id))
    for(x in block_ids){
      
      # Last time step's biomass
      B_t <- pop_df %>% filter(block_id==x & week==(t-1)) %>% pull(biomass_mt)
    
      # 2a. Calculate catch
      #########################################
      
      # No fishing
      if(effort_dynamics=="none"){
        Fmort <- 0
        C_t <- 0
        E <- 0
      }
      
      # Constant effort
      if(effort_dynamics=="constant"){
        # In season?
        fishing_yn <- pop_df %>% filter(block_id==x & week==(t-1)) %>% pull(fishing_yn)
        if(!fishing_yn){
          Fmort <- 0
          C_t <- 0
          E <- 0
        }else{
          # CA-wide effort
          E_tot <- ntraps_max_obs
          # Block-level effort
          E_prop <- unique(pop_df$pbiomass[pop_df$block_id==x]) 
          E <- E_tot * E_prop
          Fmort <- E * q
          C_t <- calc_catch(N=B_t, M=Nmort_step, F=Fmort)
        }
      }
      
      # Biomass-coupled effort
      if(effort_dynamics=="biomass-coupled"){
        # In season?
        fishing_yn <- pop_df %>% filter(block_id==x & week==(t-1)) %>% pull(fishing_yn)
        # If not in season, set effort/catch to zero
        if(!fishing_yn){
          Fmort <- 0
          C_t <- 0
          E <- 0
        # If in season, set effort/catch based on:
        # max effort if first week open or previous effort if already opened
        }else{
          # CA-wide effort
          if(is.na(E_tot_prev) | E_tot_prev==0){
            E_tot <- ntraps_max_fix
          }else{
            E_tot <- calc_effort2(E_max=ntraps_max_fix, 
                                  E_prev=E_tot_prev, 
                                  B_prev=B_tot_prev, 
                                  b0=b0_mt, a=a, b=b)
          }
          # Block-level effort
          # Proportion of biomass in cell
          B_cell <- pop_df %>% 
            filter(block_id==x & week==(t-1)) %>% 
            pull(biomass_mt)
          B_prop <- B_cell / B_tot_prev
          E_prop <- B_prop
          E <- E_tot * E_prop
          Fmort <- E * q
          C_t <- calc_catch(N=B_t, M=Nmort_step, F=Fmort)
        }
      }
      
      # 2b. Update biomass
      #########################################
      
      # Mortality: this time step's biomass
      B_t1 <- calc_abundance(N=B_t, M=Nmort_step, F=Fmort)
    
      # Record results
      pop_df$biomass_mt[pop_df$week==t & pop_df$block_id==x] <- B_t1
      pop_df$traps_n[pop_df$week==t & pop_df$block_id==x] <- E
      pop_df$catch_mt[pop_df$week==t & pop_df$block_id==x] <- C_t
      
    } # closes cell loop
  
  } # closes time loop
  
  # Return results
  return(pop_df)
  
}




