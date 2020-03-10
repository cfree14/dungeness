
# For testing
if(F){
  nyears <- 1
  step_length_yr <- 1/52 # 1 = 1 year; 1/52 = 1 week
  effort_dynamics <- "constant" # none or constant
  r0 <- 42*10^6 # virgin recruitment (R0)
  age_dist <- age_dist_stable
  a <- 0.15
  b <- 0.1
}

# Function to run model
run_model <- function(nyears, step_length_yr, r0, effort_dynamics, age_dist, a, b){
  
  # 1. Parameters
  ################################################################################
  
  # 1a. Model parameters
  ############################################
  
  # Parameters
  nsteps <- nyears / step_length_yr
  nsteps_per_year <- 1 / step_length_yr 
  ncells <- nrow(blocks)
  
  
  # 1b. Life history parameters
  ############################################
  
  # On converting mortality time-scales
  # Hordyk et al. (2015) Some explorations of the life history ratios to describe length composition, spawning-per-recruit, and the spawning potential ratio. ICES JMS.
  # "Converting between time-scales is straightforward: M = 4.6 year−1 can be converted to a monthly rate M = 4.6/12 = 0.38 month−1`,
  # where tmax = 12 months, or a weekly rate M = 4.6/52 = 0.088 week−1`, where tmax = 52 week"
  
  # Constant life history
  tmax_yr <- 10
  h <- 0.65 # steepness
  epr <- 630222 # eggs per recruit
  hsat <- 0.1 # half-saturation constant
  
  # Age-specific life history
  nmort_yr_vec <- c(0.8, 0.8, rep(0.3, tmax_yr-2))
  fecundity_vec <- c(0, 1, 2, 2, 2, rep(1, tmax_yr-5)) * 1e6
  pmature_vec <- c(0, 0.2, rep(1, tmax_yr-2))
  pretain_vec_m <- c(0,0,0,rep(1, tmax_yr-3))
  pretain_vec_f <- rep(0, tmax_yr)
  q_m <- c(0, 0, rep(0.00035, tmax_yr-2))
  q_f <- c(0, 0, 0, rep(0.00035, tmax_yr-3))
  
  # Merge age-specific life history
  lh_at_age <- tibble(age=1:tmax_yr,
                      nmort_yr=nmort_yr_vec,
                      nmort_step=nmort_yr_vec/nsteps_per_year,
                      fecundity=fecundity_vec,
                      pmature=pmature_vec,
                      pretained_m=pretain_vec_m,
                      pretained_f=pretain_vec_f,
                      catchability_m=q_m,
                      catchability_f=q_f)
  
  # Derived values
  nages <- length(1:tmax_yr)
  nsexes <- 2
  
  # 1c. Fishing parameters
  ############################################
  
  # If fishing is turned on (i.e., effort_dynamics!="none")
  if(effort_dynamics!="none"){
  
    # Start date
    start_date <- ymd("2014-10-01")
    open_date <- ymd("2014-11-15")
    close_date <- ymd("2015-07-15")

    # Measure times between dates
    start_week <- 1
    wks_since_start <- difftime(open_date, start_date, units = "week") %>% as.numeric() %>% round()
    wks_since_open <- difftime(close_date, open_date, units = "week") %>% as.numeric() %>% round()
    wks_after_close <- 52 - (wks_since_start + wks_since_open)
    
    # Create vector marking the weeks in which fishing happens (fishing season)
    fishing_yn_vec_1yr <- c(rep(F, wks_since_start), rep(T, wks_since_open), rep(F, wks_after_close))
    length(fishing_yn_vec_1yr)
    fishing_yn_vec <- rep(fishing_yn_vec_1yr, nyears)
    
    # Fleet parameters
    ntraps_max <- 173900
    ntraps_max_obs <- 143550
    ntraps_max_fix <- 100000

  }

  
  # 1d. Initialize population
  ################################
  
  # 1. Setup population data template
  # 1st time step only, others get appended as calculated
  pop_df_temp <- expand_grid(step=1, select(blocks, block_id, pbiomass), sex=c("males", "females"), age_yr=1:tmax_yr, 
                             biomass_n=NA, traps_n=NA, catch_n=NA)
  
  # 2. Setup initial population
  if(missing(age_dist)){
    pop_df <- pop_df_temp %>% 
      mutate(biomass_n=ifelse(step==1, 0.5e4, NA))
  }else{
    age_dist <- rename(age_dist, biomass_n_init=biomass_n)
    pop_df <- pop_df_temp %>% 
      # Add provided initial age distribution
      left_join(age_dist, by=c("block_id", "sex", "age_yr")) %>% 
      # Fill initial biomass with provided age distribution
      mutate(biomass_n=biomass_n_init) %>% 
      select(-biomass_n_init)
    b0 <- age_dist %>% 
      filter(sex=="males" & age_yr>=4) %>% 
      pull(biomass_n_init) %>% sum()
  }
  
  # 2. Run population model
  ################################
  
  # Loop through time steps
  t <- 2; x <- 102
  for(t in 2:nsteps){
    
    # Calculate CA wide values
    #########################################
    
    # Number of legal-sized males in previous time step
    B_tot_prev <- pop_df %>% 
      filter(step==(t-1) & sex=="males" & age_yr>=4) %>% 
      pull(biomass_n) %>% sum()
    
    # Effort in previous time step
    E_tot_prev <- pop_df %>% 
      filter(step==(t-1)) %>%
      select(block_id, traps_n) %>% 
      unique() %>% pull(traps_n) %>% sum()
    
    # Loop through cells
    #########################################
    
    # Loop through cells: x <- block_ids[1]
    print(t)
    block_ids <- sort(unique(pop_df$block_id))
    pop_t_df <- map_df(block_ids, function(x){
      
      # Last time step's biomass
      N_male_a_t <- pop_df %>% filter(block_id==x & step==(t-1) & sex=="males") %>% pull(biomass_n)
      N_female_a_t <- pop_df %>% filter(block_id==x & step==(t-1) & sex=="females") %>% pull(biomass_n)
      
      # 2a. Calculate catch
      #########################################
      
      # No fishing
      if(effort_dynamics=="none"){
        Fmort_male_a <- rep(0, tmax_yr)
        C_male_a_t <- rep(0, tmax_yr)
        E <- 0
      }
      
      # Constant effort
      if(effort_dynamics=="constant"){
        # In season?
        fishing_yn <- fishing_yn_vec[t]
        if(!fishing_yn){
          Fmort_male_a <- rep(0, tmax_yr)
          C_male_a_t <- rep(0, tmax_yr)
          E <- 0
        }else{
          # CA-wide effort
          E_tot <- ntraps_max_obs
          # Block-level effort
          E_prop <- unique(pop_df$pbiomass[pop_df$block_id==x]) 
          E <- E_tot * E_prop
          u_male_s <- E * q_m
          Nmort_male_a <- lh_at_age$nmort_step
          Fmort_male_a <- u_male_s * lh_at_age$pretained_m
          C_male_a_t <- calc_catch(N=N_male_a_t, M=Nmort_male_a, F=Fmort_male_a)
        }
      }
      
      # Biomass-coupled effort
      if(effort_dynamics=="biomass-coupled"){
        # In season?
        fishing_yn <- fishing_yn_vec[t]
        if(!fishing_yn){
          E <- 0
          Fmort_male_a <- rep(0, tmax_yr)
          C_male_a_t <- rep(0, tmax_yr)
        }else{
          # CA-wide effort
          if(t==7){
            E_tot <- ntraps_max_fix
          }else{
            # E_tot <- calc_effort(E_max=ntraps_max_fix, E_prev=E_tot_prev, B_prev=B_tot_prev, b0=b0, a=0.4, x=0.4)
            E_tot <- calc_effort2(E_max=ntraps_max_fix, E_prev=E_tot_prev, B_prev=B_tot_prev, b0=b0, a=a, b=b)
          }
          # Block-level effort
          # Proportion of biomass in cell
          B_cell <- pop_df %>% 
            filter(block_id==x & step==(t-1) & sex=="males" & age_yr>=4) %>% 
            pull(biomass_n) %>% sum()
          B_prop <- B_cell / B_tot_prev
          E_prop <- B_prop
          E <- E_tot * E_prop
          u_male_s <- E * q_m
          Nmort_male_a <- lh_at_age$nmort_step
          Fmort_male_a <- u_male_s * lh_at_age$pretained_m
          C_male_a_t <- calc_catch(N=N_male_a_t, M=Nmort_male_a, F=Fmort_male_a)
        }
      }
      
      # 2b. Update biomass
      #########################################
      
      # Mortality: this time step's biomass
      N_male_a_t1 <- calc_abundance(N=N_male_a_t, M=lh_at_age$nmort_step, F=Fmort_male_a)
      N_female_a_t1 <- calc_abundance(N=N_female_a_t, M=lh_at_age$nmort_step, F=0)
      
      # Record this time step's biomass using initial as template
      out_df <- pop_df_temp %>% 
        filter(block_id==x) %>% 
        mutate(step=t,
               biomass_n=ifelse(sex=="males", N_male_a_t1, N_female_a_t1),
               traps_n=E,
               catch_n=ifelse(sex=="males", C_male_a_t, 0))
      return(out_df)
      
    })
    
    # 2b. If it's the time step of recruitment, calculate number of recruits
    if(step_length_yr==1){
      t_spawn <- 1
    }else{
      start_date <- ymd("2014-10-01")
      spawn_date <- ymd("2015-05-01")
      t_spawn <- difftime(spawn_date, start_date, units = "week") %>% as.numeric() %>% round()
    }
    spawn_steps <- t_spawn * 1:nyears
    if(t %in% spawn_steps){
      
      # Calculate population-wide reproduction and recruitment and
      # distribute recruits proportional to catch
      
      # Reproduction (sperm limited)
      # rho <- cumsum(n_m_t1[2:tmax_yr]) / n_f_t1[1:(tmax_yr-1)]
      # err_a_t <- c(rho / (hsat + rho), 0)
      # eggs_t <- sum(n_f_t1 * lh_at_age$fecundity * lh_at_age$pmature * err_a_t)
      
      # Reproduction (not sperm limited)
      N_female_a_t_global <- pop_t_df %>% 
        filter(sex=="females") %>% 
        group_by(age_yr) %>% 
        summarise(biomass_n=sum(biomass_n, na.rm=T)) %>% 
        pull(biomass_n)
      eggs_t_global <- calc_eggprod(nfemales=N_female_a_t_global, pmature=lh_at_age$pmature, fecundity=lh_at_age$fecundity)
      
      # Recruitment (age-1s)
      recruits_t_global <- calc_recruits(h=h, r0=r0, eggs_n=eggs_t_global, epr=epr)
      
    }
    
    # 2c. If its the last time step of the year, age and senesce fish
    endyr_steps <- nsteps_per_year * 1:nyears
    if(t %in% endyr_steps){
      
      # Add recruits (age-1s) to year t dataframe
      # Assume 50:50 sex ratio (that's why divided by zero)
      pop_t_df <- pop_t_df %>% 
        # Age each age class by one year
        group_by(block_id, sex) %>% 
        mutate(biomass_n1=c(NA, biomass_n[1:(tmax_yr-1)])) %>% 
        # Add recruits
        mutate(biomass_n1=ifelse(age_yr==1, pbiomass*recruits_t_global/2, biomass_n1)) %>% 
        # Remove n column and rename (I kept this in just to check that things were working)
        select(-biomass_n) %>% 
        rename(biomass_n=biomass_n1) %>% 
        # Ungroup
        ungroup()
      
      # Confirm that recruits were distributed correctly
      if(!all.equal(sum(pop_t_df$biomass_n[pop_t_df$age_yr==1]), recruits_t_global)){
        stop("Number of recruits distributed does not match number produced.")
      }
      
    } # closes last time step if statement
    
    # Merge data from each time point
    pop_df <- rbind(pop_df, pop_t_df)
    
  } # closes time loop
  
  # Check dimensions
  if(!nrow(pop_df) == ncells * nages * nsexes * nsteps){stop("Number of rows is wrong")}
  
  # 2.Format and export results
  ################################
  
  # Format results
  pop_df1 <- pop_df %>% 
    left_join(select(vonb, -cw_mm), by=c("sex", "age_yr")) %>% 
    mutate(biomass_mt=biomass_n*weight_g/1000/1000,
           catch_mt=catch_n*weight_g/1000/1000)
  
  # Return results
  return(pop_df1)
  
  
}