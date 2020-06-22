
# For testing
if(F){
  yrs2sim <- 2015
  effort_dynamics <- "constant" # none or constant
  management <- "effort reduction"
  mgmt_options <- list(E_red_prop=0.5)
  a <- 0.15
  b <- 0.1
}

# Function to run model
run_model <- function(yrs2sim, effort_dynamics, a, b, management="none", mgmt_options=NULL){
  
  # Input checks
  ############################################
  
  # Input options
  mgmt_secenarios <- c("none", "April 1 closure", "entanglement trigger", "marine life concentration trigger", "dynamic MLC block closures", "effort reduction")
  effort_secenarios <- c("none", "constant", "biomass-coupled")
  
  # Check management input
  if(sum(!management%in%mgmt_secenarios)!=0){stop("Management scenario is not in list of options.")}
  if(!effort_dynamics%in%effort_secenarios){stop("Effort dynamics scenario is not in list of options.")}
  
  # Parameters
  ############################################
  
  # Model parameters
  ncells <- nrow(blocks)
  nyears <- length(yrs2sim)
  
  # Other parameters
  q <- 0.00035 # catchability (per trap week) - Froehlich et al. 2017 / Toft et al. 2013
  Nmort <- 0.6925 # natural mortality (yr-1) - Richerson et al. 2020
  Nmort_step <- Nmort / 52 # natural morality (wk-1) - Hordyk et al. 2015
  p_entanglement <- 1e-5 # 1e-5
  
  # On converting mortality time-scales:
  # Hordyk et al. (2015) Some explorations of the life history ratios to 
  # describe length composition, spawning-per-recruit, and the spawning potential ratio.
  # "Converting between time-scales is straightforward: M = 4.6 year−1 can be 
  # converted to a monthly rate M = 4.6/12 = 0.38 month−1`, where tmax = 12 months, 
  # or a weekly rate M = 4.6/52 = 0.088 week−1`, where tmax = 52 week"
  
  # Fleet parameters
  ntraps_max <- 130000
  if("effort reduction" %in% management){
    ntraps_max <- ntraps_max * mgmt_options$E_red_prop
  }
  
  # Loop through years
  ############################################
  
  # Loop through years
  # yr <- 2015; t <- 1; x <- 103
  for(yr in yrs2sim){
    
    # Time step key
    step_key <- time_step_key %>% 
      filter(year %in% yr)
    
    # Initial biomass
    b0_mt <- B0_stats$B0_mt_avg
    b0_mt <- rnorm(n=1, mean=B0_stats$B0_mt_avg, sd=B0_stats$B0_mt_sd)
    
    # Build data container
    # Biomass = biomass at beginning of week
    # Effort = effort implemented over the week
    # Catch = catch derived from the effort applied to the week's biomass
    pop_df <- init_pop(b0_mt, step_key, blocks, nwhales)
    
    # April 1st closure
    if("April 1 closure" %in% management){
      apr1date <- ymd(paste(yr+1, 4, 1, sep="-"))
      pop_df <- pop_df %>% 
        mutate(closure=ifelse(date >= apr1date & closure=="Season open", "April 1 closure", closure),
               fishing_yn=ifelse(date >= apr1date, F, fishing_yn))
    }
    
    # Dynamic block closures
    if("dynamic MLC block closures" %in% management){
      whales_sqkm_thresh <- 0.03
      pop_df <- pop_df %>% 
        mutate(closure=ifelse(whales_sqkm >= whales_sqkm_thresh & closure=="Season open", "Marine life concentration closure", closure),
               fishing_yn=ifelse(whales_sqkm >= whales_sqkm_thresh, F, fishing_yn))
    }
  
  
    # Run model
    ############################################
    
    # Loop through time steps
    for(t in 1:52){
      
      # Calculate CA wide values
      #########################################
      
      # If first time step
      if(t==1){
        E_total_prev <- 0
        B_total_prev <- b0_mt
        B_total_avail_prev <- 0
        B_total_avail_t <- 0
      }else{
      
        # Total number of traps (effort) in previous time step
        E_total_prev <- pop_df %>% 
          filter(week==(t-1)) %>%
          select(block_id, traps_n) %>% 
          unique() %>% pull(traps_n) %>% sum()
        
        # Total number of legal-sized males
        B_total_prev <- pop_df %>% 
          filter(week==(t-1)) %>% 
          pull(biomass_mt) %>% sum()
        
        # Total number of legal-sized males in areas open to fishing in previous time step
        B_total_avail_prev <- pop_df %>% 
          filter(week==(t-1) & fishing_yn==T) %>% 
          pull(biomass_mt) %>% sum()
        
        # Total number of legal-sized males in areas open to fishing now
        B_total_avail_t <- pop_df %>%
          filter(week==(t) & fishing_yn==T) %>%
          pull(biomass_mt) %>% sum()
        
      }
      
      # MLC surveys/management
      #########################################
      
      if("marine life concentration trigger" %in% management){
        
        # Nov 1 - Dec 31 surveys (Weeks 6-14)
        if(t %in% 6:14){
          
          # Perform survey
          survey <- nwhales_ramp_wk %>% 
            filter(week==t & season_yr==yr)
          
          # Which zones to close based on survey?
          zones_to_close <- survey %>% 
            filter(nwhales >= 20) %>% 
            pull(ramp_zone)
          
          # Close zones
          pop_df$fishing_yn[pop_df$week > t & pop_df$week <=14 & pop_df$block_ramp %in% zones_to_close & pop_df$closure=="Season open"] <- F
          pop_df$closure[pop_df$week > t & pop_df$week <=14 & pop_df$block_ramp %in% zones_to_close & pop_df$closure=="Season open"] <- "Marine life concentration closure"
          
        }
        
        # Mar 1 - July 31 surveys (23-42)
        if(t %in% 23:42){
          
          # Perform survey
          survey <- nwhales_ramp_wk %>% 
            filter(week==t & season_yr==yr)
          
          # Which zones to close based on survey?
          zones_to_close <- survey %>% 
            filter(nwhales >= 10) %>% 
            pull(ramp_zone)
          
          # Close zones
          pop_df$fishing_yn[pop_df$week > t & pop_df$week <=42 & pop_df$block_ramp %in% zones_to_close & pop_df$closure=="Season open"] <- F
          pop_df$closure[pop_df$week > t & pop_df$week <=42 & pop_df$block_ramp %in% zones_to_close & pop_df$closure=="Season open"] <- "Marine life concentration closure"
          
        }
        
      }
      
      
      # Loop through blocks
      #########################################
      
      # Loop through blocks: x <- block_ids[1]
      print(paste0(yr, ": Week ", t))
      block_ids <- sort(unique(pop_df$block_id))
      for(x in block_ids){
        
        # Extract data
        ######################
        
        # Block info
        ramp_zone <- pop_df %>% filter(block_id==x) %>% pull(block_ramp) %>% unique()
        
        # Get this time step's biomass
        B_block_t <- pop_df %>% filter(block_id==x & week==(t)) %>% pull(biomass_mt)
        
        # Get this time step's whale density, p(contamination), and fishing status
        pcomtam_block_t <- pop_df %>% filter(block_id==x & week==(t)) %>% pull(pcontam)
        whales_sqkm_block_t <- pop_df %>% filter(block_id==x & week==(t)) %>% pull(whales_sqkm)
        fishing_yn_block_t <- pop_df %>% filter(block_id==x & week==(t)) %>% pull(fishing_yn)
        
        
        # Run functions
        ######################
        
        # Calculate effort (number of traps)
        E_block_t <- calc_effort(effort_dynamics = effort_dynamics,
                                 fishing_yn = fishing_yn_block_t, 
                                 E_total_prev = E_total_prev, 
                                 B_total_prev = B_total_avail_prev,
                                 B_total_t = B_total_avail_t,
                                 B_block_t = B_block_t,
                                 b0_mt = b0_mt,
                                 a = a, 
                                 b = b, 
                                 ntraps_max=ntraps_max)
        
        # Calculate fishing mortality
        F_block_t <- calc_f(E=E_block_t, q=q)
        
        # Calculate catch
        C_block_t <- calc_catch(N=B_block_t, M=Nmort_step, F=F_block_t)
        
        # Calculate contaminated catch
        C_contam_block_t <- calc_catch_contam(C=C_block_t, pcontam=pcomtam_block_t)
        
        # Calculate number of encounters
        N_encounters_block_t <- calc_n_encounters(n_traps=E_block_t, 
                                                  whales_sqkm=whales_sqkm_block_t)
        
        # Calculate number of entanglements
        N_entanglements_block_t <- calc_n_entanglements(n_encounters=N_encounters_block_t, 
                                                        p_entanglement=p_entanglement)
        
        # Calculate nest time step's biomass
        B_block_next <- calc_abundance(N=B_block_t, M=Nmort_step, F=F_block_t)
      
        # Record results
        pop_df$biomass_mt[pop_df$week==(t+1) & pop_df$block_id==x] <- B_block_next
        pop_df$traps_n[pop_df$week==t & pop_df$block_id==x] <- E_block_t
        pop_df$catch_mt[pop_df$week==t & pop_df$block_id==x] <- C_block_t
        pop_df$catch_mt_contam[pop_df$week==t & pop_df$block_id==x] <- C_contam_block_t
        pop_df$encounters_n[pop_df$week==t & pop_df$block_id==x] <- N_encounters_block_t
        pop_df$entanglements_n[pop_df$week==t & pop_df$block_id==x] <- N_entanglements_block_t
        
        # Management actions
        ######################
        
        # Entanglement closure
        if("entanglement trigger" %in% management){
          
          # If there is an entanglement, close zone for remainder of season
          if(N_entanglements_block_t>0){
            # Shut down fishing for remainder of season in this zone
            pop_df$closure[pop_df$week > t & pop_df$block_ramp==ramp_zone & pop_df$closure=="Season open"] <- "Entanglement closure"
            pop_df$fishing_yn[pop_df$week > t & pop_df$block_ramp==ramp_zone] <- F
            print(paste0(ramp_zone, " closed in week ", t, " due to entanglement in block ", x))
          }
          
        }
        
      } # closes cell loop
    
    } # closes time loop
    
  # Merge results
  if(yr==yrs2sim[1]){pop_df_merge <- pop_df}else{pop_df_merge <- bind_rows(pop_df_merge, pop_df)}
    
  } # closes year loop
  
  # Export
  ############################################
  
  # Parameters
  params <- tibble(management = paste(management, collapse=" + "), 
                   effort_dynamics=effort_dynamics,
                   a = ifelse(effort_dynamics=="biomass-coupled", a, NA),
                   b = ifelse(effort_dynamics=="biomass-coupled", b, NA),
                   b0_mt = b0_mt,
                   q = q,
                   m_yr = Nmort,
                   m_wk = Nmort_step,
                   p_entanglement = p_entanglement)
  
  # Build output
  output <- list(results=pop_df_merge, parameters=params)
  
  # Return results
  return(output)
  
}




