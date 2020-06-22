

# Calculate effort based on scenario and whether fishing is open
calc_effort <- function(effort_dynamics, fishing_yn, 
                        E_total_prev, B_total_prev, B_total_t, B_block_t, 
                        b0_mt, a, b, ntraps_max){
  

  # If fishing is turned off or not open, set effort to 0
  if(effort_dynamics=="none" | fishing_yn==F){
    E <- 0
  }

  # If fishing is turned on and open (not out of season or closed), set effort based on scenario
  if(effort_dynamics!="none" & fishing_yn==T){
    
    # Step 1. CA-wide effort
    ####################################
    
    # If "constant" effort dynamics
    if(effort_dynamics=="constant"){
      E_tot <- ntraps_max
    }
    
    # If "biomass-coupled" effort dynamics
    if(effort_dynamics=="biomass-coupled"){
      # If zero effort before, jump to max traps
      if(E_total_prev==0){
        E_tot <- ntraps_max
      }else{
        E_tot <- calc_bcoupled_effort2(E_max=ntraps_max, 
                                       E_prev=E_total_prev, 
                                       B_prev=B_total_prev, 
                                       b0=b0_mt, a=a, b=b)
      }
    }
    
    # Step 2. Block-level effort
    ####################################
    
    # Block-level effort (# of traps)
    B_prop <- B_block_t / B_total_t
    E_prop <- B_prop
    E <- E_tot * E_prop
    
  }
    
  # Return
  return(E)
  
}


# Calculate effort
# Rosenberg et al. (2014) - Equation 13, page 17
# E_t1 = E_t * (Bt / (a*BMSY)) ^ x
# Cap effort at the maximum number of traps (limited entry fishery)
calc_bcoupled_effort1 <- function(E_max, E_prev, B_prev, b0, a, b){
  E_calc <- E_prev * (B_prev / (a * b0)) ^ b
  E_now <- pmin(E_max, E_calc)
  return(E_now)
}


# Vasconcellos and Cochrane - Equation 4ish, page 5
# a = proportion of B0 when fishery is at bioeconomic equilibrium [0,1]
# b = intrinsic rate of increase in effort
calc_bcoupled_effort2 <- function(E_max, E_prev, B_prev, b0, a, b){
  E_calc <- E_prev * (1 + b * (B_prev/(b0*a) - 1))
  E_now <- pmin(E_max, E_calc)
  return(E_now)
}




