
# Calculate effort
# Rosenberg et al. (2014) - Equation 13, page 17
# E_t1 = E_t * (Bt / (a*BMSY)) ^ x
# Cap effort at the maximum number of traps (limited entry fishery)
calc_effort <- function(E_max, E_prev, B_prev, b0, a, b){
  E_calc <- E_prev * (B_prev / (a * b0)) ^ b
  E_now <- pmin(E_max, E_calc)
  return(E_now)
}


# Vasconcellos and Cochrane - Equation  
# a = proportion of B0 when fishery is at bioeconomic equilibrium [0,1]
# x = intrinsic rate of increase in effort
calc_effort2 <- function(E_max, E_prev, B_prev, b0, a, b){
  E_calc <- E_prev * (1 + b * (B_prev/(b0*a) - 1))
  E_now <- pmin(E_max, E_calc)
  return(E_now)
}