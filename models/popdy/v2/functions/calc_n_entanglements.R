
# Calculate number of entanlements
# Randomly determine based on number of encounters and the
# probability that an encounter leads to an entanglement
calc_n_entanglements <- function(n_encounters, p_entanglement){
  
  n_entanglements <- sum(runif(n=n_encounters) < p_entanglement)
  return(n_entanglements)
  
}