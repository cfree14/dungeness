
# Calculate number of encounters
calc_n_encounters <- function(n_traps, whales_sqkm){
  
  # Calculate the probability that a trap encounters a whale
  # Calculate the number of encounters (deterministic, not random)
  p_trap_encounters_whale <- calc_p_encounter(p=whales_sqkm)
  N_encounters <- n_traps * p_trap_encounters_whale
  return(N_encounters)
  
}