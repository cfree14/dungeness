
# Calculate abundance
calc_abundance <- function(N, M, F){
  N_next <- N * exp(-(M + F))
}
