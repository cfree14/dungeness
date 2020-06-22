
# Calculate number of contaminated crabs in a sample
calc_ncontam_in_sample <- function(n_sampled=6, pcontam_vec){
  ncontam <- sapply(pcontam_vec, function(x) sum(runif(n=n_sampled, min=0, max=1) <= x))
  return(ncontam)
}
