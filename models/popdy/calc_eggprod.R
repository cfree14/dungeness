
# Calculate number of eggs
calc_eggprod <- function(nfemales, pmature, fecundity){
  eggs_n <- sum(nfemales * pmature * fecundity)
  return(eggs_n)
}
