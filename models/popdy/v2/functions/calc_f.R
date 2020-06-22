
# Calculate fishing mortality
calc_f <- function(E, q){
  Fmort <- E * q
  return(Fmort)
}