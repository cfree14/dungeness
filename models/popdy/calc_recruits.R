
# Calculate number of recruits
calc_recruits <- function(h, r0, eggs_n, epr){
  r = (4 * h * r0 * eggs_n) / (epr * r0 * (1-h) + (5*h+1) * eggs_n)
}
