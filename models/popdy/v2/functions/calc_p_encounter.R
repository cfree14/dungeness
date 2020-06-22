
# Calculate the probability that a trap encounters a whale
calc_p_encounter <- function(D=0.0135, v=3.37, t=168, p){
  p_encounter=1-exp(-2*D*v*t*p)
}