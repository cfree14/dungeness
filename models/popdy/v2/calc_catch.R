
# Calculate catch
calc_catch <- function(N, M, F){
  C <- N * F/(M+F) * (1-exp(-(M+F)))
}
