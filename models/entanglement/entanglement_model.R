
# Trap = static predator
# Whale = mobile prey

# Parameters
D <- 0.01 # risky passing distance (0.01 km = 10 m; citation)
v <- 3 # mean whale speed (5 km/hr; citation)
t <- 168 # duration of the time step (168 hrs or 1 week)

# Function to calculate p(encounter)
calc_p_encounter <- function(p){
  prob <- 1 - exp(-8 * p * D * v * t / pi)
}

# p(encounter) at various whale densities
prey_dens_sqkm <- seq(0, 0.5, 0.01) # whales
p_encounters <- calc_p_encounter(prey_dens_sqkm)

# Plot p(encounter) at various whale densities
plot(p_encounters ~ prey_dens_sqkm, type="l", bty="n", las=1, xlab="Whale density (animals / km2)", ylab="p(encounter)")
