
# Packages
library(ggplot2)
library(tidyverse)

# Parameters
r_km <- 0.0135 # risky passing distance
v_kmh <- 3.37 # mean whale speed
t_hr <- 168 # duration of the time step (168 hrs or 1 week)
A_sqkm <- 100 # 10 km x 10 km

# Approach 1: Me
###################################################

# Function
calc_p_encounter1 <- function(v, t, r, A, d){
  p_area <- (v * t * r * 2) / A
  prob <- 1 - (1 - p_area) ^ (d * A)
  return(prob)
}

# Fit and plot model
whales_sqkm <- seq(0, 0.04, 0.0005) # whales
p_encounters1 <-  calc_p_encounter1(r=r_km, v=v_kmh, t=t_hr, A=100, d=whales_sqkm)
plot(p_encounters1 ~ whales_sqkm, type="l", bty="n", las=1, 
     xlab="Whale density (animals / km2)", ylab="p(encounter)", main="Approach #1")


# Approach 2: Rowcliffe et al. 2003 JAE
###################################################

# Function
calc_p_encounter2 <- function(D, c, v, N){
  prob <- 1 - exp(-2 * D * c * v * N)
  return(prob)
}

# Fit and plot model
whales_sqkm <- seq(0, 0.04, 0.0005) # whales
p_encounters2 <-  calc_p_encounter2(D=r_km, v=v_kmh, c=t_hr, N=whales_sqkm)
plot(p_encounters2 ~ whales_sqkm, type="l", bty="n", las=1, 
     xlab="Whale density (animals / km2)", ylab="p(encounter)", main="Approach #2")


# Approach 3: Hutchinson & Waser 2007
###################################################

# Function
calc_p_encounter3 <- function(p, v, D, t){
  prob <- 1 - exp(-8 * p * v * D * t / pi)
  return(prob)
}

# Fit and plot model
whales_sqkm <- seq(0, 0.04, 0.0005) # whales
p_encounters3 <-  calc_p_encounter3(D=r_km, v=v_kmh, t=t_hr, p=whales_sqkm)
plot(p_encounters3 ~ whales_sqkm, type="l", bty="n", las=1, 
     xlab="Whale density (animals / km2)", ylab="p(encounter)", main="Approach #3")


# Final figure
###################################################

# Directories
plotdir <- "models/entanglement/figures"

# Build data
data <- tibble(dens=whales_sqkm, p_encounter=p_encounters2)

# Setup theme
my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=9),
                  plot.title=element_text(size=11),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))


# Plot data
g <- ggplot(data, aes(x=dens, y=p_encounter)) +
  geom_line() +
  labs(x="Whale density (whales per sqkm)", y="p(a trap encounters a whale)") +
  theme_bw() + my_theme
g  

# Export figure
ggsave(g, filename=file.path(plotdir, "FigSX_encounter_probability.png"), width=4, height=3, units="in", dpi=600)

  
  
