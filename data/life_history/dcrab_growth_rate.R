
# Derives length- and weight-at-age relationships for Dungeness crab
################################################################################

# Length-at-age
#####################################
# Wainwright, T. C., & Armstrong, D. A. (1993). Growth patterns in the Dungeness crab (Cancer magister Dana): 
# synthesis of data and comparison of models. Journal of Crustacean Biology, 13(1), 36-50.

# Average size at age projections based on:
# 1) Bent-line model for MI (molt increment) - Somerton (1980)
# 2) Degree-day model for IP (intermolt period) 

# 1) Bent-line model for MI (molt increment)
# MI (mm) = a + b * CW - c * CW' 
# CW = carapace width (mm), CW'  = max(0, CW - CW*), CW* is inflection point

# Male (Butler 1961, "study d"): a=1.20, b=0.248, c=0.230, X*=105
# Female (Butler 1961, "study d"): a=1.26, b=0.240, c=0.352, X*=83
# Average (Wainwright & Armstrong 1993 judgement): a=1.3, b=0.23, c=0.22, and X*=105

# 2) Degree-day model for IP (intermolt period) - Curry and Feldman (1987)
# log(IP * T') = a + b*CW so...
# IP (days) = exp(a + b*CW) / T'
#      {0,          T < Tmin
# T' = {T-Tmin,     Tmin < T < Tmax
#      {Tmax-Tmin,  Tmax < T
# Average (Wainwright & Armstrong 1993 judgement): Tmin=1.13, Tmax=13.44, a=5.25, b=0.0175


# Setup
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
datadir <- "data/life_history/data"
plotdir <- "data/life_history/figures"

# Setup theme
my_theme <- theme(axis.text=element_text(size=6),
                  axis.title=element_text(size=8),
                  plot.title=element_text(size=9),
                  legend.text=element_text(size=6),
                  legend.title=element_blank(),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))


# Derive molt increments and intermolt periods
################################################################################

# Calculate molt increment
# a=1.3; b=0.23; c=0.22; xstar=105; width_mm=0
calc_molt_increment <- function(a=1.3, b=0.23, c=0.22, xstar=105, width_mm){
  width_mm_hat <- max(0, width_mm - xstar)
  molt_inc_mm <- a + b*width_mm - c*width_mm_hat
  return(molt_inc_mm)
}

# Check and plot
# Male (Butler 1961, "study d"): a=1.20, b=0.248, c=0.230, X*=105
# Female (Butler 1961, "study d"): a=1.26, b=0.240, c=0.352, X*=83
# Average (Wainwright & Armstrong 1993 judgement): a=1.3, b=0.23, c=0.22, and X*=105
cw <- seq(0, 250, 1)
mi_m <- sapply(cw, function(x) calc_molt_increment(a=1.20, b=0.248, c=0.230, xstar=105, width_mm=x))
mi_f <- sapply(cw, function(x) calc_molt_increment(a=1.26, b=0.240, c=0.352, xstar=83, width_mm=x))
mi_avg <- sapply(cw, function(x) calc_molt_increment(a=1.3, b=0.23, c=0.22, xstar=105, width_mm=x))
plot(mi_m ~ cw, xlab="Carapace width (mm)", ylab="Molt increment (mm)")

# Build merged data
mi_data <- tibble(cw_mm=cw, Male=mi_m, Female=mi_f) %>% 
  gather(key="sex", value="mi_mm", 2:3) 

# Plot merged data
g1 <- ggplot(mi_data, aes(x=cw_mm, y=mi_mm, color=sex)) +
  geom_line() +
  labs(x="Carapace width (mm)", y="Molt increment (mm)", title="Molt increment") +
  scale_color_discrete(name="Sex") +
  theme_bw() + my_theme + theme(legend.position=c(0.8,0.6),
                                legend.background = element_rect(fill=alpha('blue', 0)))
g1

# Calculate molt intermolt period
# tmin_c=1.13; tmax_c=13.44; a=5.25; b=0.0175; width_mm=80; temp_c=15
calc_im_period <- function(tmin_c=1.13, tmax_c=13.44, a=5.25, b=0.0175, width_mm, temp_c){
  t_prime <- ifelse(temp_c < tmin_c, 0, 
                    ifelse(temp_c >= tmin_c & temp_c <= tmax_c, temp_c-tmin_c, tmax_c - tmin_c))
  ip_days <- exp(a+b*width_mm)/t_prime
  return(ip_days)
}

# Check and plot
ip_15c <- sapply(cw, function(x) calc_im_period(width_mm=x, temp_c=15))
plot(ip_15c ~ cw, xlab="Carapace width (mm)", ylab="Intermolt period (days)")

# Build merged data
ip_data <- tibble(cw_mm=cw, ip_days=ip_15c)

# Plot merged data
g2 <- ggplot(ip_data, aes(x=cw_mm, y=ip_days)) +
  geom_line() +
  labs(x="Carapace width (mm)", y="Intermolt period (days)", title="Intermolt period") +
  theme_bw() + my_theme
g2


# Length-weight regression
################################################################################

# Parameters (Zhang et al. 2004)
a <- 0.000102
b <- 3.10
wt_g <- a*cw^b

# Build merged data
lw_data <- tibble(cw_mm=cw, weight_g=wt_g)

# Plot merged data
g3 <- ggplot(lw_data, aes(x=cw_mm, y=wt_g)) +
  geom_line() +
  labs(x="Carapace width (mm)", y="Weight (g)", title="Length-weight relationship") +
  theme_bw() + my_theme
g3



# Simulate growth
################################################################################

# Number of molts to simulate
nmolts <- 20

# Set initial size as post larval size
# From OPC doc: "Larval Dungeness Crab settle on the substrate 
# after 4 months and having reached an approximate size of 5 mm (Rasmuson 2013)."
cw_mm_init <- 5
cw_mm_vec_m <- cw_mm_vec_f <- seq(0, 5, length.out=4*30)

# Loop through molts
for(i in 1:nmolts){
  
  # Male crabs
  cw_mm_now_m <- cw_mm_vec_m[length(cw_mm_vec_m)]
  inc_mm_m <- calc_molt_increment(a=1.20, b=0.248, c=0.230, xstar=105, width_mm=cw_mm_now_m)
  cw_mm_new_m <- cw_mm_now_m + inc_mm_m
  ip_days_m <- calc_im_period(width_mm=cw_mm_new_m, temp_c = 12) %>% round()
  cw_mm_new_period_m <- rep(cw_mm_new_m, ip_days_m)
  cw_mm_vec_m <- c(cw_mm_vec_m, cw_mm_new_period_m)
  
  # Female crabs
  cw_mm_now_f <- cw_mm_vec_f[length(cw_mm_vec_f)]
  inc_mm_f <- calc_molt_increment(a=1.26, b=0.240, c=0.352, xstar=83, width_mm=cw_mm_now_f)
  cw_mm_new_f <- cw_mm_now_f + inc_mm_f
  ip_days_f <- calc_im_period(width_mm=cw_mm_new_f, temp_c = 12) %>% round()
  cw_mm_new_period_f <- rep(cw_mm_new_f, ip_days_f)
  cw_mm_vec_f <- c(cw_mm_vec_f, cw_mm_new_period_f)
  
}


# Merge data
vonb_df <- rbind(tibble(sex="Male", cw_mm=cw_mm_vec_m),
                 tibble(sex="Female", cw_mm=cw_mm_vec_f)) %>% 
  group_by(sex) %>% 
  mutate(day=1:n(),
         age_yr=day/365,
         age_yr_int=ceiling(age_yr),
         weight_g=0.000102*cw_mm^3.10,
         weight_kg=weight_g/1000) %>% 
  filter(age_yr_int<=10) %>% 
  ungroup()

# Calculate stats
vonb_stats <- vonb_df %>% 
  group_by(sex, age_yr_int) %>% 
  summarize(cw_mm=median(cw_mm),
            weight_g=median(weight_g))

# Export stats
write.csv(vonb_stats, file=file.path(datadir, "dcrab_length_weight_at_age.csv"), row.names=F)
  

# Plot length-at-age
g4 <- ggplot(vonb_df, aes(x=age_yr, y=cw_mm, color=sex)) +
  geom_line() +
  geom_point(data=vonb_stats, mapping=aes(x=age_yr_int-0.5, y=cw_mm, fill=sex), 
             shape=21, color="black", show.legend=FALSE) +
  labs(x="Age (yr)", y="Carapace width (mm)",title="Length-at-age") +
  scale_color_discrete(name="Sex") +
  scale_x_continuous(breaks=0:10) +
  geom_hline(yintercept=159, linetype="dotted", lwd=0.3) +
  annotate("text", x=10, y=165, label="Legal size (6.25 in)", hjust=1, vjust=0, size=2) +
  theme_bw() + my_theme + theme(legend.position=c(0.8,0.2),
                                legend.background = element_rect(fill=alpha('blue', 0)))
g4

# Plot weight-at-age
g5 <- ggplot(vonb_df, aes(x=age_yr, y=weight_g, color=sex)) +
  geom_line() +
  geom_point(data=vonb_stats, mapping=aes(x=age_yr_int-0.5, y=weight_g, fill=sex), 
             shape=21, color="black", show.legend=FALSE) +
  labs(x="Age (yr)", y="Body weight (g)", title="Weight-at-age") +
  scale_color_discrete(name="Sex") +
  scale_x_continuous(breaks=0:10) +
  theme_bw() + my_theme + theme(legend.position=c(0.8,0.2),
                                legend.background = element_rect(fill=alpha('blue', 0)))
g5


# Build and export figure
################################################################################

# Packages
library(grid)
library(gridExtra)

# Arrange and save
g <- grid.arrange(g1, g2, g3, g4, g5, 
                  layout_matrix = rbind(c(1,1,2,2,3,3),
                                        c(4,4,4,5,5,5)))

g

# Export figure
ggsave(g, filename=file.path(plotdir, "figure_dcrab_growth_by_sex.png"), 
       width=6.5, height=5, units="in", dpi=600)



