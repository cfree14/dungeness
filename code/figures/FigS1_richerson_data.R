

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/richerson_etal_2020"
plotdir <- "figures"

# Read data
data_orig <- read.csv(file=file.path(datadir, "richerson_etal_2020_ca_preseason_dcrab_abundance.csv"), as.is=T)


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  select(-region) %>% 
  group_by(year) %>% 
  summarize_all(sum)

# Setup theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  plot.title=element_text(size=12),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Post 2013 stats
b_vals <- data %>% filter(year>=2013) %>% pull(crabs_mt)

# Fit normal distribution
b_avg <- b_vals %>% mean() / 1000
b_sd <- b_vals / 1000

# Fit log-normal distribution
lnormfit <- MASS::fitdistr(b_vals, densfun = "log-normal")
b_lnorm_avg <- exp(lnormfit$estimate["meanlog"]) / 1000

# Check log-normal fit
x <- seq(1,16000,1)
y <- dlnorm(x, meanlog = lnormfit$estimate["meanlog"], sdlog=lnormfit$estimate["sdlog"])
plot(y ~ x)

# Plot time series
g1 <- ggplot(data, aes(x=year, y=crabs_mt/1000)) +
  # Plot confidence interval
  geom_ribbon(mapping=aes(ymin=crabs_mt_lo/1000, ymax=crabs_mt_hi/1000), fill="grey80") +
  # Plot median
  geom_line() +
  geom_point() +
  # Plot reference sline
  geom_vline(xintercept=2013, linetype="solid") +
  geom_hline(yintercept=b_lnorm_avg, linetype="dotted") +
  # Labels
  labs(x="", y="Pre-season legal-sized\nmale abundance (1000s mt)") +
  ylim(c(0,17.5)) +
  # Legend
  theme_bw() + my_theme
g1


# Plot distribution
g2 <- ggplot() +
  # Plot distribution
  geom_area(mapping=aes(x=x/1000, y=y), fill="grey80") +
  # Flip coordinates
  coord_flip() +
  # Ref lines
  geom_vline(xintercept=b_lnorm_avg, linetype="dotted") +
  # Labels
  labs(x="", y="") +
  xlim(c(0,17.5)) +
  # Legend
  theme_bw() + my_theme +
  theme(axis.text.x=element_text(color="white"))
g2

# Merge
g <- gridExtra::grid.arrange(g1,g2, ncol=2, widths=c(0.8,0.2))
g  


# Export figure
ggsave(g, filename=file.path(plotdir, "FigS1_richerson_data.png"), 
       width=6.5, height=2.5, units="in", dpi=600)


