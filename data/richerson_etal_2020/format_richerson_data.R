
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/richerson_etal_2020"

# Read data
data_orig <- read.csv(file.path(datadir, "crab_model_results_ca_2020408.csv"), as.is=T)


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  rename(region=area, year=season) %>% 
  mutate(region=recode(region, "North CA"="Northern CA"), 
         crabs_mt=mean_est_thousands_mt*1000,
         crabs_mt_lo=est_2.5.*1000,
         crabs_mt_hi=est_97.5.*1000) %>% 
  select(region, year, crabs_mt, crabs_mt_lo, crabs_mt_hi)

# Summarize data
data_sum <- data %>% 
  group_by(year) %>% 
  summarize_at(.vars=c("crabs_mt", "crabs_mt_lo", "crabs_mt_hi"), .funs = sum)

# Export data
write.csv(data, file=file.path(datadir, "richerson_etal_2020_ca_preseason_dcrab_abundance.csv"), row.names = F)

# Plot data
################################################################################

# Plot abundace by region
g <- ggplot(data, aes(x=year, y=crabs_mt/1000)) +
  facet_grid(~region) +
  geom_ribbon(mapping=aes(ymin=crabs_mt_lo/1000, ymax=crabs_mt_hi/1000), 
              fill="grey80") +
  geom_line() +
  labs(x="", y="Pre-season legal-sized\nmale abundance (1000s mt)") +
  theme_bw() + 
  theme(legend.title=element_blank())
g

# Plot total abundance
crabs_mt_avg <- mean(data_sum$crabs_mt[data_sum$year>=2014])
g <- ggplot(data_sum, aes(x=year, y=crabs_mt/1000)) +
  geom_ribbon(mapping=aes(ymin=crabs_mt_lo/1000, ymax=crabs_mt_hi/1000), 
              fill="grey80") +
  geom_line() +
  geom_hline(yintercept = crabs_mt_avg/1000, lwd=1, color="red", linetype="dashed") +
  labs(x="", y="Pre-season legal-sized\nmale abundance (1000s mt)") +
  theme_bw() + 
  theme(legend.title=element_blank())
g


# Plot abundance by region in one plot
g <- ggplot(data, aes(x=year, y=crabs_mt/1000, fill=region)) +
  geom_area() +
  labs(x="", y="Pre-season legal-sized\nmale abundance (1000s mt)") +
  theme_bw() + 
  theme(legend.title=element_blank())
g

# Plot proportion of population by region
g <- ggplot(data, aes(x=year, y=crabs_mt, fill=region)) +
  geom_area(position="fill") +
  labs(x="", y="Pre-season legal-sized\nmale abundance (mt)") +
  theme_bw() + 
  theme(legend.title=element_blank())
g





