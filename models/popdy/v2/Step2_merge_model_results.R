
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
codedir <- "models/popdy/v2/functions"
indir <- "output/raw"
outdir <- "output/processed"
plotdir <- "models/figures"

# Read helper functions
sapply(list.files(codedir), function(x) source(file.path(codedir, x)))



# Merge iterations
################################################################################

# Scenarios
list.files(indir) %>% 
  substr(., 1, nchar(.)-7) %>% unique() %>% sort()

# Merge iterations
results1 <- merge_iterations(scenario="Scenario1_no_management", indir=indir, outdir=outdir)
results2 <- merge_iterations(scenario="Scenario2_Apr1_closure", indir=indir, outdir=outdir)
results3 <- merge_iterations(scenario="Scenario3_entanglement_trigger", indir=indir, outdir=outdir)
results4 <- merge_iterations(scenario="Scenario4_mlc_trigger", indir=indir, outdir=outdir)
results5 <- merge_iterations(scenario="Scenario5_mlc_entanglement_triggers", indir=indir, outdir=outdir)
results6 <- merge_iterations(scenario="Scenario6_dynamic_mlc_block_closures", indir=indir, outdir=outdir)
results7 <- merge_iterations(scenario="Scenario7_70%_effort_reduction", indir=indir, outdir=outdir)


# Merge results and calculate statistics
################################################################################

# Merge scenario results and calculate performance metrics
stats <- bind_rows(results1, results2, results3, results4, results5, results6, results7) %>% 
  # Calculate performance metrics by scenario, iteration, and season
  group_by(scenario, iteration, season) %>% 
  summarize(catch_mt=sum(catch_mt),
            nencounters=sum(encounters_n)/1e3,
            nentanglements=sum(entanglements_n),
            nblockweeks=sum(closure=="Season open")) %>% 
  ungroup() %>% 
  # Convert wide-to-long 
  gather(key="metric", value="value", 4:ncol(.)) %>% 
  # Format performance metric names
  mutate(metric=recode_factor(metric,
                              "catch_mt"="Catch (mt)",
                              "nblockweeks"="Number of block weeks open",
                              "nencounters"="Thousands of trap-whale encounters",
                              "nentanglements"="Number of entanglements")) %>% 
  # Format scenario name
  mutate(scenario=recode(scenario,
                         "Scenario1_no_management"="S1 No management",
                         "Scenario2_Apr1_closure"="S2 April 1 closure",
                         "Scenario3_entanglement_trigger"="S3 Entanglement triggered closures",
                         "Scenario4_mlc_trigger"="S4 MLC triggered closures",
                         "Scenario5_mlc_entanglement_triggers"="S5 MLC+entanglement closures",
                         "Scenario6_dynamic_mlc_block_closures"="S6 Dynamic MLC block closures",
                         "Scenario7_70%_effort_reduction"="S7 30% gear reduction"))


# Plot results
################################################################################

# Setup theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  legend.text=element_text(size=8),
                  legend.title=element_text(size=10),
                  plot.title=element_text(size=12),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Plot merged results
g <- ggplot(stats, aes(x=season, y=value, fill=scenario)) +
  geom_boxplot() +
  facet_wrap(~metric, ncol=1, scales = "free_y") +
  # Labels
  labs(x="Season", y="", main="Management scenario performance comparison") +
  # Legend
  scale_fill_discrete(name="Management scenario") +
  theme_bw() + my_theme
g



# Export figure
ggsave(g, filename=file.path(plotdir, "figure_outcome_comparison.png"), 
       width=8.5, height=6.5, units="in", dpi=600)








