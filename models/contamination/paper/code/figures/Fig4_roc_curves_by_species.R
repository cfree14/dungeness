
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "models/contamination/paper/output"
plotdir <- "models/contamination/paper/figures"

# Build data
################################################################################

# Species do
spp_do <- c("Dungeness crab", "Rock crab", "Spiny lobster", "Razor clam", "Sea mussel", "Bay mussel")

# Build ROC curve data
######################################

# Loop through species
x <- spp_do[1]
data <- purrr::map_df(spp_do, function(x) {
  
  # Read data
  infile <- paste0(tolower(gsub(" ", "_", x)), "_evaluation.Rds")
  sdata <- readRDS(file.path(datadir, infile))
  
  # ROC curve
  roc_curve <- sdata$roc_curves %>% 
    mutate(species=x) %>% 
    dplyr::select(species, everything())
  
})

# Format data
data1 <- data %>% 
  mutate(species=factor(species, levels=spp_do)) %>% 
  mutate(model_type=gsub(" \\(cDA)| \\(pDA)", "", model),
         pred_type=ifelse(grepl("cDA", model), "cDA", "pDA"))


# Build test data sample size/performance data
######################################

# Sample size stats
x <- spp_do[1]
stats <- purrr::map_df(spp_do, function(x) {
  
  # Read data
  infile <- paste0(tolower(gsub(" ", "_", x)), "_evaluation.Rds")
  sdata <- readRDS(file.path(datadir, infile))
  
  # Sample size
  n_test <- sdata$preds %>% 
    mutate(correct=true==estimate) %>% 
    group_by(model, true) %>% 
    summarize(n=n(),
              ncorrect=sum(correct),
              pcorrect=ncorrect/n) %>% 
    mutate(true=recode(true, 
                       "0"="below",
                       "1"="above")) %>% 
    mutate(species=x)
  
})

# Format stats # MAJOR HACK HERE - PREPARE STATS FOR BEST MODEL
stats_label <- stats %>% 
  filter(model=="Random forest (pDA)") %>% 
  mutate(species=factor(species, levels=spp_do)) %>% 
  mutate(label=paste0(n, " ", true, " (", round(pcorrect*100, 1), "% correct)")) %>% 
  arrange(species, desc(true)) %>% 
  group_by(species) %>% 
  summarize(label_combo=paste(label, collapse="\n"))

# Plot data
################################################################################

# My theme
my_theme <- theme(axis.text=element_text(size=6),
                  axis.title=element_text(size=8),
                  plot.title=element_blank(),
                  strip.text = element_text(size=8),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"),
                  legend.position = "bottom")

# Plot ROC curves
g <- ggplot(data1, aes(x=fpr, y=tpr, color=model_type, linetype=pred_type)) +
  # Facet by species
  facet_wrap(~species, ncol=3) +
  # Plot ROC curves
  geom_line() +
  # Plot reference line
  geom_abline(slope=1, intercept=0, col="black", linetype="dotted") +
  # Labels
  labs(x="False positive rate (1-specificity)", y="True positive rate (sensitivity)") +
  # Sample size
  geom_text(stats_label, inherit.aes=F, mapping=aes(x=1, y=0.05, label=label_combo), size=2, col="grey40", hjust=1) +
  # Legend
  scale_color_discrete(name="Model") +
  scale_linetype_discrete(name="Predictors") +
  guides(color = guide_legend(title.position="top", title.hjust = 0),
         linetype = guide_legend(title.position="top", title.hjust = 0)) +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig5_roc_curves_by_species.png"), 
       width=6.5, height=5, units="in", dpi=600)



# Plot data (the Chris Free way)
################################################################################

# Sample size stats
x <- spp_do[1]
data <- purrr::map_df(spp_do, function(x) {
  
  # Read test data
  infile <- paste0(tolower(gsub(" ", "_", x)), "_data_split.Rds")
  sdata <- readRDS(file.path(datadir, infile))
  
  # Read predictions
  infile <- paste0(tolower(gsub(" ", "_", x)), "_evaluation.Rds")
  spreds <- readRDS(file.path(datadir, infile))
  
  # Build data
  sdata1 <- sdata$data_test %>% 
    select(comm_name, sampleid, da_ppm, over) %>% 
    mutate(tempid=1:n())
  
  # Format predictions
  spreds1 <- spreds$preds %>% 
    # Add temporary id
    group_by(model) %>% 
    mutate(tempid=1:n()) %>% 
    # Add sample info
    left_join(sdata1, by="tempid")
  
  # Confirm that you did this correctly
  #sum(spreds1$over != spreds1$true)
  
})

# Data to plot
data_plot <- data %>% 
  filter(model=="Random forest (pDA)") %>% 
  mutate(da_ppm_cap=pmin(200, da_ppm))
  

# Make plot
g <- ggplot(data_plot, aes(y=prob_over, x=da_ppm_cap)) +
  facet_wrap(~comm_name, ncol=3) +
  geom_point() +
  geom_hline(yintercept=0.5) +
  geom_vline(xintercept=30) +
  labs(x="Domoic acid contamination (ppm)", y="p(above action threshold)") +
  theme_bw()
g


