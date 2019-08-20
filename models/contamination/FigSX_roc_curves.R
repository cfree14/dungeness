
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(rio)
library(zoo)
library(tidyverse)
library(lubridate)
library(gbm)
library(caret)
library(pROC)
library(tidymodels)


# Directories
codedir <- "models/contamination"
datadir <- "models/contamination/data"
plotdir <- "models/contamination/figures"

# Load helper functions
source(file.path(codedir, "helper_functions.R"))

# Load models
brt_fit <- readRDS(file.path(datadir, "brtmodel2.Rds"))
glm_fit <- readRDS(file.path(datadir, "glm_model.Rds"))
rf_fit <- readRDS(file.path(datadir, "rf_model.Rds"))

# Load data
load(file.path(datadir, "CHARM_crab_da_data_for_models.Rdata"))


# Build performance table
################################################################################

# Make predictions
glm_preds <- make_predictions(glm_fit, data_test)
rf_preds <- make_predictions(rf_fit, data_test)
brt_preds <- make_predictions(brt_fit, data_test)

# Performance metrics
data <- bind_rows(perf_metrics(glm_preds), 
                  perf_metrics(rf_preds),
                  perf_metrics(brt_preds)) %>% 
  mutate(method=c("Logistic regression", "Random forests", "Boosted regression trees")) %>% 
  select(method, everything()) %>% 
  arrange(desc(auc))

# Export 
write.csv(data, file=file.path(plotdir, "TableSX_contamination_model_performance.csv"), row.names=F)


# Plot ROC figure
################################################################################

# Function to format ROC data
format_roc <- function(x, method){
  tpr <- x$sensitivities # true positive rate (sensitivity)
  fpr <- 1-x$specificities # false positive rate (1 - specificity)
  df <- tibble(method=method, tpr=tpr, fpr=fpr) %>% 
    arrange(tpr)
  return(df)
}

# Build ROC objects
glm_roc <- plot_roc(glm_preds)
rf_roc <- plot_roc(rf_preds)
brt_roc <- plot_roc(brt_preds)

# Format ROC objects
roc_data <- bind_rows(format_roc(glm_roc, "Logistic regression"),
                      format_roc(rf_roc, "Random forests"),
                      format_roc(brt_roc, "Boosted regression trees")) %>% 
  mutate(method=factor(method, levels=c("Random forests", "Boosted regression trees", "Logistic regression")))

# Setup theme
my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=9),
                  plot.title=element_text(size=11),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Plot data
g <- ggplot(roc_data, aes(x=fpr, y=tpr, color=method)) +
  geom_line() +
  geom_abline(slope=1, intercept=0, col="black", linetype="dotted") +
  labs(x="False positive rate (1-specificity)", y="True positive rate (sensitivity)") +
  scale_color_discrete(name="Method") +
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_roc_curves.png"), width=5, height=3, units="in", dpi=600)
