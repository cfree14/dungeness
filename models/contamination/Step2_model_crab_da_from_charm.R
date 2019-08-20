

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
datadir <- "models/contamination/data"
codedir <- "models/contamination"

# Read data
data_orig <- readRDS(file.path(datadir, "CHARM_crab_da_data.Rds"))

# Load helper functions
source(file.path(codedir, "helper_functions.R"))

# Outline steps
# 1. Divide into training/testing data
# 2. Impute missing values
# 3. Train models
# 4. Inspect model performance

# Things to do:
# Use tidymodels methods for splitting data and pre-processing data across models
# Perform grid search for number of trees on random forest model
# Potentially expand grid search for BRT
# Longterm: make a whole suite of improved ML functions


# Pre-processing
################################################################################

# Split data (old way)
##########################################

# Divide into training and test datasets
set.seed(1)
sampleids_over <- data_orig$sampleid[data_orig$over==1]
sampleids_under <- data_orig$sampleid[data_orig$over==0]
sampleids_over_test <- sample(sampleids_over, 0.2*length(sampleids_over))
sampleids_under_test <- sample(sampleids_under, 0.2*length(sampleids_under))
data <- data_orig %>% 
  mutate(over=factor(over),
         dataset=ifelse(sampleid %in% c(sampleids_over_test, sampleids_under_test), "test", "train"))
data_train <- filter(data, dataset=="train") %>% 
  select(c("over", paste0("day", 0:30)))
data_test <- filter(data, dataset=="test") %>% 
  select(c("over", paste0("day", 0:30)))

# Inspect sample size in each dataset
stats <- data %>% 
  group_by(dataset, over) %>% 
  summarize(n=n())

# Split data (new way)
##########################################

# # Set seed
# set.seed(1)
# 
# # Format data
# data <- data_orig %>% 
#   select(c("over", paste0("day", 0:30))) %>% 
#   mutate(over=as.factor(over))
# 
# # Data stats
# table(data$over)
# 
# # Split into testing and training data
# data_split <- initial_split(data, prop=0.8, strata=over)
# data_test_check <- testing(data_split)
# data_train_check <- training(data_split)
# 
# # Check that there is the same distribution of unders/overs in training/testing data
# table(data_test_check$over) / nrow(data_test_check)
# table(data_train_check$over) / nrow(data_train_check)

# Impute missing values
##########################################

# Specify recipe
# Impute missing values
data_recipe <- data_train %>% 
  recipe(over ~ .) %>% 
  step_knnimpute(all_predictors(), neighbors = 3)

# Prep training data
data_prepped <- data_recipe %>% 
  prep(retain=T)

# Bake training and testing data
data_train <- data_prepped %>% 
  juice()

data_test <- data_prepped %>% 
  bake(new_data=data_test)

# Export formatted data
save(data_test, data_train, data_orig, file=file.path(datadir, "CHARM_crab_da_data_for_models.Rdata"))


# Logistic regression
################################################################################

# Train logistic regression
glm_fit <- logistic_reg(mode = "classification") %>%
  set_engine("glm") %>%
  fit(over ~ ., data=data_train)

# Export model object
saveRDS(glm_fit, file.path(datadir, "glm_model.Rds"))


# Random forest model
################################################################################

# Define tuning parameter grid
fitGrid <- expand.grid(mtry=seq(10, 20, 2))

# Define tuning and training method
fitControl <- trainControl(method="repeatedcv", number=10, repeats=10)

# Train RF model
rf_fit <- train(over ~ day0 + day1 + day2 + day3 + day4 + day5 + day6 + day7 + day8 + day9 + day10 + 
                  day11 + day12 + day13 + day14 + day15 + day16 + day17 + day18 + day19 + day20 +
                  day21 + day22 + day23 + day24 + day25 + day26 + day27 + day28 + day29 + day30,
                data=data_train,
                method="rf", 
                distribution="bernoulli", 
                metric="Kappa",
                tuneGrid=fitGrid, trControl=fitControl, na.action=na.pass, verbose=F)

# Plot RF tuning results
rf_fit$bestTune
rf_tune <- rf_fit$results
g <- ggplot(rf_tune, aes(x=mtry, y=Kappa)) +
  geom_line() +
  theme_bw()
g

# Export model object
saveRDS(rf_fit, file.path(datadir, "rf_model.Rds"))


# Boosted regression tree model
################################################################################

# Define tuning parameter grid
fitGrid <- expand.grid(interaction.depth=c(1, 3, 5, 7, 10),
                       n.trees=seq(1000, 15000, 500),
                       shrinkage=c(0.01, 0.005, 0.001), # larger numbers are slower?
                       n.minobsinnode=10)

# Define tuning and training method
fitControl <- trainControl(method="repeatedcv", number=10, repeats=3)

# Train BRT model
brt_fit <- train(over ~ day0 + day1 + day2 + day3 + day4 + day5 + day6 + day7 + day8 + day9 + day10 + 
                  day11 + day12 + day13 + day14 + day15 + day16 + day17 + day18 + day19 + day20 +
                  day21 + day22 + day23 + day24 + day25 + day26 + day27 + day28 + day29 + day30,
                data=data_train,
                method="gbm", 
                bag.fraction=0.5,
                distribution="bernoulli", 
                metric="Kappa",
                tuneGrid=fitGrid, trControl=fitControl, na.action=na.pass, verbose=F)

# Inspect tune
brt_tune <- brt_fit$results
g <- ggplot(brt_tune, aes(x=n.trees, y=Kappa, color=as.factor(interaction.depth))) +
  geom_line() +
  facet_wrap(~ shrinkage) +
  scale_color_discrete(name="Tree depth") +
  labs(x="Number of trees", y="Cohen's kappa") +
  theme_bw()
g

# Export model object
# brtfit <- readRDS(file.path(datadir, "brtmodel2.Rds"))
saveRDS(brt_fit, file.path(datadir, "brtmodel2.Rds"))




# Make predictions
glm_preds <- make_predictions(glm_fit, data_test)
rf_preds <- make_predictions(rf_fit, data_test)
brt_preds <- make_predictions(brt_fit, data_test)

# Plot ROC curve and print other stuff
plot_roc(glm_preds)
plot_roc(rf_preds)
plot_roc(brt_preds)

# Performance metrics
perf_metrics(glm_preds)
perf_metrics(rf_preds)
perf_metrics(brt_preds)







