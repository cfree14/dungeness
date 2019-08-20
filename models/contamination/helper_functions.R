# Helper functions
################################################################################

# Make predictions
# model <- rf_fit; dataset <- data_test
make_predictions <- function(model, dataset){
  
  # Measure performance (Parsnip object)
  if(length(names(model))==4){
    preds_fac <- predict(model, new_data=dataset)
    preds_prob <- predict(model, new_data=dataset, type="prob")
    preds <- dataset %>%
      # True class
      select(over) %>% 
      rename(true=over) %>% 
      # Add class prediction
      bind_cols(preds_fac) %>% 
      rename(estimate=.pred_class) %>% 
      mutate(estimate=as.factor(estimate)) %>% 
      # Add class probabilities
      bind_cols(preds_prob) %>% 
      rename(prob_under=.pred_0, prob_over=.pred_1)
  }
  
  # Measure performance (Caret object)
  if(length(names(model))>4){
    preds_fac <- predict(model, newdata=dataset)
    preds_prob <-predict(model, newdata=dataset, type="prob")
    preds <- data_test %>%
      # True class
      select(over) %>% 
      rename(true=over) %>% 
      # Add class prediction
      mutate(estimate=preds_fac) %>% 
      mutate(estimate=as.factor(estimate)) %>% 
      # Add class probabilities
      bind_cols(preds_prob) %>% 
      rename(prob_under='0', prob_over='1')
  }
  
  # Return
  return(preds)
  
}

# Measure performance
perf_metrics <- function(preds){
  acc <- accuracy(data=preds, truth="true", estimate="estimate")
  kap <- kap(data=preds, truth="true", estimate="estimate")
  roc1 <- roc(data=preds, response="true", predictor = "prob_under")
  auc <- auc(roc1)
  df <- tibble(accuracy=acc$.estimate, 
               kappa=kap$.estimate, 
               auc=as.numeric(auc))
  return(df)
}

# Plot ROC curve
plot_roc <- function(preds){
  roc1 <- roc(data=preds, response="true", predictor = "prob_under")
  plot(roc1)
  roc_auc <- auc(roc1)
  message(paste("AUC =", roc_auc))
  return(roc1)
}







