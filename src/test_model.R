# Loading libraries
library(yaml)
library(tidyverse)
library(ROCR)
library(jsonlite)

#' Test model
#'
#' Test model and save metrics on metrics folder
#'
#' @param config_path: path of config file
#'
#' Save AUC metric

test_model <- function(config_path) {
  config <- read_yaml(config_path)
  test_data_path <- config$split_data$test_path
  model_path <- config$model_path
  target <- config$base$target_col_1
  scores_path <- config$scores_path

  # Read model
  model <- readRDS(model_path)

  test_data <- read.csv(test_data_path)

  # Make predictions
  predictions <- model %>% predict(test_data %>% select(-target, -config$base$target_col_2))
  predictions_ROCR <- prediction(predictions, test_data %>% pull(target))

  # Calculate AUC ROC
  auc_roc <- performance(predictions_ROCR, measure = "auc")
  auc_roc <- auc_roc@y.values[[1]]

  # Export metrics to JSON
  scores <- list(auc_roc = auc_roc)

  export_json <- toJSON(scores, pretty = TRUE, auto_unbox = TRUE)
  write(export_json, file = scores_path)
}


args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("You have to supply config file path", call. = FALSE)
} else if (length(args) == 1) {
  test_model(args[1])
}



p <- test_model("params.yaml")
