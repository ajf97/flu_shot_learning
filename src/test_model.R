# Loading libraries
library(yaml)
library(tidyverse)
library(jsonlite)
library(mlr)

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
  target_1 <- config$base$target_col_1
  target_2 <- config$base$target_col_2
  scores_path <- config$scores_path
  id_col <- config$base$id_col

  # Read model
  model <- readRDS(model_path)

  test_data <- read_csv(test_data_path)
  test_data <- column_to_rownames(test_data, var = id_col)

  # FIX: Change this on preprocessing phase
  test_data <- test_data %>% mutate(across(where(is_character), as.factor))
  test_data <- test_data %>% mutate(across(c(target_1, target_2), as.logical))

  # Make predictions
  predictions <- predict(model, newdata = test_data)

  # Calculate AUC ROC
  metrics <- getMultilabelBinaryPerformances(predictions, measures = list(auc))

  # Export metrics to JSON
  scores <- list(
    auc_h1n1 = metrics[[1]], auc_seasonal = metrics[[2]],
    auc_avg = mean(c(metrics[[1]], metrics[[2]]))
  )

  export_json <- toJSON(scores, pretty = TRUE, auto_unbox = TRUE)
  write(export_json, file = scores_path)
}


args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("You have to supply config file path", call. = FALSE)
} else if (length(args) == 1) {
  test_model(args[1])
}
