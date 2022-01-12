# Loading libraries
library(tidyverse)
library(mlr)
library(yaml)

#' Make submission file
#'
#' Make submission file for upload to DrivenData
#'
#' @param config_path: config on yaml file

make_submission_file <- function(config_path) {
  config <- read_yaml(config_path)
  model_path <- config$model_path
  test_data_path <- config$preprocess_data$test_path
  submission_file_path <- config$base$submission_file_path
  target_1 <- config$base$target_col_1
  target_2 <- config$base$target_col_2
  id_col <- config$base$id_col

  test_data <- read_csv(test_data_path)
  test_data <- column_to_rownames(test_data, var = id_col)
  colnames(test_data) <- make.names(colnames(test_data),unique = T)
  
  model <- readRDS(model_path)

  # FIX: Change this on preprocessing phase
  test_data <- test_data %>% mutate(across(where(is_character), as.factor))
  
  # Make predictions
  predictions <- predict(model, newdata = test_data)

  # Get probabilities
  prob_target_1 <- predictions[["data"]][[paste("prob.", target_1, sep = "")]]
  prob_target_2 <- predictions[["data"]][[paste("prob.", target_2, sep = "")]]

  # Build dataframe
  test_data <- rownames_to_column(test_data, var = id_col)

  file_df <- data.frame(
    test_data[, id_col],
    prob_target_1,
    prob_target_2
  )
  colnames(file_df) <- c(id_col, target_1, target_2)

  write.csv(file_df, file = submission_file_path, row.names = FALSE)
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("You have to supply config file path", call. = FALSE)
} else if (length(args) == 1) {
  make_submission_file(args[1])
}

