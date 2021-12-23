# Loading libraries
library(yaml)
library(tidyverse)
library(rsample)

#' Split data
#'
#' Split data in train and test datasets
#'
#' @param config_path: path of config file in yaml format

split_data <- function(config_path) {

  # Reading configuration variables
  config <- read_yaml(config_path)
  train_data_path <- config$split_data$train_path
  test_data_path <- config$split_data$test_path
  raw_data_path <- config$data$raw_data
  raw_data_labels_path <- config$data$raw_data_labels
  split_ratio <- config$split_data$test_size
  seed <- config$base$random_state

  data <- read_csv(raw_data_path)
  labels <- read_csv(raw_data_labels_path)

  # Splitting data stratifying the first target variable
  data <- data %>% add_column(labels %>% select(
    config$base$target_col_1,
    config$base$target_col_2
  ))

  set.seed(seed)
  data_split <- initial_split(data,
    prop = 1 - split_ratio,
    strata = config$base$target_col_1
  )
  train_data <- training(data_split)
  test_data <- testing(data_split)

  write_csv(train_data, train_data_path)
  write_csv(test_data, test_data_path)
}


args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("You have to supply config file path", call. = FALSE)
} else if (length(args) == 1) {
  split_data(args[1])
}
