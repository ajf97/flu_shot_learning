# Loading libraries
library(yaml)
library(tidyverse)

#' Preprocess data
#'
#' Preprocess basic data
#'
#' @param config_path: path of config file in yaml format

preprocess_data <- function(config_path) {

  # Reading configuration variables
  config <- read_yaml(config_path)
  train_data_path <- config$preprocess_data$train_path
  raw_data_path <- config$data$raw_data
  raw_data_labels_path <- config$data$raw_data_labels

  data <- read_csv(raw_data_path)
  labels <- read_csv(raw_data_labels_path)

  data <- data %>% add_column(labels %>% select(
    config$base$target_col_1,
    config$base$target_col_2
  ))

  write_csv(data, train_data_path)
}


args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("You have to supply config file path", call. = FALSE)
} else if (length(args) == 1) {
  preprocess_data(args[1])
}
