# Load libraries
library(tidyverse)
library(rpart)
library(yaml)

#' Train model
#'
#' Train CART decision tree model and save model on models folder
#'
#' @param config_path: configuration params
#'
#' Save model in models folder

train_model <- function(config_path) {
  config <- read_yaml(config_path)
  train_data_path <- config$split_data$train_path
  target <- config$base$target_col_1
  model_path <- config$model_path

  # Read training data
  data <- read_csv(train_data_path)
  data = data %>% select(-config$base$target_col_2)

  model <- rpart(paste(target, "~.", sep = ""), data = data)

  # Save model
  saveRDS(model, file = model_path)
}


args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("You have to supply config file path", call. = FALSE)
} else if (length(args) == 1) {
  train_model(args[1])
}
