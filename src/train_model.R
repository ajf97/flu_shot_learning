# Load libraries
library(tidyverse)
library(rpart)
library(yaml)
library(mlr)

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
  target_1 <- config$base$target_col_1
  target_2 <- config$base$target_col_2
  model_path <- config$model_path
  id_col <- config$base$id_col
  labels <- c(target_1, target_2)

  # Read training data
  data <- read_csv(train_data_path)
  data <- column_to_rownames(data, var = id_col)

  # FIX: Change this on preprocessing phase
  data <- data %>% mutate(across(where(is_character), as.factor))
  data <- data %>% mutate(across(c(target_1, target_2), as.logical))
  
  # Set up multilabel classification
  flu_shot.task <- makeMultilabelTask(id = "flu_shot", data = data, target = labels)
  lrn.br <- makeLearner("classif.rpart", predict.type = "prob")
  lrn.br <- makeMultilabelBinaryRelevanceWrapper(lrn.br)

  model <- train(lrn.br, flu_shot.task)

  # Save model
  saveRDS(model, file = model_path)
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("You have to supply config file path", call. = FALSE)
} else if (length(args) == 1) {
  train_model(args[1])
}
