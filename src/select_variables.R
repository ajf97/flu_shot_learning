# Loading libraries
library(yaml)
library(tidyverse)
library(caret)
library(FSinR)

#' Select features
#'
#' Select features
#'
#' @param config_path: path of config file in yaml format

select_features <- function(config_path) {

  # Reading configuration variables
  config <- read_yaml(config_path)
  random_state = config$base$random_state
  id_col <- config$base$id_col
  set.seed(random_state)
  data_path <- config$preprocess_data$train_path
  output <- config$preprocess_data$select_variables 
   
  data <- read_csv(data_path)
  data <- column_to_rownames(data, var = id_col)
  
  # Data preparation
  features = data %>% select(-config$base$target_col_1,
                         -config$base$target_col_2)
  labels = data %>% select(config$base$target_col_1,
                         config$base$target_col_2)
  features_lvf = data %>% select(-config$base$target_col2)

  # LVF 
  filter_evaluator <- filterEvaluator('determinationCoefficient')
  LV_search <- LasVegas()
  results = LV_search(features_lvf, config$base$target_col_1 , filter_evaluator)
  best_features = results$bestFeatures
 
  # Sequential forward selection
  # filter_evaluator <- filterEvaluator('determinationCoefficient')
  # sfs_search <- sequentialForwardSelection()
  # results = sfs_search(features_lvf, config$base$target_col_1, filter_evaluator)
 
  write_csv(selected, output)
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("You have to supply config file path", call. = FALSE)
} else if (length(args) == 1) {
  select_features(args[1])
}
