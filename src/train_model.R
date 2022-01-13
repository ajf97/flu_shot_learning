# Load libraries
library(tidyverse)
library(rpart)
library(yaml)
library(mlr)
library(caret)
library(jsonlite)
library(parallel)
library(parallelMap)
#' Train model
#'
#' Train CART decision tree model and save model on models folder
#'
#' @param config_path: configuration params
#'
#' Save model in models folder

train_model <- function(config_path) {
  config <- read_yaml(config_path)
  train_data_path <- config$preprocess_data$train_path
  target_1 <- config$base$target_col_1
  target_2 <- config$base$target_col_2
  model_path <- config$model_path
  id_col <- config$base$id_col
  labels <- c(target_1, target_2)
  scores_path <- config$scores_path
  random_state <- config$base$random_state
  n_folds <- config$base$folds

  set.seed(random_state)

  # Read training data
  data <- read_csv(train_data_path)
  data <- column_to_rownames(data, var = id_col)

  data <- data %>% mutate(across(where(is_character), as.factor))
  data <- data %>% mutate(across(c(target_1, target_2), as.logical))
  colnames(data) <- make.names(colnames(data),unique = T)
  
  # FINE TUNING model
    flu_shot.task <- makeMultilabelTask(
      id = "flu_shot", data = data,
      target = labels
  )
  
  tree <- makeLearner("classif.rpart", predict.type = "prob")
  lrn.br <- makeMultilabelBinaryRelevanceWrapper(tree)
  
  treeParamSpace <- makeParamSet(
    makeIntegerParam("minsplit", lower = 5, upper = 20),
    makeIntegerParam("minbucket", lower = 3, upper = 10),
    makeNumericParam("cp", lower = 0.01, upper = 0.1),
    makeIntegerParam("maxdepth", lower = 3, upper = 10))
  
  randSearch <- makeTuneControlRandom(maxit = 200)
  cvForTuning <- makeResampleDesc("CV", iters = n_folds)
  
  parallelStartSocket(cpus = detectCores())
  
  tunedTreePars <- tuneParams(lrn.br, task = flu_shot.task,
                              resampling = cvForTuning,
                              par.set = treeParamSpace,
                              control = randSearch)
  
  parallelStop()
 
  
  # TRAINING model
  folds <- createFolds(data %>% pull(target_1), k = n_folds)

  cvRPART <- lapply(folds, function(x) {
    training_fold <- data[-x, ]
    test_fold <- data[x, ]

    flu_shot.task <- makeMultilabelTask(
      id = "flu_shot", data = training_fold,
      target = labels
    )
    lrn.br <- makeLearner("classif.rpart", predict.type = "prob")
    lrn.br <- makeMultilabelBinaryRelevanceWrapper(lrn.br)

    # Apply best hyperparameters
    tunedTree <- setHyperPars(lrn.br, par.vals = tunedTreePars$x)
    model <- mlr::train(tunedTree, flu_shot.task)

    predictions <- predict(model, newdata = test_fold)

    metrics <- getMultilabelBinaryPerformances(predictions, measures = list(auc))

    auc_avg <- mean(c(metrics[[1]], metrics[[2]]))

    return(list(
      model = model, auc_avg = auc_avg, h1n1_vaccine = metrics[[1]],
      seasonal_vaccine = metrics[[2]]
    ))
  })
  
  list_auc <- lapply(cvRPART, function(x) x$auc_avg)
  list_h1n1 <- lapply(cvRPART, function(x) x$h1n1_vaccine)
  list_seasonal <- lapply(cvRPART, function(x) x$seasonal_vaccine)
  index_best_model <- which(unlist(list_auc) == max(unlist(list_auc)))
  best_model <- cvRPART[[index_best_model]]$model

  # Save best model
  saveRDS(best_model, file = model_path)

  # Export metrics to JSON
  scores <- list(
    auc_avg = mean(unlist(list_auc)),
    auc_h1n1_avg = mean(unlist(list_h1n1)),
    auc_seasonal_avg = mean(unlist(list_seasonal))
  )

  export_json <- toJSON(scores, pretty = TRUE, auto_unbox = TRUE)
  write(export_json, file = scores_path)
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("You have to supply config file path", call. = FALSE)
} else if (length(args) == 1) {
  train_model(args[1])
}
