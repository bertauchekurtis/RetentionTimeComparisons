# kurtis bertauche
# 6 september 2023

training <- read.csv(file = "../data/trainingSet_withVars_DATA_TWO.csv")
testing <- read.csv(file = "../data/testingSet_withVars_DATA_TWO.csv")
set.seed(37)

calcStats <- function(actual, predicted, modelName)
{
  # residuals
  res <- actual - predicted
  # rmse
  rmse <- sqrt(mean(((res)) ^ 2))
  # mae
  mae <- mean(abs(res))
  # window size
  q <- quantile(res, probs =c(.025,.975))
  window <- abs(q[1]) + abs(q[2])
  # correlation
  cor <- cor(actual, predicted)
  # return values
  returnFrame <- data.frame(model = c(modelName),
                            rmse = c(rmse),
                            mae = c(mae),
                            window = c(window),
                            cor = c(cor))
  returnFrame
}

trainingLabels <- training$RetentionTime
testingLabels <- testing$RetentionTime

library(xgboost)

training_xgb <- xgb.DMatrix(data.matrix(training), label = trainingLabels)
testing_xgb <- xgb.DMatrix(data.matrix(testing), label = testingLabels)
load("../Models/best_xgb_model.RData")

set.seed(37)
best_xgb_model_full <- xgboost(booster = "gbtree",
                               objective = "reg:squarederror",
                               gamma = best_xgb_model$params$gamma,
                               child_weight = best_xgb_model$params$child_weight,
                               max_depth = best_xgb_model$params$max_depth,
                               subsample = best_xgb_model$params$subsample,
                               col_subsample = best_xgb_model$params$col_subsample,
                               eta = best_xgb_model$params$eta,
                               nrounds = 100000,
                               nthreads = 8,
                               data = testing_xgb,
                               early_stopping_rounds = 2)

xgb_predictions <- predict(best_xgb_model_full,
                           training_xgb)
xgb_results <- calcStats(training$RetentionTime, xgb_predictions, "XGB")
