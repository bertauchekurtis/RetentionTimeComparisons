# kurtis bertauche
# 1 september 2023

training <- read.csv(file = "../data/trainingSet_withVars_DATA_TWO.csv")
testing <- read.csv(file = "../data/testingSet_withVars_DATA_TWO.csv")
set.seed(37)

testing$Peptide.Sequence2 <- NULL
training$Peptide.Sequence2 <- NULL

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
training$RetentionTime <- NULL
testing$RetentionTime <- NULL

library(xgboost)

training_xgb <- xgb.DMatrix(data.matrix(training), label = trainingLabels)
testing_xgb <- xgb.DMatrix(data.matrix(testing), label = testingLabels)


matrixToTry <- matrix(,nrow=0,ncol=6)
for (gamma in c(0, 0.1, 0.2, 0.3, 0.4))
{
  for (child_weight in c(1,2,3,4,5,6))
  {
    for (col_subsample in c(0.8, 0.9, 1))
    {
      for (max_depth in c(9, 10, 11))
      {
        for (subsample in c(0.8, 0.9, 1))
        {
          for (eta in c(0.01, 0.05, 0.08))
          {
            matrixToTry <- rbind(matrixToTry,
                                 c(gamma,child_weight,
                                   max_depth, subsample, col_subsample,
                                   eta))
          }
        }
      }
    }
  }
}

best_xgb_model <- NULL
best_rmse <- 999

for (row in 1:nrow(matrixToTry))
{
  set.seed(37)
  model <- xgb.cv(booster = "gbtree",
                  objective = "reg:squarederror",
                  gamma = matrixToTry[row, 1],
                  child_weight = matrixToTry[row, 2],
                  max_depth = matrixToTry[row, 3],
                  subsample = matrixToTry[row, 4],
                  col_subsample = matrixToTry[row, 5],
                  eta = matrixToTry[row, 6],
                  nrounds = 10000,
                  nthreads = 8,
                  nfold = 5,
                  print_every_n = 2500,
                  early_stopping_rounds = 2,
                  data = training_xgb,
  )
  rmse <- model$evaluation_log[model$best_iteration]$test_rmse_mean
  if(rmse < best_rmse)
  {
    best_xgb_model <- model
    best_rmse <- rmse
    print("NEW BEST MODEL FOUND!")
    print("ROW:")
    print(row)
    save(best_xgb_model, file = "../Models/best_xgb_model.RData")
  }
  else
  {
    print("MODEL DID NOT IMPROVE")
    print("ROW:")
    print(row)
  }
}

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
                               data = training_xgb,
                               early_stopping_rounds = 2)

xgb_predictions <- predict(best_xgb_model_full,
                           testing_xgb)
xgb_results <- calcStats(testingLabels, xgb_predictions, "XGB")
save(best_xgb_model_full, file = "../Models/best_xgb_model_full.RData")
save(xgb_results, file = "../Results/xgb_results.RData")
