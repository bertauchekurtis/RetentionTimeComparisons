# kurtis bertauche
# 30 august 2023

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

library(ranger)

matrixToTry <- matrix(,nrow=0,ncol=2)
for (numTrees in c(500, 750, 1000, 2000, 3000, 5000, 10000))
{
  for (mtry in c(6, 8, 10, 12, 14, 16))
  {
    matrixToTry <- rbind(matrixToTry, c(numTrees, mtry))
  }
}

random_forest_model <- NULL
random_forest_prediction_error <- 999

# tune the model
for(row in 1:nrow(matrixToTry))
{
  set.seed(37)
  rfModel <- ranger(formula = RetentionTime
                    ~unmodA+unmodC+unmodD+unmodE+unmodF
                    +unmodG+unmodH+unmodI+unmodK+unmodL
                    +unmodM+unmodN+unmodP+unmodQ+unmodR
                    +unmodS+unmodT+unmodV+unmodW+unmodY
                    +modS+modT+modY+modM,
                    data = training,
                    num.trees = matrixToTry[row,1],
                    mtry = matrixToTry[row,2],
                    min.node.size = 5,
                    num.threads = 8)
  print(rfModel$prediction.error)
  if(rfModel$prediction.error < random_forest_prediction_error)
  {
    random_forest_prediction_error <- rfModel$prediction.error
    random_forest_model <- rfModel
    save(random_forest_model, file = "../Models/random_forest_model.RData")
    print("New Best Model Found. Prediction Error:")
    print(random_forest_prediction_error)
    print("Num Trees:")
    print(matrixToTry[row,1])
    print("MTRY:")
    print(matrixToTry[row,2])
  }
  rm(rfModel)
}

# evaluate the best model
load("../Models/random_forest_model.RData")
random_forest_predictions <- predict(random_forest_model,
                                     testing)
random_forest_predictions <- random_forest_predictions$predictions
rf_results <- calcStats(testing$RetentionTime, random_forest_predictions, "Random Forest")
save(rf_results, file = "../Results/rf_results.RData")
