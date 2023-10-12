training <- read.csv(file = "../data/trainingSet_withVars_DATA_TWO.csv")
testing <- read.csv(file = "../data/testingSet_withVars_DATA_TWO.csv")
set.seed(37)

library(e1071)
library(caret)

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

train_control <- trainControl(method = "cv", number = 5)
valuesToTune <- c(0.1, 0.5, 1, 2, 5, 10, 20)
# LINEAR SVM
linear_svm <- train(RetentionTime ~.,
                    data = training,
                    method = "svmLinear",
                    trControl = train_control,
                    tuneGrid = data.frame(C=valuesToTune))
save(linear_svm, file = "../Models/linear_svm.RData")

best_linear_svm <- svm(RetentionTime ~ .,
                       data = training,
                       kernel = "linear",
                       cost = linear_svm$bestTune)

linear_svm_predictions <- predict(best_linear_svm,
                                  testing)
linear_svm_results <- calcStats(testing$RetentionTime,
                                linear_svm_predictions,
                                "SVM")
save(best_linear_svm, file = "models/best_linear_svm_model.RData")
save(linear_svm_results, file = "../Results/linear_svm_results.RData")
