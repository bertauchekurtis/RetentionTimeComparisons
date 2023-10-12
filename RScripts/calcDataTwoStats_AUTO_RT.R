
calcStats <- function(predictedValue, actualValue, modelName)
{
  # residuals
  res <- actualValue - predictedValue
  
  # rmse
  rmse <- sqrt(mean(((res)) ^ 2))
  # mae
  mae <- mean(abs(res))
  # window size
  q <- quantile(res, probs =c(.025,.975))
  window <- abs(q[1]) + abs(q[2])
  # correlation
  cor <- cor(actualValue, predictedValue)
  # return values
  returnFrame <- data.frame(model = c(modelName),
                            rmse = c(rmse),
                            mae = c(mae),
                            window = c(window),
                            cor = c(cor))
  returnFrame
}

scratchModel_one_predictions <- read.csv(file = "./autoRT/dataTwo_scratchModel_one_predictions/test_evaluate.csv")
scratchModel_two_predictions <- read.csv(file = "./autoRT/dataTwo_scratchModel_two_predictions/test_evaluate.csv")
transferModel_one_predictions <- read.csv(file = "./autoRT/dataTwo_transferModel_one_predictions/test_evaluate.csv")
transferModel_two_predictions <- read.csv(file = "./autoRT/dataTwo_transferModel_two_predictions/test_evaluate.csv")

frame <- calcStats(scratchModel_one_predictions$y_pred, 
                   scratchModel_one_predictions$y, 
                   "Scratch One")
frame <- rbind(frame,
               calcStats(
                 scratchModel_two_predictions$y_pred,
                 scratchModel_two_predictions$y,
                 "Scratch Two"
               ))
frame <- rbind(frame,
               calcStats(
                 transferModel_one_predictions$y_pred,
                 transferModel_one_predictions$y,
                 "Transfer One"
               ))
frame <- rbind(frame,
               calcStats(
                 transferModel_two_predictions$y_pred,
                 transferModel_two_predictions$y,
                 "Transfer Two"
               ))



