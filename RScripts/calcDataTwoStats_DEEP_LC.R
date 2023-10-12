unCalibrated <- read.csv(file = "./deeplc_predictions_noCalibration_dataTwo.csv")
calibrated <- read.csv(file = "./deeplc_predictions_withCarlibation_dataTwo.csv")

unCalibrated$noNorm =  ((unCalibrated$predicted_tr*(max(unCalibrated$tr) - min(unCalibrated$tr)))/10) + min(unCalibrated$tr)

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

frame <- calcStats(unCalibrated$noNorm, unCalibrated$tr, "noCal")
frame <- rbind(frame,
               calcStats(calibrated$predicted_tr,
                         calibrated$tr,
                         "cal"))
