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

# SLR
slr_model <- lm(RetentionTime 
                ~unmodA+unmodC+unmodD+unmodE+unmodF
                +unmodG+unmodH+unmodI+unmodK+unmodL
                +unmodM+unmodN+unmodP+unmodQ+unmodR
                +unmodS+unmodT+unmodV+unmodW+unmodY
                +modS+modT+modY+modM, 
                data = training)
save(slr_model, file = "../Models/slr_model.RData")
slr_predictions <- predict(slr_model, testing)
slr_results <- calcStats(testing$RetentionTime, slr_predictions, "Simple Linear Regression")
save(slr_results, file = "../Results/slr_results.RData")

# Stepwise
library(leaps)
library(caret)
stepwise_model <- regsubsets(RetentionTime 
                             ~unmodA+unmodC+unmodD+unmodE+unmodF+
                              unmodG+unmodH+unmodI+unmodK+unmodL+
                              unmodM+unmodN+unmodP+unmodQ+unmodR+
                              unmodS+unmodT+unmodV+unmodW+unmodY+
                              modS+modY+modT+modM,
                              data = training, 
                             nvmax = 24, 
                             method = "exhaustive")
num_vars <- ncol(training) - 3 # don't count sequence, response, or length
testing_matrix <- model.matrix(RetentionTime 
                              ~unmodA+unmodC+unmodD+unmodE+unmodF+
                              unmodG+unmodH+unmodI+unmodK+unmodL+
                              unmodM+unmodN+unmodP+unmodQ+unmodR+
                              unmodS+unmodT+unmodV+unmodW+unmodY+
                              modS+modY+modT+modM, 
                              testing)

test_err_rmse = rep(0, times = num_vars)
mae = rep(0, times = num_vars)
qs = rep(0, times = num_vars)
cors = rep(0, times = num_vars)

for (i in seq_along(test_err_rmse)) 
{
  coefs = coef(stepwise_model, id = i)
  pred = testing_matrix[, names(coefs)] %*% coefs
  test_err_rmse[i] <- sqrt(mean((testing$RetentionTime - pred) ^ 2))
  mae[i] <- mean(abs(testing$RetentionTime - pred))
  q <- quantile((testing$RetentionTime-pred), probs =c(.025,.975))
  qs[i] <- abs(q[1]) + abs(q[2]) # total length of window
  cors[i] <- cor(pred, testing$RetentionTime)
}

# use RMSE to select best subset model
min_index <- which.min(test_err_rmse)
# save resutlts
save(stepwise_model, file = "../Models/stepwise_model.RData")
stepwise_results <- data.frame(model = c("Stepwise Regression"),
                              rmse = c(test_err_rmse[min_index]),
                              mae = c(mae[min_index]),
                              window = c(qs[min_index]),
                              cor = c(cors[min_index]))
save(stepwise_results, file = "../Results/stepwise_results.RData")

# setup for the GLM models
library(glmnet)
set.seed(37)
fold_id <- sample(rep(seq(5), length.out = nrow(training))) # prepare the cross validation
training_glm <- model.matrix(RetentionTime ~ 
                             unmodA+unmodC+unmodD+unmodE+unmodF+
                             unmodG+unmodH+unmodI+unmodK+unmodL+
                             unmodM+unmodN+unmodP+unmodQ+unmodR+
                             unmodS+unmodT+unmodV+unmodW+unmodY+
                             modS+modY+modT+modM, 
                             training)[, -1]
testing_glm <- model.matrix(RetentionTime ~ 
                            unmodA+unmodC+unmodD+unmodE+unmodF+
                            unmodG+unmodH+unmodI+unmodK+unmodL+
                            unmodM+unmodN+unmodP+unmodQ+unmodR+
                            unmodS+unmodT+unmodV+unmodW+unmodY+
                            modS+modY+modT+modM, 
                            testing)[, -1]

# ridge
fit_ridge_cv_mse <- cv.glmnet(training_glm, 
                              training$RetentionTime, 
                              alpha = 0, 
                              nfolds = 5, 
                              foldid = fold_id)
ridge_model <- glmnet(x = training_glm,
                      y = training$RetentionTime,
                      alpha = 0,
                      lambda = fit_ridge_cv_mse$lambda.min)
ridge_predictions <- predict(ridge_model, newx = testing_glm)
ridge_results <- calcStats(testing$RetentionTime, ridge_predictions, "Ridge Regression")
save(ridge_model, file = "../Models/ridge_model.RData")
save(ridge_results, file = "../Results/ridge_results.RData")

# lasso
fit_lasso_cv_mse <- cv.glmnet(training_glm,
                              training$RetentionTime,
                              alpha = 1,
                              nfolds = 5,
                              foldid = fold_id)
lasso_model <- glmnet(x = training_glm,
                      y = training$RetentionTime,
                      alpha = 1,
                      lambda = fit_lasso_cv_mse$lambda.min)
lasso_predictions <- predict(lasso_model, newx = testing_glm)
lasso_results <- calcStats(testing$RetentionTime, lasso_predictions, "Lasso Regression")
save(lasso_model, file = "../Models/lasso_model.RData")
save(lasso_results, file = "../Results/lasso_results.RData")

#elastic net
cv_control <- trainControl(method = "cv", number = 5)
fit_elastic_cv <- train(RetentionTime ~ 
                        unmodA+unmodC+unmodD+unmodE+unmodF+
                        unmodG+unmodH+unmodI+unmodK+unmodL+
                        unmodM+unmodN+unmodP+unmodQ+unmodR+
                        unmodS+unmodT+unmodV+unmodW+unmodY+
                        modS+modY+modT+modM,
                        data = training,
                        method = "glmnet",
                        trControl = cv_control,
                        foldid = fold_id,
                        tuneLength = 25)
elastic_model <- glmnet(x = training_glm,
                        y = training$RetentionTime,
                        lambda = fit_elastic_cv$bestTune$lambda,
                        alpha = fit_elastic_cv$bestTune$alpha)
elastic_predictions <- predict(elastic_model, newx = testing_glm)
elastic_results <- calcStats(testing$RetentionTime, elastic_predictions, "Elastic Net Regression")
save(elastic_model, file = "../Models/elastic_model.RData")
save(elastic_results, file = "../Results/elastic_results.RData")
