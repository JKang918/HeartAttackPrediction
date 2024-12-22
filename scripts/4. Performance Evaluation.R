# VIF
vif(stepwise_model)

# overall significance
gstat = stepwise_model$null.deviance - deviance(stepwise_model)
cbind(gstat, 1-pchisq(gstat,length(coef(stepwise_model))-1))

# goodness-of-fit

# 1. test with deviance
deviance_test <- c(deviance(stepwise_model), 1
                   -pchisq(deviance(stepwise_model),df.residual(stepwise_model)))
deviance_test

# 2. test with pearson residual
pearres <- residuals(stepwise_model, type = "pearson")
pearson_tvalue <- sum(pearres^2)
pearson_test <- c(pearson_tvalue, 1 - pchisq(pearson_tvalue,
                                             df.residual(stepwise_model)))
pearson_test

# Overfitting
# KCV (k = 10)
control <- trainControl(method = "cv", number = 10)

cv_model <- train(
  as.factor(output) ~ ., data = train,  
  method = "glm", family = "binomial",
  trControl = control, metric = "Accuracy"
)

print(cv_model)

#KCV result
print(cv_model$resample) # Accuracy for each fold
mean_accuracy <- mean(cv_model$resample$Accuracy)
sd_accuracy <- sd(cv_model$resample$Accuracy)
cat("Mean Accuracy:", mean_accuracy, "\n") #Mean Accuracy, all folds
cat("SD Accuracy:", sd_accuracy, "\n")          #Standard deviation of Accuracies, all folds
print(cv_model$results)                                 # in one shot

# train accuracy
train_preds <- predict(cv_model, train)  # train set prediction
train_accuracy <- mean(train_preds == train$output)  # Accuracy 
print(train_accuracy)