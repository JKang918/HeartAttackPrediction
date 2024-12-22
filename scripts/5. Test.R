# Test data set
# ROC
probs <- predict(cv_model, test, type = "prob")[, 2] 
roc_curve <- roc(test$output, probs)
plot(roc_curve)
auc(roc_curve)

# prediction values using test dataset
predictions <- predict(stepwise_model, newdata = test, type = "response")

# turn the output into 0 and 1 (threshold = 50%)
predicted_class <- ifelse(predictions > 0.5, 1, 0)

# Confusion Matrix 
confusionMatrix(factor(predicted_class), factor(test$output))