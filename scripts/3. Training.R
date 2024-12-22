# Initial model # All predictors
initial_model <- glm(`output` ~ ., data = train, family = binomial)
summary(initial_model)

# Variable selection # Bidirectional stepwise regression
stepwise_model <- stepAIC(initial_model, direction = "both")
summary(stepwise_model)