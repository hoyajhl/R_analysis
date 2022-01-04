## Regression using stepwise model 

# A null model with no predictors
null_model <- glm(target ~ 1, data = heart_df, family = "binomial")
null_model
# Full model using all of the potential predictors
full_model <- glm(target ~ ., data = heart_df, family = "binomial")
full_model
# Use a stepwise algorithm to build a model
step_model <- step(full_model, scope = list(lower = null_model, upper = full_model),
                   direction = "backward")
plot(step_model)
summary(step_model)
# Estimate the stepwise heart disease probability
# Model fitting / predict
step_prob <- predict(step_model, type = "response")

# Plot the ROC of the stepwise model
library(pROC)
ROC <- roc(heart_df$target, step_prob)
plot(ROC, col = "red")
ROC ## Area under the curve: 0.94

# Get the actual responses from heart_df
actual_response <- heart_df$target

# Predicted responses from the model
predicted_response <- round(fitted(step_model))
# Get a table of these values
outcomes <- table(predicted_response, actual_response)
outcomes ## 2*2 table represented by actual response*predicted response

#Confusion Matrix to evaluate the performance
library(caret)
confusionMatrix(as.factor(actual_response),as.factor(predicted_response))
confusionMatrix(data=as.factor(predicted_response), reference =as.factor(actual_response))