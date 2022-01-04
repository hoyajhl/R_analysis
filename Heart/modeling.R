## Build Simple ML models
## Logistic regression, Decision trees, and Random Forest
str(heart_df)
set.seed(8)
training_indeces <- createDataPartition(heart_df$target, p = .7, list = FALSE)
data.train <- heart_df[ training_indeces,]
data.test  <- heart_df[-training_indeces,]

# Setting 10-fold Cross-validation
fitControl <- trainControl(method="cv", number=10)

# Logitic Regression
set.seed(8)
model.lr <- train(target ~ ., 
                  data = data.train,
                  method = "glm",
                  family=binomial(),
                  trControl = fitControl)
model.lr
# Decision Tree
set.seed(8)
model.tree <- train(target ~ ., 
                    data = data.train,
                    method = "rpart",
                    trControl = fitControl)
model.tree
plot(model.tree) #comlexity parameter vs Accuracy of CV
rpart.plot(model.tree$finalModel)
#Random Forest
set.seed(8)
model.rf <- train(target ~ ., 
                  data = data.train,
                  method = "rf",
                  trControl = fitControl)
model.rf
plot(model.rf)
preds <- predict(model.rf, data.test)
confusionMatrix(preds, data.test$target)