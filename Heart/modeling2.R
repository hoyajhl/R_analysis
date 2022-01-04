##Lsso Regression, Ridge, XGB
str(heart_df)
set.seed(999)
training_indeces <- createDataPartition(heart_df$target, p = .7, list = FALSE)
data.train <- heart_df[ training_indeces,]
data.test  <- heart_df[-training_indeces,]

fitcontrol <-trainControl(method="cv", number=10)
lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))

lasso_mod <- train(target ~ .,
                   data=data.train,
                   method='glmnet', 
                   trControl= fitcontrol, 
                   tuneGrid=lassoGrid) 
lasso_mod$bestTune # alpha: 1, lambda: 0.02
plot(lasso_mod) ## Tuning Lambda (Regularization Parameter)
max(lasso_mod$results$Accuracy) #  0.8224459
min(lasso_mod$results$Accuracy) #  0.7797835

lassoVarImp <- varImp(lasso_mod,scale=F)
lassoImportance <- lassoVarImp$importance
length(lassoImportance$Overall)
vars_selected <- length(which(lassoImportance$Overall!=0))
vars_notselected <- length(which(lassoImportance$Overall==0))
cat('Lasso uses', vars_selected, 
    'variables in its model, and did not select', vars_notselected, 
    'variables.') 
### Lasso uses 13 variables in its model, and did not select 7 variables.
LassoPred <- predict(lasso_mod,data.test)
confusionMatrix(LassoPred, data.test$target) ## Accuracy : 0.8977 

##Ridge
RidgeGrid <- expand.grid(alpha = 0, lambda = seq(0.001,0.1,by = 0.0005))
ridge_mod <- train(target ~ .,
                   data=data.train,
                   method='glmnet', 
                   trControl= fitcontrol, 
                   tuneGrid=RidgeGrid) 
ridge_mod$bestTune # alpha: 0, lambda: 0.038
plot(ridge_mod) ## Tuning Lambda (Regularization Parameter)
max(ridge_mod$results$Accuracy) #  0.8216017
min(ridge_mod$results$Accuracy) #  0.8070779
RidgePred <- predict(ridge_mod,data.test)
confusionMatrix(RidgePred, data.test$target) ## Accuracy : 0.8977 

## XGB
#xgb_grid = expand.grid(
#  nrounds = 1000,
#  eta = c(0.1, 0.05, 0.01),
#  max_depth = c(2, 3, 4, 5, 6),
#  gamma = 0,
#  colsample_bytree=1,
#  min_child_weight=c(1, 2, 3, 4 ,5),
#  subsample=1)
#xgb_caret <-train(target ~ .,data=data.train,
# method='xgbTree', 
# trControl= fitcontrol, 
# tuneGrid=xgb_grid)  
#xgb_caret$bestTune
## To find out optimal parameter
## Let's use xgb package
install.packages("xgboost")
library(xgboost)
#using for xgboost
data.train.2<-subset(data.train, select=-target)
data.test.2<-subset(data.test, select=-target)
label_train <- data.train$target
label_test <- data.test$target
# put our testing & training data into two seperates Dmatrixs objects

set.seed(999)

x_train_cv<-data.matrix(data.train.2)
y_train_cv<-data.matrix(label_train)

default_param<-list(objective = "binary:logistic", #"reg:linear"
                    booster = "gbtree",
                    eta=0.05, #default = 0.3
                    gamma=0,
                    max_depth=3, #default=6
                    min_child_weight=4, #default=1
                    subsample=1,
                    colsample_bytree=1)

xgb_cv<-xgb.cv(data=x_train_cv,label=y_train_cv,
               params = default_param, 
               nrounds = 500, 
               nfold = 5, 
               showsd = T, 
               stratified = T, 
               print_every_n = 40,
               early_stopping_rounds = 10, 
               maximize = F)

#train the model using the best iteration found by cross validation
dtrain<-xgb.DMatrix(data=x_train_cv,label=y_train_cv)

xgb_mod <- xgb.train(data = dtrain, params=default_param, nrounds = 454)
xgb_mod
dtest <- xgb.DMatrix(data=data.matrix(data.test.2))
XGB_prob<- predict(xgb_mod, dtest)

# Plot the ROC of XGB
library(pROC)
ROC <- roc(label_test,XGB_prob) # Area under the curve: 0.9349
plot(ROC)

#XGB variable importance plot
install.packages("Ckmeans.1d.dp")
library(Ckmeans.1d.dp) #required for ggplot clustering
mat <- xgb.importance (feature_names = colnames(data.train.2),model = xgb_mod)
mat
# using ggplot to express Importance
# xgb.ggplot.importance(importance_matrix = mat[1:12], rel_to_first = TRUE)