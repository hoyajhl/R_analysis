##Loading package
install.packages("corrplot")
install.packages("ggraph")
install.packages("tidytext")
install.packages("wordcloud")

library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('grid') # visualisation
library('gridExtra') # visualisation

library('corrplot') # visualisation
library('ggraph') # visualisation
library('igraph') # visualisation
library('dplyr') # data manipulation
library('readr') # data input
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('stringr') # string manipulation
library('forcats') # factor manipulation
library('tidytext') # text mining
library('SnowballC') # text analysis
library('wordcloud') # test visualisation
getwd()
setwd("C:/Kaggle")
wbcd <- read.csv("data.csv", header=T, stringsAsFactors=F)
wbcd$X <- NULL ##Remove Null data
str(wbcd$diagnosis)
levels(wbcd$diagnosis)
##Reshape the datasets
wbcd <- wbcd[,-1] ## extacted all data except first column
wbcd$diagnosis <- factor(ifelse(wbcd$diagnosis=="B","Benign","Malignant"))
str(wbcd)
head(wbcd)
##correlation plot using PerformacneAnalytics
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(wbcd[,c(2:11)],histogram=TRUE, col="grey10", pch=1, 
                  main="Cancer Mean")
library(ggplot2)
library(GGally)
## correlation plot more directly
ggcorr(wbcd[,c(22:31)], name = "corr", label = TRUE)+
  theme(legend.position="none")+
  labs(title="Cancer Worst")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))
ggcorr(wbcd[,c(12:21)], name = "corr", label = TRUE)+
  theme(legend.position="none")+
  labs(title="Cancer SE")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))
ggcorr(wbcd[,c(22:31)], name = "corr", label = TRUE)+
  theme(legend.position="none")+
  labs(title="Cancer Worst")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))


##PCA Analysis
install.packages("factoextra")
library(factoextra)
wbcd_pca <- transform(wbcd) 
all_pca <- prcomp(wbcd_pca[,-1], cor=TRUE, scale = TRUE)
summary(all_pca)
###Screeplot 
#### The percentage of variability explained by the principal components 
#### can be ascertained through screeplot
fviz_eig(all_pca, addlabels=TRUE, ylim=c(0,60), geom = c("bar", "line"), barfill = "pink", barcolor="grey",linecolor = "red", ncp=10)+
  labs(title = "Cancer All Variances - PCA",
       x = "Principal Components", y = "% of variances")
#### get PCA variables
all_var <- get_pca_var(all_pca)
all_var
library("corrplot")
corrplot(all_var$cos2, is.corr=FALSE)

### PCA
set.seed(218)
res.all <- kmeans(all_var$coord, centers = 6, nstart = 25)
res.all ## k-means clusering with 6 clusters of size
grp <- as.factor(res.all$cluster)

fviz_pca_var(all_pca, col.var = grp, 
             palette = "jco",
             legend.title = "Cluster")
### PCA-Biplot
fviz_pca_biplot(all_pca, col.ind = wbcd$diagnosis, col="black",
                palette = "jco", geom = "point", repel=TRUE,
                legend.title="Diagnosis", addEllipses = TRUE)


### APPLY ML
nrows <- NROW(wbcd)
set.seed(218)                           ## fix random value
index <- sample(1:nrows, 0.7 * nrows)   ## shuffle and divide

#train <- wbcd                          ## 569 test data (100%)
train <- wbcd[index,]                   ## 398 test data (0.7)
test <- wbcd[-index,]                   ## 171 test data (0.3)

prop.table(table(train$diagnosis))
library(caret)
library(randomForest)
learn_rf <- randomForest(diagnosis~., data=train, ntree=500, proximity=T, importance=T)
pre_rf   <- predict(learn_rf, test[,-1])
cm_rf    <- confusionMatrix(pre_rf, test$diagnosis)
cm_rf

plot(learn_rf, main="Random Forest (Error Rate vs. Number of Trees)")
plot(margin(learn_rf,test$diagnosis))
