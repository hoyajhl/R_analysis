getwd()
setwd("C:/Kaggle/heart")
heart_df <- read.csv('heart.csv',encoding="UTF-8")
head(heart_df, n = 4) # Displaying the first 4 rows

###EDA
# Checking the dimension and structure of our dataset
str(heart_df) #303 obs, 14 variables

# Summary of the heart_df
summary(heart_df)
# getting a copy of the dataframe
heart <- heart_df

# Making afew changes to some of the variables that seem categorical in  nature.
#sex column 
heart_df$sex[heart_df$sex == 0] = "female"
heart_df$sex[heart_df$sex == 1] = "male"

#cp 
heart_df$cp[heart_df$cp==0] = "typical angina"
heart_df$cp[heart_df$cp==1] = "atypical angina"
heart_df$cp[heart_df$cp==2] = "non-anginal pain"
heart_df$cp[heart_df$cp==3] = "asymptotic"

#fbs
heart_df$fbs[heart_df$fbs==0] = "false"
heart_df$fbs[heart_df$fbs==1] = "true"

#exang
heart_df$exang[heart_df$exang == 1] = "yes"
heart_df$exang[heart_df$exang == 0] = "no"

#restecg
heart_df$restecg[heart_df$restecg==0] = "Nothing to note"
heart_df$restecg[heart_df$restecg==1] = "ST-T Wave abnormality"
heart_df$restecg[heart_df$restecg==2] = " Definite left ventricular hypertrophy"

#slope
heart_df$slope[heart_df$slope == 0] = "upsloping"
heart_df$slope[heart_df$slope == 1] = "flat"
heart_df$slope[heart_df$slope == 2] = "downsloping"

#thal
heart_df$thal[heart_df$thal == 1] = "normal"
heart_df$thal[heart_df$thal == 2] = "fixed defect"
heart_df$thal[heart_df$thal == 3] = "reversible defect"
head(heart_df, n = 6)

#counting the total number of missing values in the data
install.packages("naniar")
library(naniar)
n_miss(heart_df) # missing values:0

# converting some variables to factors
# categorical values into factor
cols <- c('sex', 'cp', 'fbs', 'restecg', 'exang', 'slope', 'ca', 'thal', 'target')
for (col in cols) {
  heart_df[, col] <- as.factor(heart_df[, col])
}
str(heart_df)
dim(heart_df)
library(dplyr)
heart_df <- distinct(heart_df) ## removing duplicate rows
dim(heart_df) ##One observation was dropped from detector
lapply(heart_df, levels) # checking the levels of variables in cols

# Dropping  outliers by setting a cap on ca and thal 
heart_df <-heart_df %>%
  filter(ca != 4 & thal != 0) %>%
  droplevels()

dim(heart_df)

#confirming the levels of ca and thal
levels(heart_df$ca)
levels(heart_df$thal)

# creating a two-way contingency table
table(heart_df$cp, heart_df$sex)
## Interpretation:
## There are more male than female who have typical angina
## chest pain related decrease blood supply to the heart.
## Overall, the male dominated in all the category of chest pain

##Data visualization for categorical values
library(ggthemes)
heart_df %>% 
  group_by(sex) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(sex, n, fill = sex)) +
  geom_col() + 
  geom_text(aes(label = n), position = position_dodge(width = 1), vjust = 0.25)+
  theme_economist()

library(ggrepel)
heart_df %>% 
  group_by(cp, sex) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = reorder(cp, -n), y = n, fill = sex)) +
  geom_col(position = 'dodge') + 
  geom_label_repel(aes(label = n)) + 
  labs(x = "chest pain type") + 
  theme_economist()

tab1 <- table(heart_df$cp, heart_df$sex)
tab1
options(scipen = 999, digits = 3) # Print fewer digits
prop.table(tab1, 2)  # 2 means Conditional on columns

#visualizing the numerical variables 
#Histogram with a density plot
heart_df %>% 
  keep(is.numeric) %>%  # keeping only numerical column
  gather() %>%          # Convert to key-value pair
  ggplot(aes(value)) +    # plot the values
  facet_wrap(~key, scale = "free") + # in separate panel
  geom_histogram(aes(y = ..density..), color = "darkblue", fill = "lightblue") +
  geom_density(alpha = 0.2)# + theme_economist()


#boxplot
heart_df %>% 
  keep(is.numeric) %>%  # keeping only numerical column
  gather() %>%          # Convert to key-value pair
  ggplot(aes(value)) +    # plot the values
  facet_wrap(~key, scale = "free") + # in separate panel
  geom_boxplot() + theme_economist()

# keeping only numerical column
numdata <- heart_df %>% 
  keep(is.numeric)  

# Listing all the outliers from the boxplot above
all_outliers <- boxplot(numdata, plot=FALSE)$out
all_outliers

outlier1 <- boxplot(heart_df$chol, plot=FALSE)$out
outlier1
outlier2 <- boxplot(heart_df$oldpeak, plot=FALSE)$out
outlier2
outlier3 <- boxplot(heart_df$thalach, plot=FALSE)$out
outlier3
outlier4 <- boxplot(heart_df$trestbps, plot=FALSE)$out
outlier4

# Now we can remove the rows containing the outliers
heart_df <- heart_df[-which(heart_df$chol %in% outlier1),]
heart_df <- heart_df[-which(heart_df$oldpeak %in% outlier2),]
heart_df <- heart_df[-which(heart_df$thalach %in% outlier3),]
heart_df <- heart_df[-which(heart_df$trestbps %in% outlier4),]

#There's a pairs() function which plots the numeric  variables in heart_df into a scatterplot matrix. 
#In this case, target, your binary response, is the color indicator:
pairs(heart_df[, c(1,4,5,8,10)], col =heart_df$target)


#loading the corrplot package
library(corrplot)
hd <- cor(heart) # heart being the copy of heart_df before pre-processing stage.
corrplot(hd, method = "color", addCoef.col = "white")


##Using Raw data from heart.csv
## DATA visualization 

##Relationship between ca and Target using geom_smooth plot 
library(ggplot2)
ggplot(heart,aes(x=ca,y=target))+geom_point()+geom_smooth(color="red")+
  scale_x_continuous(name="ca")+
  scale_y_continuous(name="Target")+
  ggtitle("Relationship between ca and Target")

##Relationship between Age and target with Chest Pain category
##represented by geom_smooth plot
library(ggplot2)
e=ggplot(heart,aes(x=X.U.FEFF.age,y=target))+
  geom_point(aes(shape=as.factor(cp),color=cp))+
  geom_smooth()
str(heart)
e
e+scale_x_continuous(name="Age")+
  scale_y_continuous(name="Probability of Heart Attack")+
  ggtitle("Age and target with Chest Pain category")


##Rest Blood pressure Analysis
summary(heart$trestbps)
cor(heart$trestbps,heart$X.U.FEFF.age)# 0.279
boxplot(heart$trestbps,
        col="pink",
        main="Descriptive Analysis of RBP",
        horizontal=TRUE) # Descriptive analysis using boxplot
hist(heart$trestbps,col=rainbow(7),
     main="Histogram for RBP",
     xlab="Rest Blood Pressure Class",
     ylab="Frequency",
     labels=TRUE) # Histogram Frequency of Rest BP

library(ggplot2)
f=ggplot(heart,aes(x=trestbps,y=target))+geom_point(shape=1)+geom_smooth()
f+scale_x_continuous(name="Rest Blood Pressure")+
  scale_y_continuous(name="Target")+
  ggtitle("Smooth Curve Between RBP and Target")

##Cholesterol Analysis
summary(heart$chol)
boxplot(heart$chol~as.factor(heart$sex),
        col=rainbow(4),
        main="Descriptive Analysis of Chol with Male and Female",
        xlab="Female or Male",
        ylab="Cholestrol") # Descriptive analysis using boxplot

hist(heart$chol,
     main="Histogram of Cholestrol",
     xlab="Cholestrol Class",
     ylab="Frequency",
     col=rainbow(7),labels=TRUE)
heart$sex=as.factor(heart$sex) # Histogram Frequency of Cholesterol

library(ggplot2)
ggplot(heart,aes(x=chol,y=target))+geom_point(aes(shape=as.factor(sex),color=sex))+geom_smooth()+
  ggtitle("Smooth Curve b/w Chol and Target with Factor Sex")+
  scale_x_continuous(name="Cholestrol Level")+
  scale_y_continuous(name="Target")

##CA analysis 
class(heart$ca) #integer
head(heart$ca,10)
tail(heart$ca,10)
unique(heart$ca) #0 2 1 3 4

n=table(heart$ca)
barplot(n,
        main="Barplot of ca",
        xlab="ca",
        col=rainbow(5),
        legend=rownames(n),
        ylab="Count")
#Most of population are with 0 ca category from barplot analysis
o=table(heart$target,heart$ca)
barplot(o,
        main="Stacked Barplot of ca and Target",
        xlab="ca",
        col=rainbow(2),
        legend=rownames(o),
        ylab="Count")
#People with ca category 0 are more likely to get heart attack from stacked barplot

#Relationship between ca and target
library(ggplot2)
ggplot(heart,aes(x=ca,y=target))+geom_point()+geom_smooth(color="blue")+
  scale_x_continuous(name="ca")+
  scale_y_continuous(name="Target")+
  ggtitle("Relationship between ca and Target")
#There is local minima at ca category 2
#First on increasing ca prob. of heart attack is decreasing again 
#after local minima on increasing ca, 
#prob of heart attack is keep increasing from smooth curve
