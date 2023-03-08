library(tidyverse)
library(caret)
library(gains)
library(pROC)
library(car)
library(klaR)

suppressWarnings(RNGversion("3.5.3"))


setwd("~/Desktop/Winter 2023/GSB 530/Group Project")

df <- read_excel("Big_Data_Files.xlsx", sheet = "COVID_Testing")
df <- na.omit(df)

#Clean
df$Age_60_And_Above <- ifelse(df$Age_60_And_Above == "Yes", 1, 0)
df$Male <- ifelse(df$Sex == "male", 1, 0)
df$Positive <- ifelse(df$Result == "positive", 1, 0)
df$Sex <- NULL
df$Result <- NULL

#Basic logistic regression
model1 <- glm(Positive ~ Cough + Fever + Sore_Throat + Shortness_Of_Breath + Headache + Age_60_And_Above + Contact + Male + Positive , data = df, family = "binomial")
summary(model1)

#male 
male_df <- df[df$Male == 1, ]
model2 <- glm(Positive ~ Cough + Fever + Sore_Throat + Shortness_Of_Breath + Headache + Age_60_And_Above + Contact + Positive , data = male_df, family = "binomial")
summary(model2)

########################################################################################################################################
#KNN Analysis
#############

#Standardize the predictor variables into z-scores which are unitless measures
df_knn<- scale(df[1:8])

#Add target variable (y) back into data se
df_knn<- data.frame(df_knn, df$Positive)
colnames(df_knn)[9] <- 'Positive'

#Convert target variable (y) from numerical variable to categorical variable
df_knn$Positve<- as.factor(df_knn$Positive)

#Create Partition 

set.seed(1)
myIndex<- createDataPartition(df_knn$Positive, p=0.6, list=FALSE)
trainSet <- df_knn[myIndex,]
validationSet <- df_knn[-myIndex,]
myCtrl <- trainControl(method="cv", number=10)
myGrid <- expand.grid(.k=c(1:10))
set.seed(1)
KNN_fit <- train(Positive ~ ., data=trainSet, method = "knn", trControl=myCtrl, tuneGrid = myGrid)
KNN_fit


#a. Perform KNN analysis. What is the optimal value of k? Enter the optimal k in the box below:
best_k <- KNN_fit$bestTune$k
best_k


#b-1. Report the accuracy, specificity, sensitivity, and precision rates for the validation data set.
KNN_Class <- predict(KNN_fit, newdata = validationSet)
confusion_matrix <- confusionMatrix(KNN_Class, validationSet$y, positive = '1')
confusion_matrix
precision <- confusion_matrix$byClass['Pos Pred Value']  
precision

########################################################################################################################################





