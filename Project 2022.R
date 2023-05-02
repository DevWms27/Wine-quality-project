library(tidyverse)
library(ggplot2)
library(corrplot)
library(caTools)

#reading in data

winebest <- read.table("C:/Users/devin/Documents/Working D for R script/winequality-red.csv", header = TRUE, sep = ",")
#checking for null values
#colSums(is.na(wine))
Num.cols <- sapply(winebest, is.numeric)
Cor.data <- cor(winebest[, Num.cols])
str(winebest)
corrplot(Cor.data, method = 'color') 

#cor(wine)


#alcohol, sulphates, citric acid, volatile acidity


# condensing data to four hightest correlated variables
#winebest <- wine[,c(2,3,10:12)]
head(winebest)
#rm(wine)

#summary of each variable
summary(winebest)
attach(winebest)

pairs(winebest)

###ANALYSING Distribution

boxplot(winebest$volatile.acidity)
#High 1 Low 0.1

boxplot(winebest$citric.acid)
#high 0.8 Low 0.0

boxplot(winebest$sulphates)
#high 0.9 Low 0.2

boxplot(winebest$alcohol)
#High 13.5 Low 8.5

boxplot(winebest$quality)
#high 7 low 4


##### Multiple Linear Model with interaction between citric acid and sulphates, and alcohol

model1 <- lm(quality ~ citric.acid +sulphates + alcohol, data = winebest)

summary(model1)

##### Regression with volatile acid, alcohol, and interaction between sulphates and volatile acid

model2 <- lm(quality ~ volatile.acidity + alcohol+ sulphates, data = winebest)

summary(model2)

### regression using all variables
ALLWineregression = lm(formula = quality~.,data = winebest)

summary(ALLWineregression)

### testing test model

set.seed(33)

wine_split<- sample(2,nrow(winebest), replace = TRUE, prob = c(.7,.3))
wine_train<- winebest[wine_split == 1,]
wine_test<- winebest[wine_split == 2,]

### prediction using test data
ALLWineReg_predict <- predict(ALLWineregression, newdata = wine_test)

model1_predict <- predict(model1, newdata = wine_test)

model2_predict <- predict(model2, newdata = wine_test)


tab_model2predict <- data.frame(True = wine_test$quality, Quality = model2_predict)

head(tab_model2predict,20)

tab_model1predict <- data.frame(True = wine_test$quality, Quality = model1_predict)

head(tab_model1predict,20)


tab_AllWineReg_predict <- data.frame(True = wine_test$quality, Quality = ALLWineReg_predict)

head(tab_AllWineReg_predict,20)

### prediction using training data
ALLWineReg_predicttrain <- predict(ALLWineregression, newdata = wine_train)

model1_predicttrain <- predict(model1, newdata = wine_train)

model2_predicttrain <- predict(model2, newdata = wine_train)



summary(model1)
RMSETestModel1<- sqrt(mean((wine_test$quality - model1_predict)^2))
print(RMSETestModel1)

RMSETestModel2<- sqrt(mean((wine_test$quality - model2_predict)^2))
print(RMSETestModel2)

RMSETestModelAll<- sqrt(mean((wine_test$quality - ALLWineReg_predict)^2))
print(RMSETestModelAll)



RMSETrainModel1<- sqrt(mean((wine_train$quality - model1_predicttrain)^2))
print(RMSETrainModel1)

RMSETrainModel2<- sqrt(mean((wine_train$quality - model2_predicttrain)^2))
print(RMSETrainModel2)

RMSETrainModelAll<- sqrt(mean((wine_train$quality - ALLWineReg_predicttrain)^2))
print(RMSETrainModelAll)

