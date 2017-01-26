setwd("C:/CezarFiles/DeepLearningEssentials-R")
##source("checkpoint.R")
library(caret)

## read in data and explore the data...
digits.train = read.csv("train.csv")
dim(digits.train)
head(colnames(digits.train), 10)
tail(colnames(digits.train), 10)
head(digits.train[,1:4])

## Convert labels to factors for classification and seperating features and predictors...
digits.train$label = factor(digits.train$label, levels = 0:9)
digits.X = digits.train[1:5000, -1]
digits.y = digits.train[1:5000, 1]
barplot(table(digits.y))

## build a simple nnet using caret wrapper function...
set.seed(1234)
digits.m1 = train(x = digits.X, y = digits.y, method = "nnet", 
                  tuneGrid = expand.grid(.size = c(5), .decay = 0.1),
                  trControl = trainControl(method = "none"), MaxNWts = 10000,
                  maxit = 100)
digits.yhat1 = predict(digits.m1)
barplot(table(digits.yhat1))
caret::confusionMatrix(digits.y,digits.yhat1)

## rerun using increased number of hidden neurons...
set.seed(1234)
digits.m2 = train(x = digits.X, y = digits.y, method = "nnet", 
                  tuneGrid = expand.grid(.size = c(10), .decay = 0.1),
                  trControl = trainControl(method = "none"), MaxNWts = 50000,
                  maxit = 100)
digits.yhat1 = predict(digits.m1)
barplot(table(digits.yhat1))
caret::confusionMatrix(digits.y,digits.yhat1)