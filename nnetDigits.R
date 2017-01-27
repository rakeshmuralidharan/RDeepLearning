setwd("C:/CezarFiles/DeepLearningEssentials-R")
##source("checkpoint.R")
library(caret)
library(RSNNS)

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
digits.yhat2 = predict(digits.m2)
barplot(table(digits.yhat2))
caret::confusionMatrix(digits.y,digits.yhat2)

## increasing hidden neurons to 40...
set.seed(1234)
digits.m3 = train(x = digits.X, y = digits.y, method = "nnet",
                  tuneGrid = expand.grid(.size = c(40), .decay = 0.1),
                  trControl = trainControl(method = "none"), MaxNWts = 50000,
                  maxit = 100)
digits.yhat3 = predict(digits.m3)
barplot(table(digits.yhat3))
caret::confusionMatrix(digits.y,digits.yhat3)

## Using SNNS - Stuttgart Neural Networks Simulator RSNNS...
set.seed(1234)
digits.m4 = mlp(as.matrix(digits.X), decodeClassLabels(digits.y),
                size = 40, learnFunc = "Rprop", shufflePatterns = FALSE,
                maxit = 100)
digits.yhat4 = fitted.values(digits.m4)
digits.yhat4 = encodeClassLabels(digits.yhat4)
barplot(table(digits.yhat4))
caret::confusionMatrix(xtabs(~ I(digits.yhat4 -1) + digits.y))

## Raw probabilities for in-sample data and its impact with different choices of values...
digits.yhat4.insample = fitted.values(digits.m4)
head(round(digits.yhat4.insample, 2))
table(encodeClassLabels(digits.yhat4.insample, method = "WTA", l = 0, h = 0.5))
table(encodeClassLabels(digits.yhat4.insample, method = "WTA", l = 0, h = 0))
table(encodeClassLabels(digits.yhat4.insample, method = "WTA", l = 0.4, h = 0.6))

## Prediction using out of sample data...
i2 = 5001:10000
digits.yhat4.pred = predict(digits.m4, as.matrix(digits.train[i2, -1]))
table(encodeClassLabels(digits.yhat4.pred, method = "WTA", l = 0, h = 0))
digits.yhat4.pred = encodeClassLabels(digits.yhat4.pred)
caret::confusionMatrix(xtabs(~ I(digits.yhat4.pred -1) + digits.train[i2,1]))
