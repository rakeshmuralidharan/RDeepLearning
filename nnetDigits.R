setwd("C:/CezarFiles/DeepLearningEssentials-R")
source("checkpoint.R")

## read in data and explore the data...
digits.train = read.csv("train.csv")
dim(digits.train)
head(colnames(digits.train), 10)
