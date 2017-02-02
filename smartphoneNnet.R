
## Read in the data...
uci.train.x = read.table("C:/CezarFiles/DeepLearningEssentials-R/UCI HAR Dataset/train/X_train.txt")
uci.train.y = read.table("C:/CezarFiles/DeepLearningEssentials-R/UCI HAR Dataset/train/y_train.txt")[[1]]

uci.test.x = read.table("C:/CezarFiles/DeepLearningEssentials-R/UCI HAR Dataset/test/X_test.txt")
uci.test.y = read.table("C:/CezarFiles/DeepLearningEssentials-R/UCI HAR Dataset/test/y_test.txt")[[1]]

uci.labels = read.table("C:/CezarFiles/DeepLearningEssentials-R/UCI HAR Dataset/activity_labels.txt")
barplot(table(uci.train.y))

## Choose tuning parameters...
tuning = list(size = c(40, 20, 20, 50, 50),
              maxit = c(60, 100, 100, 100, 100),
              shuffle = c(FALSE, FALSE, TRUE, FALSE, FALSE),
              params = list(FALSE, FALSE, FALSE, FALSE, c(0.1, 20, 3)))

## Setting up clusters with packages and all files to be used foreach package 
## for running the for loops in parallel...
c1 = makeCluster(5)
clusterEvalQ(c1, {library(RSNNS)})
clusterExport(c1,
              c("tuning", "uci.train.x", "uci.train.y", "uci.test.x", "uci.test.y"))
registerDoSNOW(c1)

## Running the models in parallel...
uci.models = foreach(i = 1:5, .combine = 'c') %dopar% {
  if(tuning$params[[i]][1]) {
    set.seed(1234)
    list(Model = mlp(as.matrix(uci.train.x), decodeClassLabels(uci.train.y),
                     size = tuning$size[[i]], lernFunc = "Rprop",
                     shufflePatterns = tuning$shuffle[[i]], 
                     learnFuncParams = tuning$params[[i]],
                     maxit = tuning$maxit[[i]]
         ))
  }
  else {
    set.seed(1234)
    list(Model = mlp(as.matrix(uci.train.x), decodeClassLabels(uci.train.y),
                     size = tuning$size[[i]], lernFunc = "Rprop",
                     shufflePatterns = tuning$shuffle[[i]],
                     maxit = tuning$maxit[[i]]
         ))
  }
}

## Out of sample validation using test data...also run in parallel...
clusterExport(c1, "uci.models")
uci.yhat <- foreach(i = 1:5, .combine = 'c') %dopar% {
  list(list(
    Insample = encodeClassLabels(fitted.values(uci.models[[i]])),
    Outsample = encodeClassLabels(predict(uci.models[[i]],
                                          newdata = as.matrix(uci.test.x)))
  ))
}

## Merge all data and predictions for performance evaluation...
uci.insample <- cbind(Y = uci.train.y,
                      do.call(cbind.data.frame, lapply(uci.yhat, `[[`, "Insample")))
colnames(uci.insample) <- c("Y", paste0("Yhat", 1:5))

performance.insample <- do.call(rbind, lapply(1:5, function(i) {
  f <- substitute(~ Y + x, list(x = as.name(paste0("Yhat", i))))
  uci.dat <- uci.insample[uci.insample[,paste0("Yhat", i)] != 0, ]
  uci.dat$Y <- factor(uci.dat$Y, levels = 1:6)
  uci.dat[, paste0("Yhat", i)] <- factor(uci.dat[, paste0("Yhat", i)], levels = 1:6)
  res <- caret::confusionMatrix(xtabs(f, data = uci.dat))
  
  cbind(Size = tuning$size[[i]],
        Maxit = tuning$maxit[[i]],
        Shuffle = tuning$shuffle[[i]],
        as.data.frame(t(res$overall[c("AccuracyNull", "Accuracy", "AccuracyLower", "AccuracyUpper")])))
}))
