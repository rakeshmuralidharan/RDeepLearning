## Testing h20 cluster in R...
library(h2o)
setwd("C:/CezarFiles/DeepLearningEssentials-R")

## Initializing H2O...
h2o.init(nthreads = 2, max_mem_size = "3G")
##h2o.removeAll() ## close all threads

## Linking datasets to h2o from within R...
h2oiris = as.h2o(droplevels(iris[1:100,]))
h2o.levels(h2oiris, 5)

## reading file from machine location directly to h2o...
write.csv(mtcars, file = "mtcars.csv")
h2omtcars = h2o.importFile(path = "C:/CezarFiles/DeepLearningEssentials-R/mtcars.csv")
h2omtcars

## reading data from a url to h2o...
h2obin = h2o.importFile(path = "http://www.ats.ucla.edu/stat/data/binary.csv")
h2obin
