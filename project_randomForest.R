library(tidyverse)
library(rsample)      # data splitting 
library(randomForest) # basic implementation
library(ranger)       # a faster implementation of randomForest
library(caret)        # an aggregator package for performing many machine learning models
library(h2o)          # an extremely fast java-based platform

# Read in the training data
train = read.csv("train.csv"); train
train[1,]
# Impute NA values for RF to run
set.seed(123)
train.imputed = rfImpute(SalePrice ~ ., train)
# Create model
set.seed(123)
train.rf = randomForest(SalePrice ~ ., train.imputed)
print(train.rf)
# number of trees with lowest MSE
which.min(train.rf$mse)
# RMSE of this optimal random forest
sqrt(train.rf$mse[which.min(train.rf$mse)])



# names of features
set.seed(123)
train.imputed = rfImpute(SalePrice ~ ., train)
cols = setdiff(names(train.imputed), "SalePrice")
set.seed(123)
train.rf2 <- tuneRF(x = train[cols], 
                    y = train$SalePrice,
                    ntreeTry = 500,
                    mtryStart = 5,
                    stepFactor = 1.5,
                    improve = 0.01,
                    trace = FALSE) # to not show real-time progress
