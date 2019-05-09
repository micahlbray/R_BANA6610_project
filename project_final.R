library(tidyverse) #readxl and ggplot2
library(GGally)
library(corrplot)
require(scales)
library(psych) # summarize data set
library(Hmisc) # summarize data set
library(fastDummies) # dummy variables
library(ltm) # biserial correlation for categoricals
library(DAAG) # k-folds
library(MASS) # stepAIC
library(leaps) # subsets regression
library(car) # plot subsets
library(bestglm) # subsets regression
library(rsq) # create r square estimate for glm
library(caret) # k-folds cross-validation
library(ggvis) # adjr2 viz

# Read in the training data
train = read.csv("train.csv"); train

#############################################################################
## Quick Summary
#############################################################################
dim(train) #1460 rows and 81 cols
colnames(train)
str(train)
psych::describe(train)

#############################################################################
## EDA - Nulls and Normality
#############################################################################
# check out Null Values
train.isna = as.data.frame(sapply(train, function(x) sum(is.na(x))))
names(train.isna)[names(train.isna) == "sapply(train, function(x) sum(is.na(x)))"] = "NullValues"
train.isna$Variable = rownames(train.isna)
rownames(train.isna) = c()
train.isna.bar = train.isna[train.isna$NullValues > 0, ]
# Plot Nulls
ggplot(train.isna.bar, aes(x = reorder(Variable, -NullValues, sum), y = NullValues)) + 
  geom_bar(stat = "identity") +
  ggtitle("Count of Null Values in Variables") +
  labs(x="Variable", y="NullValues") +
  scale_y_continuous(labels = comma) +
  theme(plot.title=element_text(size=16, hjust = 0.5),
        axis.title.x=element_text(size = 12, margin = margin(t = 10, r = 0, b = 10, l = 0)),
        axis.text.x  = element_text(angle = 90, vjust = 0.5))
# missing a lot of data from some variable and can probably just throw them out
# PoolQC, MiscFeature, Alley, Fence, FireplaceQu

#############################################################################
## Create new Variables
#############################################################################
train$YrBltAndRemod = train$YearBuilt + train$YearRemodAdd
train$TotalSF = train$TotalBsmtSF + train$X1stFlrSF + train$X2ndFlrSF
train$TotalSqrFt = (train$BsmtFinSF1 + train$BsmtFinSF2 +
                      train$X1stFlrSF + train$X2ndFlrSF)
train$TotalBathrooms = (train$FullBath + (0.5 * train$HalfBath) +
                          train$BsmtFullBath + (0.5 * train$BsmtHalfBath))
train$TotalPorchSF = (train$OpenPorchSF + train$X3SsnPorch + 
                        train$EnclosedPorch + train$ScreenPorch + 
                        train$WoodDeckSF)
# Bit variables
train = cbind(train, HasPool = sapply(train$PoolArea, function(x) if (x > 0) {1} else{0}))
train = cbind(train, Has2ndFloor = sapply(train$X2ndFlrSF, function(x) if (x > 0) {1} else{0}))
train = cbind(train, HasGarage = sapply(train$GarageArea, function(x) if (x > 0) {1} else{0}))
train = cbind(train, HasBsmt = sapply(train$TotalBsmtSF, function(x) if (x > 0) {1} else{0}))
train = cbind(train, HasFireplace = sapply(train$Fireplaces, function(x) if (x > 0) {1} else{0}))

#############################################################################
## Take a look at skewed data
#############################################################################
train.num = Filter(is.numeric, train) # only numeric variables
# Summary of normality by numeric variable
train.num.sum = as.data.frame(psych::describe(train.num, IQR = TRUE)); train.num.sum
# Summary of normality by categorical variable
Hmisc::describe(train.cat)

#############################################################################
## Replace NA value is numeric data
#############################################################################
# Replace NA values in data with 0
#cols.rep = c("BsmtFullBath", "BsmtUnfSF", "GarageYrBlt", "GarageArea", "GarageCars", "TotalBsmtSF")
cols.rep = names(train.num)
train[cols.rep][is.na(train[cols.rep])] = 0

#############################################################################
## Log transform skewed columns
#############################################################################
# right-skewed data that can be transformed via log or square root
right.skewed = rownames(train.num.sum[(train.num.sum$mean > train.num.sum$median) & abs(train.num.sum$skew) >= 1, ])
# left-skewed data 
left.skewed = rownames(train.num.sum[(train.num.sum$mean < train.num.sum$median) & abs(train.num.sum$skew) >= 1, ])

# Add logged variables to train data set
train_log = log(train[right.skewed])
names(train_log) = paste(names(train_log), "_log", sep = "")
train = cbind(train, train_log)
# Skewed data that could use log transform and removing the
# transforms on YearBuilt and YearRemodAdd, because they didn't add
# any value (the correlation was the same as the normal variable). 
# The data for YearRemodAdd seems bimodal and not sure what to do with it.

# clean up data where there was a log transformation on a column with a value of 0 within it
# GrLivArea_log
train$GrLivArea_log[which(is.nan(train$GrLivArea_log))] = NA
train$GrLivArea_log[which(train$GrLivArea_log == Inf)] = NA
train$GrLivArea_log[which(train$GrLivArea_log == -Inf)] = NA
# TotalBsmtSF_log
train$TotalBsmtSF_log[which(is.nan(train$TotalBsmtSF_log))] = NA
train$TotalBsmtSF_log[which(train$TotalBsmtSF_log == Inf)] = NA
train$TotalBsmtSF_log[which(train$TotalBsmtSF_log == -Inf)] = NA
# BsmtFinSF1_log
train$BsmtFinSF1_log[which(is.nan(train$BsmtFinSF1_log))] = NA
train$BsmtFinSF1_log[which(train$BsmtFinSF1_log == Inf)] = NA
train$BsmtFinSF1_log[which(train$BsmtFinSF1_log == -Inf)] = NA
# WoodDeckSF_log
train$WoodDeckSF_log[which(is.nan(train$WoodDeckSF_log))] = NA
train$WoodDeckSF_log[which(train$WoodDeckSF_log == Inf)] = NA
train$WoodDeckSF_log[which(train$WoodDeckSF_log == -Inf)] = NA
# X2ndFlrSF_log
train$X2ndFlrSF_log[which(is.nan(train$X2ndFlrSF_log))] = NA
train$X2ndFlrSF_log[which(train$X2ndFlrSF_log == Inf)] = NA
train$X2ndFlrSF_log[which(train$X2ndFlrSF_log == -Inf)] = NA

#############################################################################
## Separate data into Numerical and Categorical
#############################################################################
train.num = Filter(is.numeric, train) # only numeric variables

cols = colnames(train.num)
cols = names(train) %in% cols[!cols %in% c("SalePrice", "SalePrice_log")] 
train.cat = train[!cols] # not numeric values

#############################################################################
## Create Dummy Variables for Categorical
#############################################################################
# get dummy variables for each and remove first dummy for colinearity
train.cat.dummy = fastDummies::dummy_cols(train.cat, remove_first_dummy = TRUE)
cols = colnames(train.cat)
# remove all non-dummy columns, but keep SalePrice
cols = names(train.cat.dummy) %in% cols[!cols %in% c("SalePrice", "SalePrice_log")] 
train.cat.dummy = train.cat.dummy[!cols] # final dataframe for analysis
names(train.cat.dummy)
#############################################################################
## Histograms of SalesPrice
#############################################################################
# Graph by count
qplot(train$SalePrice,
      geom="histogram",
      #breaks=seq(34000, 755000, by = n),
      bins = 15,
      main = "Histogram for SalePrice", 
      xlab = "SalePrice",
      ylab = "Count",
      fill=I("black"), 
      col=I("gray")) +
  scale_x_continuous(labels = dollar, breaks=seq(0, 800000, 100000)) +
  theme(plot.title=element_text(size=16, hjust = 0.5),
        axis.title.y=element_text(size = 12, vjust = 0.2,
                                  margin = margin(t = 0, r = 20, b = 1, l = 0)),
        axis.title.x=element_text(size = 12, vjust = 0.2,
                                  margin = margin(t = 10, r = 0, b = 10, l = 5)),
        axis.text.y=element_text(size = 10),
        axis.text.x=element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# SalePrice is heavily right-skewed so let's see if we can normalize it with a log transformation
qplot(train$SalePrice_log,
      geom="histogram",
      #breaks=seq(34000, 755000, by = n),
      bins = 15,
      main = "Histogram for SalePrice_log", 
      xlab = "SalePrice",
      ylab = "Count",
      fill=I("black"), 
      col=I("gray")) +
  scale_x_continuous(labels = dollar) +
  theme(plot.title=element_text(size=16, hjust = 0.5),
        axis.title.y=element_text(size = 12, vjust = 0.2,
                                  margin = margin(t = 0, r = 20, b = 1, l = 0)),
        axis.title.x=element_text(size = 12, vjust = 0.2,
                                  margin = margin(t = 10, r = 0, b = 10, l = 5)),
        axis.text.y=element_text(size = 10),
        axis.text.x=element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
