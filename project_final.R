library(tidyverse) #readxl and ggplot2
library(GGally)
library(corrplot)
require(scales)
library(psych) # summarize data set
library(Hmisc) # summarize data set
#install.packages("devtools")
#devtools::install_github("jacobkap/fastDummies") #newest version of fastDummies
#devtools::install_github("taiyun/corrplot", build_vignettes = TRUE)
library(corrplot)
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
train.log = log(train[right.skewed])
names(train.log) = paste(names(train.log), "_log", sep = "")
# Skewed data that could use log transform and removing the
# transforms on YearBuilt and YearRemodAdd, because they didn't add
# any value (the correlation was the same as the normal variable). 
# The data for YearRemodAdd seems bimodal and not sure what to do with it.

# clean up data where there was a log transformation on a column with a value of 0 within it
train.log.sum = as.data.frame(psych::describe(train.log))
cols = rownames(train.log.sum[train.log.sum$range == "Inf", ])
train.log[cols][is.na(train.log[cols])] = 0
train.log[cols][which(train.log[cols] == Inf)] = NA
train.log[train.log==-Inf] = NA

train = cbind(train, train.log)
#############################################################################
## Separate data into Numerical and Categorical
#############################################################################
train.num = Filter(is.numeric, train) # only numeric variables

cols = colnames(train.num)
cols = names(train) %in% cols[!cols %in% c("SalesPrice", "SalePrice_log")]
train.cat = train[!cols] # not numeric values

#############################################################################
## Correlation - Categorical
#############################################################################
cols = colnames(train.num)
cols = names(train) %in% cols[!cols %in% "SalePrice_log"] # not numeric values
train.cat = train[!cols]
# get dummy variables for each and remove first dummy for colinearity
train.cat.dummy = fastDummies::dummy_cols(train.cat, remove_first_dummy = TRUE)
cols = colnames(train.cat)
# remove all non-dummy columns, but keep SalePrice
cols = names(train.cat.dummy) %in% cols[!cols %in% "SalePrice_log"] 
train.cat.dummy = train.cat.dummy[!cols] # final dataframe for analysis

# DataFrame of just correlation with SalesPrice
df.corr.SP.cat = data.frame(cor(train.cat.dummy$SalePrice_log, train.cat.dummy))
# Get absolute value of correlation
df.corr.SP.cat = sort(abs(df.corr.SP.cat), decreasing = TRUE)
# Look at top 15 correlated variables
cols = colnames(df.corr.SP.cat)[0:15]
train.cat.sctr = train.cat.dummy[cols]
# Since the top 6 correlated variables are related to specific areas and their quality
# and neither of them has a correlation above 0.59, we can leverage the highly correlated
# numerical variable "OverallQual" to summarize these less important categoricals
# Graph a scatter and correlation matrix together
ggpairs(train.cat.sctr,
        upper = list(continuous = wrap("cor", size = 4)),
        title = "Scatter Matrix") +
  theme(plot.title=element_text(size=10, hjust = 0.5),
        axis.title.y=element_text(size = 5, vjust = 0.2),
        axis.title.x=element_text(size = 5, vjust = 0.2),
        axis.text.y=element_text(size = 6),
        axis.text.x=element_text(size = 7),
        strip.text.y=element_text(size = 5, face = "bold"),
        strip.text.x=element_text(size = 5, face = "bold"),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank())

# Visualize Correlation Matrix in Heatmap
train.cat.cor = cor(train.cat)
corrplot(train.num.cor, method = "color", diag = FALSE) # Display the correlation coefficient


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

# SalePrice is heavily right-skewed so we will probably use the log transformation
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

#############################################################################
## Explore Outliers and Finalize Data
#############################################################################
# outliers to solve for and remove outliers
boxplot(train$OverallQual, main='OverallQual Plot', ylab='OverallQual')
boxplot(train$TotalSF_log, main='TotalSF_log Plot', ylab='TotalSF_log')
boxplot(train$LotArea_log, main='LotArea_log Plot', ylab='LotArea_log')
boxplot(train$YrBltAndRemod, main='YrBltAndRemod Plot', ylab='YrBltAndRemod')
boxplot(train$TotalPorchSF, main='TotalPorchSF Plot', ylab='TotalPorchSF')
boxplot(train$Fireplaces, main='Fireplaces Plot', ylab='Fireplaces')
boxplot(train$BsmtUnfSF, main='BsmtUnfSF Plot', ylab='BsmtUnfSF')
boxplot(train$GarageCars, main='GarageCars Plot', ylab='GarageCars')


outs.lotarea = sort(boxplot(train$LotArea_log, plot=FALSE)$out, decreasing = TRUE); outs.lotarea
outs.lotarea.frst10 = outs.lotarea[0:10] # remove top 10 outliers
n = length(outs.lotarea) - 10
outs.lotarea.lst10 = outs.lotarea[n:length(outs.lotarea)] # remove last 10 outliers
#Remove from data
train = train[-which(train$LotArea_log %in% outs.lotarea.frst10),]
train = train[-which(train$LotArea_log %in% outs.lotarea.lst10),]

#############################################################################
## Correlation
#############################################################################
# This will show what variables correlate with SalesPrice 
# and what could potentially be colinear if the variables correlate strongly
# with each other

# Get subset of numericals without Id
train.num = Filter(is.numeric, train) # only numeric variables
cols = names(train.num) %in% c("Id","SalePrice") # logical to see if included
train.num.notId = train.num[!cols]

# DataFrame of just correlation with SalesPrice
df.corr.SP = data.frame(cor(train.num.notId$SalePrice_log, train.num.notId))
# Get absolute value of correlation
df.corr.SP = sort(abs(df.corr.SP), decreasing = TRUE)
# Look at top 15 correlated variables
cols = colnames(df.corr.SP)[0:20] # look at top 20 correlated variables
train.sctr = train.num[cols]
# Graph a scatter and correlation matrix together
ggpairs(train.sctr,
        upper = list(continuous = wrap("cor", size = 3)),
        title = "Scatter Matrix") +
  theme(plot.title=element_text(size=10, hjust = 0.5),
        axis.title.y=element_text(size = 5, vjust = 0.2),
        axis.title.x=element_text(size = 5, vjust = 0.2),
        axis.text.y=element_text(size = 6),
        axis.text.x=element_text(size = 7),
        strip.text.y=element_text(size = 5, face = "bold"),
        strip.text.x=element_text(size = 6, face = "bold"),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) # dev.off() run this command in case plot sticks

# Visualize Correlation Matrix in Heatmap
cols = colnames(df.corr.SP)[0:30] # look at top 30 correlated variables
train.cor = cor(train.num.notId[cols])
corrplot(train.cor, method = "color", diag = FALSE) # Display the correlation coefficient

# Visualize Correlation Matrix
p.mat = cor.mtest(train.num.notId[cols])$p
res = cor.mtest(train.num.notId[cols], conf.level = .95)
col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(train.cor, method = "color", col = col(200),
         type = "upper", number.cex = 0.45, tl.cex = 0.6, #order = "AOE", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         diag = FALSE)

#############################################################################
## Subsetting
#############################################################################


#### Subsets Regression Method
cols = colnames(df.corr.SP)[0:35]
##### Remove columns that cause colinearity
# TotalSF_log --> "TotalSF", "TotalSqrFt_log", "TotalSqrFt", "TotalBsmtSF", "X1stFlrSF_log", "X1stFlrSF"
# GrLivArea --> "TotalRmsAbvGrnd", "X2ndFlrSF"
# YrBltAndRemod --> "YearBuilt", "YearRemodAdd"
notCols = c("TotalSF", "TotalSqrFt_log", "TotalSqrFt", "TotalBsmtSF", "X1stFlrSF_log", "X1stFlrSF", "YearBuilt", "YearRemodAdd", "TotalRmsAbvGrnd", "BsmtFinSF1", "X2ndFlrSF")
cols = cols[!cols %in% notCols]
train.sub = train[cols]
subs = regsubsets(SalePrice_log ~ ., 
                  data = train.sub, nbest=1)
# Plot a table of models showing variables in each model.
subs.plot.bar = plot(subs, scale = "adjr2", main = "Adjusted R^2")
# Visualize subset size to hit statistic
subs.plot.leg = subsets(subs, statistic="adjr2", legend = FALSE, min.size = 4, max.size = 9, main = "Adjusted R^2")
# Arcing point graph
subs.sum = summary(subs)
subs.sum.df.adjr2 = as.data.frame(subs.sum$adjr2)
names(subs.sum.df.adjr2) = "R2"
# Line graph
ggplot(subs.sum.df.adjr2, 
       aes(x = seq(1, nrow(subs.sum.df.adjr2), by = 1), 
           y = subs.sum.df.adjr2$R2)) +
  geom_line(aes(group=1)) +
  geom_point(size=3) +
  #geom_text(aes(label=Name),hjust=0, vjust=0)
  ggtitle("Best Subsets by Adjusted R^2") +
  labs(x="Subset Size", y="Adjusted R^2") +
  scale_x_continuous(breaks=seq(0, 10, 1)) + 
  theme(plot.title=element_text(size=16, face = "bold", hjust = 0.5),
        axis.title.y=element_text(size = 12, face = "bold", vjust = 0.2,
                                  margin = margin(t = 0, r = 20, b = 1, l = 0)),
        axis.title.x=element_text(size = 12, face = "bold", vjust = 0.2,
                                  margin = margin(t = 10, r = 0, b = 10, l = 5)),
        axis.text.y=element_text(size = 10),
        axis.text.x=element_text(size = 10))


#############################################################################
#### Use Leaps
# Structure data
# Remove SP
cols = names(train.sub) %in% c("SalePrice_log") # logical to see if included
train.sub.notSP = train.sub[!cols]
# Create leap object
leap = leaps(x = train.sub.notSP, y = train.sub$SalePrice_log, 
             method = "adjr2", nbest = 1,
             name = names(train.sub.notSP))
# Create df for graphing
vars = as.data.frame(leap$which)
adjr2 = leap$adjr2
sz = leap$size - 1
#lbl = leap$label
leaps.sum.df.adjr2 = data.frame(size = sz,
                                adjr2 = adjr2)
# Combine data frames
leaps.sum.df = cbind(leaps.sum.df.adjr2, vars)
# Line graph
ggplot(leaps.sum.df, 
       aes(x = leaps.sum.df$size, y = leaps.sum.df$adjr2)) +
  geom_line(aes(group=1)) +
  geom_point(size=3) +
  #geom_text(aes(label=Name),hjust=0, vjust=0)
  ggtitle("Best Subsets by Adjusted R^2") +
  labs(x="Subset Size", y="Adjusted R^2") +
  scale_x_continuous(breaks=seq(1, 30, 1)) + 
  theme(plot.title=element_text(size=16, face = "bold", hjust = 0.5),
        axis.title.y=element_text(size = 12, face = "bold", vjust = 0.2,
                                  margin = margin(t = 0, r = 20, b = 1, l = 0)),
        axis.title.x=element_text(size = 12, face = "bold", vjust = 0.2,
                                  margin = margin(t = 10, r = 0, b = 10, l = 5)),
        axis.text.y=element_text(size = 10),
        axis.text.x=element_text(size = 10))

# Best models 
leaps.sum.df
write.csv(leaps.sum.df, 
          "C:/Users/micah/Dropbox/Grad School/CU Denver/BANA_6610/project/subset.csv", 
          row.names = FALSE)


#############################################################################
#### bestglm Subsets Regression Method
train.best = cbind(train.sub.notSP, train.sub$SalePrice_log)
train.bestglm = bestglm(Xy = train.best, 
                        family = gaussian,
                        IC = "AIC", # Information criteria for
                        method = "exhaustive")
# Best models
train.bestglm$BestModels
# Summary of best model
summary(train.bestglm$BestModel)

#############################################################################
## MODEL - Final
#############################################################################
model.1 = lm(SalePrice_log ~ 
             OverallQual+TotalSF_log+GrLivArea_log+GarageCars+YrBltAndRemod+
             Fireplaces+LotArea_log+TotalPorchSF+BsmtUnfSF, 
           data = train)
summary(model.1) #adjr2 of 0.866
model.2 = lm(SalePrice_log ~ 
             OverallQual+GarageCars+
             TotalBsmtSF+X1stFlrSF_log+YearBuilt+YearRemodAdd+
             Fireplaces+WoodDeckSF+X2ndFlrSF+LotArea+BsmtFullBath+
             BsmtUnfSF+KitchenAbvGr+ScreenPorch+MSSubClass+PoolArea, 
           data = train)
summary(model.2) #adjr2 of 0.8628
model.3 = lm(SalePrice_log ~ 
               OverallQual+TotalSF_log+GarageCars+YrBltAndRemod+
               Fireplaces+LotArea_log+TotalPorchSF+BsmtUnfSF, 
             data = train)
summary(model.3) #adjr2 of 0.8646

#############################################################################
# K-fold cross-validation
train_Control = trainControl(method = "cv", number = 10, savePredictions = TRUE) # number of folds
model.model.1 = train(SalePrice_log ~ 
                        OverallQual+TotalSF_log+GrLivArea_log+GarageCars+YrBltAndRemod+
                        Fireplaces+LotArea_log+TotalPorchSF+BsmtUnfSF, 
                     data = train, 
                     "lm",
                     trControl = train_Control)
model.model.2 = train(SalePrice_log ~ 
                        OverallQual+GarageCars+
                        TotalBsmtSF+X1stFlrSF_log+YearBuilt+YearRemodAdd+
                        Fireplaces+WoodDeckSF+X2ndFlrSF+LotArea+BsmtFullBath+
                        BsmtUnfSF+KitchenAbvGr+ScreenPorch+MSSubClass+PoolArea, 
                      data = train, 
                      "lm",
                      trControl = train_Control)
model.model.3 = train(SalePrice_log ~ 
                        OverallQual+TotalSF_log+GarageCars+YrBltAndRemod+
                        Fireplaces+LotArea_log+TotalPorchSF+BsmtUnfSF, 
                      data = train, 
                      "lm",
                      trControl = train_Control)
# Summarise Results
model.model.1$results # RMSE 0.1457934
model.model.2$results # RMSE 0.1513886
model.model.3$results # RMSE 0.1453189
# Model 3 seems to have the best overall results in terms of adj rsq and has a lower RMSE

#############################################################################
## Validate the Model assumptions
# Set model to final model
# Removed GrLivArea_log to create Model 3, after VIF revealed a little colinearity
model = model.3
summary(model)
# Check the RMSE
RSS = c(crossprod(model$residuals))
MSE = RSS / length(model$residuals)
RMSE = sqrt(MSE); RMSE
# Residual plot
plot(train$SalePrice_log, model$residuals, xlab = 'Sale Price', ylab='Residuals', main ='Residual Plot')
abline(h = 0, col = 'red')
# Plot of Actual vs Predicted
plot(predict(model), train$SalePrice_log, xlab = "Predicted", ylab = "Actual", main ='Residual vs Actual Plot')
abline(a = 0, b = 1, col = 'red')
# Normal probability plot of residuals
qqnorm(model$residuals)
qqline(model$residuals, col = "red")
# Durbin Watson test
durbinWatsonTest(model) 
# Colinearity
vif(model) # no colinearity detected


#############################################################################
## Additional - Graphing
#############################################################################

############################################
####### SCATTER PLOTS
# Set theme for scatter plots
scatt_theme = theme(panel.background = element_rect(fill = '#ffffff'),
                    plot.title=element_text(size=16, hjust = 0.5),
                    axis.title.y=element_text(size = 12, vjust = 0.2,
                                              margin = margin(t = 0, r = 20, b = 1, l = 0)),
                    axis.title.x=element_text(size = 12, vjust = 0.2,
                                              margin = margin(t = 10, r = 0, b = 10, l = 5)),
                    axis.text.y=element_text(size = 10),
                    axis.text.x=element_text(size = 10),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())

# Scatter Plot YearRemodAdd and SalePrice
ggplot(train, aes(x=train$YrBltAndRemod, y=train$SalePrice)) +
  geom_point(shape=1) +     # Use hollow circles
  geom_smooth(method=lm) +  # Add linear regression line (by default includes 95% confidence region)
  ggtitle("YrBltAndRemod and SalePrice") +
  labs(x="YrBltAndRemod", y="SalePrice") +
  scale_y_continuous(labels = dollar) + 
  scatt_theme

# Scatter Plot GrLivArea and SalePrice
ggplot(train, aes(x=train$TotalSF, y=train$SalePrice)) +
  geom_point(shape=1) +     # Use hollow circles
  geom_smooth(method=lm) +  # Add linear regression line (by default includes 95% confidence region)
  ggtitle("TotalSF and SalePrice") +
  labs(x="TotalSF", y="SalePrice") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = dollar) + 
  scatt_theme

# Scatter Plot GarageArea and SalePrice
ggplot(train, aes(x=train$GarageArea, y=train$SalePrice)) +
  geom_point(shape=1) +     # Use hollow circles
  geom_smooth(method=lm) +  # Add linear regression line (by default includes 95% confidence region)
  ggtitle("GarageArea and SalePrice") +
  labs(x="GarageArea", y="SalePrice") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = dollar) + 
  scatt_theme

# Scatter Plot TotalBsmtSF and SalePrice
ggplot(train, aes(x=train$LotArea_log, y=train$SalePrice)) +
  geom_point(shape=1) +     # Use hollow circles
  geom_smooth(method=lm) +  # Add linear regression line (by default includes 95% confidence region)
  ggtitle("LotArea_log and SalePrice") +
  labs(x="LotArea_log", y="SalePrice") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = dollar) + 
  scatt_theme

############################################
####### BOX PLOTS
# Check unique values and create a factor
unique(train$OverallQual)
train$OverallQual = factor(train$OverallQual,
                           c('1','2','3','4','5','6','7','8','9','10'))
unique(train$GarageCars)
train$GarageCars = factor(train$GarageCars,
                          c('0','1','2','3','4'))
unique(train$FullBath)
train$FullBath = factor(train$FullBath,
                        c('0','1','2','3'))
unique(train$TotRmsAbvGrd)
train$TotRmsAbvGrd = factor(train$TotRmsAbvGrd,
                            c('2','3','4','5','6','7','8','9','10','11','12','14'))
# GarageCars and SalesPrice
ggplot(train, aes(x = OverallQual, y = SalePrice)) + 
  geom_boxplot() + 
  scale_y_continuous(labels = dollar)
# GarageCars and SalesPrice
ggplot(train, aes(x = GarageCars, y = SalePrice)) + 
  geom_boxplot() + 
  scale_y_continuous(labels = dollar)
# OverallQual and SalesPrice by GarageCars
ggplot(train, aes(x = OverallQual, y = SalePrice, fill = GarageCars)) + 
  geom_boxplot() + 
  scale_y_continuous(labels = dollar)
# GarageCars and SalesPrice by FullBath
ggplot(train, aes(x = GarageCars, y = SalePrice, fill = FullBath)) + 
  geom_boxplot() + 
  scale_y_continuous(labels = dollar)

