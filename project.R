library(tidyverse)
#library(readxl)
#library(ggplot2)
library(GGally)
#library(dplyr)
library(corrplot)
require(scales)
library(psych) # summarize data set
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
# Summary of normality by variable
psych::describe(train)
# missing a lot of data from:
# Alley (91 records present)
# FireplaceQu (770 records present)
# PoolQC (7 records present)
# Fence (281 records present)
# MiscFeature (54 records present)

#############################################################################
## Histograms of SalesPrice
#############################################################################
# Figure out step by 
#n = (755000 - 34000) / 20

# Graph by count
qplot(train$SalePrice,
      geom="histogram",
      #breaks=seq(34000, 755000, by = n),
      bins = 15,
      main = "Histogram for Sale Price", 
      xlab = "Sale Price",
      ylab = 'Count',
      fill=I("black"), 
      col=I("gray")) +
  scale_x_continuous(labels = dollar) +
  theme(panel.background = element_rect(fill = '#ffffff'),
        plot.title=element_text(size=16, hjust = 0.3),
        axis.title.y=element_text(size = 12, vjust = 0.2,
                                  margin = margin(t = 0, r = 20, b = 1, l = 0)),
        axis.title.x=element_text(size = 12, vjust = 0.2,
                                  margin = margin(t = 10, r = 0, b = 10, l = 5)),
        axis.text.y=element_text(size = 10),
        axis.text.x=element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#############################################################################
## Exploration - Outliers
#############################################################################
#############################################################################
# boxplots to find outliers and the variable selection was based on skew > 5
# from psych::describe() command above
boxplot(train$LotArea, main='LotArea Plot', ylab='LotArea')
ggplot(data=train, aes(x=train$Street, y=train$SalePrice)) +
  scale_y_continuous(labels = dollar) +
  geom_boxplot()
ggplot(data=train, aes(x=train$Utilities, y=train$SalePrice)) +
  scale_y_continuous(labels = dollar) +
  geom_boxplot()
ggplot(data=train, aes(x=train$Condition2, y=train$SalePrice)) +
  scale_y_continuous(labels = dollar) +
  geom_boxplot()
ggplot(data=train, aes(x=train$RoofMatl, y=train$SalePrice)) +
  scale_y_continuous(labels = dollar) +
  geom_boxplot()
ggplot(data=train, aes(x=train$Heating, y=train$SalePrice)) +
  scale_y_continuous(labels = dollar) +
  geom_boxplot()
boxplot(train$LowQualFinSF, main='LowQualFinSF Box Plot', ylab='LowQualFinSF')
ggplot(data=train, aes(x=train$GarageCond, y=train$SalePrice)) +
  scale_y_continuous(labels = dollar) +
  geom_boxplot()
boxplot(train$X3SsnPorch, main='X3SsnPorch Box Plot', ylab='X3SsnPorch')
boxplot(train$PoolArea, main='PoolArea Box Plot', ylab='PoolArea')
boxplot(train$MiscVal, main='MiscVal Box Plot', ylab='MiscVal')

#############################################################################
## Correlation - Numerical
#############################################################################
# This will show what variables correlate with SalesPrice 
# and what could potentially be colinear if the variables correlate strongly
# with each other
train.num = Filter(is.numeric, train) # only numeric variables
train.num.cor = cor(train.num)

# curious what this shows
cols = names(train.num) %in% c("Id", "SalePrice", "LotArea") # logical to see if included
train.num.notVals = train.num[!cols]
boxplot(train.num.notVals, las=2)
boxplot(train$LotArea, main='LotArea Plot', ylab='LotArea')

# Get subset of numericals without Id
cols = names(train.num) %in% c("Id") # logical to see if included
train.num.notId = train.num[!cols]

# DataFrame of just correlation with SalesPrice
df.corr.SalesPrice = data.frame(cor(train.num.notId$SalePrice, train.num.notId))
# Get absolute value of correlation
df.corr.SalesPrice = sort(abs(df.corr.SalesPrice), decreasing = TRUE)
# Look at top 15 correlated variables
cols = colnames(df.corr.SalesPrice)[0:15] # look at top 15 correlated variables
train.num.sctr = train.num[cols]
# Graph a scatter and correlation matrix together
ggpairs(train.num.sctr,
        upper = list(continuous = wrap("cor", size = 4)),
        title = "Scatter Matrix") +
        theme(plot.title=element_text(size=10, hjust = 0.5),
              axis.title.y=element_text(size = 5, vjust = 0.2),
              axis.title.x=element_text(size = 5, vjust = 0.2),
              axis.text.y=element_text(size = 6),
              axis.text.x=element_text(size = 7),
              strip.text.y=element_text(size = 5, face = "bold"),
              strip.text.x=element_text(size = 8, face = "bold"),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank()) # dev.off() run this command in case plot sticks

# Visualize Correlation Matrix in Heatmap
corrplot(train.num.cor, method = "color", diag = FALSE) # Display the correlation coefficient

# Visualize Correlation Matrix
p.mat = cor.mtest(train.num)$p
res = cor.mtest(train.num, conf.level = .95)
col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(train.num.cor, method = "color", col = col(200),
         type = "upper", number.cex = 0.45, tl.cex = 0.6, #order = "AOE", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         diag = FALSE)


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
cols = names(train.cat.dummy) %in% cols[!cols %in% "SalePrice"] 
train.cat.dummy = train.cat.dummy[!cols] # final dataframe for analysis

colnames(train.cat.dummy)
# DataFrame of just correlation with SalesPrice
df.corr.SalesPrice.cat = data.frame(cor(train.cat.dummy$SalePrice, train.cat.dummy))
# Get absolute value of correlation
df.corr.SalesPrice.cat = sort(abs(df.corr.SalesPrice.cat), decreasing = TRUE)
# Look at top 15 correlated variables
cols = colnames(df.biscorr.SalesPrice.cat)[0:15]
train.cat.sctr = train.cat.dummy[cols]
# Since the top 6 correlated variables are related to specific areas and their quality
# and neither of them has a correlation above 0.59, we can leverage the highly correlated
# numerical variable "OverallQual" to summarize these less important categoricals
cols
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
corrplot(train.num.cor, method = "color", diag = FALSE) # Display the correlation coefficient

#############################################################################
## Exploration - Graphing
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
ggplot(train, aes(x=train$YearRemodAdd, y=train$SalePrice)) +
  geom_point(shape=1) +     # Use hollow circles
  geom_smooth(method=lm) +  # Add linear regression line (by default includes 95% confidence region)
  ggtitle("YearBuilt and SalePrice") +
  labs(x="YearBuilt", y="SalePrice") +
  scale_y_continuous(labels = dollar) + 
  scatt_theme

# Scatter Plot GrLivArea and SalePrice
ggplot(train, aes(x=train$GrLivArea, y=train$SalePrice)) +
  geom_point(shape=1) +     # Use hollow circles
  geom_smooth(method=lm) +  # Add linear regression line (by default includes 95% confidence region)
  ggtitle("GrLivArea and SalePrice") +
  labs(x="GrLivArea", y="SalePrice") +
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
ggplot(train, aes(x=train$TotalBsmtSF, y=train$SalePrice)) +
  geom_point(shape=1) +     # Use hollow circles
  geom_smooth(method=lm) +  # Add linear regression line (by default includes 95% confidence region)
  ggtitle("TotalBsmtSF and SalePrice") +
  labs(x="TotalBsmtSF", y="SalePrice") +
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

#############################################################################
## Exploring MODELS
#############################################################################
# Read in data
train = read.csv("train.csv"); train

#### Variable removal and selection due to colinearity
# Used GarageCars over GarageArea, because they were highly correlated with each other
# Used TotalBsmtSF over X1stFlrSF, because they were highly correlated with each other
# Used GrLivArea over TotRmsAbvGrd, because they were highly correlated with each other

#### Subsets Regression Method
## FIRST APPROACH
subs = regsubsets(SalePrice ~ OverallQual+GrLivArea+GarageCars+GarageArea+
                  TotalBsmtSF+X1stFlrSF+FullBath+TotRmsAbvGrd+
                  YearBuilt+YearRemodAdd+Fireplaces+BsmtFinSF1+
                  WoodDeckSF+X2ndFlrSF, 
                  data = train, nbest=1)
# Plot a table of models showing variables in each model.
subs.plot = plot(subs, scale = "adjr2", main = "Adjusted R^2")
# Visualize subset size to hit statistic
subs.plot.leg = subsets(subs, statistic="adjr2", legend = FALSE, min.size = 5, main = "Adjusted R^2")
# This shows that we can use 5-8 variables for adjr2 above 0.77
# OverallQual, GrLivArea, GarageCars, TotalBsmtSF, YearRemodAdd, Fireplaces, BsmtFinSF1 and WoodDeckSF
# OverallQual, GrLivArea, GarageCars, TotalBsmtSF, YearRemodAdd, Fireplaces, and BsmtFinSF1
# OverallQual, GrLivArea, GarageCars, TotalBsmtSF, YearRemodAdd, and BsmtFinSF1
# OverallQual, GrLivArea, GarageCars, YearRemodAdd, and BsmtFinSF1

### Stepwise Regression Method after subsetting
model1 = lm(SalePrice ~ OverallQual+GrLivArea+GarageCars+TotalBsmtSF+YearRemodAdd+Fireplaces+BsmtFinSF1+WoodDeckSF, data = train)
step = stepAIC(model1, direction="both")
step$anova # Shows to remove nothing
model2 = lm(SalePrice ~ OverallQual+GrLivArea+GarageCars+TotalBsmtSF+YearRemodAdd+Fireplaces+BsmtFinSF1, data = train)
model3 = lm(SalePrice ~ OverallQual+GrLivArea+GarageCars+TotalBsmtSF+YearRemodAdd+BsmtFinSF1, data = train)
model4 = lm(SalePrice ~ OverallQual+GrLivArea+GarageCars+YearRemodAdd+BsmtFinSF1, data = train)
summary(model1) # adj rsq of 0.7841
summary(model2) # adj rsq of 0.7816
summary(model3) # adj rsq of 0.7785
summary(model4) # adj rsq of 0.7723
# Compare the subset of the original model
# Conventional to list models from smallest to largest
anova(model2, model1, test = "F")
anova(model3, model2, test = "F")
anova(model4, model3, test = "F")

## SECOND APPROACH
# Getting data ready for bestglm
train.subs = train[, c("OverallQual", "GrLivArea", "GarageCars",
                       "GarageArea","TotalBsmtSF", "X1stFlrSF","FullBath",
                       "TotRmsAbvGrd", "YearBuilt","YearRemodAdd","Fireplaces",
                       "BsmtFinSF1","WoodDeckSF","X2ndFlrSF", "SalePrice")]
colnames(train.subs)[colnames(train.subs) == "SalePrice"] = "y"
train.subs
train.bestglm = bestglm(Xy = train.subs, 
                        family = gaussian,
                        IC = "AIC", # Information criteria for
                        method = "exhaustive")
# Best models
train.bestglm$BestModels
# Summary of best model
summary(train.bestglm$BestModel)
# This has removed GrLivArea and FullBath from model, 
# shows X1stFlrSF with a higher t-score than TotalBsmtSF (we can test with step-wise),
# and shows GarageArea and TotRmsAbvGrd as not significant based on t-test
# with adj rsq of 0.7867
model5 = lm(SalePrice ~ OverallQual+GarageCars+TotalBsmtSF+X1stFlrSF+YearBuilt+YearRemodAdd+Fireplaces+
              BsmtFinSF1+WoodDeckSF+X2ndFlrSF, data = train)
step = stepAIC(model5, direction="both")
step$anova # Shows to remove nothing
# Removed TotalBsmtSF from model
model6 = lm(SalePrice ~ OverallQual+GarageCars+X1stFlrSF+YearBuilt+YearRemodAdd+Fireplaces+
              BsmtFinSF1+WoodDeckSF+X2ndFlrSF, data = train)
summary(model5) # adj rsq of 0.7864 
summary(model6) # adj rsq of 0.7852
anova(model5, model6, test = "F")

## THIRD APPROACH
# The dropterm function considers each variable individually 
# and considers what the change in residual sum of squares would be 
# if this variable was excluded from the model. 
# There is a link between this F test and the t test that appears 
# as part of the model summary – this is because of the link 
# between these two distributions
model.drop = glm(SalePrice ~ OverallQual+GrLivArea+GarageCars+GarageArea+
                  TotalBsmtSF+X1stFlrSF+FullBath+TotRmsAbvGrd+
                  YearBuilt+YearRemodAdd+Fireplaces+BsmtFinSF1+
                  WoodDeckSF+X2ndFlrSF, data = train)
dropterm(model.drop,  test = "F")
# Variables with 3 stars are:
# OverallQual, GarageCars, YearBuilt, YearRemodAdd, Fireplaces, BsmtFinSF1, WoodDeckSF
# Variables with 2 stars are:
# TotalBsmtSF
# Variables with 0 stars are:
# GrLivArea, GarageArea, X1stFlrSF, FullBath, TotRmsAbvGrd, X2ndFlrSF
model7 = lm(SalePrice ~ OverallQual+GarageCars+YearBuilt+YearRemodAdd+Fireplaces+
              BsmtFinSF1+WoodDeckSF, data = train)
summary(model7) # adj rsq of 0.7268


###########################
# Best 2 models 
# model1 with adj rsq of 0.7841 (8 variables)
# model5 with adj rsq of 0.7864 (10 variables)
###########################
# K-fold cross-validation
train_Control = trainControl(method = "cv", number = 10, savePredictions = TRUE) # number of folds
model.model1 = train(SalePrice ~ OverallQual+GrLivArea+GarageCars+TotalBsmtSF+YearRemodAdd+Fireplaces+BsmtFinSF1+WoodDeckSF, 
                     data = train, 
                     "lm",
                     trControl = train_Control)
model.model5 = train(SalePrice ~ OverallQual+GarageCars+TotalBsmtSF+X1stFlrSF+YearBuilt+YearRemodAdd+Fireplaces+BsmtFinSF1+WoodDeckSF+X2ndFlrSF, 
                     data = train, 
                     "lm",
                     trControl = train_Control)
# Summarise Results
# 0.78 - 0.8 when cross-validating multiple times
print(model.model1)
model.model1$results
print(model.model5)
model.model5$results
# Model 1 seems to have the best overall results in terms of adj rsq and minimization of errors
# and has a lower RMSE

#############################################################################
## MODEL
#############################################################################
model = lm(SalePrice ~ OverallQual+GrLivArea+GarageCars+TotalBsmtSF+YearRemodAdd+Fireplaces+BsmtFinSF1+WoodDeckSF, data = train) 

#############################################################################
# Give summary of the model
summary(model) # adj rsq of 0.7841
anova(model)
model$coefficients

#############################################################################
## Validate the Model assumptions
# Residual plot
plot(train$SalePrice, model$residuals, xlab = 'Sale Price', ylab='Residuals', main ='Residual Plot')
abline(h = 0, col = 'red')
# Plot of Actual vs Predicted
plot(predict(model), train$SalePrice, xlab = "Predicted", ylab = "Actual", main ='Residual vs Actual Plot')
abline(a = 0, b = 1, col = 'red')
# Normal probability plot of residuals
qqnorm(model$residuals)
qqline(model$residuals, col = "red") #fails because it is not a straight line
# Durbin Watson test
durbinWatsonTest(model)
# Colinearity
vif(model) # no colinearity detected

# Histogram for Residuals
qplot(model$residuals,
      geom="histogram",
      #breaks=seq(315, 545, by = n),
      bins=15,
      main = "Histogram for Residuals", 
      xlab = "Residuals",
      ylab = 'Count',
      fill=I("black"), 
      col=I("gray")) +
  scale_x_continuous(labels = dollar) +
  theme(panel.background = element_rect(fill = '#ffffff'),
        plot.title=element_text(size=16, hjust = 0.3),
        axis.title.y=element_text(size = 12, vjust = 0.2,
                                  margin = margin(t = 0, r = 20, b = 1, l = 0)),
        axis.title.x=element_text(size = 12, vjust = 0.2,
                                  margin = margin(t = 10, r = 0, b = 10, l = 5)),
        axis.text.y=element_text(size = 10),
        axis.text.x=element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#############################################################################
## MODEL - Adjustments
#############################################################################
# Adjustments after looking at the normal probabilty plot of residuals
# skewed data that could use log transform and removing the
# transforms on YearBuilt and YearRemodAdd, because they didn't add
# any value (the correlation was the same as the normal variable). 
# The data for YearRemodAdd seems bimodal and not sure what to do with it.

train$SalePrice_log = log(train$SalePrice) # right-skewed
train$GrLivArea_log = log(train$GrLivArea) # right-skewed
train$TotalBsmtSF_log = log(train$TotalBsmtSF) # right-skewed
train$X1stFlrSF_log = log(train$X1stFlrSF) # right-skewed
#train$YearBuilt_log = log(train$YearBuilt) # left-skewed
#train$YearBuilt_sqrt = sqrt(train$YearBuilt) # left-skewed
#train$YearBuilt_cubert = sign(train$YearBuilt) * abs(train$YearBuilt)^(1/3) # left-skewed
#train$YearBuilt_log10 = log10(train$YearBuilt) # left-skewed
#train$YearRemodAdd_log = log(train$YearRemodAdd) # left-skewed
#train$YearRemodAdd_sqrt = sqrt(train$YearRemodAdd) # left-skewed
#train$YearRemodAdd_cubert = sign(train$YearRemodAdd) * abs(train$YearRemodAdd)^(1/3) # left-skewed
#train$YearRemodAdd_log10 = log10(train$YearRemodAdd) # left-skewed
#train$YearRemodAdd_norm = (train$YearRemodAdd - min(train$YearRemodAdd)) / (max(train$YearRemodAdd) - min(train$YearRemodAdd))  # left-skewed
#train$YearRemodAdd_mean = abs(train$YearRemodAdd - mean(train$YearRemodAdd)) # left-skewed
train$BsmtFinSF1_log = log(train$BsmtFinSF1) # right-skewed
train$WoodDeckSF_log = log(train$WoodDeckSF) # right-skewed
train$X2ndFlrSF_log = log(train$X2ndFlrSF) # right-skewed

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
# verify the errors were removed
psych::describe(train$SalePrice_log)
psych::describe(train$GrLivArea_log)
psych::describe(train$TotalBsmtSF_log)
psych::describe(train$X1stFlrSF_log)
#psych::describe(train$YearRemodAdd_mean)
psych::describe(train$BsmtFinSF1_log)
psych::describe(train$WoodDeckSF_log)
psych::describe(train$X2ndFlrSF_log)

#############################################################################
#### REVISIT CORRELATION
train.num = Filter(is.numeric, train) # only numeric variables
cols = names(train.num) %in% c("Id","SalePrice") # logical to see if included
train.num.notId = train.num[!cols]
# DataFrame of just correlation with SalePrice_log
df.corr.SalePrice_log = data.frame(cor(train.num.notId$SalePrice_log, train.num.notId))
# Get absolute value of correlation
df.corr.SalePrice_log = sort(abs(df.corr.SalePrice_log), decreasing = TRUE)
# Look at top 20 correlated variables
cols = colnames(df.corr.SalePrice_log)[0:20]
train.num.sctr = train.num[cols]
# Graph a scatter and correlation matrix together
ggpairs(train.num.sctr,
        upper = list(continuous = wrap("cor", size = 3)),
        title = "Scatter Matrix") +
  theme(plot.title=element_text(size=10, hjust = 0.5),
        axis.title.y=element_text(size = 5, vjust = 0.2),
        axis.title.x=element_text(size = 5, vjust = 0.2),
        axis.text.y=element_text(size = 6),
        axis.text.x=element_text(size = 7),
        strip.text.y=element_text(size = 4, face = "bold"),
        strip.text.x=element_text(size = 5, face = "bold"),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) # dev.off() run this command in case plot sticks

#############################################################################
#### Variable removal and selection due to colinearity
# Used X1stFlrSF_log over X1stFlrSF, because they were highly correlated with each other
# Used X2ndFlrSF over X2ndFlrSF_log, because they were highly correlated with each other
# Best subsets kept putting all 4 of these variables in the model, which created
# a huge problem with colinearity. Not giving it the opportunity.

#### Subsets Regression Method
subs = regsubsets(SalePrice_log ~ OverallQual+GrLivArea+GarageCars+GarageArea+
                  TotalBsmtSF+X1stFlrSF+FullBath+TotRmsAbvGrd+
                  YearBuilt+YearRemodAdd+Fireplaces+BsmtFinSF1+
                  WoodDeckSF+X2ndFlrSF+GrLivArea_log+TotalBsmtSF_log+
                  X1stFlrSF_log+BsmtFinSF1_log+WoodDeckSF_log, 
                  data = train, nbest=1)
# Plot a table of models showing variables in each model.
subs.plot.bar = plot(subs, scale = "adjr2", main = "Best Subsets by Adjusted R^2")
# Visualize subset size to hit statistic
subs.plot.leg = subsets(subs, statistic="adjr2", legend = FALSE, min.size = 2, max.size = 9,  main = "Best Subsets by Adjusted R^2")
# Print labels
subs.plot.leg
# Arcing point graph
subs.sum.df.adjr2 = as.data.frame(subs.sum$adjr2)
names(subs.sum.df.adjr2) = "R2"
subs.sum.df.adjr2 %>% 
  ggvis(x=~ c(1:nrow(subs.sum.df.adjr2)), y=~R2 ) %>%
  layer_points(fill = ~ R2 ) %>%
  add_axis("y", title = "Adjusted R^2") %>% 
  add_axis("x", title = "Subset Size")
# Line graph
ggplot(subs.sum.df.adjr2, 
       aes(x = seq(1, nrow(subs.sum.df.adjr2), by = 1), 
           y = subs.sum.df.adjr2$R2)) +
  geom_line(aes(group=1)) +
  geom_point(size=3) +
  #geom_text(aes(label=Name),hjust=0, vjust=0)
  ggtitle("Best Subsets by Adjusted R^2") +
  labs(x="Subset Size", y="Adjusted R^2") +
  scale_x_continuous(breaks=seq(0, 8, 1)) + 
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
# Look at top 30 correlated variables
cols = colnames(df.corr.SalePrice_log)[0:30]
train.num.sctr = train.num[cols]
# Remove SP
cols = names(train.num.sctr) %in% c("SalePrice_log") # logical to see if included
train.num.sctr.notSP = train.num.sctr[!cols]
# Create leap object
leap = leaps(x = train.num.sctr.notSP, y = train.num.sctr$SalePrice_log, 
             method = "adjr2", nbest = 1,
             name = names(train.num.sctr.notSP))
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
leaps.sum.df[10:12, ]
leaps.sum.df[12:12, ]
write.csv(leaps.sum.df, 
          "C:/Users/mbray/Dropbox/Grad School/CU Denver/BANA_6610/project/subset.csv", 
          row.names = FALSE)

#############################################################################
#### bestglm Subsets Regression Method
train.subs = cbind(train.num.sctr.notSP, train.num.sctr$SalePrice_log)
train.bestglm = bestglm(Xy = train.subs, 
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
# Model 1
model1 = lm(SalePrice_log ~ OverallQual+GrLivArea_log+GarageCars+
              TotalBsmtSF+YearBuilt+YearRemodAdd+BsmtFinSF1+
              LotArea+MSSubClass, data = train)
summary(model1) #adjr2 of 0.8485
model1.2 = lm(SalePrice_log ~ OverallQual+GrLivArea_log+GarageCars+
              TotalBsmtSF+YearBuilt+YearRemodAdd+Fireplaces+
              BsmtFinSF1+LotArea+MSSubClass, data = train)
summary(model1.2) #adjr2 of 0.8531
model1.3 = lm(SalePrice_log ~ OverallQual+GrLivArea_log+GarageCars+
                YearBuilt+YearRemodAdd+Fireplaces+
                BsmtFinSF1+LotArea+MSSubClass, data = train)
summary(model1.3) #adjr2 of 0.8524
# Model 2
model2 = lm(SalePrice_log ~ OverallQual+GrLivArea_log+GarageCars+
             TotalBsmtSF+X1stFlrSF_log+YearBuilt+YearRemodAdd+
             Fireplaces+LotArea+BsmtUnfSF+MSSubClass, data = train)
summary(model2) # adjr2 of 0.8551
model2.1 = lm(SalePrice_log ~ OverallQual+GrLivArea_log+GarageCars+
                TotalBsmtSF+YearBuilt+YearRemodAdd+
                Fireplaces+LotArea+BsmtUnfSF+MSSubClass, data = train)
summary(model2.1) # adjr2 of 0.8542
# Model 3
model3 = lm(SalePrice_log ~ OverallQual+GrLivArea_log+GarageCars+
            TotalBsmtSF+X1stFlrSF_log+FullBath+YearBuilt+YearRemodAdd+
            Fireplaces+WoodDeckSF+X2ndFlrSF+LotArea+BsmtFullBath+
            BsmtUnfSF+KitchenAbvGr+ScreenPorch+MSSubClass+PoolArea, 
            data = train)
summary(model3) # adjr2 of 0.8637
model3.1 = lm(SalePrice_log ~ OverallQual+GrLivArea_log+GarageCars+
              TotalBsmtSF+X1stFlrSF_log+YearBuilt+YearRemodAdd+
              Fireplaces+WoodDeckSF+X2ndFlrSF+LotArea+BsmtFullBath+
              BsmtUnfSF+KitchenAbvGr+ScreenPorch+MSSubClass+PoolArea, 
              data = train)
summary(model3.1) # adjr2 of 0.8637
# Model 4
model4 = lm(SalePrice_log ~ OverallQual+GrLivArea_log+GarageCars+
              TotalBsmtSF+X1stFlrSF_log+FullBath+YearBuilt+YearRemodAdd+
              TotRmsAbvGrd+Fireplaces+BsmtFinSF1+WoodDeckSF+X2ndFlrSF+
              HalfBath+LotArea+BsmtFullBath+BsmtUnfSF+BedroomAbvGr+
              KitchenAbvGr+ScreenPorch+MSSubClass+PoolArea+X3SsnPorch, 
            data = train)
summary(model4) # adjr2 of 0.8637
model4.1 = lm(SalePrice_log ~ OverallQual+GrLivArea_log+GarageCars+
              TotalBsmtSF+X1stFlrSF_log+FullBath+YearBuilt+YearRemodAdd+
              Fireplaces+BsmtFinSF1+WoodDeckSF+X2ndFlrSF+
              HalfBath+LotArea+BsmtFullBath+BsmtUnfSF+BedroomAbvGr+
              KitchenAbvGr+ScreenPorch+MSSubClass+PoolArea+X3SsnPorch, 
            data = train)
summary(model4.1) # adjr2 of 0.8638
model4.2 = lm(SalePrice_log ~ OverallQual+GrLivArea_log+GarageCars+
                X1stFlrSF_log+YearBuilt+YearRemodAdd+
                Fireplaces+WoodDeckSF+X2ndFlrSF+LotArea+BsmtFullBath+
                KitchenAbvGr+ScreenPorch+MSSubClass+PoolArea, 
              data = train)
summary(model4.2) # adjr2 of 0.862

#############################################################################
# K-fold cross-validation
train_Control = trainControl(method = "cv", number = 10, savePredictions = TRUE) # number of folds
# Model 1.3 from Model 1
model.model1 = train(SalePrice_log ~ OverallQual+GrLivArea_log+GarageCars+
                     YearBuilt+YearRemodAdd+Fireplaces+
                     BsmtFinSF1+LotArea+MSSubClass, 
                     data = train, 
                     "lm",
                     trControl = train_Control)
# Model 2.1 from Model 2
model.model2 = train(SalePrice_log ~ OverallQual+GrLivArea_log+GarageCars+
                     TotalBsmtSF+YearBuilt+YearRemodAdd+
                     Fireplaces+LotArea+BsmtUnfSF+MSSubClass, 
                     data = train, 
                     "lm",
                     trControl = train_Control)
# Model 3.1 from Model 3
model.model3 = train(SalePrice_log ~ OverallQual+GrLivArea_log+GarageCars+
                     TotalBsmtSF+X1stFlrSF_log+YearBuilt+YearRemodAdd+
                     Fireplaces+WoodDeckSF+X2ndFlrSF+LotArea+BsmtFullBath+
                     BsmtUnfSF+KitchenAbvGr+ScreenPorch+MSSubClass+PoolArea, 
                     data = train, 
                     "lm",
                     trControl = train_Control)
# Model 4 from Model 3.1 without GrLivArea_log
model.model4 = train(SalePrice_log ~ OverallQual+GarageCars+
                    TotalBsmtSF+X1stFlrSF_log+YearBuilt+YearRemodAdd+
                     Fireplaces+WoodDeckSF+X2ndFlrSF+LotArea+BsmtFullBath+
                     BsmtUnfSF+KitchenAbvGr+ScreenPorch+MSSubClass+PoolArea, 
                     data = train, 
                     "lm",
                     trControl = train_Control)
# Summarise Results
#print(model.model1)
model.model1$results
#print(model.model2)
model.model2$results
#print(model.model3)
model.model3$results
#print(model.model4)
model.model4$results
# Model 3 seems to have the best overall results in terms of adj rsq and minimization of errors
# and has a lower RMSE

#############################################################################
## Validate the Model assumptions
# Model
model.final.1 = lm(SalePrice_log ~ OverallQual+GrLivArea_log+GarageCars+
           TotalBsmtSF+X1stFlrSF_log+YearBuilt+YearRemodAdd+
           Fireplaces+WoodDeckSF+X2ndFlrSF+LotArea+BsmtFullBath+
           BsmtUnfSF+KitchenAbvGr+ScreenPorch+MSSubClass+PoolArea, 
           data = train)
summary(model.final.1) # adjr2 of 0.8637
# X2ndFlrSF over GrLivArea_log due to colinearity
model.final.2 = lm(SalePrice_log ~ OverallQual+GarageCars+
             TotalBsmtSF+X1stFlrSF_log+YearBuilt+YearRemodAdd+
             Fireplaces+WoodDeckSF+X2ndFlrSF+LotArea+BsmtFullBath+
             BsmtUnfSF+KitchenAbvGr+ScreenPorch+MSSubClass+PoolArea, 
           data = train)
summary(model.final.2) # adjr2 of 0.8628
# GrLivArea_log over X2ndFlrSF due to colinearity
model.final.3 = lm(SalePrice_log ~ OverallQual+GrLivArea_log+GarageCars+
             TotalBsmtSF+X1stFlrSF_log+YearBuilt+YearRemodAdd+
             Fireplaces+WoodDeckSF+LotArea+BsmtFullBath+
             BsmtUnfSF+KitchenAbvGr+ScreenPorch+MSSubClass+PoolArea, 
           data = train)
summary(model.final.3) # adjr2 of 0.8616


# Set model to final model
model = model.final.2
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

# normal probability plot is virtually a straight line except for tip and tail
# where outliers could be the issue. DW statistic is 2.01, which is as close to 
# no autocorrelation as possible. VIF shows no colinearity.

#############################################################################
## MODEL - Test Validation
#############################################################################
# Read in the test data
test = read.csv("test.csv"); test
test$GrLivArea_log = log(test$GrLivArea) # right-skewed
test$X1stFlrSF_log = log(test$X1stFlrSF) # right-skewed
# clean up data where there was a log transformation on a column with a value of 0 within it
# GrLivArea_log
test$GrLivArea_log[which(is.nan(test$GrLivArea_log))] = NA
test$GrLivArea_log[which(test$GrLivArea_log == Inf)] = NA
test$GrLivArea_log[which(test$GrLivArea_log == -Inf)] = NA
# verify the errors were removed
psych::describe(test$GrLivArea_log)
# Replace NA values in data with 0
cols.rep = c("BsmtFullBath", "BsmtUnfSF", "TotalBsmtSF", "GarageCars")
test[cols.rep][is.na(test[cols.rep])] = 0

model = lm(log(SalePrice) ~ OverallQual+GarageCars+
             TotalBsmtSF+X1stFlrSF_log+YearBuilt+YearRemodAdd+
             Fireplaces+WoodDeckSF+X2ndFlrSF+LotArea+BsmtFullBath+
             BsmtUnfSF+KitchenAbvGr+ScreenPorch+MSSubClass+PoolArea, 
           data = train)
prediction = exp(predict(model, test)); prediction
#prediction = predict(model, test)
#prediction[1115:1120]
#test[1115:1120, ]
output = cbind(test, SalePrice = prediction); output
submission = output[, c("Id","SalePrice")]
write.csv(submission, 
          "C:/Users/mbray/Dropbox/Grad School/CU Denver/BANA_6610/project/submission3.csv", 
          row.names = FALSE)
