library(readxl)
library(ggplot2)
library(GGally)
library(dplyr)
library(corrplot)
require(scales)
library(psych)

train = read.csv("train.csv"); train

#############################################################################
## Quick Summary
#############################################################################
dim(train) #1460 rows and 81 cols
colnames(train)
str(train)
psych::describe(train) #summary of normality by variable

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
## Explore Outliers and Categoricals
#############################################################################
#############################################################################
# boxplots to find outliers
boxplot(train)
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


# outliers to solve for and remove outliers
# Lot size
outs.lotsize = sort(boxplot(ski$`Lot size`, plot=FALSE)$out, decreasing = TRUE); outs.lotsize
outs.lotsize = outs.lotsize[0:3] # remove top 3 outliers
# Age
outs.age = sort(boxplot(ski$`Age`, plot=FALSE)$out, decreasing = TRUE); outs.age
outs.age = outs.age[0:3] # remove top 2 outliers
#Garage
#outs.garage = sort(boxplot(ski$`Garage`, plot=FALSE)$out, decreasing = TRUE); outs.garage
#outs.garage = outs.garage[0:2] # remove top 2 outliers

#Remove from data
ski = ski[-which(ski$`Lot size` %in% outs.lotsize),]
#ski = ski[-which(ski$`Age` %in% outs.age),]
#ski = ski[-which(ski$`Mountain` %in% outs.mountain),]

#outs.mountain = sort(boxplot(ski$`Mountain`, plot=FALSE)$out, decreasing = TRUE); outs.mountain
#outs.mountain = outs.mountain[0:2] # remove top 2 outliers
#ski = ski[-which(ski$`Mountain` %in% outs.mountain),]

#outs.ft = sort(boxplot(ski$`Sq_Ft`, plot=FALSE)$out, decreasing = TRUE); outs.ft
#outs.ft = outs.ft[0:2] # remove top 2 outliers
#ski = ski[-which(ski$`Sq_Ft` %in% outs.ft),]

#############################################################################
## Correlation - Numerical
#############################################################################
# This will show what variables correlate with SalesPrice 
# and what could potentially be colinear if the variables correlate strongly
train.num = Filter(is.numeric, train)
train.num.cor = cor(train.num)

# DataFrame of just correlation with SalesPrice
cols = names(train.num) %in% c("Id") 
train.num.notSalePrice = train.num[!cols]
df.corr.SalesPrice = sort(data.frame(cor(train.num$SalePrice, 
                                         train.num.notSalePrice)), decreasing = TRUE)
cols = colnames(df.corr.SalesPrice)[0:15]
train.num.sctr = train.num[cols]
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

#Set theme for scatter plots
theme = theme(panel.background = element_rect(fill = '#ffffff'),
              plot.title=element_text(size=16, hjust = 0.5),
              axis.title.y=element_text(size = 12, vjust = 0.2,
                                        margin = margin(t = 0, r = 20, b = 1, l = 0)),
              axis.title.x=element_text(size = 12, vjust = 0.2,
                                        margin = margin(t = 10, r = 0, b = 10, l = 5)),
              axis.text.y=element_text(size = 10),
              axis.text.x=element_text(size = 10),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())

# Scatter Plot GrLivArea and Sale Price
ggplot(train, aes(x=train$GrLivArea, y=train$SalePrice)) +
  geom_point(shape=1) +     # Use hollow circles
  geom_smooth(method=lm) +  # Add linear regression line (by default includes 95% confidence region)
  ggtitle("Living Room Area and Sale Price ($)") +
  labs(x="Living Room Area", y="Sale Price ($)") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = dollar) + 
  theme()

# Scatter Plot YearBuilt and SalePrice
ggplot(train, aes(x=train$YearBuilt, y=train$SalePrice)) +
  geom_point(shape=1) +     # Use hollow circles
  geom_smooth(method=lm) +  # Add linear regression line (by default includes 95% confidence region)
  ggtitle("Year Built and Sale Price ($)") +
  labs(x="Year Built", y="Year Built") +
  #scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = dollar) + 
  theme()





#############################################################################
## Exploring MODELs
#############################################################################
model = lm(train$SalePrice ~ ., data=train)

l <- sapply(train, function(x) is.factor(x))
m <- train[, l]
ifelse(n <- sapply(m, function(x) length(levels(x))) == 1, "DROP", "NODROP")

summary(model)
models = regsubsets(train$SalePrice ~ ., train, nvmax=6)
summary(models)
res.sum <- summary(models)

stepAIC(model, direction="both")

vcov(models) # covariance matrix for model parameters 
influence(models) # regression diagnostics

# Using GarageCars in lieu of GarageArea. They are highly correlated to each other,
# which means I only needed to utilize one.
# Using TotalBsmtSF in lieu of X1stFlrSF. They are highly correlated to each other,
# which means I only needed to utilize one. I decided to go with the more continuous variable.
model = lm(SalePrice ~ GrLivArea+GarageCars+TotalBsmtSF+YearBuilt+YearRemodAdd, data = train)
summary(model)

#############################################################################
## MODEL
#############################################################################
model = lm(`Selling price` ~ Mountain+Garage+TotalBRooms+Sq_Ft, data = ski) 
# the model performed a good bit better when not pulling out outliers for "Mountain"
# and "Sq_Ft". Additonally, adding in things like "Age" or "On Market" decreased model performance.
#0.6714 w/ou outlier removal
#0.8399 remove outliers for Lot Size
#0.843 remove outliers for Lot Size and Age
#0.8423 remove outliers for Lot Size and Age and Garage

# It should be noted that removing Age from the model didn't really affect performance, but it
# would cost 1 degree of freedom to utilize. Age had a low p-value and would probably
# affect the model more poorly on a larger scale.

#############################################################################
# Give summary of the model
summary(model)
anova(model)
model$coefficients

ols_step_best_subset(model)


#############################################################################
## Validate the Model assumptions
# Residual plot
plot(ski$`Selling price`, model$residuals, xlab = 'Selling Price', ylab='Residuals', main =
       'Residual Plot')
abline(h=0, col = 'red')
# Plot of Actual vs Predicted
plot(predict(model),ski$`Selling price`,
     xlab="predicted",ylab="actual")
abline(a=0,b=1)
# Normal probability plot of residuals
qqnorm(model$residuals)
qqline(model$residuals, col = 2) #fails because it is not a straight line
# Durbin Watson test
durbinWatsonTest(model)
# Colinearity
vif(model)

