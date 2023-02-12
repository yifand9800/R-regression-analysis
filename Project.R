sephora.df <- read.csv('sephora_website_dataset.csv', stringsAsFactors = TRUE)

library(ggplot2)

#scatter plot for correlation
ggplot(data = sephora.df) +
  geom_point(aes(x = number_of_reviews, y = price))

# general price range
ggplot(data = sephora.df) +
  geom_histogram(aes(x = price), bins = 15)

# correlation between two numerical variables
cor(sephora.df$love, sephora.df$price)
cor(sephora.df$rating, sephora.df$price)

## Data Clean
selected_var <- c(6:10, 18:21)
new_sephora.df <- sephora.df[,selected_var]
new_sephora.df$online_only <- factor(new_sephora.df$online_only)
new_sephora.df$exclusive <- factor(new_sephora.df$exclusive)
new_sephora.df$limited_edition <- factor(new_sephora.df$limited_edition)
new_sephora.df$limited_time_offer <- factor(new_sephora.df$limited_time_offer)

##Heatmap
library("pheatmap")
library(RColorBrewer)
pheatmap(cor(new_sephora.df[,1:5]), col = hcl.colors(50, "Spectral"), cutree_rows = 1)


## Multiple Regression
## step2: training and validation
# 70% training + 30% validation
set.seed(123)

# choose 70% of entire rows of car.df 
train.index <- sample(1:nrow(new_sephora.df), 0.65*nrow(new_sephora.df)) 

# choose all columns with row number in train.index
train.df <- new_sephora.df[train.index, ]

# choose all column with row number excluded in train.index
valid.df <- new_sephora.df[-train.index, ]

## Multiple linear regression
# residuals & coefficients (significant variables relate to price)
sephora.model <- lm(price ~ ., data = train.df[,2:6])
summary(sephora.model) 

## step 4: interpret the coefficients (for Report)

## step 5: Predict with the model
sephora_predict.price <- predict(sephora.model, newdata = valid.df)

## step 6: Forecast
library(forecast)
accuracy(sephora_predict.price, valid.df$price)
accuracy(sephora.model$fitted.values, train.df$price)

## Logistic Regression for categorical values
# if product is online only, price is negative and value price is positive
# means Sephora marked value price of online only product much higher than selling price
logit.reg.online <- glm(online_only ~ ., data = train.df, family = 'binomial')
summary(logit.reg.online)

# people like exclusive products - holiday gift set for exclusive products?
logit.reg.excl <- glm(exclusive ~ ., data = train.df, family = 'binomial')
summary(logit.reg.excl)

logit.reg.le <- glm(limited_edition ~ ., data = train.df, family = 'binomial')
summary(logit.reg.le)

# predict on exclusive
logit.reg.pred <- predict(logit.reg.excl, valid.df, type = "response")

# set cutoff value to 0.31 and evaluate classification performance
pred <- ifelse(logit.reg.pred > 0.31, 1, 0)

library(caret)
confusionMatrix(factor(pred), factor(valid.df$exclusive), positive = "1")

## Classification Tree
library(rpart)
library(rpart.plot)
library(caret)
library(forecast)
default.ct <- rpart(price ~ ., data = train.df, method = 'anova')
rpart.plot(default.ct)
default.ct.point.pred <- predict(default.ct, valid.df)


## Cluster Analysis - K mean
# normalize data
library(caret)

# compute mean and stdev of each column
norm.values <- preProcess(new_sephora.df, method=c("center", "scale"))
# normalization
sephora.df.norm <- predict(norm.values, new_sephora.df)

set.seed(333)
km <- kmeans(sephora.df.norm, 4) ##how to choose K-mean? 
km$cluster
km$centers
km$tot.withinss
plot(c(0), xaxt = 'n', ylab = "", type = "l", 
     ylim = c(min(km$centers), max(km$centers)), xlim = c(0, 9))

## K-mean graph (k = 4)
# label x-axes
axis(1, at = c(1:9), labels = names(new_sephora.df))

# plot centroids
for (i in c(1:4))
  lines(km$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3),
                                                       "black", "dark grey"))
# name clusters
text(x = 0.5, y = km$centers[, 1], labels = paste("Cluster", c(1:4)))
