## Case Study eBay

# Histogram
library(ggplot2)
ebay.df <- read.csv('ebay.csv')
ggplot(data = ebay.df) +
  geom_histogram(aes(x = sellerRating), bins = 100)

ggplot(data = ebay.df) +
  geom_bar(aes(x = factor(Duration), y = Competitive), stat = 'summary', fun = 'mean')

# 65% trainig and 35% validation with seed set to 28
set.seed(28)
train.index <- sample(1:nrow(ebay.df), nrow(ebay.df)*0.65)
train.df <- ebay.df[train.index, ]
valid.df <- ebay.df[-train.index, ]

library(rpart)
library(rpart.plot)
library(caret)
default.ct <- rpart(Competitive ~ ., data = train.df, method = "class")
rpart.plot(default.ct, extra = 1)
default.ct.point.pred <- predict(default.ct, valid.df, type = "class")
confusionMatrix(default.ct.point.pred, factor(valid.df$Competitive))

## Fit a new model without closed prices
selected_var <- c(1, 2, 3, 4, 6, 7) # select columns number
selected.df <- train.df[, selected_var] #filter out closed prices
# build new model and classification trees
set.seed(28)
tr.index <- sample(1:nrow(selected.df), nrow(selected.df)*0.65)
tr.df <- selected.df[train.index, ]
newvaild.df <- selected.df[-train.index, ]
new.ct <- rpart(Competitive ~ ., data = tr.df, method = "class")
rpart.plot(new.ct, extra = 1)
new.ct.point.pred <- predict(new.ct, newvaild.df, type = "class")
confusionMatrix(new.ct.point.pred, factor(newvaild.df$Competitive))
