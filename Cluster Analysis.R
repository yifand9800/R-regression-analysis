## Cluster Analysis
# Load
utilities.df <- read.csv("Utilities.csv", stringsAsFactors = TRUE)

## Normalize data
library(caret)
# compute mean and stdev of each column
norm.values <- preProcess(utilities.df, method=c("center", "scale"))
# normalization
utilities.df.norm <- predict(norm.values, utilities.df)

set.seed(1234)
# use K = 6 to perform K-mean analysis
km <- kmeans(utilities.df.norm, 6)
# cluster membership: cluster from each row
km$cluster

# centroids: avg behaviors of each cluster 1:k
km$centers

# within-cluster sum of squared distances
km$withinss

# total within-cluster sum of square
km$tot.withinss

## Interpret the resulting clusters: help facilitating business decision making
# 1. obtain summary from each cluster on each variable
# 2. label clusters: assign a name

plot(c(0), xaxt = 'n', ylab = "", type = "l", 
     ylim = c(min(km$centers), max(km$centers)), xlim = c(0, 8))

# label x-axes
axis(1, at = c(1:8), labels = names(utilities.df))

# plot centroids
for (i in c(1:6))
  lines(km$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3,5),
                                                       "black", "dark grey"))
# name clusters
text(x = 0.5, y = km$centers[, 1], labels = paste("Cluster", c(1:6)))
