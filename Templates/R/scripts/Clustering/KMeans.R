# K-Means Clustering

library(cluster)

# Reading Dataset
dataset <- utils::read.csv('./input/Mall_Customers.csv')
X <- dataset[4:5]

# Using elbow method to find the optimal number of clusters
base::set.seed(6)
wcss <- base::vector()
for(i in 1:10) wcss[i] <- sum(kmeans(X,i)$withinss)
plot(1:10, wcss, type = 'b', main = paste('Clusters of Clients'), xlab = 'Number of Clusters', ylab = 'WCSS')

# Applying K-means to the Mall Dataset
base::set.seed(29)
kmeans <- kmeans(X, 5, iter.max = 300, nstart = 10)

# Visualizing Clusters
cluster::clusplot(X, kmeans$cluster, lines = 0, shade = TRUE, color = TRUE, labels = 2, plotchar = FALSE,
                  span = TRUE, main = paste('Clusters of Clients'), xlab = 'Annual Income', ylab = 'Spending Score')