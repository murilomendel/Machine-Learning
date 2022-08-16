# Hierarchical Clustering

library(cluster)

# Reading Dataset
dataset <- utils::read.csv('./input/Mall_Customers.csv')
X <- dataset[4:5]

# Using the Dendrogram to find the optimal number of clusters.
# Best = 5 clusters
dendrogram <- stats::hclust(stats::dist(X, method = 'euclidean'), method = 'ward.D')
plot(dendrogram,
     main = paste('Dendrogram'),
     xlab = 'Customers',
     ylab = 'Euclidean Distances')

# Fitting hierarchical clustering to the Mall_Customers Dataaset
hc <- stats::hclust(stats::dist(X, method = 'euclidean'), method = 'ward.D')
y_hc <- stats::cutree(hc, 5)

# Visualizing the Clusters
cluster::clusplot(X, y_hc, lines = 0, shade = TRUE, color = TRUE, labels = 2, plotchar = FALSE,
                  span = TRUE, main = paste('Clusters of Clients'), xlab = 'Annual Income', ylab = 'Spending Score')

