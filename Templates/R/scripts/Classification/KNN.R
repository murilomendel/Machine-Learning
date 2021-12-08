library(caTools)
library(class)

# K-Nearest NEighbors (KNN)
# Step 1 - Choose the number K of neighbors (Commonly K = 5)
# Step 2 - Take the K nearest neighbors of the new data point, according to the Euclidean distance
# Step 3 - Among these K neighbors, count the number of data points in each category.
# Step 4 = Assign the new data point to the category where you counted the most neighbors.

# Euclidean Distance = sqrt((x2-x1)^2 + (y2 - y1)^2)

# Importing Dataset
dataset <- utils::read.csv('./input/Social_Network_Ads.csv')
dataset <- dataset[3:5]

# Encoding the target feature as factor
dataset$Purchased <- base::factor(dataset$Purchased, levels = c(0,1))

# Splitting the dataset into Training and Test set
set.seed(123)
split <- caTools::sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set <- base::subset(dataset, split == TRUE)
test_set <- base::subset(dataset, split == FALSE)

# Feature Scaling
training_set[,1:2] = base::scale(training_set[, 1:2])
test_set[,1:2] = base::scale(test_set[, 1:2])

# Fitting the KNN to the training set and Predicting the Test set results
y_pred <- class::knn(train = training_set[,-3],
                     test = test_set[,-3],
                     cl = training_set[,3],
                     k = 5)

# Making the confusion Matrix
cm <- base::table(test_set[, 3], y_pred)
cm
