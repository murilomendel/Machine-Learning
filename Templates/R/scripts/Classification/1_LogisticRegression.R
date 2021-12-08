library(caTools)

# Logistic Regression

# Importing the dataset
dataset <- utils::read.csv('./input/Social_Network_Ads.csv')
dataset <- dataset[,3:5]
 
# Splitting the dataset into Training and Test set
set.seed(123)
split <- caTools::sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set <- base::subset(dataset, split == TRUE)
test_set <- base::subset(dataset, split == FALSE)

# Feature Scaling
training_set[,1:2] = base::scale(training_set[, 1:2])
test_set[,1:2] = base::scale(test_set[, 1:2])

# Fitting Logistic Regression to the Training set
classifier <- stats::glm(formula = Purchased ~ .,
                         family = binomial,
                         data = training_set)

# Predict Test set results
prob_pred <- stats::predict(classifier, type = 'response', newdata = test_set[-3])
y_pred <- base::ifelse(prob_pred > 0.5, 1, 0)

# Making the confusion Matrix
cm <- base::table(test_set[, 3], y_pred)

# Visualizing the training set results
