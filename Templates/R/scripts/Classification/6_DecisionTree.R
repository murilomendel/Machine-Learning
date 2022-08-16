library(rpart)
library(caTools)
library(class)

# Decision Tree

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
# training_set[,1:2] = base::scale(training_set[, 1:2])
# test_set[,1:2] = base::scale(test_set[, 1:2])

# Fitting the Naive Bayes to the training set 
classifier <- rpart::rpart(formula = Purchased ~ .,
                           data = training_set)

# Predicting the Test set results
y_pred <- predict(classifier, newdata = test_set[-3], type = 'class')

# Making the confusion Matrix
cm <- base::table(test_set[, 3], y_pred)
cm

# Plotting Decision Tree
plot(classifier)
text(classifier)
