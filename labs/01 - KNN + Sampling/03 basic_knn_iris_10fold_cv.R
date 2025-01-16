
# ---------------------------------------------------------
# Basic KNN Classification on the Iris Dataset with 10-Fold CV
# ---------------------------------------------------------

# Load necessary libraries
#install.packages("caret")  # For classification and cross-validation
#install.packages("class")  # For KNN algorithm

library(caret)
library(class)

# Load the Iris dataset (built-in R dataset)
data(iris)

# Set a seed for reproducibility
set.seed(123)

# Prepare the data: features and target variable
X <- iris[, -5]  # Selecting all columns except species (target)
y <- iris$Species  # Target variable

# Define 10-fold cross-validation control
ctrl <- trainControl(method = "cv", number = 10)

# Train the KNN classifier with k=3
knn_model <- train(X, y, method = "knn", 
                   trControl = ctrl, 
                   tuneGrid = data.frame(k = 3))

# Print model summary
print(knn_model)

# Generate predictions using the trained model
predictions <- predict(knn_model, newdata = X)

# Compute the confusion matrix
conf_matrix <- confusionMatrix(predictions, y)

# Print the confusion matrix and accuracy
cat("Confusion Matrix:\n")
print(conf_matrix$table)
cat("Overall Accuracy:", conf_matrix$overall['Accuracy'], "\n")

# ---------------------------------------------------------
# End of Script
# ---------------------------------------------------------
