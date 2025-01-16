# Install and load required packages
#install.packages("class")
#install.packages("caret")
#install.packages("ds4psy")

library(class)
library(caret)
library(ds4psy)

# Load the data_t1 dataset
data("data_t1")

# Prepare the data: Example using two numerical predictors and one classification target
data_clean <- na.omit(data_t1)  # Removing rows with missing values
X <- data_clean[, c("like_1", "bnt_1")]  # Selecting two numerical columns
y <- data_clean$gender  # Target variable

# Define the LOOCV control
ctrl <- trainControl(method = "LOOCV")

# Train the 1-NN classifier using LOOCV
set.seed(123)  # For reproducibility
knn_model <- train(X, y, method = "knn", 
                   trControl = ctrl, 
                   tuneGrid = data.frame(k = c(1,3,5,7,9)),
                   metric = "Accuracy")

print(knn_model)
cat("LOOCV Accuracy:", knn_model$results$Accuracy, "\n")
