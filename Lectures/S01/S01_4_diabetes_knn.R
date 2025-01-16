# load diabetes from kaggle


#https://www.kaggle.com/datasets/uciml/pima-indians-diabetes-database

diabetes <- read.csv("diabetes.csv", header = TRUE)
dim(diabetes)

# head diabetes
head(diabetes)

#print missing values
print(sum(is.na(diabetes)))



# split the data into training and test sets
set.seed(123)
trainingRowIndex <- sample(1:nrow(diabetes), 0.8*nrow(diabetes))
trainingData <- diabetes[trainingRowIndex,]
testData  <- diabetes[-trainingRowIndex,]


## create a function that find the best K for KNN, the function return an object containing accuracy,model,cm
findBestK <- function(trainingData, testData, maxK){
  accuracy <- rep(0, maxK)
  models <- list()
  cms <- list()
  for (i in 1:maxK){
    model <- knn(train = trainingData[,-9], test = testData[,-9], cl = trainingData$Outcome, k = i)
    cm <- table(testData$Outcome, model)
    accuracy[i] <- sum(diag(cm))/sum(cm)
    models[[i]] <- model
    cms[[i]] <- cm
  }
  bestK <- which.max(accuracy) # identify the best K
  return(list(accuracy = accuracy, model = models[[bestK]], cm = cms[[bestK]], k=bestK))
}

# test the function
bestK <- findBestK(trainingData, testData, 20)

# print a message showing the best K
print(paste("The best K is ",bestK$k))

print(bestK$accuracy)
print(bestK$cm)

# confusion matrix
cm <- table(testData$Outcome, bestK$model)
print(cm)

# accuracy
accuracy <- sum(diag(cm))/sum(cm)

print(accuracy)


library(caret)
confusionMatrix(cm) # confusion matrix with more details













