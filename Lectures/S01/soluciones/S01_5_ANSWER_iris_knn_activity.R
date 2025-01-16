
# Librerías necesarias
install.packages(c("class", "caret", "dplyr"))
library(class)
library(caret)
library(dplyr)

# Cargar el dataset Iris
data(iris)

# Explorar las primeras filas del dataset
head(iris)

# División del dataset en entrenamiento y prueba (80% - 20%)
set.seed(123)
train_index <- createDataPartition(iris$Species, p = 0.8, list = FALSE)
train_data <- iris[train_index, ]
test_data <- iris[-train_index, ]

# Separar características y etiquetas
train_X <- train_data[, -ncol(train_data)]
train_Y <- train_data$Species
test_X <- test_data[, -ncol(test_data)]
test_Y <- test_data$Species

# Aplicar KNN con distintos valores de k
k_values <- c(1, 3, 5, 7, 9)
results <- data.frame(k = integer(), Accuracy = double())

for (k in k_values) {
  knn_model <- knn(train = train_X, test = test_X, cl = train_Y, k = k)
  confusion_mat <- confusionMatrix(as.factor(knn_model), test_Y)
  results <- rbind(results, data.frame(k = k, Accuracy = confusion_mat$overall['Accuracy']))
}

# Mostrar resultados de la precisión para cada k
print(results)

# Evaluar con el mejor valor de k
best_k <- results$k[which.max(results$Accuracy)]
knn_best_model <- knn(train = train_X, test = test_X, cl = train_Y, k = best_k)
best_k
# Matriz de confusión final
final_conf_matrix <- confusionMatrix(as.factor(knn_best_model), test_Y)
print(final_conf_matrix)

cat("Actividad completada con éxito. ¡Bien hecho!")
