# Algoritmo Naive Bayes: Filtrado de spam
# Autor: David García Sabaté
# Fecha: 27/07/2019

# Carga de librerías necesarias
library(tidyverse)
#install.packages("tm")
library(tm)
#install.packages("SnowballC")
library(SnowballC)
#install.packages("wordcloud")
library(wordcloud)
library(e1071)
library(caret)

# ================================================
# Recogida de datos
# ================================================

# URL del archivo CSV con los datos
archivo <- "https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/sms_spam.csv"

# Descargamos el archivo
download.file(archivo, destfile = "sms_spam.csv")

# Leemos los datos del archivo CSV
data <- read.csv("sms_spam.csv", stringsAsFactors = FALSE)

# Factorizamos la columna 'type'
data$type <- factor(data$type)

#check data types
str(data)
# ================================================
# Limpieza y normalización de los textos
# ================================================

# Creamos un corpus de textos
sms_text <- VCorpus(VectorSource(data$text))

# Procesamiento del texto: aplicamos varias transformaciones
sms_text_clean <- sms_text %>%
  tm_map(content_transformer(tolower)) %>%  # Convertir a minúsculas
  tm_map(removeNumbers) %>%                 # Eliminar números
  tm_map(removeWords, stopwords()) %>%      # Eliminar palabras vacías
  tm_map(content_transformer(function(x) gsub("[[:punct:]]+", " ", x))) %>% # Sustituir signos de puntuación por espacios
  tm_map(stemDocument) %>%                  # Stemming
  tm_map(stripWhitespace)                   # Eliminar espacios en blanco sobrantes

# Crear una matriz de términos del documento
sms_dtm <- DocumentTermMatrix(sms_text_clean)

# ================================================
# Creación de datasets de entrenamiento y prueba
# ================================================

# División de datos en conjuntos de entrenamiento y prueba
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test <- sms_dtm[4170:5574, ]

# Etiquetas de entrenamiento y prueba
sms_train_labels <- data[1:4169, ]$type
sms_test_labels <- data[4170:5574, ]$type

# Proporción de spam en cada conjunto
cat("Proporciones en el conjunto de entrenamiento:\n")
print(prop.table(table(sms_train_labels)))
cat("Proporciones en el conjunto de prueba:\n")
print(prop.table(table(sms_test_labels)))

# ================================================
# Visualización de los datos con nubes de palabras
# ================================================

# Nube de palabras global
wordcloud(sms_text_clean, min.freq = 100, random.order = FALSE)

# Creación de subsets para SPAM y HAM
spam <- filter(data, type == "spam")
ham <- filter(data, type == "ham")

# Nubes de palabras para SPAM y HAM
cat("Nube de palabras para SPAM:\n")
wordcloud(spam$text, min.freq = 50, random.order = FALSE)
cat("Nube de palabras para HAM:\n")
wordcloud(ham$text, min.freq = 100, random.order = FALSE)

# ================================================
# Reducción de características
# ================================================

# Encontrar términos frecuentes
sms_freq_words <- findFreqTerms(sms_dtm_train, 7)

# Filtrar la matriz de términos por las palabras frecuentes
sms_dtm_freq_train <- sms_dtm_train[, sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[, sms_freq_words]

# Convertir frecuencias en categóricas (presencia/ausencia)
convert_counts <- function(x) {
  ifelse(x > 0, "yes", "no")
}

sms_dtm_freq_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_dtm_freq_test <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)

# ================================================
# Entrenamiento del modelo
# ================================================

# Crear el clasificador Naive Bayes
sms_classifier <- naiveBayes(sms_dtm_freq_train, sms_train_labels)

# ================================================
# Evaluación del modelo
# ================================================

# Predicciones en el conjunto de prueba
prediccion_test <- predict(sms_classifier, sms_dtm_freq_test)

# Matriz de confusión y métricas
cat("Evaluación del modelo (Laplace = 0):\n")
print(confusionMatrix(data = prediccion_test, reference = sms_test_labels))

# ================================================
# Mejora del modelo
# ================================================

# Entrenamiento con Laplace = 1
sms_classifier2 <- naiveBayes(sms_dtm_freq_train, sms_train_labels, laplace = 1)

# Predicciones con el modelo mejorado
prediccion_test2 <- predict(sms_classifier2, sms_dtm_freq_test)

# Matriz de confusión y métricas para el modelo mejorado
cat("Evaluación del modelo mejorado (Laplace = 1):\n")
print(confusionMatrix(data = prediccion_test2, reference = sms_test_labels))
