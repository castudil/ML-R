
# Install the ds4psy package if not already installed
#install.packages("ds4psy")

# Load the ds4psy package
library(ds4psy)

# Load the 'data_t1' dataset from the ds4psy package
data("data_t1")

# Display the first few rows of the dataset
head(data_t1)

# Summary and structure of the dataset for exploration
summary(data_t1)
str(data_t1)

# check for missing values
any(is.na(data_t1))

# count row with missing values
sum(is.na(data_t1))

# Contar las filas con valores faltantes
num_rows_with_na <- sum(apply(data_t1, 1, function(row) any(is.na(row))))
# Imprimir el resultado
cat("Número de filas con valores faltantes:", num_rows_with_na, "\n")


# Filtrar y mostrar las filas con valores faltantes
rows_with_na <- data_t1[apply(data_t1, 1, function(row) any(is.na(row))), ]
# Imprimir las filas con valores faltantes
print(rows_with_na)

# imprimir cuantas instancias
print(nrow(data_t1))



# plot missing values

#install.packages("DataExplorer")
library(DataExplorer)
plot_missing(data_t1)

# create a report using the DataExplorer package
#install.packages("rmarkdown")
#library(rmarkdown)
#create_report(data_t1, output_file = "data_t1_report.html")

# remove rows with missing values
data_t1 <- na.omit(data_t1)
plot_missing(data_t1)

# save the column "name" in a new variable
name <- data_t1$name

# remove the column "name" from the dataset
data_t1 <- data_t1[, -which(names(data_t1) == "name")]

# print the number of rows and columns in the dataset
print(dim(data_t1))

# plot data_t1 2d scatterplot for vairables like_1 and bnt_2 using the ggplot2. use points colored with "gender"
#install.packages("ggplot2")
library(ggplot2)


# Create a scatter plot for variables like_1 and bnt_2 colored by gender
ggplot(data = data_t1, aes(x = like_1, y = bnt_1, color = gender)) +
  geom_point(alpha = 0.7) +
  labs(title = "Scatterplot of like_1 vs. bnt_2",
       x = "like_1",
       y = "bnt_2",
       color = "Gender") +
  theme_minimal()

# plotting again but with jitter
ggplot(data = data_t1, aes(x = like_1, y = bnt_1, color = gender)) +
  geom_point(alpha = 0.7) +
  labs(title = "Scatterplot of like_1 vs. bnt_2",
       x = "like_1",
       y = "bnt_2",
       color = "Gender") +
  theme_minimal()+ 
  geom_jitter()

#print data
print(data_t1)



