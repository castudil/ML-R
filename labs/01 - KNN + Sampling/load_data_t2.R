
# ---------------------------------------------------------
# Loading and Displaying the data_t2 Dataset from ds4psy
# ---------------------------------------------------------

# Install the ds4psy package if not already installed
#install.packages("ds4psy")

# Load the ds4psy package
library(ds4psy)

# Load the data_t2 dataset
data("data_t2")

# Display the first few rows of the dataset
cat("First few rows of data_t2:\n")
head(data_t2)

# Display summary statistics of the dataset
cat("Summary statistics:\n")
summary(data_t2)

# Display the structure of the dataset
cat("Structure of the dataset:\n")
str(data_t2)

# ---------------------------------------------------------
# End of Script
# ---------------------------------------------------------
