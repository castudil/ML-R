# plot iris data

#install.packages("ggplot2")

library(ggplot2)

# load iris data

data(iris)

# plot iris data

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  labs(title = "Iris Data",
       x = "Sepal Length",
       y = "Sepal Width",
       color = "Species") +
  theme_minimal()

# save plot

# plot a scatter plot matrix of iris data without ggplot

pairs(iris[1:4], col = iris$Species, pch = 19)


