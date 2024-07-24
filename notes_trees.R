# Install and load the necessary package
install.packages("rpart")
library(rpart)

## Classification Tree Example:
# Load the iris dataset
data(iris)
head(iris)

# Build the classification tree model
classification_model <- rpart(Species ~ ., data = iris, method = "class")

# Visualize the tree
plot(classification_model)
text(classification_model, use.n = TRUE)

# Print a detailed summary of the tree
summary(classification_model) 
# output = predicts a grouped classification (i.e., species groups) 
# based on species characteristics, with the number of 
# each species that lands at each node (#/#/#) 

## Regression Tree Example:
# Load the mtcars dataset
data(mtcars)
head(mtcars)

# Build the regression tree model
regression_model <- rpart(mpg ~ ., data = mtcars, method = "anova")

# Visualize the tree
plot(regression_model)
text(regression_model, use.n = TRUE)

# Print a detailed summary of the tree
summary(regression_model)
# output = predicts a continuous variable (i.e., mpg) 
# based on additional characteristics (e.g., cyl, hp), 
# with the average mpg and p-value at the end of each node 
