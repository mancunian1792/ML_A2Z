#setting the directory
setwd("/home/ubuntu/Documents/R Scripts/data/Datasets/")


#Import the dataset.
dataset <- read.csv("Part 2 - Regression/Section 9 - Random Forest Regression/Position_Salaries.csv")
dataset <- dataset[, 2:3]


# Fit the model using random forest regression.
# install the package if you havent 
# install.packages('randomForest')
library(randomForest)
set.seed(1234)
regressor <- randomForest(x= dataset[1], y = dataset$Salary, ntree = 500)

# Predicting a new result
y_pred = predict(regressor, data.frame(Level = 6.5))

# Visualising the Regression Model results (for higher resolution and smoother curve)
# install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.01)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Decision Tree Regression Model)') +
  xlab('Level') +
  ylab('Salary')