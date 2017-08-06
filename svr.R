#setting the directory
setwd("/home/ubuntu/Documents/R Scripts/data/Datasets/")

dataset<- read.csv('Part 2 - Regression/Section 7 - Support Vector Regression (SVR)/Position_Salaries.csv')
dataset<-dataset[2:3]
# Do the below if you havent installed and loaded the e1071 package
#install.packages('e1071')
#library(e1071)

regressor =svm(formula = dataset$Salary ~ ., data = dataset, type = 'eps-regression' )
#Predicting a new result
y_pred = predict(regressor, data.frame(Level = 6.5))

# Visualising the Regression Model results
# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(regressor, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Regression Model)') +
  xlab('Level') +
  ylab('Salary')

# Visualising the Regression Model results (for higher resolution and smoother curve)
# install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Regression Model)') +
  xlab('Level') +
  ylab('Salary')