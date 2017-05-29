#setting the directory
setwd("/home/ubuntu/Documents/R Scripts/data/Datasets/")

#Importing the file --CSV.
salarydataset <- read.csv('Part 2 - Regression/Section 4 - Simple Linear Regression/Simple_Linear_Regression/Simple_Linear_Regression/Salary_Data.csv')

# Splitting the data into train and test data.
library(caTools)
set.seed(123)
split = sample.split(salarydataset$Salary ,SplitRatio = 2/3)
training_set <- subset(salarydataset, split == TRUE)
test_set <- subset(salarydataset, split == FALSE)

#Training the model

regressor<- lm(formula = Salary ~ YearsExperience,
               data = training_set)

#This provides information about how the regressor has performed and its statistical significance
summary(regressor)

#Predicting the test results.
y_pred <- predict(regressor,newdata = test_set)

#Visualising the training prediction results.
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$YearsExperience,y = training_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience,y = predict(regressor,newdata = training_set)),
            colour = 'blue') +
  ggtitle('Salary vs Years of Experience (Training set)') +
  xlab('Years of Experience') +
  ylab('Salary')

# Test data visualization
ggplot() +
  geom_point(aes(x = test_set$YearsExperience,y = test_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience,y = predict(regressor,newdata = training_set)),
            colour = 'blue') +
  ggtitle('Salary vs Years of Experience (Test set)') +
  xlab('Years of Experience') +
  ylab('Salary')

