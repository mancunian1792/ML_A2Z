#setting the directory
setwd("/home/ubuntu/Documents/R Scripts/data/Datasets/")

#Importing the file --CSV.
dataset <- read.csv('Part 1 - Data Preprocessing/Section 2 -------------------- Part 1 - Data Preprocessing --------------------/Data.csv')


str(dataset)
# Imputing the missing data.
# if the data is missing, then fill it with the average (user defined anonymous function) 
# which is nothing but mean function and the remove na option set to true.
dataset$Age <- ifelse(is.na(dataset$Age),
                      ave(dataset$Age,FUN = function(x) mean(x,na.rm = TRUE)),
                      dataset$Age)
dataset$Salary <- ifelse(is.na(dataset$Salary),
                      ave(dataset$Salary,FUN = function(x) mean(x,na.rm = TRUE)),
                      dataset$Salary)

# Encoding categorical data --> using factors.
dataset$Country <- factor(dataset$Country,
                          levels = c('France','Spain','Germany'),
                          labels = c(1,2,3))
dataset$Purchased <- factor(dataset$Purchased,
                          levels = c('No','Yes'),
                          labels = c(0,1))

# Splitting the data into train and test data.
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased ,SplitRatio = 0.8)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

#Feature scaling.
training_set[, 2:3] <- scale(training_set[, 2:3])
test_set[, 2:3] <- scale(test_set[, 2:3])
