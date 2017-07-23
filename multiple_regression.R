#setting the directory
setwd("/home/ubuntu/Documents/R Scripts/data/Datasets/")

#Importing the file --CSV.
startup_data <- read.csv('Part 2 - Regression/Section 5 - Multiple Linear Regression/50_Startups.csv')

# Encoding categorical data --> using factors.
startup_data$State <- factor(startup_data$State,
                             levels = c('New York','Florida','California'),
                             labels = c(1,2,3))


# Splitting the data into train and test data.
library(caTools)
set.seed(123)
split = sample.split(startup_data$Profit ,SplitRatio = 0.8)
training_set <- subset(startup_data, split == TRUE)
test_set <- subset(startup_data, split == FALSE)

#Training the model
# One way
#regressor<- lm(formula = startup_data$Profit ~ startup_data$R.D.Spend + startup_data$Administration + startup_data$Marketing.Spend +startup_data$State,
#              data = training_set)
# Trick to refer the dependant variable is linked to all indeoendant variable

regressor <- lm(formula = Profit ~ ., data = training_set )

#This provides information about how the regressor has performed and its statistical significance
summary(regressor)

