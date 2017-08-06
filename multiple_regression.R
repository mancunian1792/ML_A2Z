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

#This tells us that R.D Spend is the single statistically significant predictor.Now, constructing a model to reflect that 
regressorMulti2Single <- lm(formula = training_set$Profit ~ training_set$R.D.Spend, data = training_set)
summary(regressorMulti2Single)

y_pred <- predict(regressor, newdata = test_set)

#Building optimal model using backward elimination
opt_regressor<- lm(formula = startup_data$Profit ~ startup_data$R.D.Spend + startup_data$Administration + startup_data$Marketing.Spend +startup_data$State,
                            data = startup_data)
summary(opt_regressor)

#Removing state --- highest p-value > SL (0.05)

opt_regressor<- lm(formula = startup_data$Profit ~ startup_data$R.D.Spend + startup_data$Administration + startup_data$Marketing.Spend,
                   data = startup_data)
summary(opt_regressor)

#Removing administration 
opt_regressor<- lm(formula = startup_data$Profit ~ startup_data$R.D.Spend + startup_data$Marketing.Spend,
                   data = startup_data)
summary(opt_regressor)

#Removing Marketing spend as p-value is > 0.05(0.06)
opt_regressor<- lm(formula = startup_data$Profit ~ startup_data$R.D.Spend,
                   data = startup_data)
summary(opt_regressor)
