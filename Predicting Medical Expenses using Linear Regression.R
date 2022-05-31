library(tidyverse)
library(skimr)
library(janitor)
install.packages('psych')
library(psych)


# PROBLEM: The goal of this analysis is to use patient data to forecast
# the average medical care expenses for such population segments.
#These estimates could be used to create actuarial tables that set the price of
#yearly premiums higher or lower according to the expected treatment costs.

# Step One: Data Collection
# You can find the dataset we're using at: 
# <https://www.kaggle.com/datasets/awaiskaggler/insurance-csv>

insurance <- read.csv("insurance 2.csv", stringsAsFactors = TRUE)


# Step Two: Data Exploration and Manipulation

clean_names(insurance) # Just to make sure variables' names are consistant

names(insurance)
str(insurance)

skim_without_charts(insurance) # It'll give us the summary of dataset

attach(insurance)
round(prop.table(table(sex)) * 100)
summary(age) # Max 'age' is 64 bc people older than 64 are covered by the govt.

detach(insurance)

# To see where the missingness is: 
insurance_na <- insurance %>%
  select(age, everything()) %>%
  filter(!complete.cases(.))

# To check if there are any duplicates: 

duplicated(insurance)

# To check for normality in dependent variable 'expenses'.

summary(insurance$expenses) # It seems the distribution is right_skewed

# Let's verify it visually:

hist(insurance$expenses) # Since Linear Regression doesn't strict require normal distribution, we'll just move on:


table(insurance$region)

# To check 'correlation' among numeric features:

cor(insurance[c('age', 'bmi', 'children', 'expenses')])

# Visualizing relationships among features – the scatterplot matrix

pairs(insurance[c('age', 'bmi', 'children', 'expenses')]) # It's difficult to spot any trends in plots

# let's add some more information to the plots using pairs.panels() function from 'psych' package:

pairs.panels(insurance[c('age', 'children', 'bmi', 'expenses')])

# Step Three: Training a model

m <- lm(expenses ~ age + children + bmi + sex +
        smoker + region, data = insurance)

# The equilant command will be: 

m <- lm(expenses ~ ., data = insurance)

m

# Step Four: Evaluating Model Performance
# We don't know how well the model is doing so far, so let's evaluate the model's performance.

summary(m)

# It is not uncommon for regression models of real-world data to have fairly low R-squared values;
# a value of 0.75 is actually quite good. The size of some of the errors is a bit concerning, 
#but not surprising given the nature of medical expense data. 

# Step Five: Improving Model's Performance:

# Model specification – adding nonlinear relationships

# To add the nonlinear age to the model, we simply need to create a new variable:

insurance$age2 <- insurance$age^2

# Transformation – converting a numeric variable to a binary indicator

insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)


# So far, we have only considered each feature's individual contribution to the outcome. 
# What if certain features have a combined impact on the dependent variable? 


#For instance, smoking and obesity may have harmful effects separately,
# but it is reasonable to assume that their combined effect may be worse than the sum of each one alone.

# Putting it all together – an improved regression model

m1 <- lm(expenses ~ age + age2 + children + bmi + sex +
        bmi30*smoker + region, data = insurance)
summary(m1)

#Relative to our first model, the R-squared value has improved from 0.75 to about 0.87.

# Making predictions with a regression model

insurance$pred <- predict(m1, insurance)
cor(insurance$pred, insurance$expenses)

#The correlation of 0.93 suggests a very strong linear relationship between the predicted and actual values.
# This is a good sign—it suggests that the model is highly accurate! 

#It can also be useful to examine this finding as a scatterplot.

plot(insurance$pred, insurance$expenses)
abline(a = 0, b = 1, col = "red", lwd = 3, lty = 2)

#to estimate the insurance expenses for a 30 year old, 
#overweight, male non-smoker with two children in the Northeast, type:

predict(m1,
        data.frame(age = 30, age2 = 30^2, children = 2,
                   bmi = 30, sex = "male", bmi30 = 1,
                   smoker = "no", region = "northeast"))
# Using this value, the insurance company might need to set its prices to about $6,000 per year,
# or $500 per month in order to break even for this demographic group.

# To compare the rate for a female who is otherwise similar, use the predict() function in much the same way:

predict(m1,
        data.frame(age = 30, age2 = 30^2, children = 2,
                   bmi = 30, sex = "female", bmi30 = 1,
                   smoker = "no", region = "northeast"))
#Note that the difference between these two values, 5,973.774 - 6,470.543 = -496.769, is the same as the estimated regression model coefficient for sexmale.
# On average, males are estimated to have about $496 less in expenses for the plan per year, all else being equal.


#Following similar steps for a number of additional customer segments, 
# the insurance company would be able to develop a profitable pricing structure for various demographics.