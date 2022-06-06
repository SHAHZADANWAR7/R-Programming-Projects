# We'll try to redict concrete strength given a listing of the composition of the input materials could result
# in safer construction practices.


library(tidyverse)
library(skimr)
library(janitor)
library(corrplot)
library(neuralnet)
concrete <- read_csv('https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/concrete.csv')

# Exploration and Cleaning:

view(concrete)
glimpse(concrete) # The nine variables in the data frame correspond to the eight features and one outcome we expected

# To see where the missingness is:

concrete_na <- concrete %>%
  filter(!complete.cases(.))

# To confirm if there are any duplicates:

concrete_dup <- concrete %>%
  filter(duplicated(concrete))

# It seems there are a few duplicates. Let's get rid of them:

concrete <- concrete %>%
  filter(!duplicated(concrete))

# to have consistency in names:

clean_names(concrete)

# To get some statistics:

skim_without_charts(concrete) # It doesn't seem to be making a lot of sense.

# Let's using summary() function:

summary(concrete)

# There seems to be few distribution. let's confirm with visualization:

ggplot(concrete, aes(cement)) + geom_histogram()
ggplot(concrete, aes(slag)) + geom_histogram()

ggplot(concrete, aes(strength)) + geom_histogram()

# Let's check for correlation: We'll use corrplot() function from 'corrplot' package:
par(mfrow = c(1,1))
corr <- cor(concrete)
corrplot.mixed(corr) # cement seems to have strong correlation with strength. 

# Neural networks work best when the input data are scaled to a narrow range around zero,
# and here we see values ranging anywhere from zero to over a thousand.

# Since our data are severly non-normal, so we'll utilize 'normalization' to a zero to one range.

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

concrete_n <- as.data.frame(lapply(concrete, normalize))

# To confirm that the normalization worked, 
# we can see that the minimum and maximum strength are now zero and one, respectively:

view(concrete_n)
summary(concrete_n)

#In comparison, the original minimum and maximum values were 2.33 and 82.60:

summary(concrete$strength)

# we will partition the data into a training set with 75 percent of the examples and a testing set with 25 percent.
set.seed(123)
samp <- sample(2, nrow(concrete_n), replace = TRUE, prob = c(0.75, 0.25))
train <- concrete_n[samp == 1,]
test <- concrete_n[samp == 2,]

# We'll use the training dataset to build the neural network 
# and the testing dataset to evaluate how well the model generalizes to future results.

m <- neuralnet(strength ~ ., train, hidden = 1, act.fct = 'logistic')

# We can then visualize the network topology using the plot() function on the resulting model object:

plot(m)

p <- compute(m, test)

predicted_strength <- p$net.result

# we'll measure the correlation between our predicted concrete strength and the true value
# If the predicted and actual values are highly correlated, 
# the model is likely to be a useful gauge of concrete strength.

cor(predicted_strength, test$strength)

# the correlation here of about 0.824 indicates a fairly strong relationship. 
# This implies that our model is doing a fairly good job, even with only a single hidden node.

#Given that we only used one hidden node, it is likely that we can improve the performance of our model.
# Let's try to do a bit better.

m1 <- neuralnet(strength ~ ., train, hidden = 5)
plot(m1)
p1 <- compute(m1, test)
cor(p1$net.result, test$strength)

#Applying the same steps to compare the predicted values to the true values,
# we now obtain a correlation around 0.93


#Despite these substantial improvements, there is still more we can do to attempt to improve the model performance.

#In particular, we have the ability to add additional hidden layers and to change the network's activation function.

# In making these changes, we create the foundations of a very simple deep neural network.

#we can use a smooth approximation of the ReLU (an activation function) known as softplus or SmoothReLU

softplus <- function(x) { log(1 + exp(x)) }

#You may also find that neural networks quickly become much more complicated when we add additional hidden layers
#algorithm fails to find a useful solution due to an inability to converge in a reasonable time

# So, we won't add any additional hidden layers of nodes and go for eight hidden nodes

m2 <- neuralnet(strength ~ ., train, hidden = 8, act.fct = softplus)
plot(m2)
p2 <- compute(m2, test)

predicted_strength2 <- p2$net.result
cor(predicted_strength2, test$strength)                

test$pred <- p2$net.result

#One important thing to be aware of is that, because we had normalized the data prior to training the model,
#the predictions are also on a normalized scale from zero to one. 
# Let's unnormalize the data:


unnormalize <- function(x) {
  return((x * (max(concrete$strength)) -
            min(concrete$strength)) + min(concrete$strength))
}

set.seed(123)
samp1 <- sample(2, nrow(concrete), replace = TRUE, prob = c(0.75, 0.25))
test1 <- concrete[samp1 == 2,]

strengths <- data.frame(test1$strength, p2$net.result)


strengths$predicted <- unnormalize(strengths$p2.net.result)

strengths$p2.net.result <- NULL
cor(strengths$test1.strength, strengths$predicted)

# When applying neural networks to your own projects, 
# you will need to perform a similar series of steps to return the data to its original scale.