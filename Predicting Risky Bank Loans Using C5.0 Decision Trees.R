library(tidyverse)
library(C50)
library(skimr)
library(janitor)
library(corrplot)
library(gmodels)
credit <- read.csv('credit.csv', stringsAsFactors = TRUE)

names(credit)

credit$telephone <- NULL

str(credit)

# Let's see where the missingness is:

credit_na <- credit %>%
  filter(!complete.cases(.)) # There isn't any missigness. 

# Let's get rid of duplicates if there any:

credit <- credit %>%
  distinct()

# To make sure names are consistent:

clean_names(credit)

# Let's see what the target variable has inside:

unique(credit$default)

# Let's replace '1' and '2' with more descpritive words:

credit$default <- factor(credit$default, levels = c('1', '2'), labels = c('no', 'yes'))
# To get the summary of the data:

skim_without_charts(credit)

prop.table(table(credit$default)) * 100

ggplot(credit, aes(default, fill = job)) + geom_bar()

table(credit$checking_balance)
table(credit$savings_balance)

ggplot(credit, aes(savings_balance)) + geom_bar()

# Let's get the summary of the loan duration:

summary(credit$months_loan_duration)

# Let's visualize the same thing to have a better view:

ggplot(credit, aes(months_loan_duration)) + geom_histogram()

# Let's visualize the distribution of 'amount' variable:

ggplot(credit, aes(amount)) + geom_histogram()

# Let's zoom in on the x_axis:

ggplot(credit, aes(amount)) + geom_histogram() + coord_cartesian(ylim = c(0, 50))

# Let's see if there's any correlation between load duration and the amount:

cor(credit$amount, credit$months_loan_duration) # There's a slight positive correlation b/w amount and duration


# Let's create testing and training datasets:

samp <- sample(2, nrow(credit), replace = TRUE, prob = c(0.7, 0.3))

credit_train <- credit[samp == 1, ]
credit_test <- credit[samp == 2, ]


# If randomization was done correctly, we should have about 30 percent of loans with
# default in each of the datasets:

prop.table(table(credit_train$default)) * 100
prop.table(table(credit_test$default)) * 100
# Let's train the model:

m <- C5.0(credit_train[-17], credit_train$default)
summary(m)
m

# Let's evaluate the moded's performance:

p <- predict(m, credit_test)

# Let's investigate the performance:

mean(p == credit_test$default) 
# uh-oh, it's resulting in an accuracy of 73 percent and an error rate of 27 percent.

# Let's get the confusion matrix:

CrossTable(p, credit_test$default)

# Let's see if we can improve the performance by introducing the 'trials' parameter:

m1 <- C5.0(credit_train[-17], credit_train$default, trials = 10)

p1 <- predict(m1, credit_test)

mean(p1 == credit_test$default)

CrossTable(p1, credit_test$default) # uh-oh, it didn't show any improvements.

# Let's introduce the 'cost' paramters. We don't that some mistakes are costly than other.
# The C5.0 algorithm allows us to assign a penalty to different types of errors in order to discourage 
# a tree from making more costly mistakes 

# let's build the cost_matrix:

cost_matrix <- list(c('no', 'yes'), c('no', 'yes'))

names(cost_matrix) <- c('predicted', 'actual')

cost_matrix

#Next, we need to assign the penalty for the various types of errors by supplying four values to fill the matrix.

# Suppose we believe that a loan default costs the bank four times as much as a missed opportunity.
# Our penalty values then could be defined as:

error_cost <- matrix(c(0, 1, 4, 0), nrow = 2, dimnames = cost_matrix)

# let's build the model again with 'cost' parameter:

m2 <- C5.0(credit_train[-17], credit_train$default, trials = 10, costs = error_cost)

p2 <- predict(m2, credit_test)

mean(p2 == credit_test$default)

CrossTable(p2, credit_test$default) # The performance has improved a bit, but not as much we had hoped.

# Adding more, there are more 'false negatives' then 'false positives' which is isn't encouraging at all.
# So, we have to review the dataset again and eliminate the the variables which aren't useful in predicting outcome variable.

