# we will develop a model similar to those used at the core of the optical character recognition (OCR) software 
# often bundled with desktop document scanners or in smartphone applications.

library(kernlab)
library(e1071)

library(tidyverse)
library(janitor)
library(skimr)

# we'll use a dataset donated to the UCI Machine Learning Repository
# he dataset contains 20,000 examples of 26 English alphabet capital letters 
# as printed using 20 different randomly reshaped and distorted black-and-white fonts.

letters <- read_csv('https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/letterdata.csv')

str(letters)
letters$letter <- as_factor(letters$letter)

# Just to make sure there's consistancy in features' names:

clean_names(letters)
view(letters)

# TO check for missingness:
letters_na <- letters %>%
  filter(!complete.cases(.))

# TO confirm if there are any duplicates: 

letters_dup <- letters %>%
  filter(duplicated(letters)) # Oops!! There are over 1300 duplicates.

# Let's get rid of duplicates:
letters <- letters %>%
  distinct()

# SVM learners require all features to be numeric, and moreover,
# that each feature is scaled to a fairly small interval. But we don't have to normalize or standardize the data manually
# bc the model the R package we'll use for fitting SVM model will perform the rescalling automattically. 

# Given that there is no data preparation left to perform,
# we can move directly to the training and testing phases of the machine learning process.

samp <- sample(2, nrow(letters), replace = TRUE, prob = c(0.8, 0.2))
letters_train <- letters[samp == 1,]
letters_test <- letters[samp == 2, ]
# We'll use ksvm() from kernlab package to train the model>
# To provide a baseline measure of SVM performance, let's begin by training a simple linear SVM classifier


letters_classifier <- ksvm(letter ~ ., data = letters_train, kernel = 'vanilladot')
letters_classifier

#This information tells us very little about how well the model will perform in the real world
# We'll need to examine its performance on the testing dataset to know whether it generalizes well to unseen data

letters_predictions <- predict(letters_classifier, letters_test)
head(letters_predictions)

table(letters_predictions, letters_test$letter)


# We can simplify our evaluation by instead calculating the overall accuracy.


agreement <- letters_predictions == letters_test$letter
table(agreement)
prop.table(table(agreement)) * 100 # In percentage terms, the accuracy is about 84 percent

# an accuracy of 84 percent is not nearly high enough to be useful for OCR software
# by adjusting the SVM function parameters to train a slightly more complex model,
# we can also find that the model is useful in the real world.

# Changing the SVM kernel function

letters_classifier_rbf <- ksvm(letter ~ ., data = letters_train, kernel = 'rbfdot')
letters_predictions_rbf <- predict(letters_classifier_rbf, letters_test)

agreement_rbf <- letters_predictions_rbf == letters_test$letter
table(agreement_rbf)

prop.table(table(agreement_rbf)) * 100 # We can see an improvement in performance by about 9 percent.

# Let's try to improve further by changing the cost parameter, which modifies the width of the SVM decision boundary
# the larger the cost value, the harder the learner will try to perfectly classify every training instance

cost_values <- c(1, seq(3, 50, by = 3))

accuracy_values <- sapply(cost_values, function(x) {
  set.seed(12345)
  m <- ksvm(letter ~ ., data = letters_train, kernel = 'rbfdot', C = x)
  p <- predict(m, letters_test) 
  agree <-ifelse(p == letters_test$letter, 1, 0)
  accuracy <- sum(agree) / nrow(letters_test)
  return(accuracy)
 })

plot(cost_values, accuracy_values, type = 'b')

# setting C to a value of 10 or higher results in an accuracy of around 97 percent, which is quite an improvement in performance!

# Perhaps this is close enough to perfect for the model to be deployed in a real-world environment,
# though it may still be worth experimenting further with various kernels to see if it
# is possible to get even closer to 100 percent accuracy.