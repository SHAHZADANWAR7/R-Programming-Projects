# Our task then is to develop the best possible diagnostic machine learning algorithm in order 
# to assist the patient's medical team in determining whether the tumor is malignant or not.

library(tidyverse)
library(MASS) # Contains the 'biopsy' dataset
library(janitor)
library(reshape2)
library(corrplot)
library(car) # To incorporate 'VIF' analysis
library(leaps)
library(bestglm)

# Let's import the data:

data("biopsy")

view(biopsy)
str(biopsy)

# We can get rid of the first variable:

biopsy$ID <- NULL

# Let's get the summary:

summary(biopsy)

# Let's randomly check the distribution of two features:

ggplot(biopsy, aes(V8)) + geom_histogram() # Oops. It's not normally distributed.
ggplot(biopsy, aes(V9)) + geom_histogram() # This one isn't normally distributed either.

# Let's see where the missingness is:

biopsy_na <- biopsy %>%
  filter(!complete.cases(.)) # There seems to be some missigness.

# Let's get rid of the missingness:

biopsy <- biopsy %>%
  filter(complete.cases(.))
# Let's confirm if there are any duplicates:

biopsy_dup <- biopsy %>%
  filter(duplicated(biopsy)) # There seems to be a lot of duplicates, and we can't figure out why.
                            #And removing them isn't an option considering the size of our sample dataset>

# we will rename the variables and confirm that the code has worked as intended:

names(biopsy) = c("thick", "u.size", "u.shape", "adhsn", "s.size",
                  "nucl", "chrom", "n.nuc", "mit", "class")
names(biopsy)

# Let's conduct the overall visaul inspection by creating a matrix of boxplots:

# The following code melts the data by their values into one overall feature and groups them by class:

biopsy_m <- melt(biopsy, 'class')

ggplot(data=biopsy_m, aes(x=class, y=value)) + geom_boxplot() +facet_wrap(~variable)

#
# By inspecting the plots and applying some judgment, it is difficult to determine
# which features will be important in our classification algorithm

# Let's check the correlation between features:

corr <- cor(biopsy[, 1:9])
corrplot.mixed(corr)

# The correlation coefficients are indicating that we may have a problem with collinearity
# So, we'll have to incorporate 'VIF' analysis

# Now, let's create training and test datasets:

set.seed(123)
samp <- sample(2, nrow(biopsy), replace = TRUE, prob = c(0.7, 0.3))
biopsy_train <- biopsy[samp == 1,]
biopsy_test <- biopsy [ samp == 2,]

str(biopsy_test)

# To ensure that we have a well-balanced outcome variable between the two datasets,
# we will perform the following check:

prop.table(table(biopsy_train$class)) * 100
prop.table(table(biopsy_test$class)) *100

# It's time to create the model:

full_fit <- glm(class ~ ., data = biopsy_train, family = binomial)

summary(full_fit) 
#The summary() function allows us to inspect the coefficients and their p-values.
# We can see that only two features have p-values less than 0.05 (thickness and nuclei).

# An examination of the 95 percent confidence intervals can be called on with the confint() function, as follows:

confint(full_fit) #Note that the two significant features have confidence intervals that do not cross zero.

# In order to produce the odds ratios:

exp(coef(full_fit)) # The interpretation of an odds ratio is the change in the outcome odds resulting from a unit change in the feature.

# Now, it is time to incorporate 'VIF' analysis:

vif(full_fit)

#None of the values are greater than the VIF rule of thumb statistic of five, so collinearity does not seem to be a problem.

# let's produce some code to look at how well this model does on both the train and test sets.

biopsy_train$probs <- predict(full_fit, type = 'response')

biopsy_train$pred_class <- ifelse(biopsy_train$probs > 0.5, 'malignant', 'benign')

# Time to create confusion matrix:

table(biopsy_train$class, biopsy_train$pred_class)

agreement <- biopsy_train$class == biopsy_train$pred_class
prop.table(table(agreement)) * 100 # Our mondel has done really a great job with about 97 percent accuracy in results.

# Let's see how our model does on unseen data (biopsy_test):

biopsy_test$prob <- predict(full_fit, biopsy_test, type = 'response')

biopsy_test$pred_class <- ifelse(biopsy_test$prob > 0.5, 'malignant', 'benign')

mean(biopsy_test$pred_class == biopsy_test$class) # Our model has done better than it did on training set.

# Let's try to improve performance by 'Cross_Validation' method:

biopsy_train$y = rep(0,474)
biopsy_train$y[biopsy_train$class == "malignant"] = 1

# A quick double check is required in order to confirm that it worked:

head(biopsy_train[ ,13])

# to get rid of extreneous cloums:

biopsy_cv <- biopsy_train[ ,-10:-12]

head(biopsy_cv)

# Here is the code to run in order to use the CV technique with our data:

bestglm::bestglm(Xy = biopsy_cv, IC="CV", CVArgs=list(Method="HTF", K=10,
                                             REP=1), family=binomial)

# We can put these features in glm() and then see how well the model did on the train and test sets.

reduce.fit <-  glm(class~thick+u.size+nucl, family=binomial, data = biopsy_train)

## Rest of the procedure is the same as we did before. 

# Note: If you find out your model isn't doing great with reduced features. Try using 'bestglm()' again
# this time using the best subsets with the information criterion set to BIC:

bestglm(Xy= biopsy_cv, IC="BIC", family=binomial)

bic.fit=glm(class~thick+adhsn+nucl+n.nuc, family=binomial, data=train)

# Next, follow the same procedure as we've done before. Cheers!!!!!