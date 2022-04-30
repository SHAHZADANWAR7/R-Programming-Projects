library(tidyverse)
library(gmodels)

# since 'knn()' function is in 'class' package:
library(class)

# for this project, we'll utilize the Breast Cancer Wisconsin (diagnostic) dataset:

wbcd <- read.csv('wisc_bc_data.csv', stringsAsFactors = FALSE)

# to see the sturcutre of variables:

str(wbcd)

# since variable named 'id' doesn't provide any useful info, so:

wbcd$id <- NULL

# variable named 'diagnosis' is the outcome we hope to predict:

table(wbcd$diagnosis)
prop.table(table(wbcd$diagnosis)) * 100

# Let's turn 'diagnosis' variable into factor:
wbcd$diagnosis <- as.factor(wbcd$diagnosis)

# to confirm if it's been converted into factor:
class(wbcd$diagnosis)

# Now let's give 'B', and 'M' values more informative names:

wbcd <- wbcd %>%
  mutate(diagnosis = recode(diagnosis, 'B' = 'Benign', 'M' = 'Malignent'))

# The remaining thirty features all are numeric which is the requirement for knn,
# so we're good to go. 

summary(wbcd[c('radius_mean', 'area_mean', 'smoothness_mean')])

# Since distance calculation for KNN is heavily dependent on measurement scale 
# of input features, so, let's rescale them using 'min-max normalization' method:

normalize <- function(x)  {
  return ((x - min(x)) / (max(x) - min(x)))
}

# To see if the function works properly:
normalize(c(1, 2, 3, 4, 5))

# Let's apply it to all the numeric features in our data:

wbcd_n <- as.data.frame(lapply(wbcd [2:31], normalize))

# To confirm that transformation was applied correctly:

summary(wbcd_n$area_mean)

# Creating training and testing datasets:
# Training dataset will be used to build the k_NN model,
# Testing dataset will be used to measure predictive accurage of the model

wbcd_train <- wbcd_n[1:469,]
wbcd_test <- wbcd_n[470:569,]

# Remeber when we normalized our numeric variables, we excluded target variable 'diagnonsi',
# We'll need to store those class lables in factor vectors,
# split bw training and testing datasets:

wbcd_train_lables <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

# Let's build the model:
# We'll choose the 'k' equal to the sqrt of training examples, 

wbcd_test_pred <- knn(
  train = wbcd_train, test = wbcd_test, cl =wbcd_train_lables, k = 21
)
# the knn() returns a factor vector with predicted class for each of the instances
# in testing dataset:

# let's evaluate how well the predicted classes in the 'wbcd_test_pred' vector match the
#actual values in the wbcd_test_labels vector. For that, we'll need a function CrossTable()
# from a package called 'gmodels'. 

CrossTable(wbcd_test_pred, wbcd_test_labels)

# Whoa!!! 98 percent accuracy seems impressive for a few lines of R code,
# we might try another iteration of the model to see if we can improve the
# performance and reduce the number of values that have been incorrectly classified, 
# especially because the errors were dangerous false negatives.

# To see if our model accurace improves, we'll try two methods: First, we'll use 
# z-score standardization instead of min-max normalization, 
#Second, we'll try different k values:

wbcd_z <- as.data.frame(scale(wbcd[, -1]))

# To confirm that the transformation was applied correctly,
# we can look at the summary statistics:

summary(wbcd_z$area_mean)

wbcd_tr <- wbcd_z[1:469, ]
wbcd_te <- wbcd_z[470:569, ]
wbcd_tr_labels <- wbcd[1:469, 1]
wbcd_te_labels <- wbcd[470:569, 1]
wbcd_te_pred <- knn(train = wbcd_tr, test = wbcd_te,
                    cl = wbcd_tr_labels, k = 21)
CrossTable(x = wbcd_te_labels, y = wbcd_te_pred,
           prop.chisq = FALSE)
# We don't see any improvement.. let's try different k values:

