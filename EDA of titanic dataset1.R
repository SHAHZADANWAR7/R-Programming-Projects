library(tidyverse)

titanic <- read.csv('train.csv', stringsAsFactors = FALSE)

names(titanic)
# To look at the structure of of variables:
glimpse(titanic)

# To see where the missingness is:
titanic %>%
  filter(!complete.cases(.)) %>%
  view()


# To see if there are any duplicates:
duplicated(titanic)

# To get rid of duplicates if there are any: 

titanic %>%
  distinct()

# 'Age' summary statistics: 
summary(titanic$Age)

# To see the variable 'Age' data distribution:

hist(titanic$Age)

# To see the summary statistics of ticket price:
summary(titanic$Fare)

# How many passengers survived?

table(titanic$Survived)

prop.table(table(titanic$Survived)) * 100

# To turn 'Age' variable into factorial..
titanic <- titanic %>%
  mutate(Age = cut(Age, breaks = 5, labels = c('children', 'Teenagers', 'Young',
                                               'Middle_age', 'Aged')))

titanic %>%
  ggplot(aes(Age)) + geom_bar()

#  Which age group had a better chance of survival?
titanic %>%
  ggplot(aes(x = Age, fill = factor(Survived))) + geom_bar(position = 'dodge')



# Which gender had a better chance of survival?

ggplot(titanic, aes(x = factor(Sex), fill = factor(Survived))) +
  geom_bar(position = 'fill')

# Which social class had a better chance of survival?
ggplot(titanic, aes(x = factor(Pclass), fill = factor(Survived))) +
  geom_bar(position = 'dodge')