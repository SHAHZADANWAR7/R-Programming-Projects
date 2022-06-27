# Groceies dataset Exploratory Data Analysis.

library(tidyverse)
library(lubridate)
library(wordcloud)

groceries <- read.csv('Groceries_dataset.csv') # link to the source of the dataset: 'https://www.kaggle.com/datasets/heeraldedhia/groceries-dataset'


str(groceries)

groceries$Date <- dmy(groceries$Date)

groceries_n <- groceries %>%
  group_by(Date) %>%
  summarise(n = n())

# Let's visualize the distribution of 'n' variable which is number of total items perchased on single day:

ggplot(groceries_n, aes(n)) + geom_histogram(binwidth = 30) # It reveals really an interesting patterns. On most days, puchased items were b/w 45 and 75.

# Let's change the binwidth, it might reveal some interesting patters:

ggplot(groceries_n, aes(n)) + geom_histogram(binwidth = 10) # It shows there were about 250 days, when 40 to 60 items were purchases.


# Let's visualize the most purchased items:

wordcloud(groceries$itemDescription)

wordcloud(groceries$itemDescription, min.freq = 30, random.order = FALSE) # We can see that vegetables, root soda, fruit, yogurt, milk, and rollsbuns are the most purchased items.

# Let's see who are the top ten customers:

groceries %>%
  group_by(Member_number) %>%
  summarise(n = n()) %>%
  arrange(desc(n))




# Let's create 'weekday' variable:

groceries_n <- groceries_n %>%
  mutate(wday = wday(Date, label = TRUE))

ggplot(groceries_n, aes(wday, n)) + geom_boxplot() # It doesn't reveal anything except for one thing that fewer people go shopping on Mondays.

# let's zoom in on 'Sundays'.

groceries_n %>%
  filter(wday == 'Sun') %>%
  ggplot(aes(Date, n)) + geom_point() + geom_line() +scale_x_date(date_breaks = '1 month', date_labels = '%b') 

# It reveals really an interesting pattern. From 'Oct 2014' to 'Dec 2013', Fewer people went shopping
# than the same time period in 2015. This might be due to economic recovery after great recession.


# Let's visualize the month by month trends:

groceries_n <- groceries_n %>%
  mutate(month = month(Date, label = TRUE))

ggplot(groceries_n, aes(month, n)) + geom_boxplot() # It shows very few people go shopping during 'February' and 'Septermber'.

# Let's create a 'year' variable and zoom in on 'Febuary':
groceries_n <- groceries_n %>%
  mutate(year = year(Date))

groceries_n %>%
  filter(month == 'Feb' & year == '2014') %>%
  ggplot(aes(Date, n)) + geom_point() + geom_line() + geom_smooth(se = F) # It shows majority of the people did shop at the beginning and end of the month. 

# let's see if there was the same trend in 2015.

groceries_n %>%
  filter(month == 'Feb'& year == '2015') %>%
  ggplot(aes(Date, n)) + geom_point() + geom_line() # In March 2015, we can't observe any strong trends. Oversall, sales are stable.

# it'd be interesting to see the sales trends in the month of December:

groceries_n %>%
  filter(month == 'Dec' & year == '2014') %>%
  ggplot(aes(Date, n)) + geom_point() + geom_line() # we can observe that there's a spike in sales right before Christmas period and dip in sales right after Christmas. 

# Let's see if there was the same trend in the year '2015':

groceries_n %>%
  filter(month == 'Dec' & year == '2015') %>%
  ggplot(aes(Date, n)) + geom_point() + geom_line() # almost the same trend, but not exactly as we had hoped. 

# let's see if there's change in month by month sales:

groceries_n1 <- groceries_n %>%
  group_by(month) %>%
  summarise(total_by_month = sum(n)) %>%
  arrange(desc(total_by_month)) %>%
  view() # We can see that most of the transactions occured in 'Aug' followed by 'May'.
