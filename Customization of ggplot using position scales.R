install.packages('tidyverse')
library(tidyverse)
install.packages('janitor')
library(janitor)
install.packages('skimr')
library(skimr)
install.packages('scales')
library(scales)
library(hexbin)
# To remove any object available in 'Environment' pane:
rm(list = ls())

mpg <- mpg

# To get the names of coloumns:
names(mpg)

glimpse(mpg)
rename_with(mpg, toupper)
rename_with(mpg, tolower)

# To make sure we have unique and consistant names:
clean_names(mpg)

# To get robust summary of 'mpg' dataset:
skim_without_charts(mpg)

# To remove graphs if available in 'Help' pane:
dev.off()

# The use of 'Position scales' to write titles, set limits on axis, 
# to show where the tickmarks appear on the axis, 
# and labels on tickmarks

ggplot(mpg, aes(displ, hwy, color = class)) + geom_point() +
  labs(title = 'Highway Mileage VS Engine Displacement', x = 'Engine Displacement',
       y = 'Highway Mileage') + scale_x_continuous(limits = c(1, 8), 
                                                   breaks = c(2, 3, 4, 5, 6),
                                                   labels = c('two', 'three',
                                                              'four', 'five', 'six')) +
  scale_y_continuous(limits = c(0, 50), breaks = c(10, 20, 30, 40),
                     labels = c('ten', 'twenty', 'thirty', 'fourty')) +
  theme_linedraw()

# To zoom in on part of the plot to see if there are any outliers that need to be removed:

diamonds <- diamonds
str(diamonds)
 
# To remove duplicates if there are any:
diamonds %>%
  distinct()

# To see where the missingness is:
diamonds %>%
  filter(!complete.cases(.))

# use of 'coord_cartesian()' function to zoom in on parts of the plots:

basegraph <- ggplot(diamonds, aes(x = y)) + geom_histogram(binwidth = 0.5)

basegraph + coord_cartesian(ylim = c(0, 200))


# Use of 'scales' package to show currency value:
ggplot(diamonds, aes(carat, price)) + geom_point() + 
  scale_y_continuous(labels = scales::label_dollar())

# When dataset is too big and overplotting happens,
# we use of 'geom_bin2d' and 'geom_hex' for better visualization:

ggplot(diamonds, aes(carat, price)) + geom_bin2d()

ggplot(diamonds, aes(carat, price)) + geom_hex()


