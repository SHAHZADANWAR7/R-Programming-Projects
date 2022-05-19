library(tidyverse)
library(skimr)
library(janitor)
library(scales)

# To get the list of built in datasets:
data()

mtcars <- mtcars
glimpse(mtcars)
# TO get a robust summary of dataset:

skim_without_charts(mtcars)

# To make sure all coloumn names are unique and consistant:

clean_names(mtcars)

# Base graph:

basegraph <- ggplot(mtcars, aes(wt, mpg)) + geom_point() 

# To increase the axis limits of basegraph:

basegraph2 <- basegraph + xlim(1,7) + ylim(10,45)
basegraph2

# let's annotate the basegraph:

basegraph + annotate('text', x = 5.1, y = 15,
                     label = c('Extreme values'), alpha = 0.5, angle = 45,
                     color = 'red', size = 4)

# Another example of annotation with the same graph:

basegraph + annotate('text', x = c(5.1, 2.5), y = c(15, 32.2),
                     label = c('Extreme Values', 'Unusual Values'), angle = 45,
                     color = 'red', size = 4, alpha = .4)

# Annotation also allows to add different kinds of shapes to graph:

# To add rectangle to the graph:
basegraph2 + annotate('rect', xmin = c(2, 5), xmax = c(2.5, 6), ymin = c(25, 10),
                      ymax = c(35, 20), alpha = 0.3, color = 'purple')

# To add segment:
basegraph
basegraph + annotate('segment', x = 2, xend = 3, y = 3, yend = 15, size = 5,
                     alpha = 0.2, color = 'red')
