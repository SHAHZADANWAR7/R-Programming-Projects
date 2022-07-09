
library(tidyverse)
library(directlabels)
library(ggthemes)
# Load_in the data:
data(mpg)

# Explore the data:
dim(mpg)

names(mpg)

# To check if there's missingness:

mpg_na <- mpg %>%
  filter(!complete.cases(.))

# Let's see if there any duplicates:

mpg_dup <- mpg %>%
  filter(duplicated(mpg)) # Since we don't have many observations, so we won't touch duplicates:

# Let's free up some space:

rm(mpg_dup, mpg_na, mpg0)


# Building the base graph:

p <- ggplot(mpg, aes(displ, hwy, color = class)) + geom_point() +
  labs(title = 'Fuel Efficiency decreases as the Engine Size Increases', x = 'Engine Displacement (L)',
       y = 'Highway Mileage')

# Changing the entire appearance using custom themes with 'ggthemes' package:


p + geom_rangeframe() + theme_tufte()

p + theme_economist() + scale_color_economist()

p + theme_stata()

p + theme_wsj() + scale_color_wsj('colors6')

p + theme_calc() + scale_color_calc()

p + theme_hc() +  scale_color_hc()

# Trying out different themes from ggplot2:
p + theme_light()
p + theme_minimal()
p + theme_classic(base_size = 10, base_family = 'serif')
p + theme_void()
p + theme_dark()

# Customizing the plot panel background and grid lines:

p + theme(panel.background = element_rect(fill = 'lightblue', color = 'lightblue', linetype = 'solid'),
          panel.grid.major = element_line(size = 0.4, linetype = 'solid', color = 'white'),
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid', color = 'white'))

# Remove Plot Panel Border and grid lines:

p + theme(panel.background = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          axis.line = element_line(size = 0.25, linetype = 'dotted'))

# Change the plot background color (not panel):

p + theme(plot.background = element_rect(fill = 'grey98'))

# Changing the text font and size:

p + theme(text = element_text(family = 'serif', size = 10),
          axis.text = element_text(family = 'mono', face = 'bold'), 
          title = element_text(size =14),
          legend.background = element_rect(fill = 'light pink'))

# Adding direct labels to the points:

p + geom_dl(aes(label = class), method = 'smart.grid') + theme(legend.position = 'None')

