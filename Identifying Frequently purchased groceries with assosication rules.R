# We'll perform market basket analysis that is is used behind the scenes for the recommendation systems used in many brick-and-mortar and online retailers.

# For the purpose of this exercise, we'll use public dataset available on kaggle. 
library(tidyverse)
library(wordcloud)
install.packages("arules")
library(arules)

groceries_cloud <- read.csv('Groceries_dataset.csv')

wordcloud(groceries_cloud$itemDescription, min.freq = 50, random.order = FALSE)

# Because it's a transactional data, we need a special function 'read.transactions()' from 'arules' package:


groceries <- read.transactions('Groceries_dataset.csv', sep = ' ')


summary(groceries) 
# From summary we can calculate that (rows*columns) * density = 58992 items were purchased over the last two years period (ignoring the fact that duplicates of the same items might have been purchased)

# It shows there are 38766 rows and 37979 columns with density of 0.00005, which means that out of total matrix cells, there are
# (37979 * 38766) * 0.00005 are non zero matrix cells.
# 37979 columns are the items that might appear  in someone's grocery basket

# With an additional step, we can determine that the average transaction contained 58992 / 38766 = 1.52 distinct grocery items.

# The next block of summary() output lists the items that were most commonly found in the transactional data

# Finally, summary() output also presents a set of statistics about the size of the transactions

# Let's look at the first few transactions in the dataset:


inspect(groceries[1:5])

# These transactions match our look at the original CSV file

# To examine a particular item (that is, a column of data), use the [row, column] matrix notion.
# Using this with the itemFrequency() function allows us to see the proportion of transactions that contain the specified item

itemFrequency(groceries[, 1:5]) # it shows that appetizer appeared in about 0.17 percent of transactions.

# Notice that the items in the sparse matrix are sorted in columns by alphabetical order

# Visualizing item support – item frequency plots

# To present these statistics visually, use the itemFrequencyPlot() function

#This creates a bar chart depicting the proportion of transactions containing specified items
itemFrequencyPlot(groceries, support = 0.1) # This isn't particulary helpful because we have too many columns. 

# If you would rather limit the plot to a specific number of items, use itemFrequencyPlot() with the topN parameter:

itemFrequencyPlot(groceries, topN = 10)

# In addition to looking at specific items, it's also possible to obtain a bird's-eye view of the entire sparse matrix using the image() function.

image(groceries[1:5]) # Because there way too many columns and rows, so it isn't helpful at all.

# we'll combine it with the sample() function to view the sparse matrix:

image(sample(groceries, 20)) # it still isn't helpful because there are way too many columns. So, we'll just skip this step.

# patterns in the diagram may help reveal interesting segments of transactions and items, particularly if the data is sorted in interesting ways.

# Let's train the model:

#With data preparation complete, we can now work at finding associations among shopping cart items

rules <- apriori(groceries, parameter = list(support = 0.1, confidence = 0.8, minlen = 1))

myrules # uh-oh, using the default settings of support = 0.1 and confidence = 0.8 results in a set of zero rules.

# We need to widen our search now.


# there can sometimes be a fair amount of trial and error needed to find the support and confidence parameters that produce a reasonable number of association rules


# One way to approach the problem of setting a minimum support is to think about the smallest number of transactions needed before you would consider a pattern interesting

# to consider an item important, let's suppose it has to show up on at least 300 transactions out of total transactions:
# in that case support vector would be 200/38766 = 0.005:

# The appropriate minimum confidence level depends a great deal on the goals of your analysis. If you start with a conservative value,
# you can always reduce it to broaden the search if you aren't finding actionable intelligence.

# We'll start with a confidence threshold of 0.25, which means that in order to be included in the results, the rule has to be correct at least 25 percent of the time

# adding more, it is helpful to set minlen = 2 to eliminate rules that contain fewer than two items


groceryrules <- apriori(groceries, parameter = list(support =
                                                      0.005, confidence = 0.25, minlen = 2))
groceryrules

summary(groceryrules)

# the summary statistics show us four columns and we haven't considered the 'lift' matric yet.

#The lift of a rule measures how much more likely one item or itemset is to be purchased relative to its typical rate of purchase, given that you know another item or itemset has been purchased.

# A large lift value is therefore a strong indicator that a rule is important and reflects a true connection between the items.

# Let's look at the rules now using insepct() function:

inspect(groceryrules)

# The rules seem be to be so trivial or inexplicable. Let's change the parameters to get few more rules:

groceryrules2 <- apriori(groceries, parameter = list(support =
                                                      0.001, confidence = 0.30, minlen = 2))
groceryrules2

inspect(groceryrules2)

# The life value is too high, which means these rules are too trivial to be used. So, we need to keep changing the parameters until we find some useful useful.
# Depending upon the objectives of the market basket analysis, the most useful rules might be those with the highest support, confidence, or lift


