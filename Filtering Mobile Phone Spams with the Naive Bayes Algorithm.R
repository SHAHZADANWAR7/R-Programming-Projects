library(tidyverse)
library(tm) # To do the text mining
library(SnowballC) # To perform the stemming process
library(wordcloud) # To visualize the text data
library(e1071) # To incorporate 'NaiveBayes' algorithm
library(gmodels) # To create confusion matrix

sms_raw <- read_csv('sms_spam.csv')

str(sms_raw)
 

# The type element is currently a character vector. Let's convert it into factor vector:

sms_raw$type <- as.factor(sms_raw$type)

# Let's figure out what percent of SMS messages are labeled as spam:

table(sms_raw$type)

prop.table(table(sms_raw$type)) * 100 # There are about 13.5 percent of SMS messages labeled as spam. 

# Text data is challenging to prepare because it is necessary to transform the words and sentences into a form that a computer can understand.

# We will transform our data into a representation known as bag-of-words, which ignores word order
# and simply provides a variable indicating whether the word appears at all:

# Data Preparation: 

# The first step in processing text data involves creating a corpus:

sms_corpus <- VCorpus(VectorSource(sms_raw$text))

print(sms_corpus)

# To view the actual message text, the as.character() function must be applied to the desired messages.

as.character(sms_corpus[[1]])

# because the tm corpus is essentially a complex list, we can use list operations to select documents in the corpus.
# The inspect() function shows a summary of the result. 

inspect(sms_corpus[1:2])

# To view multiple documents, we'll need to apply as.character() to several items in the sms_corpus object.

lapply(sms_corpus[1:2], as.character)

# To perform our analysis, we need to divide these messages into individual words.
#  First, we need to clean the text to standardize the words and remove punctuation, stop words, numbers, and white spaces:

# Our first transformation will standardize the messages to use only lowercase characters. 

sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))

# Let's see if the transformation worked: 

as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])


# Let's continue our cleanup by removing numbers from the SMS messages.

sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)

# Let's remove uninteresting words (stop words) such as but, and, to or etc:

sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())

# it's time to get rid of the punctuation too:

sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)

# Another common standardization for text data involves reducing words to their root form in a process called stemming.

sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)

# After removing numbers, stop words, and punctuation, and also performing stemming, 
# the text messages are left with the blank spaces that once separated the now-missing pieces.
# the final step in our text cleanup process is to remove additional whitespace using the built-in stripWhitespace() transformation

sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

# splitting text documents into words:

#the final step is to split the messages into individual terms through a process called tokenization. 

sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

sms_dtm

# creating training and test datasets:

# We'll divide the data into two portions: 70 percent for training and 30 percent for testing.

sms_dtm_train <- sms_dtm[1:3901,]
sms_dtm_test <- sms_dtm[3902:5574,]
# For convenience later on, it is also helpful to save a pair of vectors with the labels for each of the rows in the training and testing matrices.


sms_train_labels <- sms_raw[1:3901, ]$type
sms_test_labels <- sms_raw[3902:5574,]$type

#To confirm that the subsets are representative of the complete set of SMS data, let's compare the proportion of spam in the training and test data frames:

prop.table(table(sms_train_labels)) * 100
prop.table(table(sms_test_labels)) * 100 #Both the training data and test data contain about 13 percent spam. 

# Visualizing text data – word clouds

wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)

# A perhaps more interesting visualization involves comparing the clouds for SMS spam and ham.

#Since we did not construct separate corpora for spam and ham, this is an appropriate time to create separate corpora:

ham <- subset(sms_raw, type == 'ham')
spam <- subset(sms_raw, type == 'spam')

par(mfrow = c(1,2))

wordcloud(ham$text, max.words = 30, scale = c(3, 0.5)) # Instead, ham messages use words such as can, sorry, need, and time.
wordcloud(spam$text, max.words = 30, scale = c(3, 0.5)) #Spam messages include words such as urgent, free, mobile, claim, and stop; these terms do not appear in the ham cloud at all.

# creating indicator features for frequent words
#  Currently, the sparse matrix includes over 6,500 features; this is a feature for every word that appears in at least one SMS message.
# It's unlikely that all of these are useful for classification. To reduce the number of features, we'll eliminate any word that appears in less than five messages, 


sms_freq_terms <- findFreqTerms(sms_dtm_train, 5)

summary(sms_freq_terms) 
str(sms_freq_terms) #A peek into the contents of the vector shows us that there are 1,112 terms appearing in at least five SMS messages:

sms_dtm_freq_train <- sms_dtm_train[ , sms_freq_terms]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_terms]

# The Naive Bayes classifier is usually trained on data with categorical features.
# let's create a function to convert counts to 'yes' or 'no' strings:

convert_counts <- function(x) {
  x <- ifelse(x > 0, 'Yes', 'No')
}

# We now need to apply convert_counts() to each of the columns in our sparse matrix.

sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2 , convert_counts)

#The result will be two character-type matrices, each with cells indicating "Yes" or "No" for
# whether the word represented by the column appears at any point in the message represented by the row. 

# Training the model:


sms_classifier <- naiveBayes(sms_train, sms_train_labels)

# To evaluate the SMS classifier, we need to test its predictions on the unseen messages in the test data.

sms_type_pred <- predict(sms_classifier, sms_test)

# Let's see how it performed on test dataset:

mean(sms_type_pred == sms_test_labels) # Whoa!!!! It's just super amazing!! 

CrossTable(sms_type_pred, sms_test_labels) # The model only misclassified 34 SMS messages which is about two percent of the SMS messages.

# Let's see if we could improve the model performance by introducing 'Laplace' parameter:

sms_classifier_laplace <- naiveBayes(sms_train, sms_train_labels, laplace = 1)
sms_type_pred_laplace <- predict(sms_classifier_laplace, sms_test)

mean(sms_type_pred_laplace == sms_test_labels)

agreement <- sms_type_pred_laplace == sms_test_labels

table(agreement)

CrossTable(sms_type_pred_laplace, sms_test_labels)

# uh_oh, it misclassified more SMS messages than before. It seems that it's substantial considering that the model's accuracy was already quite impressive
# We'd need to be careful before tweaking the model too much more, as it is important to maintain a balance between being overly aggressive and overly passive when filtering spam.
# Users would prefer that a small number of spam messages slip through the filter rather than an alternative in which ham messages are filtered too aggressively.
