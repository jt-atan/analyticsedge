#Notes Week 5 edX 
#MITx: 15.071x The Analytics Edge
#Text Analytics
'
Part One: Bag of Words Model in R
'
tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)
#You will always need to add the extra argument when working on a text analytics problem!
str(tweets)
#has the tweet and the average sentiment score

tweets$Negative = as.factor(tweets$Avg <= -1)

#install.packages('tm')
#install.packages('SnowballC')
library(tm)
library(SnowballC)

#A corpus is a collection of documents. We need to convert the tweets to a corpus for preprocessing

#need corpus and vector source
#need corpus and vector source functions

corpus = VCorpus(VectorSource(tweets$Tweet))

#Let's look at the corpus
corpus
corpus[[1]]$content

#let's lowercase everything
corpus = tm_map(corpus, content_transformer(tolower))
corpus[[1]]$content

#this is how we remove punctuation
corpus = tm_map(corpus, removePunctuation)
corpus[[1]]$content

stopwords("english")[1:10]

# Remove stopwords and apple

corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))

corpus[[1]]$content

# Stem document 

corpus = tm_map(corpus, stemDocument)

corpus[[1]]$content

frequencies = DocumentTermMatrix(corpus)
frequencies

inspect(frequencies[1000:1005, 505:515])
findFreqTerms(frequencies, lowfreq = 20)

#only keep terms that appear in 2% or more of the tweets
sparse = removeSparseTerms(frequencies, 0.995)
sparse

#now let's convert the sparse matrix into a dataframe to use for our predictive models
#this converts it to a dataframe
tweetsSparse = as.data.frame(as.matrix(sparse))

#r struggles with variable names that start with a number! so we use make na,es
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))
#DO THIS EVERY TIME YOU BUILD A DATA FRAME USING TEXT ANALYTICS

tweetsSparse$Negative = tweets$Negative
library(caTools)

set.seed(123)

split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
trainSparse = subset(tweetsSparse, split == TRUE)
testSparse = subset(tweetsSparse, split == FALSE)

#which words appear 100 times?
findFreqTerms(frequencies, lowfreq = 100)


#video 7

library(rpart)
library(rpart.plot)

tweetCART = rpart(Negative ~ ., data = trainSparse, method = 'class')
prp(tweetCART)

predictCART = predict(tweetCART, newdata = testSparse, type = 'class')
table(testSparse$Negative, predictCART)

#let's get the accuracy
(294+18)/(294++6+37+18)

#what is the baseline model? always predicts non negative
table(testSparse$Negative)
300/355

#so the cart model does better than the baseline. how about RANDOM FOREST MODEL?

library(randomForest)
set.seed(123)
tweetRF = randomForest(Negative ~ ., data=trainSparse)

#Let's make predictions from the random forest
predictRF = predict(tweetRF, newdata = testSparse)
table(testSparse$Negative, predictRF)
#get accuracy of the random forest predictive model
(293+21)/(293+21+34+7)
#so .88

#so by using a bag of words model an random forest or cart, we can reasonably predict sentiment wit htweets!

#now let's try a LGM model
tweetLog = glm(Negative ~ ., data = trainSparse, family = 'binomial')
predictions = predict(tweetLog, newdata=testSparse, type="response")
table(testSparse$Negative, predictions >= 0.5)
(257+34)/(257+34+21+43)


#Part three, predicting the Enron email scandals
emails = read.csv('energy_bids.csv', stringsAsFactors = FALSE)
summary(emails)
emails$email[1]

str(emails)
emails$responsive
emails$responsive[1]
strwrap(emails$email[2])
table(emails$responsive)

#load the tm library to do text manipulation
library(tm)
corpus = VCorpus(VectorSource(emails$email))
corpus[[1]]$content

#So let's preprocess the text data in R!
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)

#look at first email
strwrap(corpus[[1]]$content)

#Video 4!!!
#create the matrx

#this creates the document term matrix. 
dtm = DocumentTermMatrix(corpus)
dtm

#let's remove the sparse terms to reduce our corpus
dtm = removeSparseTerms(dtm, 0.97)
dtm

#create data frame, this creates the frequencies 
labeledTerms = as.data.frame(as.matrix(dtm))

#Add in the outcome variable, if the email was responsive. creates a matrix of word occurances by email
labeledTerms$responsive = emails$responsive
str(labeledTerms)

#so let's start by building a cart model!
library(caTools)
set.seed(144)
spl = sample.split(labeledTerms$responsive, 0.7)

train = subset(labeledTerms, spl == TRUE)
test = subset(labeledTerms, spl == FALSE)

library(rpart)
library(rpart.plot)

emailCART = rpart(responsive ~., data = train, method = 'class')
prp(emailCART)

#let's make the predictions for the test set
pred = predict(emailCART, newdata = test)
#these are the predicted probablities of the document being responsive. so wewant the 
# rightmost column
pred[1:20,]
pred.prob = pred[,2]
#what's the baseline
table(test$responsive, pred.prob >= 0.5)
(195+25)/(195+25+17+20)
#baseline accuracy is therefore 85.6%

#so what's ours?
table(test$responsive)
215/(215+42)
#so ours is 83.7% accuracy! so a false positive is OK, but a false negative is bad!!!!

#so we need to look at other cutoffs for our ROC curve
library(ROCR)
predROCR = prediction(pred.prob, test$responsive)
perfROCR = performance(predROCR, 'tpr', 'fpr')
plot(perfROCR, colorize = TRUE)

#compute the AUC
performance(predROCR, 'auc')@y.values





