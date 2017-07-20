"
MITx: 15.071x The Analytics Edge
Homework 4: Text Analysis using CBOW and more
"
library(tm)
library(SnowballC)
library(caTools)
library(rpart)

wiki = read.csv('wiki.csv', stringsAsFactors = FALSE)
str(wiki)
summary(wiki) 
table(wiki$Vandal)

#Now we need to create a corpus, and check
corpusAdded = VCorpus(VectorSource(wiki$Added))

#remove stopwords
corpusAdded = tm_map(corpusAdded, removeWords, stopwords('english'))

#now let's stem them
corpusAdded = tm_map(corpusAdded, stemDocument)

#now we build the document term matrix 
dtmAdded = DocumentTermMatrix(corpusAdded)

summary(dtmAdded)
dtmAdded

#filter out sparse terms, keep ones that appear in 0.3% of the revisions
sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded

#now we convert it to a dataframe
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

#Now we do the same thing for but for the Removed column
corpusRemoved = VCorpus(VectorSource(wiki$Removed))
#remove stopwords again
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords('english'))
#remove stems
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)

sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))

colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
ncol(wordsRemoved)


#now we combine them
wikiWords = cbind(wordsAdded, wordsRemoved)
#The cbind function combines two sets of variables for the same observations into one data frame. 
wikiWords$Vandal = wiki$Vandal
set.seed(123)
spl = sample.split(wikiWords$Vandal, SplitRatio = 0.7)
wikiTrain = subset(wikiWords, spl==TRUE)
wikiTest = subset(wikiWords, spl==FALSE)

table(wikiTest$Vandal)
618/(618+545)

#make a cart model for prediction
cartPred = rpart(Vandal ~., data = wikiTrain, method = 'class')
cartPredictions = predict(cartPred, newdata = wikiTest, type = 'class')

#let's get the accuracy
table(wikiTest$Vandal, cartPredictions)

#plot rpart
library(rpart.plot)
prp(cartPred)

wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)

wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)

newCart = rpart(Vandal ~.,data = wikiTrain2, method = 'class' )
newPred = predict(newCart, newdata = wikiTest2, type = 'class')
table(wikiTest2$Vandal, newPred)
(609+57)/(609+57+488+9)

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

summary(wikiWords2$NumWordsRemoved)
wikiTrain3 = subset(wikiWords2, spl == TRUE)
wikiTest3 = subset(wikiWords2, spl==FALSE)

threeCart = rpart(Vandal ~ ., data = wikiTrain3, method = 'class')
threePred = predict(threeCart, newdata = wikiTest3, type = 'class')

table(wikiTest3$Vandal, threePred)
(514+248)/(514+248+297+248)

wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

wikiTrain4 = subset(wikiWords3, spl==TRUE)
wikiTest4 = subset(wikiWords3, spl==FALSE)

wikiCART4 = rpart(Vandal ~ ., data=wikiTrain4, method="class")
predictTestCART4 = predict(wikiCART4, newdata=wikiTest4, type="class")
table(wikiTest4$Vandal, predictTestCART4)
prp(wikiCART4)

