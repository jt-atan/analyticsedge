"
MITx: 15.071x The Analytics Edge
Homework 7: Plotting and Visualization

"
library(ggplot2)
library(ggmap)
library(maps)

statesMap = map_data("state")
str(statesMap)
head(statesMap)
summary(statesMap)
table(statesMap$group)
length(table(statesMap$group))

ggplot(statesMap, aes(x = long,y = lat, group = group)) + geom_polygon(fill = 'white', color = 'black')

polling = read.csv('PollingImputed.csv')
str(polling)
summary(polling)
#split into train test 
Train = subset(polling,Year <2009 )
Test = subset(polling, Year == 2012)

mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
TestPrediction = predict(mod2, newdata=Test, type="response")
TestPredictionBinary = as.numeric(TestPrediction > 0.5)
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)

table(predictionDataFrame$TestPredictionBinary)
summary(predictionDataFrame)

predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
predictionMap = predictionMap[order(predictionMap$order),]

str(predictionMap)
str(statesMap)

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")

#change colors from gradient to blue or red for dem and discrete outcomes
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color= 'black') + scale_fill_gradient(low = 'blue', high = 'red', guide = 'legend', breaks = c(0,1), labels = c('Democrat','Republican'), name = 'PREDICT 2012')

#continuous instead of discrete
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) + geom_polygon(color= 'black') + scale_fill_gradient(low = 'blue', high = 'red', guide = 'legend', breaks = c(0,1), labels = c('Democrat','Republican'), name = 'PREDICT 2012')

table(predictionMap$TestPrediction)
table(predictionMap$TestPrediction,predictionMap$Test.State)

table(predictionMap$Test.State, predictionMap$TestPrediction)

predictionMap$TestPrediction

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color= 'black') + scale_fill_gradient(low = 'blue', high = 'red', guide = 'legend', breaks = c(0,1), labels = c('Democrat','Republican'), name = 'PREDICT 2012')
?geom_polygon

edges = read.csv('edges.csv')
users = read.csv('users.csv')
str(edges)
summary(edges)
str(users)
summary(users)
summary(edges)
edges$V1[2]
nrow(edges)

table(users$gender, users$school)

install.packages('igraph')
library(igraph)

?graph.data.frame

g = graph.data.frame(edges, FALSE, users)
plot(g, vertex.size=5, vertex.label=NA)
degree(g)
sort(degree(g))

V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)

V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"

#now set for school
V(g)$color[V(g)$school == 'A'] = 'blue'
V(g)$color[V(g)$school == 'AB'] = 'green'
plot(g, vertex.label = NA)
summary(users)

#now for locale
V(g)$color[V(g)$locale == 'A'] = 'pink'
V(g)$color[V(g)$local == 'B'] = 'orange'
plot(g, vertex.label = NA)
summary(users)

'
part 3
'
library(tm)
library(SnowballC)

tweets = read.csv('tweets.csv', stringsAsFactors = FALSE)
install.packages('wordcloud')
library(wordcloud)

#build a corpus
corpus = VCorpus(VectorSource(tweets$Tweet))

#convert to lowercase
corpus = tm_map(corpus, content_transformer(tolower))

#remove punctuation
corpus = tm_map(corpus, removePunctuation)

#remove english language stop words
corpus = tm_map(corpus, removeWords, c('apple',stopwords('english')))


#now build a document term matrix of the results
tweetMatrix = DocumentTermMatrix(corpus)

#convert matrix to datafram alltweets
allTweets = as.data.frame(as.matrix(tweetMatrix))

summary(allTweets)
#
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25), random.order = FALSE)

?wordcloud
display.brewer.all()
