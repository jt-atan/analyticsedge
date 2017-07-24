"
MITx: 15.071x The Analytics Edge
Homework 6: Kmeans and Hierarchical Clustering
"
dailykos = read.csv('dailykos.csv')
str(dailykos)

#compute distances 
distkos = dist(dailykos, method = 'euclidean')
clusterKos = hclust(distkos, method = 'ward.D')
plot(clusterKos)
clusterGroups = cutree(clusterKos, k =7)

c1 = subset(dailykos, clusterGroups == 1)
c2 = subset(dailykos, clusterGroups == 2)
c3 = subset(dailykos, clusterGroups == 3)
c4 = subset(dailykos, clusterGroups == 4)
c5 = subset(dailykos, clusterGroups == 5)
c6 = subset(dailykos, clusterGroups == 6)
c7 = subset(dailykos, clusterGroups == 7)
nrow(c7)
tail(sort(colMeans(c1)))
tail(sort(colMeans(c2)))
tail(sort(colMeans(c3)))
tail(sort(colMeans(c4)))
tail(sort(colMeans(c5)))
tail(sort(colMeans(c6)))
tail(sort(colMeans(c7)))

#2.1, k means clustering. first we need to convert it to a matrix, then a vector?
k = 7
kosMatrix = as.matrix(dailykos)

set.seed(1000)
kosKMC = kmeans(dailykos, centers = k)
str(kosKMC)
kosKMC[3]

#let's get the clusters

kosCluster1 = kosKMC$cluster ==1
kosCLuster2 = kosKMC$cluster ==2
kosCluster3 = kosKMC$cluster ==3
kosCluster4 = kosKMC$cluster ==4
kosCluster5 = kosKMC$cluster ==5
kosCluster6 = kosKMC$cluster ==6
kosCluster7 = kosKMC$cluster ==7
table(kosKMC$cluster)
summary(kosCluster7)

#their way of doing it....
KmeansCluster = kosKMC
KmeansCluster1 = subset(dailykos, KmeansCluster$cluster == 1)
KmeansCluster2 = subset(dailykos, KmeansCluster$cluster == 2)
KmeansCluster3 = subset(dailykos, KmeansCluster$cluster == 3)
KmeansCluster4 = subset(dailykos, KmeansCluster$cluster == 4)
KmeansCluster5 = subset(dailykos, KmeansCluster$cluster == 5)
KmeansCluster6 = subset(dailykos, KmeansCluster$cluster == 6)
KmeansCluster7 = subset(dailykos, KmeansCluster$cluster == 7)
str(KmeansCluster1)

#top six words
tail(sort(colMeans(KmeansCluster1)))
tail(sort(colMeans(KmeansCluster2)))
tail(sort(colMeans(KmeansCluster3)))
tail(sort(colMeans(KmeansCluster4)))
tail(sort(colMeans(KmeansCluster5)))
tail(sort(colMeans(KmeansCluster6)))
tail(sort(colMeans(KmeansCluster7)))

table(clusterGroups, KmeansCluster$cluster)
'PART 3
MARKET SEGMENTATION FOR AIRLINES...QUITE IMPORTANT!!!
'
library(caret)
airlines = read.csv('AirlinesCluster.csv')
summary(airlines)

#**************IMPORTANT TO REMEMBER, HOW TO NORMALIZE DATA IN 4**********
#so we need to preprocess and normalize the data
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
#The first command pre-processes the data, 
# and the second command performs the normalization.
#this set the means to 0
summary(airlinesNorm)

#2.1 hierarchical clutering
#compute distances, hclust, cutree?
airDist = dist(airlinesNorm, method = 'euclidean')
airHclust = hclust(airDist, method = 'ward.D')
plot(airHclust)
#now divide into cutree with 5 clusters
airClusterGroups = cutree(airHclust, k = 5)
airC1 = subset(airlinesNorm, airClusterGroups == 1)
str(airC1)
#alternatively, get a table of it:
table(airClusterGroups)
#let's get the centroids, but cross ref against the un normalized data to better
# interpret it?
tapply(airlines$Balance, airClusterGroups, mean)
tapply(airlines$QualMiles, airClusterGroups, mean)
tapply(airlines$BonusMiles, airClusterGroups, mean)
tapply(airlines$BonusTrans, airClusterGroups, mean)
tapply(airlines$FlightMiles, airClusterGroups, mean)
tapply(airlines$FlightTrans, airClusterGroups, mean)
tapply(airlines$DaysSinceEnroll, airClusterGroups, mean)
summary(airC1)

#instead of all that stuff, we could use lapply to find it instead
lapply(split(airlines, airClusterGroups), colMeans)

#and now k means clustering after that!
k = 5 
set.seed(88)
airKMC = kmeans(airlinesNorm,centers = k, iter.max = 1000)
table(airKMC$cluster)
airKMC$cluster

'
PART THREE PREDICTING STOCK OUTCOMES USING LOGISTIC REGRESSION OH YEAH
SUPER COOL BABY
'

stocks = read.csv('StocksCluster.csv')
str(stocks)
summary(stocks)
table(stocks$PositiveDec)
6324/(5256+6324)

#Using LGM first
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

#create lgm, create predictions on threshold 0.5, compute accuracy
StockModel = glm(PositiveDec ~ ., data = stocksTrain, family = binomial)
modelPredict = predict(StockModel, newdata = stocksTest)
table(stocksTest$PositiveDec, modelPredict >= 0.5)
PredictTrain = predict(StockModel, type = 'response')
table(stocksTrain$PositiveDec, PredictTrain > 0.5)

PredictTest = predict(StockModel, newdata = stocksTest, type = 'response')
table(stocksTest$PositiveDec, PredictTest > 0.5)
(417+1553)/(417+1553+1160+344)

'
fuck this shit
(1427+312)/(1427+150+1585+312)
table(stocksTrain$PositiveDec)
(4427)/(4427+3679)
newPredict = predict(StockModel, newdata = stocksTrain)
table(stocksTrain$PositiveDec, newPredict >= 0.5)
(3324+737)/(3324+355+3690+737)
summary(StockModel)
table(newPredict >= 0.5)
table(stocksTrain$PositiveDec)
4427/(4427+3679)
table(StockModel$data$PositiveDec)
'

table(stocksTest$PositiveDec)
1897/(1897+1577)


#now we fucking cluster

limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

#HOW TO DO NORMALIZATION THAT HELPS WITH CLUSTERING
library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc,limitedTrain)
normTest = predict(preproc, limitedTest)

summary(normTest)
summary(normTrain)
mean(normTrain$ReturnJan) 
mean(normTest$ReturnJan)

#and now we k means
set.seed(144)
km = kmeans(normTrain, centers = 3)
table(km$cluster)

#now we make some predictions
library(flexclust)
km.kcca = as.kcca(km,normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata = normTest)
table(clusterTest)


#4.1 cluster specific predictions
stocksTrain1 = subset(stocksTrain, clusterTrain == 1)
stocksTrain2 = subset(stocksTrain, clusterTrain ==2)
stocksTrain3 = subset(stocksTrain, clusterTrain ==3)

stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)

mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

#so we can see that the stocks in cluster three have the best values for
# going up over time


#so now we can build a Logistric Regression model from our clusters for stock prediction
# on top of the k means clusters that we have built!!! Very cool

StocksModel1 = glm(PositiveDec ~ ., data = stocksTrain1, family = binomial)
StocksModel2 = glm(PositiveDec ~ ., data = stocksTrain2, family = binomial)
StocksModel3 = glm(PositiveDec ~ ., data = stocksTrain3, family = binomial)

summary(StocksModel1$coefficients)
summary(StocksModel2$coefficients)
StocksModel1$coefficients
StocksModel2$coefficients
StocksModel3$coefficients


#now we make some predictions
PredictTest1 = predict(StocksModel1, newdata = stocksTest1, type = 'response')
PredictTest2 = predict(StocksModel2, newdata = stocksTest2, type = 'response')
PredictTest3 = predict(StocksModel3, newdata = stocksTest3, type = 'response')

table(stocksTest1$PositiveDec, PredictTest1 > 0.5)
(30+774)/(30+774+471+23)
table(stocksTest2$PositiveDec, PredictTest2 >0.5)
(388+757)/(388+309+757+626)
table(stocksTest3$PositiveDec, PredictTest3 > 0.5)
(49+13)/(49+13+13+21)

#this is how we compute the overall test set accuracy
AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
table(AllOutcomes, AllPredictions > 0.5)
(467+1544)/(467+1544+353+1110)
#finished.

#ps: i love you

data(quakes)

ggplot() + geom_point(data=quakes,aes(x=lat,y=long,colour=stations))
