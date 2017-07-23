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
summary(airlinesNorm)

