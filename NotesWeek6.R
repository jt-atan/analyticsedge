#Notes Week 6 edX 
#MITx: 15.071x The Analytics Edge
#Clustering
'
Part One: Netflix hierarchical clustering
'

movies = read.table('movielens.txt', header = FALSE, sep = '|', quote = "\"")
str(movies)

colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")
str(movies)
# Remove unnecessary variables

movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL

sum(movies$Comedy)
table(movies$Western)
table(movies$Romance, movies$Drama)

#to compute the distances, we use the dist function
# we cluster on columns 2 -20
distances = dist(movies[2:20], method = 'euclidian')
clusterMovies = hclust(distances, method = 'ward.D')
plot(clusterMovies)
#it puts all of the data points on the bottom, so it's hard to read
# you need to understand the dataset to have an idea of how many clusters you need

clusterGroups = cutree(clusterMovies, k=10)
#Now let's figure out what the clusters are like

tapply(movies$Action, clusterGroups, mean)
#So what is this doing? It divides our datapoints into the ten values, and computes the
# action variable for each cluster. The action variable is originally 0/1, so we can see
# the effect of where action falls in the cluster

'So if someone likes men in black, what other movies can we recommend?'

tapply(movies$Romance, clusterGroups, mean)
subset(movies, Title == 'Men in Black (1997)')
clusterGroups[257]
cluster2 = subset(movies, clusterGroups ==2)

#Let's show what god movies to recommend if you like Men in Black
cluster2$Title[1:20]

'
******
This is really cool!!! because you can create product clusters based on product types, etc
So if you are buying something, you can say "other people typically buy this as well...
Good/great for sales teams like ppl at R&S!!
*******
'
#Quick question, let's re run for 2 clusters instead of others

clusterGroupsTwo = cutree(clusterMovies, k=2)
tapply(movies, clusterGroupsTwo, mean)
#What's an easier way to get column cluster distributions? Use this:
cm = colMeans(subset(movies[2:20], clusterGroupsTwo == 1))
#Where the number is the cluster you select to see distribution

#and what about all at once? Create a spplit then tdo it
spl = split(movies[2:20], clusterGroups)
lapply(spl, colMeans)

#clustering algorithms form similar customers or similar items is very important for
# businesses . super cool and super important! I should look again at the fast.ai 
# deep learning course and implement the deep learning recommendation system
# and make a blog post about it

'
Part 2 Predicting Heart Attacks!!!
'

'680 people every day die of sudden cardiac death!
only 27% recognize the symptoms and call 911 for health. 25% are "silent"
what are the clinical characteristics of heart attacks that are missed?


In this lecture, they have a massive dataset of insurance claims. They split the cost
into three buckets, 1 to 3, depending on claims and severity. 
60 % fall into bucket 1, low claims less than 10k per year, the third have the highest claims
by far
Random forest is GREAT for selecting feature importance and as a learning algorithm
itself. 

This focuses of K means clustering

K means: we specificy the number of clusters K. Then we randomly assign each data
point to a cluster. we assign each point to the closest cluster centroid.
then we repeat steps 4-5 until no improvement is made

So using random forests WITH clustering performs 65% over the 49%!!!

Clusters are unique and reveal unique patterns in the data

The analytics edge: using pattern recognition can be used for finding early 
heart failures in populations using clustering and random forests.

this allows early detection of this deadly disease
'


'PART 3 - RECITATION
MEDICAL IMAGE SEGMENTATION FOR MRI IMAGES
image segmentation processes digital images into regions
'

flower = read.csv("flower.csv", header = FALSE)
summary(flower)
str(flower)
#so we need to change the data type from variables to a matrix
flowerMatrix = as.matrix(flower)
str(flowerMatrix)

#to perform ANY type of clustering, we need to convert the matrix of pixel 
# instenities to a vector

flowerVector = as.vector(flowerMatrix)
str(flowerVector)
#the str shows 2500, because it's 50x50 for the flower matrix

#converting it to a matrix AND THEN a vector is a crucial step, or else 
# it won't work!
fv2 = as.vector(flower)
str(fv2)

distance = dist(flowerVector, method = 'euclidean')

#now we can cluster the intensity values using hierarchical clustering
clusterIntensity = hclust(distance, method = 'ward.D')

#Ward's method finds the distance between both areas, and minimizes the
# variance between each cluster, and the distance between each cluster

#now we plot!
plot(clusterIntensity)

#so we can visualize the cuts by actually plotting the rectangles around the
# tree
rect.hclust(clusterIntensity, k = 3, border = 'red')

#so let us split the data into these three clusters
flowerClusters = cutree(clusterIntensity, k=3)

str(flowerClusters)

#how do we find the mean intensity of each cluster?
tapply(flowerVector, flowerClusters, mean)
#but flower clusters is a vector!
#we can do it using the dimension function
dim(flowerClusters) = c(50,50)
#we are doing 50,50 because our image is 50,50

image(flowerClusters, axes = FALSE)
image(flowerMatrix, axes = FALSE, col = grey(seq(0,1, length= 256)))

'Now for the MRI image
'

healthy = read.csv('healthy.csv', header = FALSE)
healthyMatrix = as.matrix(healthy)
str(healthyMatrix)
image(healthyMatrix, axes = FALSE, col = grey(seq(0,1,length = 256)))

#convert to vector
healthyVector = as.vector(healthyMatrix)
distance = dist(healthyVector, method = 'euclidean')
str(healthyVector)
n = 365636
n*(n-1)/2
#Since it's so big, we can't use hierarchical clustering

#So is there another method we could use? KMEANS!
# k is the cluster numbers 
#randomly assign each point to a cluster 
#compute cluster centroids 
# then reassign data points according to closest cluster centroid

#So how do we select k?
# So setting the # of clusters depends on what you are trying to extract
# from the image
k = 5
set.seed(1)
#we set iterations since it can take very long to converge
KMC = kmeans(healthyVector, centers=k, iter.max = 1000)
str(KMC)

#we are extracting the information of the cluster vector and putting it 
# into a new variable
healthyClusters = KMC$cluster
#Using kmeans we have the answers already!
#here is how we get the centers
KMC$centers[2]
dim(healthyClusters) = c(nrow(healthyMatrix), ncol(healthyMatrix))
#now let's plot it!
image(healthyClusters, axes = FALSE, col = rainbow(k))

#****
#so can we use it to identify tumors of a sick patient???
tumor = read.csv('tumor.csv', header = FALSE)
tumorMatrix = as.matrix(tumor)
tumorVector = as.vector(tumorMatrix)
#so we use our original k means clustering of a healthy brain to compare against
#sick ones!
library(flexclust)
#flexclust has k centroids cluster analysis
KMC.kcca = as.kcca(KMC, healthyVector)
tumorClusters = predict(KMC.kcca, newdata = tumorVector)
#that assigned a value of 1-5 for the intensity values of the tumor vector
dim(tumorClusters) = c(nrow(tumorMatrix), ncol(tumorMatrix))
image(tumorClusters, axes = FALSE, col = rainbow(k))

#SCREE PLOTS IN R
#a faster method below to see the effects of all the cluster possibilities
SumWithinss = sapply(2:10, function(x) sum(kmeans(healthyVector, centers=x, iter.max=1000)$withinss))
NumClusters = seq(2,10,1)
plot(NumClusters, SumWithinss, type="b")



