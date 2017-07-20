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