#Notes Week 7 edX 
#MITx: 15.071x The Analytics Edge
#Visualizations in R with ggplot2
'
Part One: Visualizing Chicago Crime
'
mvt = read.csv('mvt.csv', stringsAsFactors = FALSE)
str(mvt)

#strip datetime, convert to weekdays and hours for the heatmap
mvt$Date = strptime(mvt$Date, format = "%m/%d/%y %H:%M")
mvt$Weekday = weekdays(mvt$Date)
mvt$Hour = mvt$Date$hour
str(mvt)

#now let's make some line plots!
WeekdayCounts = as.data.frame(table(mvt$Weekday))
str(WeekdayCounts)
library(ggplot2)
#groups all of our data into one plot
ggplot(WeekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group =1 ))

#so we need to have the order be meaningful, so we do an ordered Factor, using the factor function
WeekdayCounts$Var1 = factor(WeekdayCounts$Var1, ordered = TRUE, levels = c('Sunday', 'Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'))

#plot again
ggplot(WeekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group =1 ))
#add labels
ggplot(WeekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group =1 )) + xlab('day of week') + ylab('totol motor thefts')
#add dashed line instead of solid
ggplot(WeekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group =1 ), linetype = 2) + xlab('day of week') + ylab('totol motor thefts')
#vary line transparency
ggplot(WeekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group =1 ), alpha = 0.3) + xlab('day of week') + ylab('totol motor thefts')


table(mvt$Weekday, mvt$Hour)

DayHourCounts = as.data.frame(table(mvt$Weekday, mvt$Hour))
str(DayHourCounts)
DayHourCounts$Hour = as.numeric(as.character(DayHourCounts$Var2))

#adds color to it
ggplot(DayHourCounts, aes(x = Hour, y = Freq)) + geom_line(aes(group=Var1, color = Var1))

DayHourCounts$Var1 = factor(DayHourCounts$Var1, ordered = TRUE, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq))

ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name = "Total MV Thefts", low = 'white', high = 'darkred') + theme(axis.title.y = element_blank())

install.packages('maps')
install.packages('ggmap')

library(maps)
library(ggmap)

chicago = get_map(location = 'chicago', zoom = 11)
ggmap(chicago)
ggmap(chicago) + geom_point(data = mvt[1:100,], aes(x = Longitude, y = Latitude))

LatLonCounts = as.data.frame(table(round(mvt$Longitude,2), round(mvt$Latitude,2)))
# Convert our Longitude and Latitude variable to numbers:
LatLonCounts$Long = as.numeric(as.character(LatLonCounts$Var1))
LatLonCounts$Lat = as.numeric(as.character(LatLonCounts$Var2))

str(LatLonCounts)
# Plot these points on our map:
ggmap(chicago) + geom_point(data = LatLonCounts, aes(x = Long, y = Lat, color = Freq, size=Freq))

# Change the color scheme:
ggmap(chicago) + geom_point(data = LatLonCounts, aes(x = Long, y = Lat, color = Freq, size=Freq)) + scale_colour_gradient(low="yellow", high="red")

# We can also use the geom_tile geometry
ggmap(chicago) + geom_tile(data = LatLonCounts, aes(x = Long, y = Lat, alpha = Freq), fill="red")


str(LatLonCounts)
LatLonCounts2 = subset(LatLonCounts, Freq >0)

str(LatLonCounts2)
1638-686
ggmap(chicago) + geom_tile(data=LatLonCounts2, aes(x = Long, y = Lat, alpha=Freq), fill="red")


# Load our data:
murders = read.csv("murders.csv")

str(murders)

# Load the map of the US
statesMap = map_data("state")
str(statesMap)

# Plot the map:
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") 

# Create a new variable called region with the lowercase names to match the statesMap:
murders$region = tolower(murders$State)

# Join the statesMap data and the murders data into one dataframe:
#this is an important dataframe function
murderMap = merge(statesMap, murders, by="region")
str(murderMap)

# Plot the number of murder on our map of the United States:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Murders)) + geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")

# Plot a map of the population:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Population)) + geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")

# Create a new variable that is the number of murders per 100,000 population:
murderMap$MurderRate = murderMap$Murders / murderMap$Population * 100000

# Redo our plot with murder rate:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = MurderRate)) + geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")

# Redo the plot, removing any states with murder rates above 10:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = MurderRate)) + geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend", limits = c(0,10))

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = GunOwnership)) + geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")

x =1 
'
For ggplot, you have a three layers. 1: the data frame. 2. the aes, the aesthetics, x and y values
and their respective colors, and third, the type of graph you actually want to graph
'


WHO = read.csv("WHO.csv")
str(WHO)

#make a plot of fertility rate
plot(WHO$GNI, WHO$FertilityRate)

#now use ggplot2
library(ggplot2)
scatterplot = ggplot(WHO, aes(x = GNI, y = FertilityRate))
scatterplot + geom_point()
#or....
scatterplot + geom_line()
#or....
scatterplot + geom_point(color='blue', size = 3, shape = 8)

scatterplot + geom_point(color='darkred', size = 3, shape = 15)

fertilityGNIplot = scatterplot + geom_point(color='darkred', size = 3, shape = 8) + ggtitle("Fertility rate vs GNI")
fertilityGNIplot
#how to print a plot to a pdf file
pdf('Myplot.pdf')
print(fertilityGNIplot)
dev.off() #closes the file

#now let's plot a color density too!!!!!
ggplot(WHO, aes(x = GNI, y = FertilityRate, color = Region)) + geom_point()

#now for life expectancy
ggplot(WHO, aes(x = GNI, y = FertilityRate, color = LifeExpectancy)) + geom_point()

ggplot(WHO, aes(x = FertilityRate, y = Under15, color = LifeExpectancy)) + geom_point()

#so log would be better here maybe?
ggplot(WHO, aes(x = log(FertilityRate), y = Under15, color = LifeExpectancy)) + geom_point()
#looks good!

#let's plot a line

model = lm(Under15 ~ log(FertilityRate), data =WHO)
summary(model)

ggplot(WHO, aes( x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = 'lm')
#by default it draws 95% confidence interval
# let's change that to 99
ggplot(WHO, aes( x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = 'lm', level = 0.99)
ggplot(WHO, aes( x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = 'lm', se = FALSE, color = 'orange')

#quick question
ggplot(WHO, aes(x = FertilityRate, y = Under15, color = Region)) + geom_point()

'
Part 3 recitation
'
'A good visualization conveys the key information you want to tell by the data

A bad visualization hides trends or information, this is the malice part.'

library(ggplot2)
intl = read.csv('intl.csv')
str(intl)

#let's make a first plot
ggplot(intl, aes(x = Region, y = PercentOfIntl)) + geom_bar(stat = 'identity') + geom_text(aes(label = PercentOfIntl))
#ggplot defaults for alphabetical for the x axis
#what we need to do is make region an ordered factor instead of an unordered factor
intl = transform(intl, Region = reorder(Region, -PercentOfIntl))
str(intl)
intl$PercentOfIntl = intl$PercentOfIntl * 100 
ggplot(intl, aes(x = Region, y = PercentOfIntl)) + geom_bar(stat = 'identity', fill = 'blue') + geom_text(aes(label = PercentOfIntl), vjust = -0.4) + ylab('Percent of intl students') + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))



intlall = read.csv('intlall.csv', stringsAsFactors = TRUE)
head(intlall)
# Those NAs are really 0s, and we can replace them easily
intlall[is.na(intlall)] = 0

head(intlall)

world_map = map_data("world")
str(world_map)
# Lets merge intlall into world_map using the merge command
world_map = merge(world_map, intlall, by.x ="region", by.y = "Citizenship")
str(world_map)

ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white", color="black") +
  coord_map("mercator")

# Reorder the data
world_map = world_map[order(world_map$group, world_map$order),]

# Redo the plot
ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white", color="black") +
  coord_map("mercator")

# Lets look for China
table(intlall$Citizenship) 

# Lets "fix" that in the intlall dataset
intlall$Citizenship[intlall$Citizenship=="China (People's Republic Of)"] = "China"

# We'll repeat our merge and order from before
world_map = merge(map_data("world"), intlall, 
                  by.x ="region",
                  by.y = "Citizenship")
world_map = world_map[order(world_map$group, world_map$order),]

ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=Total), color="black") +
  coord_map("mercator")

# We can try other projections - this one is visually interesting
ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=Total), color="black") +
  coord_map("ortho", orientation=c(20, 30, 0))

ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=Total), color="black") +
  coord_map("ortho", orientation=c(-37, 175, 0))
