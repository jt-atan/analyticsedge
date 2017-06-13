#Part 1
getwd()
mvt = read.csv('mvtWeek1.csv')
summary(mvt)
str(mvt)
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert
hist(mvt$Date, breaks=100)
boxplot(Date ~ Arrest, data = mvt)
boxplot(mvt$Date ~ mvt$Arrest)
str(mvt$Date)
summary(mvt)
summary(mvt$Date)
str(mvt$Date)
subset(mvt, Year = 2001)
yr2001 = subset(mvt, Year = 2001)
summary(yr2001)
yr2001$Arrest
summary(yr2001$Arrest)
15536/(15536+176105)
str(yr2001$Arrest)
table(mvt$Year, mvt$Arrest)
1212/(13068+1212)
550/(550+13542)
sort(table(mvt$LocationDescription))
Top5 = subset(mvt, LocationDescription == "STREET" | LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)" | LocationDescription == "ALLEY" | LocationDescription == "GAS STATION" | LocationDescription == "DRIVEWAY - RESIDENTIAL")
summary(Top5)
str(Top5)
Top5$LocationDescription = factor(Top5$LocationDescription)
str(Top5)
table(Top5$LocationDescription, Top5$Arrest)
gs = subset(Top5, LocationDescription == "GAS STATION")
summary(gs)
gs$LocationDescription = factor(gs$LocationDescription)
str(gs)
table(gs$LocationDescription, gs$Weekday)
table(Top5$LocationDescription, Top5$Weekday)
#Part 2





