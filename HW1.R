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

#Part 2
IBM = read.csv("IBMStock.csv")
GE = read.csv("GEStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
Boeing = read.csv("BoeingStock.csv")

#1.1 
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

str(IBM)

summary(IBM)

summary(GE)

summary(CocaCola)

summary(Boeing)

sd(ProcterGamble$StockPrice)

plot(CocaCola$Date, CocaCola$StockPrice, type='l', col = 'red')

lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = 'blue')

abline(v=as.Date(c("2003-03-01")),lwd=2)

abline(v=as.Date(c("1983-01-01")),lwd=2)

#3.1
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432],type='l', col='red', ylim=c(0,210))
lines(GE$Date[301:432], GE$StockPrice[301:432],type='l', col='wheat2', ylim=c(0,210))
lines(IBM$Date[301:432], IBM$StockPrice[301:432],type='l', col='steelblue4', ylim=c(0,210))
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432],type='l', col='seagreen1', ylim=c(0,210))
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432],type='l', col='yellow', ylim=c(0,210))

#3.3
abline(v=as.Date(c("1997-09-01")), lwd=2)
abline(v=as.Date(c("1997-11-01")), lwd=2)

#3.4
abline(v=as.Date(c("2004-01-01")), lwd=2)
abline(v=as.Date(c("2005-12-31")),lwd=2)
tapply(IBM$StockPrice, months(IBM$Date), mean)
mean(IBM$StockPrice)
which(months(IBM$Date) > mean(IBM$StockPrice))

#4.2
tapply(GE$StockPrice, months(GE$Date), mean)
mean(GE$StockPrice)
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)
mean(CocaCola$StockPrice)


#Part 3
CPS = read.csv("CPSData.csv")
summary(CPS)
str(CPS)
summary(CPS$State)
sort(table(CPS$State))
summary(CPS$Citizenship)
(116639+7073)/(116639+7073+7590)

table(CPS$Race, CPS$Hispanic)
#2.1
summary(CPS)
table(CPS$State, is.na(CPS$Married))

#2.3
table(CPS$State, is.na(CPS$MetroAreaCode))
table(CPS$Region, is.na(CPS$MetroAreaCode))

#2.5
sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))

#3.1
MetroAreaMap = read.csv('MetroAreaCodes.csv')
CountryMap= read.csv('CountryCodes.csv')
str(MetroAreaMap)
str(CountryMap)
summary(MetroAreaMap)

#3.2
CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y = "Code", all.x = TRUE)
summary(CPS)

