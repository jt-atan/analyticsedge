#HW2
clim_change = read.csv('climate_change.csv')
summary(clim_change)

train_clim = subset(clim_change, Year <= 2006)
summary(train_clim)

test_clim = subset(clim_change, Year >2006)

#Calculate temp on training set
tempMd1 = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data =train_clim)
summary(tempMd1)
cor(train_clim)

tempMd2 = lm(Temp ~ MEI + TSI + Aerosols + N2O, data=train_clim)
summary(tempMd2)

#*** Remember this function, step iterates through all possibilities to find the best function given
#the dependent variables
bestModel = step(tempMd1)
summary(bestModel)

#Predict!! Good to remember this

results = predict(bestModel, newdata = test_clim)
summary(results)
SSE = sum((test_clim$Temp - results)^2)
SST = sum((test_clim$Temp - mean(train_clim$Temp))^2)
1 - (SSE/SST)

'''
*****Part 2*****
'''

pisaTrain = read.csv('pisa2009train.csv')
pisaTest = read.csv('pisa2009test.csv')

str(pisaTrain)

#average reading score by sex
tapply(pisaTrain$readingScore, pisaTrain$male, mean)

#missing data
summary(pisaTrain)

#omit missing data
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
str(pisaTrain)
str(pisaTest)

#Set the reference level of our factoring to White, since it is the most common Race variable
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth,"White")

#lin reg
lmScore = lm(readingScore ~., data=pisaTrain)
summary(lmScore)

#RMSE
summary(lmScore)$sigma

#predict
myPreds = predict(lmScore, newdata = pisaTest)
summary(myPreds)
637.7-353.2

#Get SSE and RMSE of predictions
SSE = sum((pisaTest$readingScore - myPreds)^2)
SST = sum((pisaTest$readingScore - mean(pisaTrain$readingScore))^2)
SST
SSE
summary(myPreds)
sqrt(SSE/990)
summary(lmScore)
#r-squared
1 - SSE/SST
#baseline model predicted score
mean(pisaTrain$readingScore)

'''
Part 3 FluTrain
'''

FluTrain = read.csv('FluTrain.csv')
summary(FluTrain)
str(FluTrain)
max(FluTrain$ILI)

#Get weeks wil max ILI and Queries for Flu
xx = subset(FluTrain, ILI >= 7.6, select = Week)
summary(xx)
str(xx)
max(FluTrain$Queries)
xx = subset(FluTrain, Queries >= 1.0, select = Week)
summary(xx)
str(xx)
xx
#** Other important possibilites
which.max(FluTrain$Queries)
FluTrain$Week[303]
#This was better to do it by...

hist(FluTrain$ILI)
plot(FluTrain$Queries,log(FluTrain$ILI))

FluTrend1 = lm(log(ILI) ~ Queries, data = FluTrain)
summary(FluTrend1)

#expand correlations
cor(FluTrain$Queries, log(FluTrain$ILI))

#test Flu data
FluTest=read.csv('FluTest.csv')
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
summary(PredTest1)
FluTest$Week[11]
PredTest1[11]

#Relative Error
(FluTest$ILI[11] - PredTest1[11])/FluTest$ILI[11]

#RSME Error between predictions and actual values
sqrt(mean((FluTest$ILI-PredTest1)^2))


#3.4.1
install.packages("zoo")
library(zoo)
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
summary(ILILag2)
summary(FluTrain)
plot(log(FluTrain$ILILag2),log(FluTrain$ILI))

FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)
summary(FluTrend2)
summary(FluTrend1)

#5.1
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)
summary(FluTest$ILILag2)

#5.3
str(FluTrain)
FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[2] = FluTrain$ILI[417]
#5.4
PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
sqrt(mean((FluTest$ILI-PredTest2)^2))
