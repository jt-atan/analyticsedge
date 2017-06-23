#Lecture Notes
wine = read.csv('wine.csv')
str(wine)
summary(wine)

model1 = lm(Price ~ AGST, data = wine)
summary(model1)
#Adjusted R squared adjusts depending on how effective a variable is in a given model,
#The adjusted R squared can lower depending on the model
#Multiple R squared will always in crease

#Let's get residuals
model1$residuals

#This calculates the Sum of Squared Errors
SSE = sum(model1$residuals^2)
SSE
#Now a new model with more
model2 = lm(Price ~ AGST + HarvestRain, data = wine)
summary(model2)
SSE = sum(model2$residuals^2)
SSE

#third model with all independent variables
model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data=wine)
summary(model3)
SSE = sum(model3$residuals^2)
SSE

#Finger Ex
flm = lm(Price ~ HarvestRain + WinterRain, data = wine)
summary(flm)

#notes
summary(model3)
#A coefficient of zero on lm summary means the independent variable does not affect the dependent variable
#If the coefficient is not significantly diff from 0, good to remove from the model
#T-value on the summary is the estimate divided by the standard error
#the larger the abs. value of the t_value the more likely the coefficient is significant
#It's helpful to look at the star(***) coding scheme, which codes the significance. Things with *
#in the value mean that it's significant

model4 = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data = wine)
summary(model4)
#Age and France Pop are correlated, which is why there is more significance added. Aka multi colinearity

#Finger Ex
flm = lm(Price ~ HarvestRain + WinterRain, data = wine)
summary(flm)

#Notes

'''
But what is correlation? Measures the linear relationship between two variabels
+1 = perfect postive linear relationship

Value between +1 to -1. Closer to zero means that they are not linearly correlated.
'''
cor(wine$WinterRain, wine$Price)
cor(wine$Age,wine$FrancePop)
#this gets it for all of the variables
cor(wine)

#So what happens if we remove Age and FrancePop at the same time?
model5 = lm(Price ~ AGST + HarvestRain + WinterRain, data=wine)
summary(model5)
#**So multicolinearity reminds us that some variables only are interpretable in the presence of other variables being
# used.
#Correlation greater than +/- .7 is a cause for concern!!!!
cor(wine)

#Let's test predictions
wineTest = read.csv('wine_test.csv')
str(wineTest)
#to predict....
predictTest = predict(model4, newdata = wineTest)
predictTest
SSE = sum((wineTest$Price - predictTest)^2)
SST = sum((wineTest$Price - mean(wine$Price))^2)
1 - SSE/SST


##******part 2

