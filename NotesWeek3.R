#Notes Week 3 edX 
#MITx: 15.071x The Analytics Edge
#Logistic Regression

#Logistic regression helps calculate categorical variable values, like whether to buy, hold -
#or sell a stock. It creates a categorical probability outcome with odds for the probability

#Very similar to deep learning, which is stacked logistic regression.

#Logit calculation: 
-1.5 + 3*(1) + (-.5*5)
#odds
exp(-1)
#p(y=1)
1/(1+exp(1))


#Quality Dataset
quality = read.csv('quality.csv')
str(quality)

#A standard baseline method is to predict the most frequent outcome for all observations.
#i.e. for the training data, 98/131 is good care, to 75%
install.packages('caTools')
library(caTools)
#set seed to be same for each time
set.seed(88)

split = sample.split(quality$PoorCare, SplitRatio = .75)

#split shows which ones are in training and testing set with BOOLEAN operator
split

qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split==FALSE)

#let's check rows
nrow(qualityTest)
nrow(qualityTrain)

#Build LogR mode, family =binomial tells it to build a log R model
qualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, family = binomial, data= qualityTrain)
summary(qualityLog)

#AIC is like Adjusted R squared, provides a mean for model selection.
#Preferred model is one with the most minimal AIC!!!

predictTrain = predict(qualityLog, type='response' )
#type-'response' tells it to give probabilities

summary(predictTrain)

#now we check results
tapply(predictTrain, qualityTrain$PoorCare, mean)

#QQs
newModel = glm(PoorCare ~ StartedOnCombination + ProviderCount, family = binomial, data = qualityTrain)
summary(newModel)
#positive coefficient....indicating poor care so I guess poor care is 1 not 0

#thresholds...with no preference in erros (over sensitive or under sensitive), the best threshold to select
# would be t=0.5
#illustrated by a confusion matrix
#Sensitivity = True Positives / (True Positives + False Negatives) <- AKA True Positive Rate
#Specificity = True Negatives/ (True Negatives + False Positives) <- AKA True Negative Rate

#First let's try 0.5 and a confusion matrix
table(qualityTrain$PoorCare, predictTrain >0.5)
sensitiv = 10/(10+15)
specific = 70/(70+4)
sensitiv
specific

table(qualityTrain$PoorCare, predictTrain >0.7)
table(qualityTrain$PoorCare, predictTrain >0.2)


"
ROC Curves

ROC Curves allows us to see which value of thresholds are best.

The ROC curve ccaptures all thresholds simultaneously

"
#To generate an ROC curve in R we need new package
install.packages("ROCR")
library(ROCR)
ROCRpred = prediction(predictTrain, qualityTrain$PoorCare)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
#Now we plot
plot(ROCRperf)

#Now we add colors
plot(ROCRperf, colorize = TRUE)

#Add ticks
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj = c(-0.2,0.7))

"
Interpreting the model
Check co-efficients, do they make sense? And check for multi-colinearilty!!

AUROC/AUC:

AUC shows an absolute measure of the quality of a prediction. "

predictTest = predict(qualityLog, type="response", newdata=qualityTest)

#Compute AUC
#*** Very important to remember!
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

"
Week 3 Notes, Part Two
The Framingham Heart Study
"
framingham = read.csv('framingham.csv')

#First randomly split into a training set and testing set
str(framingham)
library(caTools)
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)

#When you have more data, you can put more in the testing set than in the training set. Need 50-80% of the data
# in the training set

train = subset(framingham, split == TRUE)
test = subset(framingham, split == FALSE)

framinghamLog = glm(TenYearCHD ~ ., data = train, family = binomial)
summary(framinghamLog)

predictTest = predict(framinghamLog, type = "response", newdata = test)
table(test$TenYearCHD, predictTest  >0.5)

#What is the accuracy of the model?
(1069+11)/(1069+6+11+187)

#The baseline method would choose CHF ten year as zero.
#Total true negative/ total # obs
(1069+6)/(1069+6+187+11)

#So the model barely beats baseline....
#Let's create an AUC

library(ROCR)

ROCRpred = prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)

#So our AUC is 74%, can tell pretty well the CHD risk then. Can diffferentiate between low risk and high
# risk patients pretty well.

#QQ Sensitivity and Specificity
#Sensitivity is: TP/TP+FN
11/(11+187)
#Specificity is: TN/TN + FP
1069/(1069+6)

"
External validation is critical where the populations are uniform, need to test on other populatins
"

"
RECITATION: ELECTION PREDICTION
"

polling = read.csv('PollingData.csv')
str(polling)

table(polling$Year)
#5 states missing in 2012, they were very sure where they would vote. So only making predictions
# for 45 states

summary(polling)

" What do we do for missing values? We can do something called Multiple Imputation, that will
infer what the values are for the missing data, i.e. if all the other ones are negative, we 
can safely assume that it is Negative.

Using Multiple Imputation by Chained Equations, or mice

"
install.packages('mice')
library(mice)

# We need to select only the variables of the survey for imputation

simple = polling[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount")]
summary(simple)

#Multiple Imputation Changes the values each time you run it
set.seed(144)
imputed = complete(mice(simple))

summary(imputed)

#Last step is to copy the Rasmusson and SurveyUSA variables back into the original
polling$Rasmussen = imputed$Rasmussen
polling$SurveyUSA = imputed$SurveyUSA

polling = read.csv('PollingData_Imputed.csv')
summary(polling)
#Boom they are no longer missing any values!

#We are going to train on data from 2004 and 2008 elections and test on 2012 elections

Train = subset(polling, Year == 2004 | Year == 2008)
Test = subset(polling, Year == 2012)

table(Train$Republican)

#The baseline model predicts republicans winning the state. 
#But it's a poor model.How do we make a better one?
table(sign(Train$Rasmussen))

table(Train$Republican, sign(Train$Rasmussen))
#This shows a confusion matrix of accuracy.
#This is a better baseline to use! Better than the nieve republican baseline

cor(Train)
#Can't take correlation of names of states of course
cor(Train[c('Rasmussen', 'SurveyUSA', 'PropR','DiffCount','Republican')])
#So Rasmussen and SurveyUSA are quite large correlation, so combining them together isn't so
# great for building a regression model

mod1 = glm(Republican ~ PropR, data = Train, family = binomial)
summary(mod1)

pred1 = predict(mod1, type = 'response')
table(Train$Republican, pred1 >= 0.5)

mod2 = glm(Republican ~ SurveyUSA + DiffCount, data = Train, family = binomial)
pred2 = predict(mod2, type = 'response')
table(Train$Republican, pred2 >= 0.5)

summary(mod2)

table(Test$Republican, sign(Test$Rasmussen))

#Now we predict on test
TestPrediction = predict(mod2, newdata=Test, type = 'response')

table(Test$Republican, TestPrediction >= 0.5 )

"AOC is not so important here because we need to just get democrat or republican, and 0.5 is 
a good metric to use since it goes either way.

But we missed one...so now what?
"

subset(Test, TestPrediction >= 0.5 & Republican == 0)

#Fin!