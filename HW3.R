"
MITx: 15.071x The Analytics Edge
Homework 3: Logistic Regression
"
songs = read.csv('songs.csv')
#yr 2010
str(songs)
str(subset(songs, year == 2010))

#michael jackson?
str(subset(songs, artistname=='Michael Jackson'))
MJ = subset(songs, artistname == 'Michael Jackson')
summary(MJ)

#WHich songs are top10?
MJ$Top10
sum(MJ$Top10)

subset(MJ, Top10 == 1)
#Can also do...
MJ[c('songtitle', 'Top10')]

summary(songs)
unique(MJ$timesignature)
table(songs$timesignature)
max(songs$tempo)
subset(songs, tempo == 244.307)

which.max(songs$tempo) 
songs$songtitle[6206]

#Problem 2.1
SongsTrain = subset(songs, year <= 2009)
SongsTest = subset(songs, year == 2010)

#to remove some rows for training
nonvars = c('year','songtitle','artistname','songID','artistID')
SongsTrain = SongsTrain[,!(names(SongsTrain) %in% nonvars)]
SongsTest = SongsTest[,!(names(SongsTest) %in% nonvars)]

SongsLog1 = glm(Top10 ~ ., data=SongsTrain,family = binomial)
summary(SongsLog1)

#beware of multi colinearities
cor(SongsTrain$loudness, SongsTrain$energy)

#the - removes a column from consideration, ONLY WORKS FOR NUMERIC VALUES
SongsLog2 = glm(Top10 ~. -loudness, data=SongsTrain, family = binomial)
SongsLog3 = glm(Top10 ~. -energy, data = SongsTrain, family = binomial)

summary(SongsLog2)
summary(SongsLog3)

#4.1 - making predictions on the test set - DON'T FORGET TO HAVE TYPE RESPONSE
myPrediction = predict(SongsLog3, newdata = SongsTest, type = 'response')
summary(myPrediction)
table(SongsTest$Top10, myPrediction >= 0.45)

(309+19)/(309+5+40+19)

#Baseline model accuracy
table(SongsTest$Top10)

#checking predictions again for TPs and FPs
table(SongsTest$Top10, myPrediction >= 0.45)

#Sensitivity and Specificity? 
#Sensitivity is TP/(TP +FN) I think?
#Specificity is TN/(TN+FP)
table(SongsTest$Top10)
19/(19+40)
309/(309+5)
'
So this model is quite good _BECAUSE IT IS SPECIFIC_. The edge here is because while it may miss a lot, it will
be 98% right when it does make a prediction!!! Quite good!!!

This is good for sales and customer applications to me.
'

'*******PART 2******'
parole = read.csv('parole.csv')
str(parole)
summary(parole)
table(parole$violator)
table(parole$state)

#state and crime are factors, so we need to convert them. important to remember!
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

summary(parole)

#3.1
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

#4.1
paroleModel1 = glm(violator ~ ., data = train, family = binomial)
summary(paroleModel1)
summary(parole)
str(test)

#5.1 predict
myPredictions = predict(paroleModel1, newdata = test, type = 'response')
summary(myPredictions)
12/(12+11)
167/(167 + 12)
(167+12)/(167+12+12+11)

#threshold accuracy
table(test$violator, myPredictions >= 0.5)
#baseline accuracy:
179/(179+23)

table(test$violator, myPredictions >= 0.1)

'
The model at cutoff 0.5 has 12 false positives and 11 false negatives, while the baseline model has 0 false positives and 23 false negatives. Because a parole board is likely to assign more cost to a false negative, the model at cutoff 0.5 is likely of value to the board.
From the previous question, the parole board would likely benefit from decreasing the logistic regression cutoffs, which decreases the false negative rate while increasing the false positive rate.
'

#AUC
library(ROCR)
ROCRprediction = prediction(myPredictions, test$violator)
auc = as.numeric(performance(ROCRprediction, "auc")@y.values)
auc

'******PART 3******'
loans = read.csv('loans.csv')
str(loans)
summary(loans)
table(loans$not.fully.paid)
1533/8045
1533/(1533+8045)
is.na(loans$pub.rec)

#We need to impute the missing data - this is how you impute missing variables in R
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans),'not.fully.paid')
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

#let's load the imputed data from the dataset to ensure accurate results
loans = read.csv('loans_imputed.csv')

#make prediction
set.seed(144)

#Here is how you do train-test-split in R and do a logistic regression in R
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)

loansModel1 = glm(not.fully.paid ~ ., data = train, family = binomial)
summary(loansModel1)

#What are the odds of repayment if person A has 700 FICO and person B has 710 FICO?
(-.009317*700)-(-9.317*710)
exp((-.009317*700)+9.187)-exp((-.009317*710)+9.187)
#fuck! this is the right answer:
-0.009317 * -10

#2.3
#predict the probabilities of the model.
predicted.risk = predict(loansModel1, newdata = test, type = 'response')
#confusion matrix
table(test$not.fully.paid,predicted.risk >= 0.5)
#accuracy
(2400+3)/(2400+13+457+3)
#baseline prediction
table(test$not.fully.paid,predicted.risk >= 0.0)
#we're supposed to add it?
test$predicted.risk = predicted.risk

#AUC calculation:
library(ROCR)
ROCRpreds = prediction(predicted.risk, test$not.fully.paid)
auc = as.numeric(performance(ROCRpreds, 'auc')@y.values)
auc

#bivariate Logistic regression model (is a model with a single independent variable)
bivariateModel = glm(not.fully.paid ~ int.rate, data = train, family = binomial)
summary(bivariateModel)

bvPredictions= predict(bivariateModel, newdata = test, type = 'response')
max(bvPredictions)
table(test$not.fully.paid, bvPredictions >= 0.0)
summary(bvPredictions)
#since it only had one column, no probabilities were over 0.5

#auc again
library(ROCR)
ROCRpreds = prediction(bvPredictions, test$not.fully.paid)
auc = as.numeric(performance(ROCRpreds, 'auc')@y.values)
auc

#How much does a 10 dollar investment with continuous compounding interest pay
# back after 3 years? 6% interest
10*exp(.06*3)


#get profit on loans if you invest $1 in each of them 
test$profit = exp(test$int.rate*3)-1
#this is a good command to know, subtract -1 from profit where the loans are not fully paid
test$profit[test$not.fully.paid == 1] -1
max(test$profit)

#subset high interest loans
HighInterestTestSet = subset(test, int.rate >= 0.15)
summary(HighInterestTestSet)
HighInterestTestSet$profit = exp(HighInterestTestSet$int.rate*3) - 1
HighInterestTestSet$profit[HighInterestTestSet$not.fully.paid == 1] -1
summary(HighInterestTestSet)

highInterest = subset(test, int.rate >= 0.15)
summary(highInterest$profit)
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1

#6.3
'** IMPORTANT R COMMAND! this sorts a column by risk and returns a total value'
cutoff = sort(highInterest$predicted.risk, decreasing = FALSE)[100]
str(highInterest)
str(cutoff)
selectedLoans = subset(highInterest, predicted.risk <= cutoff)
str(selectedLoans)
#what is the profit of the investor of the top 100 loans for this cutoff predicted risk?
summary(selectedLoans)
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)
str(selectedLoans)

