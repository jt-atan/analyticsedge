#Notes Week 4 edX 
#MITx: 15.071x The Analytics Edge
#Decision Trees
'
PART ONE: PREDICTING SUPREME COURT DECISIONS USING CART AND DECISION TREES WITH CROSS-VALIDATION
'
stevens = read.csv('stevens.csv')
str(stevens)

#need to do train test split
#sample.split

library(caTools)
set.seed(3000)

spl = sample.split(stevens$Reverse, SplitRatio = 0.7)
Train = subset(stevens, spl == TRUE)
Test = subset(steves, spl == FALSE)
install.packages('rpart')
library('rpart')
install.packages('rpart.plot')
library('rpart.plot')

#let's build a tree! Minbucket stops the branching, preventing overfitting, at 25
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = 'class', minbucket = 25)
prp(StevensTree)

#A cart tree is a series of decision models easily explained
#note to give the type = 'class' to get predictions of threshold .5
PredictCART = predict(StevensTree, newdata = Test, type = 'class')
table(Test$Reverse, PredictCART)
(41+71)/(41+71+36+22)
#Baseline accuracy of always predicting reverse is .547, and a logistic regression model would be .66

library(ROCR)
#now generate predictions WITHOUT the class argument
PredictROC = predict(StevensTree, newdata = Test)
PredictROC

pred = prediction(PredictROC[,2], Test$Reverse)
perf = performance(pred, 'tpr', 'fpr')
plot(perf)

#plot auc
auc = as.numeric(performance(pred, 'auc')@y.values)
auc

#let's see how the minibucket size of 5 does
SecStevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = 'class', minbucket = 100)
prp(SecStevensTree)

'Random Forests: Designed to improve the prediction accuracy of ART, and works by building a large number of 
CART trees.

To make a prediction, each forest predicts an outcome and then the trees that are the mmost common
are the branch that are followed.

So the data used for the training data is selected  randomly wit h replacement!

eg: 5 types of data, 1-5

Can have 2,4,5,2 (so it is with replacement) <- 1st tree
or 3,5,1,5,2  <- second tree 

so you get a forest of many different trees!

Parameter values:

- Minimum number of observations in a subset, or minbucket. Called NODESIZE
- a smaller value of nodesize takes a longer time, much more computationally  instensive.
 - ntrees, # of trees, dont make small, a couple hundred trees will do it'

#Let's do it!
install.packages('randomForest')
library(randomForest)
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, nodesize = 25, ntree = 200)
#Note the warning message, we added method = class, so it was clear we were doing classificaiton!
# But trees can also do regression problems!!! So if we want to do a classification problem
#So we need to convert to a factor

Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)

#So let's do it again
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, nodesize = 25, ntree = 200)
#So no warning message this time! We can make predictions.

PredictForest = predict(StevensForest, newdata=Test)
table(Test$Reverse, PredictForest)
(40+74)/(40+37+19+74)
#67% accuracy! Random Forest improved it over cart!!!!
#random forests have a random component to them!!!!

#qq, set seeds to different levels, rebuild
set.seed(100)
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, nodesize = 25, ntree = 200)
PredictForest = predict(StevensForest, newdata=Test)
table(Test$Reverse, PredictForest)
(43+74)/(43+34+19+74)

set.seed(200)
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, nodesize = 25, ntree = 200)
PredictForest = predict(StevensForest, newdata=Test)
table(Test$Reverse, PredictForest)
(44+76)/(33+17+44+76)
#So the accuracy is varying a bit, and that is because the dataset is a bit noisy. A less noisy dataset 
# will not change much!

#so if min bucket is to small, overfitting might occur! If it's too big, it could be too simple!

'So instead we use K-fold cross validation! A good way to select the parameter value

First split the training set into k equally sized subsets, or folds. K =5.

Then select k-1, or 4 folds, for training, then 1 last fold for the validation set.

So cross validation builds many models for each parameter'

#CP is the Complexity Parameter. Measures the trade off between complexity and accuracy on a training set
#A cp value that is too large may be too simple!

#How to do Cross Validation in R
install.packages('caret')
library(caret)
install.packages('e1071')
library(e1071)

#Set folds
numFolds = trainControl(method='cv', number=10)
summary(numFolds)
cpGrid = expand.grid(.cp=seq(0.01,0.5,0.01)) #0.1 to 0.5 in increments of 0.01
#Now we can do Cross-Validation
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method='rpart', trControl = numFolds, tuneGrid = cpGrid)

# so now we build a new CART  model with this value of CP !!!
StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = 'class', cp = 0.18)
PredictCV = predict(StevensTreeCV, newdata = Test, type = 'class')
table(Test$Reverse, PredictCV)
#accuracy
(59+64)/(59+64+18+29)
#72% accuracy!!!!!!
#Cross Validation allows us to significantly increase our accuracy by selecting the best parameters for our model
#Cross Validation allows us to select the right smart parameter value

#Being able to predict Supreme Court decisions is very powerful!

'
PART TWO: The d2Hawkeye story on healthcare analytics and costs

'
Claims = read.csv('ClaimsData.csv')
summary(claims)
str(Claims)

table(Claims$bucket2009)/nrow(Claims)

library(caTools)
set.seed(88)
spl = sample.split(Claims$bucket2009, SplitRatio = 0.6)
ClaimsTrain = subset(Claims, spl == TRUE)
ClaimsTest = subset(Claims, spl == FALSE)

summary(ClaimsTrain)

#Let's get the baseline method
#Which is the accuracy
table(ClaimsTest$bucket2009, ClaimsTest$bucket2008)
(110138+10721+2774+1539+104)/nrow(ClaimsTest)

PenaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow = TRUE, nrow = 5)
PenaltyMatrix

#Penalty Error
as.matrix(table(ClaimsTest$bucket2009,ClaimsTest$bucket2008))*PenaltyMatrix
sum(as.matrix(table(ClaimsTest$bucket2009,ClaimsTest$bucket2008))*PenaltyMatrix)/nrow(ClaimsTest)

#quick questions - new baseline and penalty error
#predict baseline cost bucket 1 for everyone
#acc
122978/(122978+34840+16390+7937+1057)
122978/(nrow(ClaimsTest))
table(ClaimsTest$bucket2009)

sum(as.matrix(table(ClaimsTest$bucket2009,ClaimsTest$bucket2009))*PenaltyMatrix)/nrow(ClaimsTest)

# Build a CART model to predict costs in R
library(rpart)
library(rpart.plot)
ClaimsTree = rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=ClaimsTrain, method="class", cp=0.00005)
#Not doing Cross Validation since there are 275k and it will take a long time. The R commands using CV here
# are the same as it would be for the Supreme Court decisions

#let's look at the tree:
prp(ClaimsTree)

#let's make some predictions, and get confusion matrix
PredictTest = predict(ClaimsTree, newdata = ClaimsTest, type = 'class')
table(ClaimsTest$bucket2009, PredictTest)
#accuracy
(114141+16102+118+201+0)/(nrow(ClaimsTest))
#so .713% accuracy

#For Penalty Error, we can do the same as before
as.matrix(table(ClaimsTest$bucket2009, PredictTest))*PenaltyMatrix
#So this multiplies it by the corresponding nubmer in the penalty Matrix
#Now we just sum it all up
sum(as.matrix(table(ClaimsTest$bucket2009, PredictTest))*PenaltyMatrix)/nrow(ClaimsTest)
#So a penalty error of .74
# this is high because we are not optimizing for loss.
#We need to add a param to our Claims Tree for the loss function
#params = list(loss=PenaltyMatrix)
ClaimsTree = rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=ClaimsTrain, method="class", cp=0.00005, parms=list(loss=PenaltyMatrix))
#Now it will choose different splits to minimize the worst types of errors. We may get a lower
# accuracy, but a lower penalty error as well
#make predictions again, and accuracy/penalty lower
PredictTest = predict(ClaimsTree, newdata = ClaimsTest, type = 'class')
table(ClaimsTest$bucket2009, PredictTest)
(94310+18942+4692+636+2)/(nrow(ClaimsTest))
sum(as.matrix(table(ClaimsTest$bucket2009, PredictTest))*PenaltyMatrix)/nrow(ClaimsTest)
PenaltyMatrix
