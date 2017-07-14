"
MITx: 15.071x The Analytics Edge
Homework 4: Decision Trees
"
gerber = read.csv('gerber.csv')
summary(gerber)
mean(gerber$voting)
tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$self, mean)
tapply(gerber$voting, gerber$neighbors, mean)
summary(gerber)

#Logistic Regression model
Voting = glm(voting ~ civicduty + hawthorne + self + neighbors, data = gerber, family = binomial)
summary(Voting)

#preddictions with threshold, get accuracy 
votingPredictions = predict(Voting, type = 'response')
table(gerber$voting, votingPredictions >= 0.3)
#now calculate accuracy from the confusion matrix
(134513+51966)/(134513+51966+100875+56730)

#0.5 threshold
table(gerber$voting, votingPredictions >= 0.5)
235388/(235388+108696)

#auc calculation
library(ROCR)
ROCRpreds = prediction(votingPredictions, gerber$voting)
auc = as.numeric(performance(ROCRpreds, 'auc')@y.values)
auc

"
Now build a regression TREE. This is important because REGRESSION TREES are not a class.
A class tree would split with a threshold of .5, and we want a tree to converge to a number.

This will split people into different probabilities of voting. But how to know which splts?
"

CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data = gerber)
prp(CARTmodel)

#now with cp = 0.0
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + 
                     self + neighbors + sex, data = gerber, cp = 0.0)
prp(CARTmodel2)

#regress ion tree wit hjust control, then another with sex and control
justControl = rpart(voting ~ control, data=gerber, cp = 0.0)
bothIncl = rpart(voting ~ control + sex, data = gerber, cp = 0.0)

prp(justControl, digits = 6)
prp(bothIncl, digits = 6)

#new logistic regression
newGLM = glm(voting ~ sex + control, data = gerber, family = binomial)
summary(newGLM)

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(newGLM, newdata=Possibilities, type='response')

LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)
predict(newGLM, newdata=Possibilities, type="response")
predict(LogModel2, newdata=Possibilities, type="response")

290456 -290456

#part 2
letters = read.csv('letters_ABPR.csv')
letters$isB = as.factor(letters$letter == "B")

set.seed(1000)
split = sample.split(letters$isB, SplitRatio = 0.5)

Train = subset(letters, split == TRUE)
Test = subset(letters, split == FALSE)

summary(letters$isB)
2350/(2350+766)

CARTb = rpart(isB ~ . - letter, data = Train, method = 'class')
preds = predict(CARTb, newdata = Test, type = 'class')
table(Test$isB, preds)

#acc
(1118+340)/(1118+57+43+340)

#now using random forest
library(randomForest)
myRandomForest = randomForest(isB ~ . - letter, data = Train)
rfPreds = predict(myRandomForest, newdata = Test, type = 'class')
table(Test$isB, rfPreds)
(1164+373)/(1164+373+11+10)

#now we predict for all
#convert to factor first
letters$letter = as.factor( letters$letter )
set.seed(2000)
split = sample.split(letters$letter, SplitRatio = 0.5)
newTrain = subset(letters, split == TRUE)
newTest = subset(letters, split == FALSE)

#in mutli class classification, the simplest method is to predict the most common class
# as the baseline
table(letters$letter)
803/(803+758+766+789)

#classification tree for letter
letterCART = rpart(letter ~ . - isB, data = newTrain)
letPreds = predict(letterCART, newdata = newTest, type = 'class')

#accuracy of prediction using card
table(newTest$letter, letPreds)
(348+318+363+340)/nrow(newTest)

#now random forest!
set.seed(1000)
letRandomForst = randomForest(letter ~ . - isB - letter, data = newTest)
letRFPreds = predict(letRandomForst, newdata = newTest, type = 'class')

table(newTest$letter, letRFPreds)
(395+383+401+379)/nrow(newTest)

'
Part 3 - Predicting Earning from Census Data
'
census = read.csv('census.csv')
library(caTools)

#train  test split
set.seed(2000)
split = sample.split(census$over50k, SplitRatio = 0.6)
cenTrain = subset(census, split == TRUE)
cenTest = subset(census, split == FALSE)

fittyLM = glm(over50k ~ ., data = cenTrain, family = 'binomial')
summary(fittyLM)

#accuracy calculation of a logistic regression prob. calc
cenPredict = predict(fittyLM, newdata = cenTest, type = 'response')
table(cenTest$over50k, cenPredict >= 0.5)
(9051+1888)/(9051+1888+662+1190)
table(cenTest$over50k)
9713/(9713+3078)
library(ROCR)

aucPreds = prediction(cenPredict, cenTest$over50k)
as.numeric(performance(aucPreds, 'auc')@y.values)

#part 2, now for classification trees
library(rpart.plot)
#don't forget to set method  = class since this is a class prediction!
cenTree = rpart(over50k ~ ., data = cenTest, method = 'class')
prp(cenTree)

#now for decision tree accuracy calculation in R
treePreds = predict(cenTree, newdata = cenTest)
table(cenTest$over50k, treePreds)
(9421+1604)/(9421+1604+1474+472)

#get auc of the cart model
aucTree = prediction(treePreds[,2], cenTest$over50k)
as.numeric(performance(aucTree, 'auc')@y.values)

library(rpart)
#create random forest
cenForest = randomForest(over50k ~ ., data = cenTrain, method = 'class')

#to see in a random forest which variables are most used
#important!!!
vu = varUsed(cenForest, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(cenForest$forest$xlevels[vusorted$ix]))

#get random forst impurity metric
varImpPlot(cenForest)

#now for cross validation!
cpGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
library(caret)
library(e1071)
numFolds = trainControl(method= 'cv', number=10)
train(over50k ~ ., data = cenTrain, method='rpart', trControl = numFolds, tuneGrid = cpGrid)

#now fit a cart model
newCenTree = rpart(over50k ~ ., data = cenTrain, method = 'class', cp = 0.002)
#now for decision tree accuracy calculation in R
treePreds = predict(newCenTree, newdata = cenTest, type = 'class')
str(treePreds)
summary(treePreds)
table(cenTest$over50k, treePreds)
prp(newCenTree)


library(ggplot2)
library(ggjoy)

summary(diamonds)

ggplot(diamonds, aes(x=price, y=cut, group=cut)) +
  geom_joy(scale=4) + theme_joy() +
  scale_y_discrete(expand=c(0.01, 0)) +   # will generally have to set the `expand` option
  scale_x_continuous(expand=c(0, 0))      # for both axes to remove unneeded padding
