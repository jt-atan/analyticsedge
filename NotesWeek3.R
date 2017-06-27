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




