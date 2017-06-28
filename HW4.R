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
