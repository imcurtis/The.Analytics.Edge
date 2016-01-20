gerber <- read.csv("gerber.csv")
str(gerber)

#1.1
#What proportion of people in this dataset voted in this election?
mean(gerber$voting)

#1.2

#Which of the four "treatment groups" had the largest percentage of people who actually 
#voted (voting = 1)?

VotingCivicDuty <- table(gerber$voting, gerber$civicduty)
12021/(12021+26197+96675+209191)
#0.03493624

VotingHawthorne <- table(gerber$voting, gerber$hawthorne)
VotingHawthorne
12316/(12316+96380+25888+209500)
#0.03579359

VotingSelf <- table(gerber$voting, gerber$self)
VotingSelf
13191/(13191+25027+95505+210361)
#0.03833657

VotingNeighbors <- table(gerber$voting, gerber$neighbors)
VotingNeighbors
14438/(14438+211625+23763+94258)
#0.04196068

#1.3

votes <- glm(voting ~ civicduty + hawthorne + neighbors + self, data=gerber, family="binomial")
summary(votes)

#1.4
#Using a threshold of 0.3, what is the accuracy of the logistic regression model?

preds <- predict(votes, type = "response")
table(gerber$voting, preds > .3)

(51966+134513)/(51966+134513+100875+56730)

#1.5
#Using a threshold of 0.5, what is the accuracy of the logistic regression model?

preds <- predict(votes, type = "response")
table(gerber$voting, preds > .5)
235388/(108696+235388)

#1.6
#baseline accuracy
table(gerber$voting)
235388/(108696+235388)

require(ROCR)
ROCRpred <- prediction(preds, gerber$voting)
auc <- as.numeric(performance(ROCRpred, "auc")@y.values)
auc

#2.1
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
#Plot the tree. What happens, and if relevant, why?

require(rpart)
require(rpart.plot)
rpart.plot(CARTmodel)
prp(CARTmodel)

#No variables are used (the tree is only a root node) - none of the variables make 
#a big enough effect to be split on.

#2.2
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

#What do you observe about the order of the splits?
#Neighbor is first, civic duty is last

#2.3
#.31

#2.4
CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)

#3.1
CARTmodel4 = rpart(voting ~ control, data=gerber, cp = 0.0)
prp(CARTmodel4, digits = 6)

CARTmodel4
abs(0.2966383 - 0.3400004)

#3.2
CARTmodel5 = rpart(voting ~ control + sex, data=gerber, cp = 0.0)
prp(CARTmodel5, digits = 6)

#determine who is affected more by NOT being in the control group
abs(0.290456 - 0.302795)
abs(0.334176 - 0.345818)
0.012339 - 0.011642

#3.3
#Going back to logistic regression now, create a model using "sex" and "control". 
#Interpret the coefficient for "sex":

controlandsex = glm(voting ~ control + sex, data=gerber, family="binomial")
controlandsex

summary(controlandsex)

#3.4
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(controlandsex, newdata=Possibilities, type="response")

.2908065-.290456

#3.5
controlandsex2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
controlandsex2


#3.6

predict(controlandsex2, newdata=Possibilities, type="response")


0.2904558 - 0.290456


