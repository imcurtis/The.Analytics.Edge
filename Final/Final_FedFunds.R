##Problem 1
fedFunds <- read.csv("federalFundsRate.csv", stringsAsFactors=FALSE)
mean(fedFunds$RaisedFedFunds == "1")

##Problem 2
which.max(fedFunds$Chairman)
table(fedFunds$Chairman)
#Alan Greenspan

##Problem 3
as.factor(fedFunds$Chairman)
as.factor(fedFunds$DemocraticPres)
as.factor(fedFunds$RaisedFedFunds)
#RandomForest

##Problem 4
set.seed(201)

library(caTools)

spl = sample.split(fedFunds$RaisedFedFunds, 0.7)

training = subset(fedFunds, spl=TRUE)
testing = subset(fedFunds, spl=FALSE)

##Problem 5
glmTraining = glm(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection, data=training, family="binomial")
summary(glmTraining)

##Problem 6
newdata = data.frame(PreviousRate=1.7, Streak=-3, Unemployment=5.1, HomeownershipRate=65.3, DemocraticPres=1, MonthsUntilElection=18)
predict(glmTraining, newdata, type="response")

##Problem 7
#Option 3

##Problem 8
predictions = predict(glmTraining, newdata=testing, type="response")
predictions
table(predictions>0.5, testing$RaisedFedFunds)
table(training$RaisedFedFunds)

predictions1 = predict(glmTraining, type="response")
sign(20)
table(testing$RaisedFedFunds, predictions >= 0.5)
subset(testing, predictions)

##Problem 9
library(ROCR)
ROCRpred = prediction(predictions, testing$RaisedFedFunds)
as.numeric(performance(ROCRpred, "auc")@y.values)


##Problem 12

library(ROCR)
PredictROC = predict(glmTraining, newdata=testing)
pred = prediction(PredictROC, testing$RaisedFedFunds)
perf = performance(pred, "tpr", "fpr")
plot(perf, main="ROC curve", colorize=TRUE)
perf

##Problem 13
#Last choice

##Problem 14
set.seed(201)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
fitTing = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = (1:50)*0.001)
train(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection, data = training, method = "rpart", trControl = fitTing, tuneGrid = cartGrid )

##Problem 15
tree = rpart(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection, data = training, method = "class", control = rpart.control(cp = 0.016))
prp(tree)

##Problem 16
#Answered correctly

##Problem 17
predictions1 = predict(tree, newdata=testing, type="class")
table(predictions1, testing$RaisedFedFunds)






