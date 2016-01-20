#1.1

census <- read.csv("census.csv")

set.seed(2000)

library(caTools)

split = sample.split(census$over50k, SplitRatio = 0.6)

train = subset(census, split == TRUE)

test = subset(census, split == FALSE)

mod = glm(over50k ~., data = train, family="binomial")

summary(mod)

#1.2

preds <- predict(mod, newdata = test, type= "response")
table(test$over50k, preds > 0.5)

length(preds)
length(test$over50k)
#1.3
table(test$over50k)

9713/nrow(test)

#1.4

require(ROCR)
ROCRpred <- prediction(preds, test$over50k)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = T, print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-.2, 1, .7))
auc <- as.numeric(performance(ROCRpred, "auc")@y.values)
auc

#2.1
require(rpart)
library(rpart.plot)
CARTCensus = rpart(over50k ~ ., data=train, method="class")
prp(CARTCensus)

#2.2-2.3

#2.4

CARTPredict <- predict(CARTCensus, newdata = test, type = "class")
table(test$over50k, CARTPredict)

(9255+1561)/nrow(test)

#2.5

CARTmodel <- rpart(over50k ~., data = train)
CART2 <- predict(CARTmodel, newdata = test)
table(test$over50k, CART2[,2] > 0.5)

(9255+1561)/nrow(test)

ROCRpred <- prediction(CART2[,2], test$over50k)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = T, print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-.2, 1, .7))
auc <- as.numeric(performance(ROCRpred, "auc")@y.values)
auc

#3.1
set.seed(1)

trainSmall = train[sample(nrow(train), 2000), ]

set.seed(1)
require(randomForest)

bForest = randomForest(over50k ~., data=trainSmall)
predB = predict(bForest, newdata = test)
table(test$over50k, predB)

(9427+1347)/nrow(test)

#3.2

vu = varUsed(bForest, count=TRUE)

vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)

dotchart(vusorted$x, names(bForest$forest$xlevels[vusorted$ix]))

#3.3

varImpPlot(bForest)

#4.1
library(caret)
library(e1071)
set.seed(2)

numfolds=trainControl(method="cv",number=10)
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
train(over50k~.,data=train,method="rpart",trControl=numfolds,tuneGrid=cartGrid)

#4.2

CARTmodel5 = rpart(over50k ~., data=train, cp = 0.002, method = "class")
prediction = predict(CARTmodel5, newdata = test, type = "class")
prp(CARTmodel5, digits = 6)

table(test$over50k, prediction)
(9114+1884)/nrow(test)

#4.3

summary(CARTmodel5)


