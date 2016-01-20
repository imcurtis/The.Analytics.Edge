#1.1
letters = read.csv("letters_ABPR.csv")
letters$isB = as.factor(letters$letter == "B")

library(caTools)
set.seed(1000)
split = sample.split(letters$isB, SplitRatio = 0.5)
lettersTrain = subset(letters, split==TRUE)
lettersTest = subset(letters, split==FALSE)

table(lettersTest$isB)

summary(letters)
2350/(766+2350)

#1.2
require(rpart)
library(rpart.plot)
CARTb = rpart(isB ~ . - letter, data=lettersTrain, method="class")

predCART = predict(CARTb, newdata = lettersTest, type="class")
table(lettersTest$isB, predCART)
(1118+340)/(1118+340+43+57)

#1.3
set.seed(1000)
require(randomForest)

bForest = randomForest(isB ~. - letter, data=letters)
predB = predict(bForest, newdata = lettersTest)
table(lettersTest$isB, predB)

#2.1
set.seed(2000)
letters$letter = as.factor(letters$letter)

lettersSplit = sample.split(letters$letter, SplitRatio = 0.5)
train2 = subset(letters, split==TRUE)
test2 = subset(letters, split==FALSE)

table(test2$letter)/nrow(test2)

#2.2
CARTc = rpart(letter ~ . - isB, data=train2, method="class")
predCART2 = predict(CARTc, newdata = test2, type="class")
table(test2$letter, predCART2)
(338+294+357+365)/nrow(test2)

#2.3
set.seed(1000)
require(randomForest)

cForest = randomForest(letter ~. - isB, data=train2)
predC = predict(cForest, newdata = test2)
table(test2$letter, predC)
(381+373+391+383)/nrow(test2)




