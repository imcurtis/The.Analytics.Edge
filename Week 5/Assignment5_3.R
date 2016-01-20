#1.1
emails <- read.csv("emails.csv", stringsAsFactors=FALSE)
str(emails)

#1.2
sum(emails$spam == 1)

#1.3
(emails$text[1])

#1.4
#No

#1.5
max(nchar(emails$text))

#1.6
which.min(nchar(emails$text))

#2.1
corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
dtm

#2.2
spdtm = removeSparseTerms(dtm, 0.95)
spdtm

#2.3
emailsSparse = as.data.frame(as.matrix(spdtm))

colnames(emailsSparse) = make.names(colnames(emailsSparse))
which.max(colSums(emailsSparse))

#2.4
emailsSparse$spam = emails$spam
sort(colSums(subset(emailsSparse, spam == 0)))
sort(colSums(subset(emailsSparse, spam == 0)))

#2.5
sort(colSums(subset(emailsSparse, spam == 1)))

#3.1
emailsSparse$spam = as.factor(emailsSparse$spam)

set.seed(123)

split = sample.split(emailsSparse$spam, SplitRatio = 0.7)

train = subset(emailsSparse, split==TRUE)
test = subset(emailsSparse, split==FALSE)

spamLog = glm(spam ~ ., data=train, family="binomial")
spamCART = rpart(spam ~ ., data=train, method="class")

library(randomForest)
set.seed(123)

spamRF = randomForest(Negative ~ ., data=train)

predictions = predict(spamLog, newdata=train, type="response")
table(predictions < 0.00001)
table(predictions > 0.99999)
table(predictions >= 0.00001 & predictions <= 0.99999)

predTrainCART = predict(spamCART)[,2]
predTrainRF = predict(spamRF, type="prob")[,2]

#3.2
summary(spamLog)

#3.3
library(rpart)
library(rpart.plot)
spamCART = rpart(spam ~ ., data=train, method="class")
predTrainCART = predict(spamCART)[,2]
prp(spamCART)

#3.4
table(train$spam, predictions > 0.5)
(3052+954)/(3052+954+4)

#3.5
require(ROCR)
pred = prediction(predictions, train$spam)
auc <- as.numeric(performance(pred, "auc")@y.values)
auc

#3.6
predTrainCART = predict(spamCART, type="class")
table(train$spam, predTrainCART)
(2877+896)/nrow(train)

#3.7

require(ROCR)
library(ROCR)
predictionTrainCART = prediction(predTrainCART, train$spam)

as.numeric(performance(predictionTrainCART, "auc")@y.values)
auc

#3.8










