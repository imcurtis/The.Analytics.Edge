#1.1
clinical_trial <- read.csv("clinical_trial.csv", stringsAsFactors=FALSE)
max(nchar(clinical_trial$abstract))

#1.2
sum(nchar(clinical_trial$abstract) == 0)

#1.3
which.min(nchar(clinical_trial$title))
clinical_trial$title[1258]

#2.1
corpusTitle <- Corpus(VectorSource(clinical_trial$title))
corpusAbstract <- Corpus(VectorSource(clinical_trial$abstract))

corpusTitle = tm_map(corpusTitle, tolower)
corpusAbstract = tm_map(corpusAbstract, tolower)

corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)

corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)

corpusTitle = tm_map(corpusTitle, removeWords, c(stopwords("english")))
corpusAbstract = tm_map(corpusAbstract, removeWords, c(stopwords("english")))

corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)

dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

length(stopwords("english"))
str(dtmTitle)

str(dtmAbstract)

#2.3
colSums(dtmAbstract)
which.max(colSums(dtmAbstract))

#3.1
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))

colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

colnames(dtmTitle)
colnames(dtmAbstract)

#3.2
dtm = cbind(dtmTitle, dtmAbstract)

dtm$trial = clinical_trial$trial

#3.3
library(caTools)

set.seed(144)

split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)

train = subset(dtm, split==TRUE)
test = subset(dtm, split==FALSE)

table(train$trial)

#3.4
library(rpart)
library(rpart.plot)

trialCART = rpart(trial ~ ., data=train, method="class")

prp(trialCART)

#3.5
trialCART = glm(trial ~ ., data=train, family="binomial")

predictCART = predict(trialCART, newdata=train, type="class")

predTrain=predict(trialCART)[,2]

summary(predTrain)

#3.7
table(train$trial, predTrain >= 0.5)

(464+610)/(464+610+115+107)

464/(464+107)

610/(610+115)

#4.1
trialCART = glm(trial ~ ., data=train, family="binomial")

predTest = predict(trialCART, newdata=test, type="response")[,2]

table(test$trial, predTest >= 0.5)

(250+167)/(250+167+68+79)

#4.2
require(ROCR)
pred = prediction(predTest, test$trial)
auc <- as.numeric(performance(ROCRpred, "auc")@y.values)
auc







